open StdLabels

let capture (scope : Scopes.Scope.t) : string option =
  match scope with
  | Scopes.Scope.Keyword_control _ | Scopes.Scope.Keyword_other _ -> Some "keyword"
  | Scopes.Scope.Keyword_operator_arithmetic
  | Scopes.Scope.Keyword_operator_comparison
  | Scopes.Scope.Keyword_operator_logical
  | Scopes.Scope.Keyword_operator_assignment
  | Scopes.Scope.Keyword_operator_bitwise
  | Scopes.Scope.Keyword_operator_other _ -> Some "operator"
  | Scopes.Scope.Storage_type _ | Scopes.Scope.Storage_modifier -> Some "keyword"
  | Scopes.Scope.Entity_name_function -> Some "function"
  | Scopes.Scope.Entity_name_type_struct
  | Scopes.Scope.Entity_name_type_enum
  | Scopes.Scope.Entity_name_type_trait
  | Scopes.Scope.Entity_name_type_class
  | Scopes.Scope.Entity_name_type _ -> Some "type"
  | Scopes.Scope.Variable_parameter -> Some "variable.parameter"
  | Scopes.Scope.Variable_language -> Some "variable.builtin"
  | Scopes.Scope.Variable_other -> Some "variable"
  | Scopes.Scope.Variable_other_member -> Some "variable.member"
  | Scopes.Scope.String_quoted_single
  | Scopes.Scope.String_quoted_double
  | Scopes.Scope.String_quoted_triple
  | Scopes.Scope.String_quoted_other _ -> Some "string"
  | Scopes.Scope.Comment_line_slashes
  | Scopes.Scope.Comment_line_hash
  | Scopes.Scope.Comment_line_other _
  | Scopes.Scope.Comment_block -> Some "comment"
  | Scopes.Scope.Constant_numeric_integer
  | Scopes.Scope.Constant_numeric_float
  | Scopes.Scope.Constant_numeric_hex
  | Scopes.Scope.Constant_numeric_other -> Some "number"
  | Scopes.Scope.Constant_language -> Some "constant.builtin"
  | Scopes.Scope.Punctuation_separator | Scopes.Scope.Punctuation_accessor ->
    Some "punctuation.delimiter"
  | Scopes.Scope.Punctuation_definition _ -> Some "punctuation.special"
  | Scopes.Scope.Punctuation_section _
  | Scopes.Scope.Punctuation_section_begin _
  | Scopes.Scope.Punctuation_section_end _ -> Some "punctuation.bracket"
  | Scopes.Scope.Meta _ -> None
  | Scopes.Scope.Custom _ -> None
;;

(* A literal, spelled the way a query file spells one. The quote, the
   backslash and the three whitespace escapes are the only ones written. A
   query file is UTF-8, so an arrow goes in as an arrow. *)
let quote (text : string) : string =
  let buf = Buffer.create (String.length text + 2) in
  Buffer.add_char buf '"';
  String.iter text ~f:(fun c ->
    match c with
    | '"' | '\\' ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf c
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c);
  Buffer.add_char buf '"';
  Buffer.contents buf
;;

(* Every kind the emitted grammar can match.

   A token can be declared and never reach the structure: a resync anchor is
   one, because it says where recovery stops rather than what a production
   holds, and recovery is the half tree-sitter does not take. A query naming
   a literal the grammar never produces is dead text in a file people read. *)
let matched_kinds (facts : Core.Facts.t) : Core.Kind.Set.t =
  let seen = ref Core.Kind.Set.empty in
  let note (kind : Core.Kind.t) : unit = seen := Core.Kind.Set.add kind !seen in
  let note_all (kinds : Core.Kind.t array) : unit = Array.iter kinds ~f:note in
  Array.iter
    Core.Facts.(facts.tokens)
    ~f:(fun (token : Core.Token.def) ->
      if Core.Token.is_trivia token then note token.kind);
  Array.iter
    Core.Facts.(facts.rules)
    ~f:(fun (rule : Core.Rule.def) ->
      (match rule.frame with
       | Core.Rule.Delimited { open_; close; sep; _ } ->
         note open_;
         note close;
         (match sep with
          | Some { sep_tok; _ } -> note sep_tok
          | None -> ())
       | Core.Rule.Separated { sep_tok; _ } -> note sep_tok
       | Core.Rule.Plain | Core.Rule.Committed _ -> ());
      Array.iter rule.children ~f:(fun (child : Core.Rule.child) -> note_all child.alts));
  Array.iter
    Core.Facts.(facts.blocks)
    ~f:(fun (block : Core.Block.def) ->
      note_all block.atoms;
      Array.iter block.prefix ~f:(fun (op : Core.Block.op) -> note op.op_kind);
      Array.iter block.infix ~f:(fun (op : Core.Block.op) -> note op.op_kind);
      Array.iter block.postfix ~f:(fun (postfix : Core.Block.postfix) ->
        note postfix.p_lead;
        match postfix.p_body with
        | Core.Block.Nothing -> ()
        | Core.Block.Then kinds -> note_all kinds
        | Core.Block.Enclosed { close; content } ->
          note close;
          (match content with
           | Core.Block.One kinds -> note_all kinds
           | Core.Block.Many { elem; sep } ->
             note_all elem;
             (match sep with
              | Some { sep_tok; _ } -> note sep_tok
              | None -> ()))));
  !seen
;;

(* A literal token is an anonymous node, named in a query by its own text.
   Everything else is a named node, named by its rule. *)
let pattern_for (facts : Core.Facts.t) (kind : Core.Kind.t) : string option =
  match Core.Facts.token_of_kind facts kind with
  | Some token ->
    (match Core.Token.text token with
     | Some text -> Some (quote text)
     | None -> Some ("(" ^ Node.of_token token.name ^ ")"))
  | None ->
    (match Core.Facts.rule_of_kind facts kind with
     | Some rule -> Some ("(" ^ Node.of_rule rule.name ^ ")")
     | None -> None)
;;

let single (child : Core.Rule.child) : Core.Kind.t option =
  match Array.length child.alts with
  | 1 -> Some child.alts.(0)
  | _ -> None
;;

let is_emitted (rule : Core.Rule.def) : bool =
  match rule.origin with
  | Core.Rule.User | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> true
;;

(* -- highlights.scm -------------------------------------------------------- *)

(* A capture on one child position of one rule.

   [(struct name: (ident) @type)] says: the [ident] in the [name] field of a
   [struct], and nothing else. A per-position scope exists for exactly this,
   and tree-sitter states it directly where TextMate had to fold the
   position into a regex. *)
let position_patterns (facts : Core.Facts.t) (scopes : Scopes.t) : string list =
  Array.to_list Core.Facts.(facts.rules)
  |> List.filter ~f:is_emitted
  |> List.concat_map ~f:(fun (rule : Core.Rule.def) ->
    List.init ~len:(Array.length rule.children) ~f:(fun index -> index)
    |> List.filter_map ~f:(fun index ->
      let child = rule.children.(index) in
      let scope =
        match Scopes.child scopes rule.id ~child:index with
        | Some scope -> Some scope
        | None ->
          if rule.identity = Some index then Scopes.identity scopes rule.id else None
      in
      match scope, single child with
      | None, _ | _, None -> None
      | Some scope, Some kind ->
        (match capture scope, pattern_for facts kind with
         | Some capture, Some inner ->
           Some
             (Printf.sprintf
                "(%s %s: %s @%s)"
                (Node.of_rule rule.name)
                (Core.Grammar.Name.Child.to_string child.child_name)
                inner
                capture)
         | _ -> None)))
;;

(* A capture on a whole node, where the author scoped the rule itself. *)
let rule_patterns (facts : Core.Facts.t) (scopes : Scopes.t) : string list =
  Array.to_list Core.Facts.(facts.rules)
  |> List.filter ~f:is_emitted
  |> List.filter_map ~f:(fun (rule : Core.Rule.def) ->
    match Scopes.rule scopes rule.id with
    | None -> None
    | Some scope ->
      (match capture scope with
       | None -> None
       | Some capture -> Some (Printf.sprintf "(%s) @%s" (Node.of_rule rule.name) capture)))
;;

(* The catch-alls: what a token is called wherever nothing more specific
   applies. *)
let token_patterns (facts : Core.Facts.t) (scopes : Scopes.t) : string list =
  let matched = matched_kinds facts in
  Array.to_list Core.Facts.(facts.tokens)
  |> List.filter ~f:(fun (token : Core.Token.def) -> Core.Kind.Set.mem matched token.kind)
  |> List.filter_map ~f:(fun (token : Core.Token.def) ->
    match Scopes.token scopes token.id with
    | None -> None
    | Some scope ->
      (match capture scope, Core.Token.text token with
       | None, _ -> None
       | Some capture, Some text -> Some (Printf.sprintf "%s @%s" (quote text) capture)
       | Some capture, None ->
         Some (Printf.sprintf "(%s) @%s" (Node.of_token token.name) capture)))
;;

let section (title : string) (patterns : string list) : string =
  match patterns with
  | [] -> ""
  | patterns -> "; " ^ title ^ "\n" ^ String.concat ~sep:"\n" patterns ^ "\n\n"
;;

let highlights (scopes : Scopes.t) : string =
  let facts = Scopes.facts scopes in
  "; Generated by lingo from the grammar. Do not edit by hand.\n\
   ;\n\
   ; The first pattern that matches a node wins, so what is written for one\n\
   ; position comes before what is written for the token itself.\n\n"
  ^ section "one position of one rule" (position_patterns facts scopes)
  ^ section "a whole node" (rule_patterns facts scopes)
  ^ section "every occurrence of a token" (token_patterns facts scopes)
;;

(* -- folds.scm ------------------------------------------------------------- *)

(* A matched pair is a fold region, and the grammar already says which rules
   have one. Nothing is declared for this file. *)
let folds (scopes : Scopes.t) : string =
  let facts = Scopes.facts scopes in
  let regions =
    Array.to_list Core.Facts.(facts.rules)
    |> List.filter ~f:is_emitted
    |> List.filter_map ~f:(fun (rule : Core.Rule.def) ->
      match rule.frame with
      | Core.Rule.Delimited _ ->
        Some (Printf.sprintf "(%s) @fold" (Node.of_rule rule.name))
      | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> None)
  in
  "; Generated by lingo from the grammar. Do not edit by hand.\n\
   ;\n\
   ; A rule framed by a matched pair is a fold region.\n\n"
  ^
  match regions with
  | [] -> ""
  | regions -> String.concat ~sep:"\n" regions ^ "\n"
;;
