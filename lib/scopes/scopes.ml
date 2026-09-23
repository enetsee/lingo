open StdLabels
module Scope = Scope

type override =
  | Token of
      { token : Core.Grammar.Name.Token.t
      ; scope : Scope.t
      }
  | Rule of
      { rule : Core.Grammar.Name.Rule.t
      ; scope : Scope.t
      }
  | Identity of
      { rule : Core.Grammar.Name.Rule.t
      ; scope : Scope.t
      }
  | Child of
      { rule : Core.Grammar.Name.Rule.t
      ; child : Core.Grammar.Name.Child.t
      ; scope : Scope.t
      }

type finding =
  | Unknown_token of Core.Grammar.Name.Token.t
  | Unknown_rule of Core.Grammar.Name.Rule.t
  | Unknown_child of
      { rule : Core.Grammar.Name.Rule.t
      ; child : Core.Grammar.Name.Child.t
      }
  | No_identity_child of Core.Grammar.Name.Rule.t
  | Malformed_scope of
      { override : string
      ; reason : string
      }

(* Rendered as the author typed it, so a finding can be matched against the
   grammar file by eye. *)
let override_to_string (override : override) : string =
  match override with
  | Token { token; _ } ->
    Printf.sprintf "Token %S" (Core.Grammar.Name.Token.to_string token)
  | Rule { rule; _ } -> Printf.sprintf "Rule %S" (Core.Grammar.Name.Rule.to_string rule)
  | Identity { rule; _ } ->
    Printf.sprintf "Identity %S" (Core.Grammar.Name.Rule.to_string rule)
  | Child { rule; child; _ } ->
    Printf.sprintf
      "Child %S/%S"
      (Core.Grammar.Name.Rule.to_string rule)
      (Core.Grammar.Name.Child.to_string child)
;;

let finding_to_string (finding : finding) : string =
  match finding with
  | Unknown_token name ->
    Printf.sprintf "no token named %S" (Core.Grammar.Name.Token.to_string name)
  | Unknown_rule name ->
    Printf.sprintf "no production named %S" (Core.Grammar.Name.Rule.to_string name)
  | Unknown_child { rule; child } ->
    Printf.sprintf
      "production %S has no child named %S"
      (Core.Grammar.Name.Rule.to_string rule)
      (Core.Grammar.Name.Child.to_string child)
  | No_identity_child name ->
    Printf.sprintf
      "production %S names itself by no child, so an identity scope on it reaches nothing"
      (Core.Grammar.Name.Rule.to_string name)
  | Malformed_scope { override; reason } -> Printf.sprintf "%s: %s" override reason
;;

let pp_finding (fmt : Format.formatter) (finding : finding) : unit =
  Format.pp_print_string fmt (finding_to_string finding)
;;

(* Findings sort by what they are and then by the name they carry, so a
   grammar gives the same list every run and a test can write it down. *)
let finding_rank (finding : finding) : int =
  match finding with
  | Malformed_scope _ -> 0
  | Unknown_token _ -> 1
  | Unknown_rule _ -> 2
  | Unknown_child _ -> 3
  | No_identity_child _ -> 4
;;

let compare_finding (a : finding) (b : finding) : int =
  let rank = Int.compare (finding_rank a) (finding_rank b) in
  if rank <> 0 then rank else String.compare (finding_to_string a) (finding_to_string b)
;;

type t =
  { facts : Core.Facts.t
  ; token_scope : Scope.t option array (* Indexed by token id. *)
  ; rule_scope : Scope.t option array (* Indexed by rule id. *)
  ; identity_scope : Scope.t option array (* Indexed by rule id. *)
  ; child_scope : Scope.t option array array (* Indexed by rule id, then child. *)
  }

let facts (t : t) : Core.Facts.t = t.facts

(* -- the derived defaults -------------------------------------------------- *)

(* The characters a scope path admits. A derived segment comes from a name
   the author wrote, and a token name may hold an apostrophe, which is an
   OCaml identifier character and never a scope one. Substituting keeps a
   legal grammar from being rejected over a scope the author never wrote. An
   author's own segment is reported instead; see [check_scope]. *)
let sanitise (segment : string) : string =
  String.map segment ~f:(fun c ->
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> c
    | _ -> '_')
;;

let starts_with (s : string) ~(prefix : string) : bool =
  String.length s >= String.length prefix
  && String.equal (String.sub s ~pos:0 ~len:(String.length prefix)) prefix
;;

let common_prefix (a : string) (b : string) : string =
  let limit = min (String.length a) (String.length b) in
  let shared = ref 0 in
  while !shared < limit && a.[!shared] = b.[!shared] do
    incr shared
  done;
  String.sub a ~pos:0 ~len:!shared
;;

(* The literal text a regex has to start with, as far as it is forced.

   [//[^\n]*] gives ["//"], and so does [//[^/\n][^\n]*|//], because both
   arms of the alternation start the same way. [(a|b)c] gives [""].

   The predecessor could not walk the term. It rendered the regex to
   Oniguruma and read the string back, so the result moved whenever the
   renderer's escaping did. The second element of the pair says whether the
   term was literal all the way through, so a sequence can carry on past
   it. *)
let literal_prefix (regex : Redfa.Regex.t) : string =
  let rec walk (regex : Redfa.Regex.t) : string * bool =
    match regex with
    | Redfa.Regex.Eps -> "", true
    | Redfa.Regex.Chars set when Ucharset.is_singleton set ->
      (match Ucharset.min_elt_opt set with
       | Some codepoint ->
         let buf = Buffer.create 4 in
         Buffer.add_utf_8_uchar buf (Uchar.of_int codepoint);
         Buffer.contents buf, true
       | None -> "", false)
    | Redfa.Regex.Seq items ->
      List.fold_left items ~init:("", true) ~f:(fun (acc, carry) item ->
        if not carry
        then acc, false
        else (
          let text, whole = walk item in
          acc ^ text, whole))
    | Redfa.Regex.Alt (first :: rest) ->
      ( List.fold_left
          rest
          ~init:(fst (walk first))
          ~f:(fun acc item -> common_prefix acc (fst (walk item)))
      , false )
    | _ -> "", false
  in
  fst (walk regex)
;;

(* Whether the token's text can hold a newline.

   A block comment and a line comment differ here and nowhere reliable
   else. A line comment's regex cannot match a newline, because a token is a
   lexeme and line termination belongs to the formatter. Matching on the
   token's name instead would read [block] and [line] in one grammar and
   [comment] and [doc_comment] in the next.

   The decision procedure is unbounded in general, so it runs under a budget.
   A comment regex settles in a handful of states. Anything that does not is
   treated as a line comment, and the prefix test below still applies to
   that shape. *)
let admits_newline (regex : Redfa.Regex.t) : bool =
  let with_newline =
    Redfa.Regex.seqs
      [ Redfa.Regex.star Redfa.Regex.any
      ; Redfa.Regex.singleton_char '\n'
      ; Redfa.Regex.star Redfa.Regex.any
      ]
  in
  match
    Redfa.Regex.is_empty_language_within
      ~max_states:1000
      (Redfa.Regex.inter regex with_newline)
  with
  | Some empty -> not empty
  | None -> false
;;

(* What a trivia token is called.

   [Reformat] trivia is whitespace. The formatter drops it and re-emits the
   spacing, so there is nothing to colour. [Preserve] trivia is a comment.
   The class exists for comments, and only the shape of this one is left to
   work out. *)
let trivia_scope (token : Core.Token.def) : Scope.t option =
  match token.trivia with
  | None -> None
  | Some Core.Grammar.Reformat -> None
  | Some Core.Grammar.Preserve ->
    if admits_newline token.regex
    then Some Scope.Comment_block
    else (
      let prefix = literal_prefix token.regex in
      if starts_with prefix ~prefix:"//"
      then Some Scope.Comment_line_slashes
      else if starts_with prefix ~prefix:"#"
      then Some Scope.Comment_line_hash
      else
        Some
          (Scope.Comment_line_other
             (sanitise (Core.Grammar.Name.Token.to_string token.name))))
;;

(* What a token is doing in the grammar, where that is one thing.

   A comma and a closing brace are both punctuation, and a theme colours
   them differently. The facts say which one a token is. A brace is a brace
   because a rule is framed by it, and a plus is an operator because
   a block's table holds it. The token's own declaration gives none of this:
   it says how the bytes are matched and nothing else.

   [Opens] and [Closes] are claimed only where the token does nothing else.
   A brace that is also an ordinary child somewhere occurs outside any frame,
   and one scope has to cover every occurrence. *)
type role =
  | Operator (** In a block's prefix or infix table, or a postfix with no body. *)
  | Accessor (** The lead of a postfix that takes one symbol after it, as in [x.f]. *)
  | Opens
  | Closes
  | Ordinary

let token_roles (facts : Core.Facts.t) : role array =
  let count = Array.length Core.Facts.(facts.tokens) in
  let operator = Array.make count false in
  let accessor = Array.make count false in
  let opens = Array.make count false in
  let closes = Array.make count false in
  let ordinary = Array.make count false in
  let mark (flags : bool array) (kind : Core.Kind.t) : unit =
    match Core.Facts.token_of_kind facts kind with
    | Some token -> flags.(token.id) <- true
    | None -> ()
  in
  let mark_all (flags : bool array) (kinds : Core.Kind.t array) : unit =
    Array.iter kinds ~f:(mark flags)
  in
  Array.iter
    Core.Facts.(facts.rules)
    ~f:(fun (rule : Core.Rule.def) ->
      (match rule.frame with
       | Core.Rule.Delimited { open_; close; sep; _ } ->
         mark opens open_;
         mark closes close;
         (match sep with
          | Some { sep_tok; _ } -> mark ordinary sep_tok
          | None -> ())
       | Core.Rule.Separated { sep_tok; _ } -> mark ordinary sep_tok
       | Core.Rule.Plain | Core.Rule.Committed _ -> ());
      Array.iter rule.children ~f:(fun (child : Core.Rule.child) ->
        mark_all ordinary child.alts));
  Array.iter
    Core.Facts.(facts.blocks)
    ~f:(fun (block : Core.Block.def) ->
      let op (op : Core.Block.op) : unit = mark operator op.op_kind in
      Array.iter block.prefix ~f:op;
      Array.iter block.infix ~f:op;
      Array.iter block.postfix ~f:(fun (postfix : Core.Block.postfix) ->
        match postfix.p_body with
        | Core.Block.Nothing -> mark operator postfix.p_lead
        | Core.Block.Then kinds ->
          mark accessor postfix.p_lead;
          mark_all ordinary kinds
        | Core.Block.Enclosed { close; content } ->
          mark opens postfix.p_lead;
          mark closes close;
          (match content with
           | Core.Block.One kinds -> mark_all ordinary kinds
           | Core.Block.Many { elem; sep } ->
             mark_all ordinary elem;
             (match sep with
              | Some { sep_tok; _ } -> mark ordinary sep_tok
              | None -> ()))));
  Array.init count ~f:(fun id ->
    if operator.(id)
    then Operator
    else if accessor.(id)
    then Accessor
    else if opens.(id) && (not closes.(id)) && not ordinary.(id)
    then Opens
    else if closes.(id) && (not opens.(id)) && not ordinary.(id)
    then Closes
    else Ordinary)
;;

(* The bracket a literal stands for, for the three shapes a theme names.
   Anything else keeps the section prefix and takes the token's own name as
   the shape, so a theme rule on [punctuation.section] still fires on a
   grammar bracketed by guillemets. *)
let section_scope (token : Core.Token.def) ~(opening : bool) : Scope.t =
  let side = if opening then "begin" else "end" in
  let named (section : Scope.section) : Scope.t =
    if opening
    then Scope.Punctuation_section_begin section
    else Scope.Punctuation_section_end section
  in
  match Core.Token.text token with
  | Some "(" | Some ")" -> named Scope.Parens
  | Some "[" | Some "]" -> named Scope.Brackets
  | Some "{" | Some "}" -> named Scope.Braces
  | _ ->
    Scope.Custom
      ("punctuation.section."
       ^ sanitise (Core.Grammar.Name.Token.to_string token.name)
       ^ "."
       ^ side)
;;

(* The scope a token gets where the author sets none.

   A keyword's segment is its token name. The name and the text agree
   wherever the text is an identifier, which is the ordinary case. They
   differ only where the text could not be a name: [kw ~name:"arrow" "->"]
   would otherwise render [keyword.other.->].

   A pattern token gets nothing, whatever it is doing. [ident] and [number]
   have one shape in a grammar and two colours on a page. Guessing from the
   name would quote a grammar whose token is called [string_of_int]. *)
let default_token_scope (role : role) (token : Core.Token.def) : Scope.t option =
  let named () : string = sanitise (Core.Grammar.Name.Token.to_string token.name) in
  match token.trivia with
  | Some _ -> trivia_scope token
  | None ->
    (match token.klass, role with
     | Core.Grammar.Pattern _, _ -> None
     | _, Operator -> Some (Scope.Keyword_operator_other (named ()))
     | Core.Grammar.Keyword _, _ -> Some (Scope.Keyword_other (Some (named ())))
     | Core.Grammar.Punctuation _, Accessor -> Some Scope.Punctuation_accessor
     | Core.Grammar.Punctuation _, Opens -> Some (section_scope token ~opening:true)
     | Core.Grammar.Punctuation _, Closes -> Some (section_scope token ~opening:false)
     | Core.Grammar.Punctuation _, Ordinary -> Some Scope.Punctuation_separator)
;;

(* -- resolving the overrides ----------------------------------------------- *)

let find_token (facts : Core.Facts.t) (name : Core.Grammar.Name.Token.t)
  : Core.Token.def option
  =
  Array.find_opt
    Core.Facts.(facts.tokens)
    ~f:(fun (token : Core.Token.def) -> Core.Grammar.Name.Token.equal token.name name)
;;

(* Every rule, including the shapes an expression block desugars into, so
   an override can name one of those. *)
let find_rule (facts : Core.Facts.t) (name : Core.Grammar.Name.Rule.t)
  : Core.Rule.def option
  =
  Array.find_opt
    Core.Facts.(facts.rules)
    ~f:(fun (rule : Core.Rule.def) -> Core.Grammar.Name.Rule.equal rule.name name)
;;

let child_index (rule : Core.Rule.def) (name : Core.Grammar.Name.Child.t) : int option =
  let found = ref None in
  Array.iteri rule.children ~f:(fun index (child : Core.Rule.child) ->
    if !found = None && Core.Grammar.Name.Child.equal child.child_name name
    then found := Some index);
  !found
;;

let check_scope (override : override) (scope : Scope.t) : finding option =
  match Scope.check scope with
  | None -> None
  | Some reason ->
    Some (Malformed_scope { override = override_to_string override; reason })
;;

let of_facts (facts : Core.Facts.t) ~(overrides : override list)
  : (t, finding list) result
  =
  let rules = Core.Facts.(facts.rules) in
  let tokens = Core.Facts.(facts.tokens) in
  let t =
    { facts
    ; token_scope =
        (let roles = token_roles facts in
         Array.map tokens ~f:(fun (token : Core.Token.def) ->
           default_token_scope roles.(token.id) token))
    ; rule_scope = Array.make (Array.length rules) None
    ; identity_scope = Array.make (Array.length rules) None
    ; child_scope =
        Array.map rules ~f:(fun (rule : Core.Rule.def) ->
          Array.make (Array.length rule.children) None)
    }
  in
  let findings = ref [] in
  let report (finding : finding) : unit = findings := finding :: !findings in
  (* A malformed scope is reported and dropped. Storing it would push the
     failure into whichever backend rendered it first, where the override
     that carries it is no longer to hand. *)
  let place (override : override) (scope : Scope.t) (set : unit -> unit) : unit =
    match check_scope override scope with
    | Some finding -> report finding
    | None -> set ()
  in
  let with_rule (name : Core.Grammar.Name.Rule.t) (k : Core.Rule.def -> unit) : unit =
    match find_rule facts name with
    | None -> report (Unknown_rule name)
    | Some rule -> k rule
  in
  List.iter overrides ~f:(fun (override : override) ->
    match override with
    | Token { token = name; scope } ->
      (match find_token facts name with
       | None -> report (Unknown_token name)
       | Some token ->
         place override scope (fun () -> t.token_scope.(token.id) <- Some scope))
    | Rule { rule = name; scope } ->
      with_rule name (fun rule ->
        place override scope (fun () -> t.rule_scope.(rule.id) <- Some scope))
    | Identity { rule = name; scope } ->
      with_rule name (fun rule ->
        match rule.identity with
        | None -> report (No_identity_child name)
        | Some _ ->
          place override scope (fun () -> t.identity_scope.(rule.id) <- Some scope))
    | Child { rule = name; child; scope } ->
      with_rule name (fun rule ->
        match child_index rule child with
        | None -> report (Unknown_child { rule = name; child })
        | Some index ->
          place override scope (fun () -> t.child_scope.(rule.id).(index) <- Some scope)));
  match List.sort_uniq ~cmp:compare_finding !findings with
  | [] -> Ok t
  | findings -> Error findings
;;

(* -- reading --------------------------------------------------------------- *)

let token (t : t) (token : Core.Token.id) : Scope.t option = t.token_scope.(token)
let rule (t : t) (rule : Core.Rule.id) : Scope.t option = t.rule_scope.(rule)
let identity (t : t) (rule : Core.Rule.id) : Scope.t option = t.identity_scope.(rule)

let child (t : t) (rule : Core.Rule.id) ~(child : int) : Scope.t option =
  t.child_scope.(rule).(child)
;;

let resolve (t : t) ~(rule : Core.Rule.id) ~(child : int) ~(token : Core.Token.id)
  : Scope.t option
  =
  match t.child_scope.(rule).(child) with
  | Some scope -> Some scope
  | None ->
    let def = Core.Facts.rule t.facts rule in
    if def.identity = Some child
    then (
      match t.identity_scope.(rule) with
      | Some scope -> Some scope
      | None -> t.token_scope.(token))
    else t.token_scope.(token)
;;

let is_token_scope (t : t) ~(rule : Core.Rule.id) ~(child : int) ~(token : Core.Token.id)
  : bool
  =
  match resolve t ~rule ~child ~token, t.token_scope.(token) with
  | Some here, Some there -> Scope.equal here there
  | _ -> false
;;

(* -- printing -------------------------------------------------------------- *)

let pp_scope (fmt : Format.formatter) (scope : Scope.t option) : unit =
  match scope with
  | None -> Format.pp_print_string fmt "-"
  | Some scope -> Scope.pp fmt scope
;;

let pp (fmt : Format.formatter) (t : t) : unit =
  Format.fprintf fmt "tokens@.";
  Array.iter
    Core.Facts.(t.facts.tokens)
    ~f:(fun (token : Core.Token.def) ->
      Format.fprintf
        fmt
        "  %-20s %a@."
        (Core.Grammar.Name.Token.to_string token.name)
        pp_scope
        t.token_scope.(token.id));
  Format.fprintf fmt "rules@.";
  Array.iter
    Core.Facts.(t.facts.rules)
    ~f:(fun (def : Core.Rule.def) ->
      Format.fprintf
        fmt
        "  %-20s %a"
        (Core.Grammar.Name.Rule.to_string def.name)
        pp_scope
        t.rule_scope.(def.id);
      (match def.identity with
       | Some _ -> Format.fprintf fmt " identity=%a" pp_scope t.identity_scope.(def.id)
       | None -> ());
      Format.fprintf fmt "@.";
      Array.iteri def.children ~f:(fun index (child : Core.Rule.child) ->
        match t.child_scope.(def.id).(index) with
        | None -> ()
        | Some scope ->
          Format.fprintf
            fmt
            "    %-18s %a@."
            (Core.Grammar.Name.Child.to_string child.child_name)
            Scope.pp
            scope))
;;
