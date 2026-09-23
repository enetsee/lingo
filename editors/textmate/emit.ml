open StdLabels

type ctx =
  { facts : Core.Facts.t
  ; scopes : Scopes.t
  ; language : string
  ; alias : Core.Rule.id -> Core.Rule.id
  ; raw : Core.Rule.id -> Yojson.Basic.t list
  }

let has_trivia (facts : Core.Facts.t) : bool =
  Array.exists Core.Facts.(facts.tokens) ~f:Core.Token.is_trivia
;;

(* -- fragments ------------------------------------------------------------- *)

let optional_field (key : string) (value : Yojson.Basic.t option)
  : (string * Yojson.Basic.t) list
  =
  match value with
  | Some value -> [ key, value ]
  | None -> []
;;

let scope_string (ctx : ctx) (scope : Scopes.Scope.t) : string =
  Scopes.Scope.to_string ~language:ctx.language scope
;;

let named (ctx : ctx) (scope : Scopes.Scope.t option) : (string * Yojson.Basic.t) list =
  match scope with
  | Some scope -> [ "name", `String (scope_string ctx scope) ]
  | None -> []
;;

(* The capture map for a regex whose whole match is the thing being
   scoped. *)
let whole_match (ctx : ctx) (scope : Scopes.Scope.t option) : Yojson.Basic.t option =
  match scope with
  | None -> None
  | Some scope -> Some (`Assoc [ "0", `Assoc (named ctx (Some scope)) ])
;;

let captures (ctx : ctx) (indexed : (int * Scopes.Scope.t) list) : Yojson.Basic.t =
  `Assoc
    (List.map indexed ~f:(fun (index, scope) ->
       string_of_int index, `Assoc (named ctx (Some scope))))
;;

let include_key (key : string) : Yojson.Basic.t =
  `Assoc [ "include", `String ("#" ^ key) ]
;;

let rule_include (ctx : ctx) (rule : Core.Rule.id) : Yojson.Basic.t =
  include_key (Key.of_rule (Core.Facts.rule ctx.facts (ctx.alias rule)).name)
;;

(* The scope a production's own span takes.

   Where the author set none, the name of the rule becomes one:
   [StructBody] is [meta.struct.body]. The trailing segment is dropped where
   it repeats the language, so a production named [Rust] in language [rust]
   is [meta.rust] rather than [meta.rust.rust]. *)
let wrap_scope (ctx : ctx) (rule : Core.Rule.def) : Scopes.Scope.t =
  match Scopes.rule ctx.scopes rule.id with
  | Some scope -> scope
  | None ->
    let dotted = Key.dotted rule.name in
    let language = String.lowercase_ascii ctx.language in
    let width = String.length dotted in
    let tail = String.length language in
    if String.equal dotted language
    then Scopes.Scope.Custom "meta"
    else if
      width > tail + 1
      && dotted.[width - tail - 1] = '.'
      && String.equal (String.sub dotted ~pos:(width - tail) ~len:tail) language
    then Scopes.Scope.Meta (String.sub dotted ~pos:0 ~len:(width - tail - 1))
    else Scopes.Scope.Meta dotted
;;

(* -- per-child patterns ---------------------------------------------------- *)

(* [use_identity] is false inside a region whose [begin] could not take the
   identity child. See {!Shape.Region}. *)
let scope_at
      (ctx : ctx)
      (rule : Core.Rule.def)
      ~(index : int)
      ~(token : Core.Token.id)
      ~(use_identity : bool)
  : Scopes.Scope.t option
  =
  if (not use_identity) && rule.identity = Some index
  then Scopes.token ctx.scopes token
  else Scopes.resolve ctx.scopes ~rule:rule.id ~child:index ~token
;;

(* A pattern for one token in one child position, or nothing where the
   grammar-wide token entry already says the same.

   Emitting both changes what matches. A per-child pattern is tried before
   that entry, and that entry puts a longer literal ahead of the shorter one
   it starts with. So a per-child [=] emitted beside an unchanged scope
   would claim the [=] of [==] at that position alone. *)
let token_pattern
      (ctx : ctx)
      (rule : Core.Rule.def)
      ~(index : int)
      ~(token : Core.Token.def)
      ~(use_identity : bool)
  : Yojson.Basic.t option
  =
  let scope = scope_at ctx rule ~index ~token:token.id ~use_identity in
  let redundant =
    match scope, Scopes.token ctx.scopes token.id with
    | Some here, Some there -> Scopes.Scope.equal here there
    | _ -> false
  in
  if redundant
  then None
  else (
    match Oniguruma.of_token_at_child ctx.facts token with
    | Error _ -> None
    | Ok regex -> Some (`Assoc (named ctx scope @ [ "match", `String regex ])))
;;

let child_patterns_at
      (ctx : ctx)
      (rule : Core.Rule.def)
      ~(index : int)
      ~(use_identity : bool)
  : Yojson.Basic.t list
  =
  let child = rule.children.(index) in
  Array.to_list child.alts
  |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
    match Core.Facts.token_of_kind ctx.facts kind with
    | Some token -> token_pattern ctx rule ~index ~token ~use_identity
    | None ->
      (match Core.Facts.rule_of_kind ctx.facts kind with
       | Some target -> Some (rule_include ctx target.id)
       | None -> None))
;;

(* Children that point at a matched pair go first.

   A pattern list has no precedence beyond the order it is written in, and
   an open-ended child can reach the same opening token as a bracketed
   sibling. A production holding a scrutinee expression and a braced body
   would otherwise let the expression take the [{]. *)
let child_patterns (ctx : ctx) (rule : Core.Rule.def) ~(from : int) ~(use_identity : bool)
  : Yojson.Basic.t list
  =
  let opens_a_pair (index : int) : bool =
    match Shape.rule_of_child ctx.facts rule.children.(index) with
    | None -> false
    | Some target ->
      (match target.frame with
       | Core.Rule.Delimited _ -> true
       | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> false)
  in
  let indices =
    List.init ~len:(Array.length rule.children - from) ~f:(fun i -> i + from)
  in
  let bracketed, rest = List.partition ~f:opens_a_pair indices in
  List.concat_map (bracketed @ rest) ~f:(fun index ->
    child_patterns_at ctx rule ~index ~use_identity)
;;

(* The separator of a framed body is not a child, so nothing above emits it.

   The grammar-wide entry already covers a separator that carries a scope,
   and a second pattern for the same token would compete with the body's own
   end at the same position. A separator with no scope takes a bare match,
   which keeps the body from leaving it to whatever pattern reaches it
   first. *)
let separator_pattern (ctx : ctx) (sep : Core.Token.def option) : Yojson.Basic.t list =
  match sep with
  | None -> []
  | Some sep ->
    (match Scopes.token ctx.scopes sep.id with
     | Some _ -> []
     | None ->
       (match Oniguruma.of_token sep with
        | Error _ -> []
        | Ok regex -> [ `Assoc [ "match", `String regex ] ]))
;;

let with_tails (ctx : ctx) (patterns : Yojson.Basic.t list) : Yojson.Basic.t list =
  let trivia = if has_trivia ctx.facts then [ include_key "trivia" ] else [] in
  patterns @ trivia @ [ include_key "tokens" ]
;;

(* -- the four shapes ------------------------------------------------------- *)

let entry (ctx : ctx) (rule : Core.Rule.def) : Yojson.Basic.t =
  let raw = ctx.raw rule.id in
  match Shape.of_rule ctx.facts ctx.scopes rule with
  | Shape.Delimited { open_; close; sep } ->
    let regex (token : Core.Token.def) : string =
      match Oniguruma.of_token token with
      | Ok regex -> regex
      | Error _ -> Oniguruma.escape (Option.value (Core.Token.text token) ~default:"")
    in
    let body =
      child_patterns ctx rule ~from:rule.body_from ~use_identity:true
      @ separator_pattern ctx sep
      @ raw
    in
    `Assoc
      (named ctx (Some (wrap_scope ctx rule))
       @ [ "begin", `String (regex open_); "end", `String (regex close) ]
       @ optional_field
           "beginCaptures"
           (whole_match ctx (Scopes.token ctx.scopes open_.id))
       @ optional_field "endCaptures" (whole_match ctx (Scopes.token ctx.scopes close.id))
       @ [ "patterns", `List (with_tails ctx body) ])
  | Shape.Region { begin_regex; begin_captures; end_regex; body_from; use_identity } ->
    let body = child_patterns ctx rule ~from:body_from ~use_identity @ raw in
    `Assoc
      (named ctx (Some (wrap_scope ctx rule))
       @ [ "begin", `String begin_regex; "end", `String end_regex ]
       @ (match begin_captures with
          | [] -> []
          | indexed -> [ "beginCaptures", captures ctx indexed ])
       @ [ "patterns", `List (with_tails ctx body) ])
  | Shape.Capture parts ->
    let separator = Oniguruma.neutralise (Oniguruma.trivia_separator ctx.facts) in
    let regex =
      String.concat
        ~sep:separator
        (List.map parts ~f:(fun (part : Shape.part) ->
           "(" ^ Oniguruma.neutralise part.regex ^ ")"))
    in
    let indexed =
      List.mapi parts ~f:(fun index (part : Shape.part) -> index + 1, part.scope)
      |> List.filter_map ~f:(fun (index, scope) ->
        match scope with
        | Some scope -> Some (index, scope)
        | None -> None)
    in
    `Assoc
      (named ctx (Some (wrap_scope ctx rule))
       @ [ "match", `String regex ]
       @
       match indexed with
       | [] -> []
       | indexed -> [ "captures", captures ctx indexed ])
  | Shape.Flat ->
    (* A flat entry splices into whichever list includes it, and every list
       that does already trails the trivia and token entries. Repeating them
       here would emit the same two patterns once per hop. *)
    let separator =
      match rule.frame with
      | Core.Rule.Separated { sep_tok; _ } ->
        separator_pattern ctx (Core.Facts.token_of_kind ctx.facts sep_tok)
      | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Delimited _ -> []
    in
    `Assoc
      [ ( "patterns"
        , `List (child_patterns ctx rule ~from:0 ~use_identity:true @ separator @ raw) )
      ]
;;

(* -- the expression block -------------------------------------------------- *)

(* An operator's operand, where it takes exactly one symbol and that symbol
   reaches a token. A chain of forwarding rules is walked through, so
   [x.f] scopes the [f] by the position at the end of the chain. *)
let operand (ctx : ctx) (kinds : Core.Kind.t array)
  : (string * Scopes.Scope.t option) option
  =
  if Array.length kinds <> 1
  then None
  else (
    match Core.Facts.token_of_kind ctx.facts kinds.(0) with
    | Some token ->
      (match Oniguruma.of_token token with
       | Error _ -> None
       | Ok regex -> Some (regex, Scopes.token ctx.scopes token.id))
    | None ->
      (match Core.Facts.rule_of_kind ctx.facts kinds.(0) with
       | None -> None
       | Some target ->
         (match Shape.leaf_token ctx.facts target with
          | None -> None
          | Some (leaf_rule, leaf_child, token) ->
            (match Oniguruma.of_token token with
             | Error _ -> None
             | Ok regex ->
               Some
                 ( regex
                 , Scopes.resolve
                     ctx.scopes
                     ~rule:leaf_rule
                     ~child:leaf_child
                     ~token:token.id )))))
;;

let postfix_wrap (ctx : ctx) (rule : Core.Rule.id) : Scopes.Scope.t =
  wrap_scope ctx (Core.Facts.rule ctx.facts rule)
;;

(* [x.f]: the operator token and what follows it, as one match with two
   groups. A pattern list would scope every identifier in the block as a
   member, since a list is tried at every position with nothing to tie the
   identifier to the dot. *)
let access_pattern (ctx : ctx) (postfix : Core.Block.postfix) (kinds : Core.Kind.t array)
  : Yojson.Basic.t option
  =
  match Core.Facts.token_of_kind ctx.facts postfix.p_lead, operand ctx kinds with
  | Some lead, Some (rhs_regex, rhs_scope) ->
    (match Oniguruma.of_token lead with
     | Error _ -> None
     | Ok lead_regex ->
       let separator = Oniguruma.neutralise (Oniguruma.trivia_separator ctx.facts) in
       let regex =
         "("
         ^ Oniguruma.neutralise lead_regex
         ^ ")"
         ^ separator
         ^ "("
         ^ Oniguruma.neutralise rhs_regex
         ^ ")"
       in
       let indexed =
         (match Scopes.token ctx.scopes lead.id with
          | Some scope -> [ 1, scope ]
          | None -> [])
         @
         match rhs_scope with
         | Some scope -> [ 2, scope ]
         | None -> []
       in
       Some
         (`Assoc
             (named ctx (Some (postfix_wrap ctx postfix.p_rule))
              @ [ "match", `String regex ]
              @
              match indexed with
              | [] -> []
              | indexed -> [ "captures", captures ctx indexed ])))
  | _ -> None
;;

(* [x(a, b)] and [x[i]]: a matched pair that recurses into the block. *)
let enclosed_pattern
      (ctx : ctx)
      (block_key : string)
      (postfix : Core.Block.postfix)
      ~(close : Core.Kind.t)
      ~(content : Core.Block.content)
  : Yojson.Basic.t option
  =
  match
    ( Core.Facts.token_of_kind ctx.facts postfix.p_lead
    , Core.Facts.token_of_kind ctx.facts close )
  with
  | Some open_, Some close ->
    (match Oniguruma.of_token open_, Oniguruma.of_token close with
     | Ok open_regex, Ok close_regex ->
       let sep =
         match content with
         | Core.Block.One _ -> None
         | Core.Block.Many { sep = None; _ } -> None
         | Core.Block.Many { sep = Some { sep_tok; _ }; _ } ->
           Core.Facts.token_of_kind ctx.facts sep_tok
       in
       let inner = [ include_key block_key ] @ separator_pattern ctx sep in
       Some
         (`Assoc
             (named ctx (Some (postfix_wrap ctx postfix.p_rule))
              @ [ "begin", `String open_regex; "end", `String close_regex ]
              @ optional_field
                  "beginCaptures"
                  (whole_match ctx (Scopes.token ctx.scopes open_.id))
              @ optional_field
                  "endCaptures"
                  (whole_match ctx (Scopes.token ctx.scopes close.id))
              @ [ "patterns", `List inner ]))
     | _ -> None)
  | _ -> None
;;

let block_entry (ctx : ctx) (block : Core.Block.def) : Yojson.Basic.t =
  let block_key = Key.of_rule (Core.Facts.rule ctx.facts block.rule_id).name in
  let atoms =
    Array.to_list block.atoms
    |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
      match Core.Facts.rule_of_kind ctx.facts kind with
      | Some target -> Some (rule_include ctx target.id)
      | None -> None)
  in
  let postfix =
    Array.to_list block.postfix
    |> List.filter_map ~f:(fun (postfix : Core.Block.postfix) ->
      match postfix.p_body with
      | Core.Block.Nothing -> None
      | Core.Block.Then kinds -> access_pattern ctx postfix kinds
      | Core.Block.Enclosed { close; content } ->
        enclosed_pattern ctx block_key postfix ~close ~content)
  in
  `Assoc [ "patterns", `List (with_tails ctx (atoms @ postfix)) ]
;;

(* -- the two grammar-wide entries ------------------------------------------ *)

let trivia_entry (ctx : ctx) : Yojson.Basic.t option =
  let patterns =
    Array.to_list Core.Facts.(ctx.facts.tokens)
    |> List.filter ~f:Core.Token.is_trivia
    |> List.filter_map ~f:(fun (token : Core.Token.def) ->
      match Oniguruma.of_token token with
      | Error _ -> None
      | Ok regex ->
        Some
          (`Assoc
              (named ctx (Scopes.token ctx.scopes token.id) @ [ "match", `String regex ])))
  in
  match patterns with
  | [] -> None
  | patterns -> Some (`Assoc [ "patterns", `List patterns ])
;;

(* A token used only as one half of a matched pair is left out.

   The pair's own rule scopes it, in the [begin] or the [end], and a second
   pattern for the same token competes with that [end] at the same position.
   A token that is also an ordinary child somewhere has occurrences outside
   any pair, so it stays: leaving it out would leave those unscoped. *)
let bracket_only (facts : Core.Facts.t) : Core.Kind.t list =
  let module Kinds = Set.Make (Core.Kind) in
  let brackets = ref Kinds.empty in
  let children = ref Kinds.empty in
  Array.iter
    Core.Facts.(facts.rules)
    ~f:(fun (rule : Core.Rule.def) ->
      (match rule.frame with
       | Core.Rule.Delimited { open_; close; _ } ->
         brackets := Kinds.add open_ (Kinds.add close !brackets)
       | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> ());
      Array.iter rule.children ~f:(fun (child : Core.Rule.child) ->
        Array.iter child.alts ~f:(fun (kind : Core.Kind.t) ->
          children := Kinds.add kind !children)));
  Array.iter
    Core.Facts.(facts.blocks)
    ~f:(fun (block : Core.Block.def) ->
      Array.iter block.postfix ~f:(fun (postfix : Core.Block.postfix) ->
        match postfix.p_body with
        | Core.Block.Enclosed { close; _ } ->
          brackets := Kinds.add postfix.p_lead (Kinds.add close !brackets)
        | Core.Block.Nothing | Core.Block.Then _ -> ()));
  Kinds.elements (Kinds.diff !brackets !children)
;;

let tokens_entry (ctx : ctx) : Yojson.Basic.t =
  let excluded = bracket_only ctx.facts in
  let carried =
    Array.to_list Core.Facts.(ctx.facts.tokens)
    |> List.filter ~f:(fun (token : Core.Token.def) ->
      (not (Core.Token.is_trivia token))
      && not (List.exists excluded ~f:(Core.Kind.equal token.kind)))
  in
  let literals, patterns =
    List.partition carried ~f:(fun (token : Core.Token.def) ->
      Core.Token.text token <> None)
  in
  let width (token : Core.Token.def) : int =
    String.length (Option.value (Core.Token.text token) ~default:"")
  in
  let literals =
    List.stable_sort literals ~cmp:(fun a b -> Int.compare (width b) (width a))
  in
  let patterns_of (token : Core.Token.def) : Yojson.Basic.t option =
    let scope = Scopes.token ctx.scopes token.id in
    (* A pattern token with no scope is left out. It is open ended, and
       consuming it here would take text a body pattern was going to reach.
       A literal with no scope is kept. It stops an identifier regex from
       swallowing a keyword nobody coloured. *)
    match scope, Core.Token.text token with
    | None, None -> None
    | _ ->
      (match Oniguruma.of_token token with
       | Error _ -> None
       | Ok regex -> Some (`Assoc (named ctx scope @ [ "match", `String regex ])))
  in
  `Assoc [ "patterns", `List (List.filter_map (literals @ patterns) ~f:patterns_of) ]
;;
