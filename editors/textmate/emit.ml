open StdLabels

type ctx =
  { facts : Core.Facts.t
  ; scopes : Scopes.t
  ; language : string
  ; alias : Core.Rule.id -> Core.Rule.id
  ; raw : Core.Rule.id -> Yojson.Basic.t list
  ; clash : Clash.t
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

(* A position tied to the literal beside it, as one match.

   Before: [(data)\s+([A-Z]...)] matches only where [data] does, and scopes
   both. After: [(?:"...")(?=\s*:)] scopes the string only where a colon
   follows, and leaves the colon to whatever scopes it anyway. A lookbehind
   would do the first without consuming [data], but a lookbehind has to be
   fixed width, and the trivia between the two is not. *)
let pair_pattern (ctx : ctx) (pair : Clash.pair) : Yojson.Basic.t option =
  let separator = Oniguruma.neutralise (Oniguruma.trivia_separator ctx.facts) in
  match Oniguruma.of_token_at_child ctx.facts pair.token with
  | Error _ -> None
  | Ok regex ->
    (match pair.anchor with
     | Clash.Before { lead; lead_scope } ->
       (match Oniguruma.of_token lead with
        | Error _ -> None
        | Ok lead_regex ->
          let indexed =
            (match lead_scope with
             | Some lead_scope -> [ 1, lead_scope ]
             | None -> [])
            @ [ 2, pair.scope ]
          in
          Some
            (`Assoc
                [ ( "match"
                  , `String
                      ("("
                       ^ Oniguruma.neutralise lead_regex
                       ^ ")"
                       ^ separator
                       ^ "("
                       ^ Oniguruma.neutralise regex
                       ^ ")") )
                ; "captures", captures ctx indexed
                ]))
     | Clash.After next ->
       (match Oniguruma.of_token next with
        | Error _ -> None
        | Ok next_regex ->
          Some
            (`Assoc
                (named ctx (Some pair.scope)
                 @ [ ( "match"
                     , `String
                         ("(?:"
                          ^ Oniguruma.neutralise regex
                          ^ ")(?="
                          ^ separator
                          ^ "(?:"
                          ^ Oniguruma.neutralise next_regex
                          ^ "))") )
                   ]))))
;;

(* The pairs of every position a place tries, ahead of everything else in
   it. Anywhere later, a pattern spliced in earlier could take the text
   first. See {!Clash}. *)
let place_pairs (ctx : ctx) (place : Clash.place) : Yojson.Basic.t list =
  List.filter_map (Clash.pairs ctx.clash place) ~f:(pair_pattern ctx)
;;

(* A pattern for one token in one child position, or nothing where the
   grammar-wide token entry already says the same.

   Emitting both changes what matches. A per-child pattern is tried before
   that entry, and that entry puts a longer literal ahead of the shorter one
   it starts with. So a per-child [=] emitted beside an unchanged scope
   would claim the [=] of [==] at that position alone.

   A position tied to a literal is emitted at the front of each place that
   tries it, and nothing is emitted here. See {!Clash}. *)
let token_pattern
      (ctx : ctx)
      (rule : Core.Rule.def)
      (shape : Shape.t)
      ~(index : int)
      ~(token : Core.Token.def)
  : Yojson.Basic.t option
  =
  let scope = Shape.body_scope ctx.scopes rule shape ~index ~token:token.id in
  if Clash.is_paired ctx.clash ~rule:rule.id ~child:index ~token:token.id
  then None
  else (
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
      | Ok regex -> Some (`Assoc (named ctx scope @ [ "match", `String regex ]))))
;;

let child_patterns_at (ctx : ctx) (rule : Core.Rule.def) (shape : Shape.t) ~(index : int)
  : Yojson.Basic.t list
  =
  let child = rule.children.(index) in
  Array.to_list child.alts
  |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
    match Core.Facts.token_of_kind ctx.facts kind with
    | Some token -> token_pattern ctx rule shape ~index ~token
    | None ->
      (match Core.Facts.rule_of_kind ctx.facts kind with
       | Some target -> Some (rule_include ctx target.id)
       | None -> None))
;;

let child_patterns (ctx : ctx) (rule : Core.Rule.def) (shape : Shape.t) ~(from : int)
  : Yojson.Basic.t list
  =
  List.concat_map (Shape.emission_order ctx.facts rule ~from) ~f:(fun index ->
    child_patterns_at ctx rule shape ~index)
;;

(* The same for the children in [lo, hi], which is one side of a cut. *)
let child_patterns_in
      (ctx : ctx)
      (rule : Core.Rule.def)
      (shape : Shape.t)
      ~(lo : int)
      ~(hi : int)
  : Yojson.Basic.t list
  =
  Shape.emission_order ctx.facts rule ~from:lo
  |> List.filter ~f:(fun (index : int) -> index <= hi)
  |> List.concat_map ~f:(fun (index : int) -> child_patterns_at ctx rule shape ~index)
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

(* -- the shapes ------------------------------------------------------------ *)

(* One part of a cut region: the children from [lo] as far as the child it is
   cut at, behind whatever [inner] holds.

   A part opens only where the region around it does, at the start. Without
   that the first one opens again on its own keyword further on: the second
   [if] of [if ... else if ...] would be a second first part of the outer
   [if], where the parser has an [if] of its own inside the first.

   The first part takes what the region opens on. Every part around it takes
   nothing, and [taking] says which this is. A part that takes nothing stands
   where it opened, so an end that looks behind is kept off that place. *)
let part
      (ctx : ctx)
      (rule : Core.Rule.def)
      (shape : Shape.t)
      ~(opens : string)
      ~(taking : bool)
      ~(begin_captures : (int * Scopes.Scope.t) list)
      ~(lo : int)
      ~(split : Shape.split)
      ~(pairs : Yojson.Basic.t list)
      ~(inner : Yojson.Basic.t list)
  : Yojson.Basic.t
  =
  let close =
    match split.close with
    | Shape.Behind text ->
      [ "end", `String ((if taking then "" else "(?!\\G)") ^ Shape.behind text) ]
    | Shape.Literal token ->
      let regex =
        match Oniguruma.of_token_at_child ctx.facts token with
        | Ok regex -> regex
        | Error _ -> Oniguruma.escape (Option.value (Core.Token.text token) ~default:"")
      in
      [ "end", `String regex ]
      @ optional_field
          "endCaptures"
          (whole_match
             ctx
             (Shape.body_scope ctx.scopes rule shape ~index:split.upto ~token:token.id))
  in
  `Assoc
    ([ "begin", `String ("\\G" ^ opens) ]
     @ close
     @ (match begin_captures with
        | [] -> []
        | indexed -> [ "beginCaptures", captures ctx indexed ])
     @ [ ( "patterns"
         , `List
             (with_tails
                ctx
                (inner @ pairs @ child_patterns_in ctx rule shape ~lo ~hi:split.upto)) )
       ])
;;

(* A bracket of a matched pair, as the pair's own entry writes it. *)
let bracket (token : Core.Token.def) : string =
  match Oniguruma.of_token token with
  | Ok regex -> regex
  | Error _ -> Oniguruma.escape (Option.value (Core.Token.text token) ~default:"")
;;

(* The one regex of a single-match entry, a group per part. *)
let capture_regex (ctx : ctx) (parts : Shape.part list) : string =
  let separator = Oniguruma.neutralise (Oniguruma.trivia_separator ctx.facts) in
  String.concat
    ~sep:separator
    (List.map parts ~f:(fun (part : Shape.part) ->
       "(" ^ Oniguruma.neutralise part.regex ^ ")"))
;;

(* A region, whole or cut. A cut region opens where its first part does
   without taking anything, so the first part is the first thing it tries,
   and it cannot end where it opened. Each further cut is a part around the
   parts before it, which opens at the same place and ends at its own cut. *)
let region
      (ctx : ctx)
      (rule : Core.Rule.def)
      (shape : Shape.t)
      ~(raw : Yojson.Basic.t list)
      ~(begin_regex : string)
      ~(begin_captures : (int * Scopes.Scope.t) list)
      ~(end_regex : string)
      ~(body_from : int)
      ~(ahead : bool)
      ~(splits : Shape.split list)
  : Yojson.Basic.t
  =
  let pairs = place_pairs ctx (Clash.Body rule.id) in
  match splits with
  | [] ->
    (* A [begin] that takes nothing leaves the region where it opened, and
       an [end] that matched there would end it before anything was read. *)
    let end_regex = if ahead then "(?!\\G)" ^ end_regex else end_regex in
    `Assoc
      (named ctx (Some (wrap_scope ctx rule))
       @ [ "begin", `String begin_regex; "end", `String end_regex ]
       @ (match begin_captures with
          | [] -> []
          | indexed -> [ "beginCaptures", captures ctx indexed ])
       @ [ ( "patterns"
           , `List
               (with_tails
                  ctx
                  (pairs @ child_patterns ctx rule shape ~from:body_from @ raw)) )
         ])
  | first :: _ ->
    let parts, rest =
      List.fold_left
        splits
        ~init:([], body_from)
        ~f:(fun (inner, lo) (split : Shape.split) ->
          let innermost = split == first in
          ( [ part
                ctx
                rule
                shape
                ~opens:(if innermost then begin_regex else "")
                ~taking:(innermost && not ahead)
                ~begin_captures:(if innermost then begin_captures else [])
                ~lo
                ~split
                ~pairs
                ~inner
            ]
          , split.upto + 1 ))
    in
    let after =
      child_patterns_in ctx rule shape ~lo:rest ~hi:(Array.length rule.children - 1)
    in
    `Assoc
      (named ctx (Some (wrap_scope ctx rule))
       @ [ "begin", `String ("(?=" ^ begin_regex ^ ")")
         ; "end", `String ("(?!\\G)" ^ end_regex)
         ]
       @ [ "patterns", `List (with_tails ctx (parts @ pairs @ after @ raw)) ])
;;

let entry (ctx : ctx) (rule : Core.Rule.def) : Yojson.Basic.t =
  let raw = ctx.raw rule.id in
  let shape = Shape.of_rule ctx.facts ctx.scopes rule in
  match shape with
  | Shape.Delimited { open_; close; sep } ->
    let regex = bracket in
    let body =
      place_pairs ctx (Clash.Body rule.id)
      @ child_patterns ctx rule shape ~from:rule.body_from
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
  | Shape.Region { begin_regex; begin_captures; end_regex; body_from; ahead; splits; _ }
    ->
    region
      ctx
      rule
      shape
      ~raw
      ~begin_regex
      ~begin_captures
      ~end_regex
      ~body_from
      ~ahead
      ~splits
  | Shape.Open { begin_regex; begin_captures; body_from; ahead; until; splits; _ } ->
    let end_regex =
      "(?="
      ^ String.concat
          ~sep:"|"
          (List.map until ~f:(fun (token : Core.Token.def) ->
             match Oniguruma.of_token_at_child ctx.facts token with
             | Ok regex -> regex
             | Error _ ->
               Oniguruma.escape (Option.value (Core.Token.text token) ~default:"")))
      ^ ")"
    in
    region
      ctx
      rule
      shape
      ~raw
      ~begin_regex
      ~begin_captures
      ~end_regex
      ~body_from
      ~ahead
      ~splits
  | Shape.Tailed { from; lead; until } ->
    let regex (token : Core.Token.def) : string =
      match Oniguruma.of_token_at_child ctx.facts token with
      | Ok regex -> regex
      | Error _ -> Oniguruma.escape (Option.value (Core.Token.text token) ~default:"")
    in
    let ahead = "(?=" ^ String.concat ~sep:"|" (List.map until ~f:regex) ^ ")" in
    let tail =
      `Assoc
        ([ "begin", `String (regex lead); "end", `String ahead ]
         @ optional_field
             "beginCaptures"
             (whole_match
                ctx
                (Shape.body_scope ctx.scopes rule shape ~index:from ~token:lead.id))
         @ [ ( "patterns"
             , `List
                 (with_tails
                    ctx
                    (place_pairs ctx (Clash.Body rule.id)
                     @ child_patterns_in
                         ctx
                         rule
                         shape
                         ~lo:(from + 1)
                         ~hi:(Array.length rule.children - 1))) )
           ])
    in
    `Assoc
      [ ( "patterns"
        , `List (child_patterns_in ctx rule shape ~lo:0 ~hi:(from - 1) @ [ tail ] @ raw) )
      ]
  | Shape.Capture parts ->
    let regex = capture_regex ctx parts in
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
    `Assoc [ "patterns", `List (child_patterns ctx rule shape ~from:0 @ separator @ raw) ]
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

(* What sits inside an [Enclosed] postfix.

   Usually the block itself, as in [x(a, b)] where an argument is an
   expression. Where the author put a production there instead, a labelled
   argument or a statement in a trailing block, it is that production, and
   including the block would lose every scope the production carries. A
   content of tokens alone falls back to the block, whose tails colour
   them. *)
let content_includes (ctx : ctx) (block_key : string) (content : Core.Block.content)
  : Yojson.Basic.t list
  =
  let kinds =
    match content with
    | Core.Block.One kinds -> kinds
    | Core.Block.Many { elem; _ } -> elem
  in
  match
    Array.to_list kinds
    |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
      match Core.Facts.rule_of_kind ctx.facts kind with
      | Some target -> Some (rule_include ctx target.id)
      | None -> None)
  with
  | [] -> [ include_key block_key ]
  | includes -> includes
;;

(* [x(a, b)] and [x[i]]: a matched pair that recurses into the block.
   [behind] goes in front of the opener, and is what the opener has to stand
   behind. *)
let enclosed_pattern
      (ctx : ctx)
      (block_key : string)
      (postfix : Core.Block.postfix)
      ~(behind : string)
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
       (* The operand's own list trails the trivia and token entries, as the
          body of any pair does. An expression's entry trails neither. *)
       let inner =
         with_tails
           ctx
           (place_pairs ctx (Clash.Enclosed postfix.p_rule)
            @ content_includes ctx block_key content
            @ separator_pattern ctx sep)
       in
       Some
         (`Assoc
             (named ctx (Some (postfix_wrap ctx postfix.p_rule))
              @ [ "begin", `String (behind ^ open_regex); "end", `String close_regex ]
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

(* What an entry opens on, where it opens a region or is one match. A list
   opens on nothing of its own. *)
let opening (ctx : ctx) (rule : Core.Rule.def) : string option =
  match rule.origin with
  | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> None
  | Core.Rule.User ->
    (match Shape.of_rule ctx.facts ctx.scopes rule with
     | Shape.Delimited { open_; _ } -> Some (bracket open_)
     | Shape.Region { begin_regex; _ } | Shape.Open { begin_regex; _ } -> Some begin_regex
     | Shape.Capture parts -> Some (capture_regex ctx parts)
     | Shape.Tailed _ | Shape.Flat -> None)
;;

(* -- an operand and what is written behind it ------------------------------

   An operator that encloses its operand opens on a bracket, and so may an
   atom: [f(x)] is a call and [(x)] is a parenthesised expression. A list
   has no way to say which, since both stand in it and it tries the same
   patterns at every position.

   So an operand opens a region of its own, and the operators that follow an
   operand are tried in that region and nowhere else. It ends in front of
   the first thing that is none of them, whatever that is: [end] is a
   lookahead for anything at all, and it is tried after the operators. The
   region takes trivia too, so a call on the next line is still a call.

   A token atom opens the region by being taken. An atom that is a region
   or a single match opens one around itself: on a lookahead for what the
   atom opens on, so the atom is the first thing in it. The operators there
   refuse the position the region opened at, which is the atom's own
   bracket, and take every one after it.

   An atom that is a list is spliced as before. What it ends on is an
   operand of its own, or a token that takes no operator.

   An operator that encloses its operand has no pattern in the block's own
   list. A bracket met there stands behind no operand, so it is an atom's. *)

(* Every operator of a block that is written behind its operand. [later]
   keeps one that encloses its operand off the position its region opened
   at, and [enclosing] says whether those are wanted at all. *)
let postfix_patterns
      (ctx : ctx)
      (block : Core.Block.def)
      ~(enclosing : bool)
      ~(later : bool)
  : Yojson.Basic.t list
  =
  let block_key = Key.of_rule (Core.Facts.rule ctx.facts block.rule_id).name in
  Array.to_list block.postfix
  |> List.filter_map ~f:(fun (postfix : Core.Block.postfix) ->
    match postfix.p_body with
    | Core.Block.Nothing -> None
    | Core.Block.Then kinds -> access_pattern ctx postfix kinds
    | Core.Block.Enclosed _ when not enclosing -> None
    | Core.Block.Enclosed { close; content } ->
      enclosed_pattern
        ctx
        block_key
        postfix
        ~behind:(if later then "(?!\\G)" else "")
        ~close
        ~content)
;;

(* An operator that is one token, which the grammar-wide entry scopes in a
   list. Inside an operand's region it needs a pattern of its own, or the
   region would end at it and [x?(y)] would lose its call. *)
let bare_postfix_patterns (ctx : ctx) (block : Core.Block.def) : Yojson.Basic.t list =
  Array.to_list block.postfix
  |> List.filter_map ~f:(fun (postfix : Core.Block.postfix) ->
    match postfix.p_body, Core.Facts.token_of_kind ctx.facts postfix.p_lead with
    | Core.Block.Nothing, Some lead ->
      (match Oniguruma.of_token_at_child ctx.facts lead with
       | Ok regex ->
         Some
           (`Assoc
               (named ctx (Scopes.token ctx.scopes lead.id) @ [ "match", `String regex ]))
       | Error _ -> None)
    | (Core.Block.Nothing | Core.Block.Then _ | Core.Block.Enclosed _), _ -> None)
;;

let operand_region
      (ctx : ctx)
      (block : Core.Block.def)
      ~(opens : (string * Yojson.Basic.t) list)
      ~(later : bool)
      ~(atoms : Yojson.Basic.t list)
  : Yojson.Basic.t
  =
  let trivia = if has_trivia ctx.facts then [ include_key "trivia" ] else [] in
  `Assoc
    (opens
     @ [ "end", `String "(?=\\S)"
       ; "applyEndPatternLast", `Int 1
       ; ( "patterns"
         , `List
             (postfix_patterns ctx block ~enclosing:true ~later
              @ bare_postfix_patterns ctx block
              @ atoms
              @ trivia) )
       ])
;;

let block_entry (ctx : ctx) (block : Core.Block.def) : Yojson.Basic.t =
  let rule_of (kind : Core.Kind.t) : Core.Rule.def option =
    match Core.Facts.rule_of_kind ctx.facts kind with
    | Some target -> Some (Core.Facts.rule ctx.facts (ctx.alias target.id))
    | None -> None
  in
  let encloses =
    Array.exists block.postfix ~f:(fun (postfix : Core.Block.postfix) ->
      match postfix.p_body with
      | Core.Block.Enclosed _ -> true
      | Core.Block.Nothing | Core.Block.Then _ -> false)
  in
  let includes (wrapped : Core.Rule.def -> bool) : Yojson.Basic.t list =
    Array.to_list block.atoms
    |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
      match rule_of kind with
      | Some target when not (wrapped target) -> Some (rule_include ctx target.id)
      | Some _ | None -> None)
  in
  (* The entry trails neither the trivia nor the token entry. It is spliced
     into the list that holds the expression, and the token entry in the
     middle of that list would take the keyword of every region listed after
     it. The list that holds it trails both. *)
  if not encloses
  then
    `Assoc
      [ ( "patterns"
        , `List
            (includes (fun _ -> false)
             @ postfix_patterns ctx block ~enclosing:true ~later:false) )
      ]
  else (
    (* The atoms that open on something, each with what it opens on. *)
    let around =
      Array.to_list block.atoms
      |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
        match rule_of kind with
        | Some target ->
          (match opening ctx target with
           | Some regex -> Some (target, regex)
           | None -> None)
        | None -> None)
    in
    let is_around (rule : Core.Rule.def) : bool =
      List.exists around ~f:(fun ((other : Core.Rule.def), _) -> other.id = rule.id)
    in
    let around_region =
      match around with
      | [] -> []
      | around ->
        [ operand_region
            ctx
            block
            ~opens:
              [ ( "begin"
                , `String
                    ("(?="
                     ^ String.concat
                         ~sep:"|"
                         (List.map around ~f:(fun (_, regex) ->
                            "(?:" ^ Oniguruma.neutralise regex ^ ")"))
                     ^ ")") )
              ]
            ~later:true
            ~atoms:
              (List.map around ~f:(fun ((rule : Core.Rule.def), _) ->
                 rule_include ctx rule.id))
        ]
    in
    let tokens =
      Array.to_list block.atoms
      |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
        match Core.Facts.token_of_kind ctx.facts kind with
        | Some token ->
          (match Oniguruma.of_token_at_child ctx.facts token with
           | Ok regex -> Some (token, regex)
           | Error _ -> None)
        | None -> None)
    in
    let token_region =
      match tokens with
      | [] -> []
      | tokens ->
        let scoped =
          List.mapi tokens ~f:(fun index ((token : Core.Token.def), _) ->
            index + 1, Scopes.token ctx.scopes token.id)
          |> List.filter_map ~f:(fun (index, scope) ->
            match scope with
            | Some scope -> Some (index, scope)
            | None -> None)
        in
        [ operand_region
            ctx
            block
            ~opens:
              ([ ( "begin"
                 , `String
                     (String.concat
                        ~sep:"|"
                        (List.map tokens ~f:(fun (_, regex) ->
                           "(" ^ Oniguruma.neutralise regex ^ ")"))) )
               ]
               @
               match scoped with
               | [] -> []
               | scoped -> [ "beginCaptures", captures ctx scoped ])
            ~later:false
            ~atoms:[]
        ]
    in
    `Assoc
      [ ( "patterns"
        , `List
            (around_region
             @ includes is_around
             @ token_region
             @ postfix_patterns ctx block ~enclosing:false ~later:false) )
      ])
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
