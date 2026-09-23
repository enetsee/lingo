open StdLabels

type part =
  { regex : string
  ; scope : Scopes.Scope.t option
  }

type t =
  | Delimited of
      { open_ : Core.Token.def
      ; close : Core.Token.def
      ; sep : Core.Token.def option
      }
  | Region of
      { begin_regex : string
      ; begin_captures : (int * Scopes.Scope.t) list
      ; end_regex : string
      ; body_from : int
      ; use_identity : bool
      }
  | Capture of part list
  | Flat

(* -- reading a child ------------------------------------------------------- *)

let single_kind (child : Core.Rule.child) : Core.Kind.t option =
  match Array.length child.alts with
  | 1 -> Some child.alts.(0)
  | _ -> None
;;

let token_of_child (facts : Core.Facts.t) (child : Core.Rule.child)
  : Core.Token.def option
  =
  match single_kind child with
  | None -> None
  | Some kind -> Core.Facts.token_of_kind facts kind
;;

let rule_of_child (facts : Core.Facts.t) (child : Core.Rule.child) : Core.Rule.def option =
  match single_kind child with
  | None -> None
  | Some kind -> Core.Facts.rule_of_kind facts kind
;;

let is_required (child : Core.Rule.child) : bool =
  match child.modifier with
  | Core.Grammar.Exactly_one -> true
  | Core.Grammar.Zero_or_one | Core.Grammar.Zero_or_more | Core.Grammar.One_or_more ->
    false
;;

let is_plain (rule : Core.Rule.def) : bool =
  match rule.frame with
  | Core.Rule.Plain -> true
  | Core.Rule.Committed _ | Core.Rule.Delimited _ | Core.Rule.Separated _ -> false
;;

(* Only a [Plain] rule is walked through. A framed one is a region or a
   loop, and inlining its regex into a caller's single match would throw
   that away. The depth limit and the visited list guard against a rule
   graph that loops: the checker allows [A = B] and [B = A] as long as
   neither is left-recursive through a nullable. *)
let leaf_token (facts : Core.Facts.t) (rule : Core.Rule.def)
  : (Core.Rule.id * int * Core.Token.def) option
  =
  let rec loop ~(depth : int) ~(visited : Core.Rule.id list) (rule : Core.Rule.def)
    : (Core.Rule.id * int * Core.Token.def) option
    =
    if depth > 8 || List.mem rule.id ~set:visited || not (is_plain rule)
    then None
    else (
      match Array.length rule.children with
      | 1 when is_required rule.children.(0) ->
        let child = rule.children.(0) in
        (match token_of_child facts child with
         | Some token -> Some (rule.id, 0, token)
         | None ->
           (match rule_of_child facts child with
            | None -> None
            | Some next -> loop ~depth:(depth + 1) ~visited:(rule.id :: visited) next))
      | _ -> None)
  in
  loop ~depth:0 ~visited:[] rule
;;

(* -- the single-regex shape ------------------------------------------------ *)

(* A child that can be one capture group: its regex, and the scope that
   group gets.

   Where the child points at a chain of forwarding rules, the scope comes
   from the position at the end of the chain unless this position overrides
   it. So [Field.ty -> Type.name -> ident] picks up the scope set on
   [Type.name]. Resolving the token alone would colour every [ident] reached
   through any chain as a plain variable. *)
let part_of_child
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(index : int)
  : part option
  =
  let child = rule.children.(index) in
  if not (is_required child)
  then None
  else (
    match token_of_child facts child with
    | Some token ->
      (match Oniguruma.of_token token with
       | Error _ -> None
       | Ok regex ->
         Some
           { regex
           ; scope = Scopes.resolve scopes ~rule:rule.id ~child:index ~token:token.id
           })
    | None ->
      (match rule_of_child facts child with
       | None -> None
       | Some target ->
         (match leaf_token facts target with
          | None -> None
          | Some (leaf_rule, leaf_child, token) ->
            (match Oniguruma.of_token token with
             | Error _ -> None
             | Ok regex ->
               let scope =
                 match Scopes.child scopes rule.id ~child:index with
                 | Some scope -> Some scope
                 | None ->
                   Scopes.resolve scopes ~rule:leaf_rule ~child:leaf_child ~token:token.id
               in
               Some { regex; scope }))))
;;

(* This shape needs every child to inline, and more than one of them. One
   child needs no capture group: it is a pattern list of length one, and
   emitting it as a match would add a wrapper scope where a plain include
   reads better. *)
let capture_parts (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def)
  : part list option
  =
  let count = Array.length rule.children in
  if count < 2
  then None
  else (
    let parts = ref [] in
    let ok = ref true in
    for index = count - 1 downto 0 do
      if !ok
      then (
        match part_of_child facts scopes rule ~index with
        | Some part -> parts := part :: !parts
        | None -> ok := false)
    done;
    if !ok then Some !parts else None)
;;

(* -- the region shape ------------------------------------------------------ *)

(* The token a region opens on. An anchor has to be a fixed string, and a
   keyword or a literal spelled in punctuation is one. Opening a region on a
   pattern token would fire it at every identifier in the file. *)
let opening_token (facts : Core.Facts.t) (rule : Core.Rule.def) : Core.Token.def option =
  if Array.length rule.children = 0
  then None
  else (
    let child = rule.children.(0) in
    if not (is_required child)
    then None
    else (
      match token_of_child facts child with
      | None -> None
      | Some token ->
        (match token.klass with
         | Core.Grammar.Keyword _ | Core.Grammar.Punctuation _ -> Some token
         | Core.Grammar.Pattern _ -> None)))
;;

(* The text a region ends behind.

   A lookbehind has to be fixed width, so only a literal will do: either the
   rule's own last token, or the closer of a matched pair the last child
   opens. A trailing pattern token gives nothing. So does a trailing child
   that may be absent: an anchor the input never reaches leaves the region
   open to the end of the file. *)
let closing_text (facts : Core.Facts.t) (rule : Core.Rule.def) : string option =
  let count = Array.length rule.children in
  if count = 0
  then None
  else (
    let child = rule.children.(count - 1) in
    if not (is_required child)
    then None
    else (
      let literal_of (token : Core.Token.def) : string option = Core.Token.text token in
      match token_of_child facts child with
      | Some token -> literal_of token
      | None ->
        (match rule_of_child facts child with
         | None -> None
         | Some target ->
           (match target.frame with
            | Core.Rule.Delimited { close; _ } ->
              (match Core.Facts.token_of_kind facts close with
               | None -> None
               | Some token -> literal_of token)
            | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> None))))
;;

(* The region's [begin]: the opening token, and the identity child after it
   where the shape allows.

   A production names itself by one of its children, and that child's scope
   is the one a reader looks for: the function name in
   [entity.name.function]. That child stays out of the body. A body pattern
   is tried at every position, so the return type of [fn main() -> Foo]
   would otherwise come out as a function name too. In the [begin] regex it
   is a numbered group, and a group matches once. *)
let region_begin
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      (lead : Core.Token.def)
  : string * (int * Scopes.Scope.t) list * int * bool
  =
  let lead_regex =
    match Oniguruma.of_token_at_child facts lead with
    | Ok regex -> Oniguruma.neutralise regex
    | Error _ -> Oniguruma.escape (Option.value (Core.Token.text lead) ~default:"")
  in
  let lead_scope = Scopes.resolve scopes ~rule:rule.id ~child:0 ~token:lead.id in
  let alone () =
    let captures =
      match lead_scope with
      | Some scope -> [ 0, scope ]
      | None -> []
    in
    lead_regex, captures, 1, false
  in
  match rule.identity with
  | None ->
    ( lead_regex
    , (match lead_scope with
       | Some s -> [ 0, s ]
       | None -> [])
    , 1
    , true )
  | Some 0 ->
    ( lead_regex
    , (match lead_scope with
       | Some s -> [ 0, s ]
       | None -> [])
    , 1
    , true )
  | Some 1 when Array.length rule.children > 1 ->
    let child = rule.children.(1) in
    let folded =
      if not (is_required child)
      then None
      else (
        match part_of_child facts scopes rule ~index:1 with
        | None -> None
        | Some part ->
          (* [part_of_child] resolves through the chain, and the identity
             override sits above that: the author said what this production
             calls itself, not what the token at the end of the chain is. *)
          let scope =
            match Scopes.child scopes rule.id ~child:1 with
            | Some scope -> Some scope
            | None ->
              (match Scopes.identity scopes rule.id with
               | Some scope -> Some scope
               | None -> part.scope)
          in
          Some (part.regex, scope))
    in
    (match folded with
     | None -> alone ()
     | Some (identity_regex, identity_scope) ->
       let separator = Oniguruma.neutralise (Oniguruma.trivia_separator facts) in
       let regex =
         "("
         ^ lead_regex
         ^ ")"
         ^ separator
         ^ "("
         ^ Oniguruma.neutralise identity_regex
         ^ ")"
       in
       let captures =
         (match lead_scope with
          | Some scope -> [ 1, scope ]
          | None -> [])
         @
         match identity_scope with
         | Some scope -> [ 2, scope ]
         | None -> []
       in
       regex, captures, 2, true)
  | Some _ -> alone ()
;;

let region (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def) : t option =
  match opening_token facts rule, closing_text facts rule with
  | Some lead, Some closing ->
    let begin_regex, begin_captures, body_from, use_identity =
      region_begin facts scopes rule lead
    in
    Some
      (Region
         { begin_regex
         ; begin_captures
         ; end_regex = "(?<=" ^ Oniguruma.escape closing ^ ")"
         ; body_from
         ; use_identity
         })
  | _ -> None
;;

(* -- the decision ---------------------------------------------------------- *)

let of_rule (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def) : t =
  match rule.frame with
  | Core.Rule.Delimited { open_; close; sep; _ } ->
    let token (kind : Core.Kind.t) : Core.Token.def option =
      Core.Facts.token_of_kind facts kind
    in
    (match token open_, token close with
     | Some open_, Some close ->
       Delimited
         { open_
         ; close
         ; sep =
             (match sep with
              | None -> None
              | Some { sep_tok; _ } -> token sep_tok)
         }
     | _ -> Flat)
  | Core.Rule.Committed _ ->
    (match region facts scopes rule with
     | Some shape -> shape
     | None ->
       (match capture_parts facts scopes rule with
        | Some parts -> Capture parts
        | None -> Flat))
  | Core.Rule.Plain ->
    (match capture_parts facts scopes rule with
     | Some parts -> Capture parts
     | None -> Flat)
  | Core.Rule.Separated _ -> Flat
;;

(* -- forwarding ------------------------------------------------------------ *)

(* A child that leaves no trace in the emitted body: a token whose scope at
   this position is the one the grammar-wide token list already gives it. *)
let is_covered_elsewhere
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(index : int)
  : bool
  =
  let child = rule.children.(index) in
  let covered (kind : Core.Kind.t) : bool =
    match Core.Facts.token_of_kind facts kind with
    | None -> false
    | Some token ->
      Scopes.is_token_scope scopes ~rule:rule.id ~child:index ~token:token.id
  in
  Array.length child.alts > 0 && Array.for_all child.alts ~f:covered
;;

let forwards_to
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      ~(raw : Core.Rule.id -> bool)
      (rule : Core.Rule.def)
  : Core.Rule.id option
  =
  if (not (is_plain rule)) || raw rule.id || Scopes.rule scopes rule.id <> None
  then None
  else (
    let target = ref None in
    let blocked = ref false in
    Array.iteri rule.children ~f:(fun index (child : Core.Rule.child) ->
      if !blocked
      then ()
      else if not (is_required child)
      then blocked := true
      else (
        match rule_of_child facts child with
        | Some target_rule ->
          (match !target with
           | None -> target := Some target_rule.id
           | Some _ -> blocked := true)
        | None ->
          if not (is_covered_elsewhere facts scopes rule ~index) then blocked := true));
    if !blocked then None else !target)
;;
