open StdLabels

type part =
  { regex : string
  ; scope : Scopes.Scope.t option
  }

type close =
  | Literal of Core.Token.def
  | Behind of string

type split =
  { upto : int
  ; close : close
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
      ; ahead : bool
      ; splits : split list
      }
  | Capture of part list
  | Open of
      { begin_regex : string
      ; begin_captures : (int * Scopes.Scope.t) list
      ; body_from : int
      ; use_identity : bool
      ; ahead : bool
      ; until : Core.Token.def list
      ; splits : split list
      }
  | Tailed of
      { from : int
      ; lead : Core.Token.def
      ; until : Core.Token.def list
      }
  | Flat

type conflict =
  | Shares of
      { token : Core.Token.def
      ; children : int list
      }
  | Closes of
      { child : int
      ; text : string
      }

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
  | Core.Grammar.Zero_or_one | Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _ ->
    false
;;

let is_plain (rule : Core.Rule.def) : bool =
  match rule.frame with
  | Core.Rule.Plain -> true
  | Core.Rule.Committed _ | Core.Rule.Delimited _ | Core.Rule.Separated _ -> false
;;

(* Only a [Plain] rule the author wrote is walked through. A framed one is a
   region or a loop, and inlining its regex into a caller's single match
   would throw that away. An expression block is no single token either,
   whatever its one atom is: [(f : int)] fits one match and
   [(f : int -> int)] does not. The depth limit and the visited list guard
   against a rule graph that loops: the checker allows [A = B] and [B = A]
   as long as neither is left-recursive through a nullable. *)
let leaf_token (facts : Core.Facts.t) (rule : Core.Rule.def)
  : (Core.Rule.id * int * Core.Token.def) option
  =
  let rec loop ~(depth : int) ~(visited : Core.Rule.id list) (rule : Core.Rule.def)
    : (Core.Rule.id * int * Core.Token.def) option
    =
    let written =
      match rule.origin with
      | Core.Rule.User -> true
      | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> false
    in
    if depth > 8 || List.mem rule.id ~set:visited || (not (is_plain rule)) || not written
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

(* Children that point at a matched pair go first.

   A pattern list has no precedence beyond the order it is written in, and
   an open-ended child can reach the same opening token as a bracketed
   sibling. A production holding a scrutinee expression and a braced body
   would otherwise let the expression take the [{]. *)
let emission_order (facts : Core.Facts.t) (rule : Core.Rule.def) ~(from : int) : int list =
  let opens_a_pair (index : int) : bool =
    match rule_of_child facts rule.children.(index) with
    | None -> false
    | Some target ->
      (match target.frame with
       | Core.Rule.Delimited _ -> true
       | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> false)
  in
  let indices =
    List.init ~len:(max 0 (Array.length rule.children - from)) ~f:(fun i -> i + from)
  in
  let bracketed, rest = List.partition ~f:opens_a_pair indices in
  bracketed @ rest
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

(* What a rule ends on, whichever way it is derived. [Loops] is a rule met
   again on the way down, which ends as the rule being asked about does. *)
type ending =
  | Ends of string
  | Loops
  | Open

let rec ending_of_kind
          (facts : Core.Facts.t)
          ~(visited : Core.Rule.id list)
          (kind : Core.Kind.t)
  : ending
  =
  let literal (token : Core.Token.def option) : ending =
    match Option.bind token Core.Token.text with
    | Some text -> Ends text
    | None -> Open
  in
  match Core.Facts.token_of_kind facts kind with
  | Some token -> literal (Some token)
  | None ->
    (match Core.Facts.rule_of_kind facts kind with
     | None -> Open
     | Some target ->
       if List.mem target.id ~set:visited
       then Loops
       else (
         match target.frame, target.origin with
         | Core.Rule.Delimited { close; _ }, _ ->
           literal (Core.Facts.token_of_kind facts close)
         | (Core.Rule.Plain | Core.Rule.Committed _), Core.Rule.User ->
           let count = Array.length target.children in
           if count = 0
           then Open
           else
             ending_of_child
               facts
               ~visited:(target.id :: visited)
               target.children.(count - 1)
         | Core.Rule.Separated _, _
         | ( (Core.Rule.Plain | Core.Rule.Committed _)
           , (Core.Rule.Pratt_block | Core.Rule.Pratt_role _) ) -> Open))

(* Every alternative has to end on the one text. One that loops back agrees
   with the rest, and a child of nothing but those ends on nothing known. *)
and ending_of_child
      (facts : Core.Facts.t)
      ~(visited : Core.Rule.id list)
      (child : Core.Rule.child)
  : ending
  =
  if not (is_required child)
  then Open
  else
    Array.fold_left
      child.alts
      ~init:Loops
      ~f:(fun (sofar : ending) (kind : Core.Kind.t) ->
        match sofar, ending_of_kind facts ~visited kind with
        | Open, _ | _, Open -> Open
        | Loops, other | other, Loops -> other
        | Ends one, Ends other -> if String.equal one other then Ends one else Open)
;;

(* The text a region ends behind.

   A lookbehind has to be fixed width, so only a literal will do: the rule's
   own last token, or the closer of a matched pair the last child opens, or
   the one text every production the last child may hold ends on. An [if]
   whose last child is a block or another [if] ends behind a brace either way.
   A trailing pattern token gives nothing. So does a trailing child that may
   be absent: an anchor the input never reaches leaves the region open to the
   end of the file. *)
let closing_text (facts : Core.Facts.t) (rule : Core.Rule.def) : string option =
  let count = Array.length rule.children in
  if count = 0
  then None
  else (
    match ending_of_child facts ~visited:[ rule.id ] rule.children.(count - 1) with
    | Ends text -> Some text
    | Loops | Open -> None)
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

(* What a rule opens on.

   A literal is one, and has always been. A rule that starts with a name
   opens on the name, which the [begin] takes as it takes a keyword. A rule
   of several children that starts with nothing of its own opens on a
   lookahead for the tokens it may start with, where every one of them is a
   literal: a lambda's head starts with the bracket of its parameters.

   A name or a lookahead opens a region at every place its list meets the
   token. That is right only where nothing else in the list takes the same
   token, which {!alone} settles. *)
type lead =
  | Takes of Core.Token.def
  | Looks of Core.Token.def list

let other_lead (facts : Core.Facts.t) (rule : Core.Rule.def) : lead option =
  let written =
    match rule.origin, rule.frame with
    | Core.Rule.User, (Core.Rule.Plain | Core.Rule.Committed _) -> true
    | Core.Rule.User, (Core.Rule.Delimited _ | Core.Rule.Separated _)
    | (Core.Rule.Pratt_block | Core.Rule.Pratt_role _), _ -> false
  in
  if
    (not written)
    || Array.length rule.children < 2
    || (not (is_required rule.children.(0)))
    || Core.Facts.is_nullable facts rule.id
  then None
  else (
    match token_of_child facts rule.children.(0) with
    | Some token ->
      (match token.klass with
       | Core.Grammar.Pattern _ -> Some (Takes token)
       | Core.Grammar.Keyword _ | Core.Grammar.Punctuation _ -> None)
    | None ->
      let first = Core.Kind.Set.elements (Core.Facts.first_of facts rule.id) in
      let tokens = List.filter_map first ~f:(Core.Facts.token_of_kind facts) in
      if
        tokens <> []
        && List.length tokens = List.length first
        && List.for_all tokens ~f:(fun (token : Core.Token.def) ->
          Core.Token.text token <> None)
      then Some (Looks tokens)
      else None)
;;

(* The [begin] of a lead, with whether it is a lookahead. *)
let begin_of
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      (lead : lead)
  : string * (int * Scopes.Scope.t) list * int * bool * bool
  =
  match lead with
  | Takes token ->
    let regex, captures, body_from, use_identity = region_begin facts scopes rule token in
    regex, captures, body_from, use_identity, false
  | Looks tokens ->
    let regex (token : Core.Token.def) : string =
      match Oniguruma.of_token_at_child facts token with
      | Ok regex -> regex
      | Error _ -> Oniguruma.escape (Option.value (Core.Token.text token) ~default:"")
    in
    "(?=" ^ String.concat ~sep:"|" (List.map tokens ~f:regex) ^ ")", [], 0, false, true
;;

(* -- what a pattern list cannot tell apart --------------------------------- *)

(* The matched pair a child holds. *)
let pair_of_child (facts : Core.Facts.t) (child : Core.Rule.child)
  : (Core.Rule.def * Core.Kind.t * Core.Kind.t) option
  =
  match rule_of_child facts child with
  | None -> None
  | Some target ->
    (match target.frame with
     | Core.Rule.Delimited { open_; close; _ } -> Some (target, open_, close)
     | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> None)
;;

(* The fixed text a child ends on: a literal token's own, or the closer of the
   pair it holds. *)
let ends_on (facts : Core.Facts.t) (child : Core.Rule.child) : string option =
  match token_of_child facts child with
  | Some token -> Core.Token.text token
  | None ->
    (match pair_of_child facts child with
     | Some (_, _, close) ->
       Option.bind (Core.Facts.token_of_kind facts close) Core.Token.text
     | None -> None)
;;

(* The same through whatever the child may hold: a rule that ends on one
   text whichever production it is. *)
let ends_behind (facts : Core.Facts.t) (rule : Core.Rule.def) (child : Core.Rule.child)
  : string option
  =
  match ending_of_child facts ~visited:[ rule.id ] child with
  | Ends text -> Some text
  | Loops | Open -> None
;;

let ends_with ~(suffix : string) (text : string) : bool =
  let width = String.length suffix in
  String.length text >= width
  && String.equal (String.sub text ~pos:(String.length text - width) ~len:width) suffix
;;

(* Children in [lo, hi] that hold different pairs opening on one token. Two
   that hold the same pair are one pattern, which every occurrence reaches,
   so a pair counts once however many children hold it. *)
let shared_openers (facts : Core.Facts.t) (rule : Core.Rule.def) ~(lo : int) ~(hi : int)
  : conflict list
  =
  let groups = Hashtbl.create 4 in
  for index = lo to hi do
    match pair_of_child facts rule.children.(index) with
    | None -> ()
    | Some (target, open_, _) ->
      let key = Core.Kind.to_int open_ in
      let held =
        match Hashtbl.find_opt groups key with
        | Some (_, held) -> held
        | None -> []
      in
      if not (List.exists held ~f:(fun (_, pair) -> pair = target.id))
      then Hashtbl.replace groups key (open_, (index, target.id) :: held)
  done;
  Hashtbl.fold
    (fun _ (open_, held) acc ->
       match held, Core.Facts.token_of_kind facts open_ with
       | _ :: _ :: _, Some token ->
         Shares { token; children = List.rev_map held ~f:fst } :: acc
       | _ -> acc)
    groups
    []
;;

(* Children in [lo, hi) that end on text a lookbehind for [behind] matches. A
   region that ends behind its last child ends behind one of these first. *)
let early_closers
      (facts : Core.Facts.t)
      (rule : Core.Rule.def)
      ~(lo : int)
      ~(hi : int)
      ~(behind : string)
  : conflict list
  =
  List.init ~len:(max 0 (hi - lo)) ~f:(fun offset -> lo + offset)
  |> List.filter_map ~f:(fun (index : int) ->
    match ends_on facts rule.children.(index) with
    | Some text when ends_with ~suffix:behind text ->
      Some (Closes { child = index; text })
    | Some _ | None -> None)
;;

let region_conflicts
      (facts : Core.Facts.t)
      (rule : Core.Rule.def)
      ~(lo : int)
      ~(hi : int)
      ~(behind : string)
  : conflict list
  =
  shared_openers facts rule ~lo ~hi @ early_closers facts rule ~lo ~hi ~behind
;;

(* -- what a child's own patterns open ------------------------------------- *)

(* Which rules are regions that end in front of what follows them, by rule.

   Whether a rule is one depends on what its children take, and what they
   take depends on which rules under them are regions and which are lists.
   So the answer is found for every rule at once. Every rule that could be
   one starts as one, and a rule that fails its own test is struck out until
   none is. A rule struck out is a list from then on, so its children count
   toward whatever holds it, and that can strike out another.

   The table is kept for the scopes it was asked about last. Every question
   below is asked of one grammar many times over, and the table is the same
   each time. [decide] is {!open_region}, which is defined after everything
   it reads. *)
let decide : (Core.Facts.t -> Scopes.t -> Core.Rule.def -> bool) ref =
  ref (fun _ _ _ -> false)
;;

let kept : (Scopes.t * bool array) option ref = ref None

let ends_ahead (facts : Core.Facts.t) (scopes : Scopes.t) : bool array =
  match !kept with
  | Some (key, table) when key == scopes -> table
  | Some _ | None ->
    let rules = Core.Facts.(facts.rules) in
    let table = Array.make (Array.length rules) true in
    kept := Some (scopes, table);
    let struck = ref true in
    while !struck do
      struck := false;
      Array.iteri rules ~f:(fun index (rule : Core.Rule.def) ->
        if table.(index) && not (!decide facts scopes rule)
        then (
          table.(index) <- false;
          struck := true))
    done;
    table
;;

(* Whether a rule that opens on a literal and ends on a fixed text is kept a
   list all the same. See {!region}. [claimed] is {!leads_with_claim}, which
   is defined after what it reads. *)
let claimed : (Core.Facts.t -> Scopes.t -> Core.Rule.def -> index:int -> bool) ref =
  ref (fun _ _ _ ~index:_ -> false)
;;

let kept_for_a_claim (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def)
  : bool
  =
  match opening_token facts rule with
  | None -> false
  | Some lead ->
    let _, _, body_from, _ = region_begin facts scopes rule lead in
    let derived = ends_on facts rule.children.(Array.length rule.children - 1) = None in
    derived && !claimed facts scopes rule ~index:body_from
;;

(* Whether a rule is a region: one the list that holds it sees only the
   opening of. A rule that opens on a literal and ends on a fixed text is
   one. Any other is one where the table says so. A rule that can be a
   single match is that first, except that a rule which contains its own
   errors is a region first where it has a fixed end. *)
let regional (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def) : bool =
  let fixed =
    opening_token facts rule <> None
    && closing_text facts rule <> None
    && not (kept_for_a_claim facts scopes rule)
  in
  let conditional () : bool = (ends_ahead facts scopes).(rule.id) in
  match rule.origin, rule.frame with
  | Core.Rule.User, Core.Rule.Plain ->
    capture_parts facts scopes rule = None && (fixed || conditional ())
  | Core.Rule.User, Core.Rule.Committed _ ->
    fixed
    || ((closing_text facts rule <> None || capture_parts facts scopes rule = None)
        && conditional ())
  | Core.Rule.User, (Core.Rule.Delimited _ | Core.Rule.Separated _)
  | (Core.Rule.Pratt_block | Core.Rule.Pratt_role _), _ -> false
;;

(* Whether a rule emits as a bare list, which is spliced into whatever
   includes it. A list with a tail of its own counts as spliced whole, so
   this errs toward saying a list reaches more. *)
let spliced (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def) : bool =
  match rule.frame with
  | Core.Rule.Delimited _ -> false
  | Core.Rule.Separated _ -> true
  | Core.Rule.Plain | Core.Rule.Committed _ ->
    capture_parts facts scopes rule = None && not (regional facts scopes rule)
;;

let block_of (facts : Core.Facts.t) (rule : Core.Rule.def) : Core.Block.def option =
  Array.find_opt
    Core.Facts.(facts.blocks)
    ~f:(fun (block : Core.Block.def) -> block.rule_id = rule.id)
;;

(* Where the tail of a list starts, for a list that has one. [tail] is
   {!tailed}'s answer, which is defined after everything it reads. A rule
   asked about while it is being asked about has none. *)
let tail : (Core.Facts.t -> Scopes.t -> Core.Rule.def -> int option) ref =
  ref (fun _ _ _ -> None)
;;

let asked : Core.Rule.id list ref = ref []

let tail_from (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def)
  : int option
  =
  if List.mem rule.id ~set:!asked
  then None
  else (
    asked := rule.id :: !asked;
    Fun.protect
      ~finally:(fun () -> asked := List.filter !asked ~f:(fun id -> id <> rule.id))
      (fun () -> !tail facts scopes rule))
;;

(* The tokens a child's patterns take at the level of the list that holds it,
   each with what takes it.

   A matched pair takes the bracket it opens on, and a region or a single
   match the tokens it starts with. Those are named by their rule. A token
   that stands loose in the list is named by a number of its own: a token the
   child holds, or one a spliced rule or an expression block under it holds.
   So one pair reached twice is one taker, and a name held in two places is
   two.

   An operator that encloses its operand takes nothing here. Its bracket is
   tried inside the region of the operand in front of it. A list with a tail
   of its own is read as far as the tail, and the literal the tail opens on.

   [visited] is shared by one question, so a rule two children reach is read
   once, under the first. *)
let rec takes_kind
          (facts : Core.Facts.t)
          (scopes : Scopes.t)
          ~(visited : Core.Rule.id list ref)
          ~(next : int ref)
          (kind : Core.Kind.t)
  : (Core.Kind.t * int) list
  =
  let loose (kind : Core.Kind.t) : (Core.Kind.t * int) list =
    decr next;
    [ kind, !next ]
  in
  match Core.Facts.rule_of_kind facts kind with
  | None -> loose kind
  | Some target when List.mem target.id ~set:!visited -> []
  | Some target ->
    visited := target.id :: !visited;
    let through (kinds : Core.Kind.t array) : (Core.Kind.t * int) list =
      List.concat_map (Array.to_list kinds) ~f:(takes_kind facts scopes ~visited ~next)
    in
    (match block_of facts target, target.frame with
     | Some block, _ ->
       let ops (table : Core.Block.op array) : (Core.Kind.t * int) list =
         List.concat_map (Array.to_list table) ~f:(fun (op : Core.Block.op) ->
           loose op.op_kind)
       in
       let postfix =
         List.concat_map
           (Array.to_list block.postfix)
           ~f:(fun (postfix : Core.Block.postfix) ->
             match postfix.p_body with
             | Core.Block.Nothing | Core.Block.Then _ -> loose postfix.p_lead
             | Core.Block.Enclosed _ -> [])
       in
       through block.atoms @ ops block.prefix @ ops block.infix @ postfix
     | None, Core.Rule.Delimited { open_; _ } -> [ open_, target.id ]
     | None, (Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _) ->
       if spliced facts scopes target
       then (
         let separator =
           match target.frame with
           | Core.Rule.Separated { sep_tok; _ } -> loose sep_tok
           | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Delimited _ -> []
         in
         let count = Array.length target.children in
         let upto, tail =
           match tail_from facts scopes target with
           | Some from -> from, [ from ]
           | None -> count, []
         in
         separator
         @ List.concat_map
             (List.init ~len:upto ~f:(fun index -> index) @ tail)
             ~f:(fun (index : int) -> through target.children.(index).alts))
       else
         List.map
           (Core.Kind.Set.elements (Core.Facts.first_of facts target.id))
           ~f:(fun (kind : Core.Kind.t) -> kind, target.id))
;;

(* What the children [lo, hi] take, as {!takes_kind} reads each. *)
let takes
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(lo : int)
      ~(hi : int)
  : (Core.Kind.t * int) list
  =
  (* A list is not read again inside itself. A region is: one that holds
     itself takes what it opens on. *)
  let visited = ref (if spliced facts scopes rule then [ rule.id ] else []) in
  let next = ref 0 in
  List.init ~len:(max 0 (hi - lo + 1)) ~f:(fun offset -> lo + offset)
  |> List.concat_map ~f:(fun (index : int) ->
    List.concat_map
      (Array.to_list rule.children.(index).alts)
      ~f:(takes_kind facts scopes ~visited ~next))
;;

(* Whether the children [lo, hi] take [token] at the level of their list. A
   part that ends on a token ends at the first place its list meets it. *)
let stands_loose
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(lo : int)
      ~(hi : int)
      (token : Core.Kind.t)
  : bool
  =
  List.exists (takes facts scopes rule ~lo ~hi) ~f:(fun (kind, _) ->
    Core.Kind.equal kind token)
;;

(* Whether the children [lo, mid] and the children after [mid] take one token
   by two takers. One list over both would read each occurrence as the first
   of the two: a bracket as the wrong pair, or a name as the wrong rule. In
   two lists each side reads its own. *)
let divides
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(lo : int)
      ~(mid : int)
      ~(hi : int)
  : bool
  =
  let before = takes facts scopes rule ~lo ~hi:mid in
  let after = takes facts scopes rule ~lo:(mid + 1) ~hi in
  List.exists before ~f:(fun (kind, taker) ->
    List.exists after ~f:(fun (other, by) ->
      Core.Kind.equal kind other && (taker < 0 || by < 0 || taker <> by)))
;;

(* Whether the first thing a child puts in the list is a token of its own: a
   token child, or a spliced rule that starts with one. A literal in front of
   such a child may be tied to it as a pair. An expression block is not one:
   nothing ties its atoms to a literal outside it. *)
let rec leads_with_token
          (facts : Core.Facts.t)
          (scopes : Scopes.t)
          ~(visited : Core.Rule.id list)
          (child : Core.Rule.child)
  : bool
  =
  Array.exists child.alts ~f:(fun (kind : Core.Kind.t) ->
    match Core.Facts.token_of_kind facts kind, Core.Facts.rule_of_kind facts kind with
    | Some _, _ -> true
    | None, None -> false
    | None, Some target ->
      (not (List.mem target.id ~set:visited))
      && block_of facts target = None
      && spliced facts scopes target
      && Array.length target.children > 0
      && leads_with_token facts scopes ~visited:(target.id :: visited) target.children.(0))
;;

(* Whether the first thing the child at [index] puts in the list is a token
   scoped for its position. Such a token may be tied to the literal in front
   of it, as one match over both. A region that opens on that literal takes it
   first, and the tie never matches. The backend then refuses the grammar. *)
let rec leads_with_claim
          (facts : Core.Facts.t)
          (scopes : Scopes.t)
          ~(visited : Core.Rule.id list)
          (rule : Core.Rule.def)
          ~(index : int)
  : bool
  =
  index < Array.length rule.children
  && Array.exists rule.children.(index).alts ~f:(fun (kind : Core.Kind.t) ->
    match Core.Facts.token_of_kind facts kind, Core.Facts.rule_of_kind facts kind with
    | Some token, _ ->
      not (Scopes.is_token_scope scopes ~rule:rule.id ~child:index ~token:token.id)
    | None, None -> false
    | None, Some target ->
      (not (List.mem target.id ~set:visited))
      && block_of facts target = None
      && spliced facts scopes target
      && leads_with_claim facts scopes ~visited:(target.id :: visited) target ~index:0)
;;

let () =
  claimed
  := fun facts scopes rule ~index ->
       leads_with_claim facts scopes ~visited:[ rule.id ] rule ~index
;;

(* -- cutting a list in two -------------------------------------------------- *)

(* Every child a part could end on, with whether cutting there is worth it
   where nothing forces a cut.

   The first part runs from the opening token to the child, and ends on it:
   by taking it where it is a literal, and behind its closer where it is a
   matched pair. A literal is no cut in front of a child that leads with a
   token: the two may be tied as a pair, and the part's end would take the
   literal before that pair matched. A literal that stands earlier among the
   rule's own children is none either, since the part ends at the first.

   [closing] is the text the rule's region ends behind, where it has one.
   Neither side of a cut may leave two of the rule's own pairs on one opener,
   and the region must not end where the first part does.

   A cut is worth it where the two sides take some token by two takers, and
   where the literal it ends on does not also stand loose inside the first
   part. The return type of a definition and its body both reach a brace, and
   one list reads every brace as the first of them. *)
let cuts
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(from : int)
      ~(closing : string option)
  : (split * bool) list
  =
  let last = Array.length rule.children - 1 in
  let clean ~(lo : int) ~(hi : int) ~(behind : string option) : bool =
    match behind with
    | Some behind -> region_conflicts facts rule ~lo ~hi ~behind = []
    | None -> shared_openers facts rule ~lo ~hi = []
  in
  let ends_region (text : string) : bool =
    match closing with
    | Some closing -> ends_with ~suffix:closing text
    | None -> false
  in
  let cut (upto : int) : (split * bool) option =
    let child = rule.children.(upto) in
    if not (is_required child)
    then None
    else (
      match token_of_child facts child with
      | Some token ->
        let stands_earlier =
          List.exists
            (List.init ~len:(upto - from) ~f:(fun offset -> from + offset))
            ~f:(fun (index : int) ->
              Array.exists rule.children.(index).alts ~f:(Core.Kind.equal token.kind))
        in
        let before_a_token =
          leads_with_token facts scopes ~visited:[ rule.id ] rule.children.(upto + 1)
        in
        (match Core.Token.text token with
         | None ->
           (* A name, which the part ends by taking as it would a literal.
              No pair ties a literal to it, since a tie leads with a
              literal. It is worth taking where what follows takes the same
              name: the label of [jump ok (x)] is no operand, and the
              expression behind it starts with one. *)
           let loose_before =
             stands_loose facts scopes rule ~lo:from ~hi:(upto - 1) token.kind
           in
           if
             shared_openers facts rule ~lo:from ~hi:(upto - 1) = []
             && (not stands_earlier)
             && (not loose_before)
             && clean ~lo:(upto + 1) ~hi:last ~behind:closing
           then
             Some
               ( { upto; close = Literal token }
               , divides facts scopes rule ~lo:from ~mid:(upto - 1) ~hi:last
                 || stands_loose facts scopes rule ~lo:(upto + 1) ~hi:last token.kind )
           else None
         | Some text ->
           if
             shared_openers facts rule ~lo:from ~hi:(upto - 1) = []
             && (not stands_earlier)
             && (not (ends_region text))
             && (not before_a_token)
             && clean ~lo:(upto + 1) ~hi:last ~behind:closing
           then
             Some
               ( { upto; close = Literal token }
               , divides facts scopes rule ~lo:from ~mid:(upto - 1) ~hi:last
                 && not
                      (stands_loose facts scopes rule ~lo:from ~hi:(upto - 1) token.kind)
               )
           else None)
      | None ->
        (* A matched pair, or a rule that ends on one text whatever it
           holds: a lambda's head ends on its arrow. *)
        (match ends_behind facts rule child with
         | None -> None
         | Some text ->
           if
             clean ~lo:from ~hi:upto ~behind:(Some text)
             && (not (ends_region text))
             && clean ~lo:(upto + 1) ~hi:last ~behind:closing
           then
             Some
               ( { upto; close = Behind text }
               , divides facts scopes rule ~lo:from ~mid:upto ~hi:last )
           else None))
  in
  List.filter_map
    (List.init ~len:(max 0 (last - from)) ~f:(fun offset -> from + offset))
    ~f:cut
;;

(* Where to cut a region. One whose list cannot tell two of its own children
   apart takes the first cut there is, and the backend refuses the grammar
   where there is none. After that, and in any other region, every cut worth
   taking is taken, each in what the one before it left. A method's
   parameters, its return type and its body are three lists. *)
let rec worth_cutting
          (facts : Core.Facts.t)
          (scopes : Scopes.t)
          (rule : Core.Rule.def)
          ~(from : int)
          ~(closing : string option)
  : split list
  =
  match List.find_opt (cuts facts scopes rule ~from ~closing) ~f:snd with
  | Some (split, _) ->
    split :: worth_cutting facts scopes rule ~from:(split.upto + 1) ~closing
  | None -> []
;;

let split_of
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(from : int)
      ~(closing : string)
  : split list
  =
  let last = Array.length rule.children - 1 in
  let closing = Some closing in
  if region_conflicts facts rule ~lo:from ~hi:last ~behind:(Option.get closing) <> []
  then (
    match cuts facts scopes rule ~from ~closing with
    | (split, _) :: _ ->
      split :: worth_cutting facts scopes rule ~from:(split.upto + 1) ~closing
    | [] -> [])
  else worth_cutting facts scopes rule ~from ~closing
;;

(* A lookbehind for the text a region ends behind. A word takes a boundary on
   each side, or [(?<=end)] would end the region behind [append]. *)
let behind (text : string) : string =
  if Oniguruma.is_word_shaped text
  then "(?<=\\b" ^ Oniguruma.escape text ^ ")\\b"
  else "(?<=" ^ Oniguruma.escape text ^ ")"
;;

(* A rule whose last child is a literal or a matched pair has always been a
   region. One that ends on a fixed text only through what its last child may
   hold is a region where that refuses nothing: a claim behind its opening
   literal keeps it a list, as it was. *)
let region
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(lead : lead)
  : t option
  =
  match closing_text facts rule with
  | Some closing ->
    let begin_regex, begin_captures, body_from, use_identity, ahead =
      begin_of facts scopes rule lead
    in
    let last = Array.length rule.children - 1 in
    let splits = split_of facts scopes rule ~from:body_from ~closing in
    (* A rule that opens on a literal, and whose last child is a literal or
       a matched pair, has always been a region, and the backend refuses it
       where its list cannot tell two children apart. Any other is one only
       where nothing of that kind is left, so that no grammar is refused for
       a region it did not have before. *)
    let added =
      opening_token facts rule = None || ends_on facts rule.children.(last) = None
    in
    let left =
      splits = []
      && region_conflicts facts rule ~lo:body_from ~hi:last ~behind:closing <> []
    in
    if
      ((not ahead)
       && opening_token facts rule <> None
       && kept_for_a_claim facts scopes rule)
      || (added && left)
    then None
    else
      Some
        (Region
           { begin_regex
           ; begin_captures
           ; end_regex = behind closing
           ; body_from
           ; use_identity
           ; ahead
           ; splits
           })
  | None -> None
;;

(* -- a region that ends in front of what follows it ------------------------ *)

(* A rule that opens on a literal and ends on nothing fixed has no text to
   look behind for. It still has an end the text shows: the first token that
   may follow the rule. So it is a region that ends in front of any of
   those.

   [val pattern = value] is one. Left a list, its pattern is spliced into
   the block the statement stands in, and the pattern's bracket is the first
   on that token in the block's whole list. As a region it keeps the pattern
   to itself, and the cut at [=] keeps the pattern from the value.

   The region is exact where none of the tokens that may follow is one its
   own list takes. The parser leaves the rule at such a token, and the list
   has no use for it. An operator of the expression a rule ends in is no
   such token: the expression takes it, in the parser and here, which is why
   the set read is the one that leaves those out.

   A token that may follow has to be a literal, or there is nothing to look
   ahead for. The end of the input follows some rules too, and needs no
   pattern: a region still open there ends with the file.

   A rule may hold a token that also follows it: [fn (x) => body] holds an
   arrow, and an arrow ends the guard a [fn] may stand in. The end would
   fire on the rule's own arrow. Cut at that arrow, the end is tried only in
   the list of what comes after it, so a cut that clears the tokens is
   taken for that alone.

   A claim behind the opening literal keeps the rule a plain list, for the
   reason {!region} gives. *)
(* The tokens the region ends in front of, and where it is cut, or nothing
   where its last list takes one of those tokens whatever the cuts. *)
let open_parts
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(from : int)
  : (Core.Token.def list * split list) option
  =
  let last = Array.length rule.children - 1 in
  let after = Core.Kind.Set.elements (Core.Facts.follow_outside_of facts rule.id) in
  let until = List.filter_map after ~f:(Core.Facts.token_of_kind facts) in
  (* The end is tried in the list of the children from [lo] on, so those are
     the ones that must take none of the tokens it looks ahead for. *)
  let clear ~(lo : int) : bool =
    let taken = takes facts scopes rule ~lo ~hi:last in
    not
      (List.exists until ~f:(fun (token : Core.Token.def) ->
         List.exists taken ~f:(fun (kind, _) -> Core.Kind.equal kind token.kind)))
  in
  let worth = worth_cutting facts scopes rule ~from ~closing:None in
  let rest =
    match List.rev worth with
    | (split : split) :: _ -> split.upto + 1
    | [] -> from
  in
  let splits =
    if clear ~lo:rest
    then Some worth
    else (
      match
        List.find_opt
          (cuts facts scopes rule ~from:rest ~closing:None)
          ~f:(fun ((split : split), _) -> clear ~lo:(split.upto + 1))
      with
      | Some (split, _) -> Some (worth @ [ split ])
      | None -> None)
  in
  match splits with
  | Some splits
    when until <> []
         && List.for_all until ~f:(fun (token : Core.Token.def) ->
           Core.Token.text token <> None) -> Some (until, splits)
  | Some _ | None -> None
;;

let open_region
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      ~(lead : lead)
  : t option
  =
  if closing_text facts rule <> None
  then None
  else (
    let begin_regex, begin_captures, body_from, use_identity, ahead =
      begin_of facts scopes rule lead
    in
    let tied =
      (not ahead)
      && opening_token facts rule <> None
      && leads_with_claim facts scopes ~visited:[ rule.id ] rule ~index:body_from
    in
    match open_parts facts scopes rule ~from:body_from with
    | Some (until, splits) when not tied ->
      Some
        (Open
           { begin_regex; begin_captures; body_from; use_identity; ahead; until; splits })
    | Some _ | None -> None)
;;

(* -- the lists a grammar's entries are tried in ----------------------------- *)

(* The children that share a list, for each rule that owns one: the body of a
   pair, and each part of a region between its cuts. Nothing here asks
   {!alone}, which reads this. *)
let parts_of (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def)
  : (int * int) list
  =
  let last = Array.length rule.children - 1 in
  match rule.frame with
  | Core.Rule.Delimited _ -> [ 0, last ]
  | Core.Rule.Separated _ -> []
  | Core.Rule.Plain | Core.Rule.Committed _ ->
    if not (regional facts scopes rule)
    then []
    else (
      let lead =
        match opening_token facts rule with
        | Some token -> Some (Takes token)
        | None -> other_lead facts rule
      in
      match lead with
      | None -> []
      | Some lead ->
        let _, _, from, _, _ = begin_of facts scopes rule lead in
        let splits =
          match closing_text facts rule with
          | Some closing -> split_of facts scopes rule ~from ~closing
          | None ->
            (match open_parts facts scopes rule ~from with
             | Some (_, splits) -> splits
             | None -> [])
        in
        let lo, parts =
          List.fold_left splits ~init:(from, []) ~f:(fun (lo, parts) (split : split) ->
            split.upto + 1, (lo, split.upto) :: parts)
        in
        List.rev ((lo, last) :: parts))
;;

(* Every list: a rule's parts, the operand of each operator that encloses
   one, and the root. Each is the symbols it holds. *)
let lists (facts : Core.Facts.t) (scopes : Scopes.t) : Core.Kind.t array list list =
  let of_rules =
    Array.to_list Core.Facts.(facts.rules)
    |> List.concat_map ~f:(fun (rule : Core.Rule.def) ->
      match rule.origin with
      | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> []
      | Core.Rule.User ->
        let tail =
          match rule.frame with
          | Core.Rule.Plain when spliced facts scopes rule ->
            (match tail_from facts scopes rule with
             | Some from -> [ from + 1, Array.length rule.children - 1 ]
             | None -> [])
          | Core.Rule.Plain
          | Core.Rule.Committed _
          | Core.Rule.Delimited _
          | Core.Rule.Separated _ -> []
        in
        List.map
          (parts_of facts scopes rule @ tail)
          ~f:(fun (lo, hi) ->
            List.init
              ~len:(max 0 (hi - lo + 1))
              ~f:(fun offset -> rule.children.(lo + offset).alts)))
  in
  let of_operators =
    Array.to_list Core.Facts.(facts.blocks)
    |> List.concat_map ~f:(fun (block : Core.Block.def) ->
      Array.to_list block.postfix
      |> List.filter_map ~f:(fun (postfix : Core.Block.postfix) ->
        match postfix.p_body with
        | Core.Block.Enclosed { content = Core.Block.One kinds; _ } -> Some [ kinds ]
        | Core.Block.Enclosed { content = Core.Block.Many { elem; _ }; _ } ->
          Some [ elem ]
        | Core.Block.Nothing | Core.Block.Then _ -> None))
  in
  let of_roots =
    List.map
      Core.Facts.(facts.roots)
      ~f:(fun (root : Core.Rule.id) -> [ [| (Core.Facts.rule facts root).kind |] ])
  in
  of_rules @ of_operators @ of_roots
;;

(* Whether a rule is the one thing that takes each token it opens on, in
   every list that holds it. A name or a lookahead opens the rule's region
   wherever its list meets the token, so a second thing that takes the token
   there would be read as this rule. *)
let alone (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def) (lead : lead)
  : bool
  =
  let opens =
    match lead with
    | Takes token -> [ token.kind ]
    | Looks tokens -> List.map tokens ~f:(fun (token : Core.Token.def) -> token.kind)
  in
  List.for_all (lists facts scopes) ~f:(fun (symbols : Core.Kind.t array list) ->
    let visited = ref [] in
    let next = ref 0 in
    let taken =
      List.concat_map symbols ~f:(fun (alts : Core.Kind.t array) ->
        List.concat_map (Array.to_list alts) ~f:(takes_kind facts scopes ~visited ~next))
    in
    (not (List.mem rule.id ~set:!visited))
    || List.for_all opens ~f:(fun (kind : Core.Kind.t) ->
      List.for_all taken ~f:(fun (other, taker) ->
        (not (Core.Kind.equal kind other)) || taker = rule.id)))
;;

(* The lists that give one token to two regions. A list takes the first
   pattern that matches, so every occurrence of the token opens the first of
   the two. A pair and a region are named by their rule here. Two tokens that
   stand loose are no such case: neither opens anything. *)
let undecided (facts : Core.Facts.t) (scopes : Scopes.t)
  : (Core.Kind.t * Core.Rule.id * Core.Rule.id) list
  =
  List.concat_map (lists facts scopes) ~f:(fun (symbols : Core.Kind.t array list) ->
    let visited = ref [] in
    let next = ref 0 in
    let taken =
      List.concat_map symbols ~f:(fun (alts : Core.Kind.t array) ->
        List.concat_map (Array.to_list alts) ~f:(takes_kind facts scopes ~visited ~next))
    in
    List.concat_map taken ~f:(fun (kind, taker) ->
      List.filter_map taken ~f:(fun (other, by) ->
        if Core.Kind.equal kind other && taker >= 0 && (by > taker || by < 0)
        then Some (kind, taker, if by < 0 then taker else by)
        else None)))
  |> List.sort_uniq ~cmp:compare
;;

(* The test a rule's place in the table stands on. A rule that opens on a
   literal and ends on a fixed text needs none. One that ends on nothing
   fixed has to end in front of what follows it. One that opens on no
   literal has to be alone on what it opens on. *)
let () =
  decide
  := fun facts scopes rule ->
       match opening_token facts rule, other_lead facts rule with
       | Some token, _ ->
         closing_text facts rule <> None
         || open_region facts scopes rule ~lead:(Takes token) <> None
       | None, Some lead ->
         alone facts scopes rule lead
         && (closing_text facts rule <> None
             || open_region facts scopes rule ~lead <> None)
       | None, None -> false
;;

(* The separator and the closer of the list a rule is an element of, where
   every rule that holds it is a matched pair with a separator. A matched
   pair wraps one child, so an element is followed by one of the two and
   nothing else. *)
let element_of (facts : Core.Facts.t) (rule : Core.Rule.def)
  : (Core.Token.def * Core.Token.def) option
  =
  let holds (kinds : Core.Kind.t array) : bool =
    Array.exists kinds ~f:(Core.Kind.equal rule.kind)
  in
  let from_blocks =
    Array.exists
      Core.Facts.(facts.blocks)
      ~f:(fun (block : Core.Block.def) ->
        holds block.atoms
        || Array.exists block.postfix ~f:(fun (postfix : Core.Block.postfix) ->
          match postfix.p_body with
          | Core.Block.Nothing -> false
          | Core.Block.Then kinds -> holds kinds
          | Core.Block.Enclosed { content = Core.Block.One kinds; _ } -> holds kinds
          | Core.Block.Enclosed { content = Core.Block.Many { elem; _ }; _ } -> holds elem))
  in
  if from_blocks || List.mem rule.id ~set:Core.Facts.(facts.roots)
  then None
  else (
    let around =
      Array.to_list Core.Facts.(facts.rules)
      |> List.filter_map ~f:(fun (parent : Core.Rule.def) ->
        if
          not
            (Array.exists parent.children ~f:(fun (child : Core.Rule.child) ->
               holds child.alts))
        then None
        else (
          match parent.frame with
          | Core.Rule.Delimited { close; sep = Some { sep_tok; _ }; _ } ->
            (match
               ( Core.Facts.token_of_kind facts sep_tok
               , Core.Facts.token_of_kind facts close )
             with
             | Some sep, Some close
               when Core.Token.text sep <> None && Core.Token.text close <> None ->
               Some (Some (sep, close))
             | _ -> Some None)
          | Core.Rule.Delimited _
          | Core.Rule.Plain
          | Core.Rule.Committed _
          | Core.Rule.Separated _ -> Some None))
    in
    match around with
    | Some (sep, close) :: rest
      when List.for_all rest ~f:(fun (other : (Core.Token.def * Core.Token.def) option) ->
             match other with
             | Some (other_sep, other_close) ->
               other_sep.id = sep.id && other_close.id = close.id
             | None -> false) -> Some (sep, close)
    | _ -> None)
;;

(* An element of a separated list in a matched pair has an end to look ahead
   for: the separator or the closer. So what follows a literal of its own can
   be a region, from that literal to whichever of the two comes first.

   [pattern => body] opens on no literal. Its pattern and its body both reach
   a bracket, and spliced into the list of arms the pattern's is the first.

   Neither token may stand loose in the tail, or the region would end there.
   One that stands inside a pair or a region the tail reaches is taken
   there. *)
let tailed (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def) : t option =
  match element_of facts rule with
  | None -> None
  | Some (sep, close) ->
    let last = Array.length rule.children - 1 in
    let tail (from : int) : t option =
      let child = rule.children.(from) in
      match token_of_child facts child with
      | Some lead
        when is_required child
             && Core.Token.text lead <> None
             && lead.id <> sep.id
             && lead.id <> close.id ->
        let stands_earlier =
          List.exists
            (List.init ~len:from ~f:(fun index -> index))
            ~f:(fun (index : int) ->
              Array.exists rule.children.(index).alts ~f:(Core.Kind.equal lead.kind))
        in
        let loose (token : Core.Token.def) : bool =
          stands_loose facts scopes rule ~lo:(from + 1) ~hi:last token.kind
        in
        if
          (not stands_earlier)
          && (not (stands_loose facts scopes rule ~lo:0 ~hi:(from - 1) lead.kind))
          && (not
                (leads_with_token
                   facts
                   scopes
                   ~visited:[ rule.id ]
                   rule.children.(from + 1)))
          && (not (loose sep))
          && (not (loose close))
          && divides facts scopes rule ~lo:0 ~mid:(from - 1) ~hi:last
        then Some (Tailed { from; lead; until = [ sep; close ] })
        else None
      | Some _ | None -> None
    in
    List.find_map
      (List.init ~len:(max 0 (last - 1)) ~f:(fun offset -> offset + 1))
      ~f:tail
;;

let () =
  tail
  := fun facts scopes rule ->
       match tailed facts scopes rule with
       | Some (Tailed { from; _ }) -> Some from
       | Some (Delimited _ | Region _ | Open _ | Capture _ | Flat) | None -> None
;;

(* -- the decision ---------------------------------------------------------- *)

let of_rule (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def) : t =
  let conditional () : bool = (ends_ahead facts scopes).(rule.id) in
  let lead () : lead option =
    match opening_token facts rule with
    | Some token -> Some (Takes token)
    | None -> if conditional () then other_lead facts rule else None
  in
  let fixed () : t option =
    Option.bind (lead ()) (fun lead -> region facts scopes rule ~lead)
  in
  let list () : t =
    match
      if conditional ()
      then Option.bind (lead ()) (fun lead -> open_region facts scopes rule ~lead)
      else None
    with
    | Some shape -> shape
    | None ->
      (match tailed facts scopes rule with
       | Some shape -> shape
       | None -> Flat)
  in
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
    (match fixed () with
     | Some shape -> shape
     | None ->
       (match capture_parts facts scopes rule with
        | Some parts -> Capture parts
        | None -> list ()))
  | Core.Rule.Plain ->
    (match capture_parts facts scopes rule with
     | Some parts -> Capture parts
     | None ->
       (match fixed () with
        | Some shape -> shape
        | None -> list ()))
  | Core.Rule.Separated _ -> Flat
;;

(* What a rule's list still cannot tell apart, after any cut. A region that was
   cut has nothing left on either side, which is what makes a cut one. A list
   with a tail of its own answers as the plain list it would otherwise be. *)
let conflicts (facts : Core.Facts.t) (scopes : Scopes.t) (rule : Core.Rule.def)
  : conflict list
  =
  let last = Array.length rule.children - 1 in
  match of_rule facts scopes rule with
  | Region { splits = _ :: _; _ } | Open { splits = _ :: _; _ } -> []
  | Region { splits = []; body_from; _ } ->
    (match closing_text facts rule with
     | Some behind -> region_conflicts facts rule ~lo:body_from ~hi:last ~behind
     | None -> shared_openers facts rule ~lo:body_from ~hi:last)
  | Open { splits = []; body_from; _ } -> shared_openers facts rule ~lo:body_from ~hi:last
  | Delimited _ | Capture _ | Tailed _ | Flat -> shared_openers facts rule ~lo:0 ~hi:last
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

(* A rule that is a region is no forwarder. Forwarded, what it holds would
   stand loose in every list that holds the rule: the body of a handler
   beside the body of the [try] in front of it, and the type behind a colon
   beside the pattern in front of the colon. As a region it is tried only
   behind the literal the rule opens on. *)
let forwards_to
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      ~(raw : Core.Rule.id -> bool)
      (rule : Core.Rule.def)
  : Core.Rule.id option
  =
  let target_of (rule : Core.Rule.def) : Core.Rule.id option =
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
  in
  match target_of rule with
  | Some target ->
    (match of_rule facts scopes rule with
     | Region _ | Open _ -> None
     | Delimited _ | Capture _ | Tailed _ | Flat -> Some target)
  | None -> None
;;

(* -- what a body list carries ---------------------------------------------- *)

let body_from (rule : Core.Rule.def) (shape : t) : int option =
  match shape with
  | Flat | Tailed _ -> Some 0
  | Region { body_from; _ } | Open { body_from; _ } -> Some body_from
  | Delimited _ -> Some rule.body_from
  | Capture _ -> None
;;

(* A list spliced elsewhere, or a region whose [begin] could not take the
   name, scopes the name as its token. See {!t.Region}. *)
let body_scope
      (scopes : Scopes.t)
      (rule : Core.Rule.def)
      (shape : t)
      ~(index : int)
      ~(token : Core.Token.id)
  : Scopes.Scope.t option
  =
  let drops_identity =
    match shape with
    | Flat | Tailed _ -> true
    | Region { use_identity; _ } | Open { use_identity; _ } -> not use_identity
    | Delimited _ | Capture _ -> false
  in
  if drops_identity && rule.identity = Some index
  then Scopes.token scopes token
  else Scopes.resolve scopes ~rule:rule.id ~child:index ~token
;;
