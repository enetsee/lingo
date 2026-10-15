open StdLabels

type place =
  | Root
  | Body of Core.Rule.id
  | Enclosed of Core.Rule.id

type anchor =
  | Before of
      { lead : Core.Token.def
      ; lead_scope : Scopes.Scope.t option
      }
  | After of Core.Token.def

type pair =
  { rule : Core.Rule.id
  ; child : int
  ; token : Core.Token.def
  ; scope : Scopes.Scope.t
  ; anchor : anchor
  }

type unanchored =
  { rule : Core.Rule.id
  ; child : int
  ; token : Core.Token.id
  ; against : (Core.Rule.id * int) option
  }

type t =
  { pairs : (place * pair list) list
  ; paired : (Core.Rule.id * int * Core.Token.id, unit) Hashtbl.t
  ; unanchored : unanchored list
  }

(* One token a place tries, in the order the place tries it.

   [scope] is what the token's own emission gives it, and [wanted] is what
   the author asked for. The two differ only for a name a list keeps out of
   its body. [fallback] marks the grammar-wide token list, which is a
   pattern the engine tries and not a place the grammar puts the token. *)
type member =
  { at : (Core.Rule.id * int) option
  ; token : Core.Token.def
  ; scope : Scopes.Scope.t option
  ; wanted : Scopes.Scope.t option
  ; fallback : bool
  }

let same_scope (a : Scopes.Scope.t option) (b : Scopes.Scope.t option) : bool =
  Option.equal Scopes.Scope.equal a b
;;

(* -- whether two tokens can match the same text ----------------------------- *)

let literal_in (literal : Core.Token.def) (text : string) (pattern : Core.Token.def)
  : bool
  =
  match literal.klass with
  | Core.Grammar.Keyword _ when Oniguruma.is_word_shaped text -> false
  | Core.Grammar.Keyword _ | Core.Grammar.Punctuation _ | Core.Grammar.Pattern _ ->
    Redfa.Ast.eval (Redfa.Regex.to_ast pattern.regex) text
;;

(* Two literals overlap only when they are one token. A literal that starts
   a longer one is {!Oniguruma.of_token_at_child}'s concern. A keyword with
   a word shape never overlaps a pattern, because every per-child pattern is
   fenced off it. Two patterns overlap where their languages meet. The
   budget keeps a pathological pair from stalling the backend, and a pair
   that exhausts it counts as overlapping. Reporting a clash that is not
   there costs the author an anchor. Missing one costs them a wrong colour
   that nothing reports. *)
let overlaps (a : Core.Token.def) (b : Core.Token.def) : bool =
  if a.id = b.id
  then true
  else (
    match Core.Token.text a, Core.Token.text b with
    | Some _, Some _ -> false
    | Some text, None -> literal_in a text b
    | None, Some text -> literal_in b text a
    | None, None ->
      (match
         Redfa.Regex.is_empty_language_within
           ~max_states:4096
           (Redfa.Regex.inter a.regex b.regex)
       with
       | Some empty -> not empty
       | None -> true))
;;

(* -- what each place tries ------------------------------------------------- *)

(* A flat entry included in a place is spliced into it, so its members are
   the place's own, at the point of the include. Anything that opens a
   region or matches as one regex is opaque: what it holds is tried in a
   place of its own, or not at all.

   An expression block is spliced like a flat entry, and its list ends in
   the trivia and token entries. Those land in the middle of the including
   place. *)
let places (scopes : Scopes.t) ~(raw : Core.Rule.id -> bool) : (place * member list) list =
  let facts = Scopes.facts scopes in
  let rules = Core.Facts.(facts.rules) in
  let shapes =
    Array.map rules ~f:(fun (rule : Core.Rule.def) ->
      lazy (Shape.of_rule facts scopes rule))
  in
  let block_of (id : Core.Rule.id) : Core.Block.def option =
    Array.find_opt
      Core.Facts.(facts.blocks)
      ~f:(fun (block : Core.Block.def) -> block.rule_id = id)
  in
  let loose (token : Core.Token.def) ~(fallback : bool) : member =
    let scope = Scopes.token scopes token.id in
    { at = None; token; scope; wanted = scope; fallback }
  in
  let occurrence (kind : Core.Kind.t) : member list =
    match Core.Facts.token_of_kind facts kind with
    | Some token when not (Core.Token.is_trivia token) -> [ loose token ~fallback:false ]
    | Some _ | None -> []
  in
  let tokens_tail : member list =
    Array.to_list Core.Facts.(facts.tokens)
    |> List.filter_map ~f:(fun (token : Core.Token.def) ->
      if Core.Token.is_trivia token
      then None
      else (
        match Core.Token.text token, Scopes.token scopes token.id with
        | None, None -> None
        | _ -> Some (loose token ~fallback:true)))
  in
  let rec include_ ~(visited : Core.Rule.id list ref) (target : Core.Rule.def)
    : member list
    =
    if List.mem target.id ~set:!visited
    then []
    else (
      visited := target.id :: !visited;
      match block_of target.id with
      | Some block -> block_members ~visited block
      | None ->
        (match Shape.forwards_to facts scopes ~raw target with
         | Some next -> include_ ~visited (Core.Facts.rule facts next)
         | None ->
           (match Lazy.force shapes.(target.id) with
            | Shape.Flat -> body ~visited target Shape.Flat ~from:0
            | Shape.Delimited _ | Shape.Region _ | Shape.Capture _ -> [])))
  and body
        ~(visited : Core.Rule.id list ref)
        (rule : Core.Rule.def)
        (shape : Shape.t)
        ~(from : int)
    : member list
    =
    Shape.emission_order facts rule ~from
    |> List.concat_map ~f:(fun (index : int) ->
      Array.to_list rule.children.(index).alts
      |> List.concat_map ~f:(fun (kind : Core.Kind.t) ->
        match Core.Facts.token_of_kind facts kind with
        | Some token when Core.Token.is_trivia token -> []
        | Some token ->
          [ { at = Some (rule.id, index)
            ; token
            ; scope = Shape.body_scope scopes rule shape ~index ~token:token.id
            ; wanted = Scopes.resolve scopes ~rule:rule.id ~child:index ~token:token.id
            ; fallback = false
            }
          ]
        | None ->
          (match Core.Facts.rule_of_kind facts kind with
           | Some target -> include_ ~visited target
           | None -> [])))
  and block_members ~(visited : Core.Rule.id list ref) (block : Core.Block.def)
    : member list
    =
    let atoms =
      Array.to_list block.atoms
      |> List.concat_map ~f:(fun (kind : Core.Kind.t) ->
        match Core.Facts.rule_of_kind facts kind with
        | Some target -> include_ ~visited target
        | None -> occurrence kind)
    in
    let ops (table : Core.Block.op array) : member list =
      Array.to_list table
      |> List.concat_map ~f:(fun (op : Core.Block.op) -> occurrence op.op_kind)
    in
    let bare_postfix =
      Array.to_list block.postfix
      |> List.concat_map ~f:(fun (postfix : Core.Block.postfix) ->
        match postfix.p_body with
        | Core.Block.Nothing -> occurrence postfix.p_lead
        | Core.Block.Then _ | Core.Block.Enclosed _ -> [])
    in
    atoms @ ops block.prefix @ ops block.infix @ bare_postfix @ tokens_tail
  in
  let fresh () : Core.Rule.id list ref = ref [] in
  let root =
    match Core.Facts.(facts.roots) with
    | first :: _ -> [ Root, include_ ~visited:(fresh ()) (Core.Facts.rule facts first) ]
    | [] -> []
  in
  let regions =
    Array.to_list rules
    |> List.filter_map ~f:(fun (rule : Core.Rule.def) ->
      match rule.origin with
      | Core.Rule.Pratt_role _ | Core.Rule.Pratt_block -> None
      | Core.Rule.User ->
        let shape = Lazy.force shapes.(rule.id) in
        (match shape with
         | Shape.Delimited _ | Shape.Region _ ->
           (match Shape.body_from rule shape with
            | Some from ->
              Some (Body rule.id, body ~visited:(ref [ rule.id ]) rule shape ~from)
            | None -> None)
         | Shape.Flat | Shape.Capture _ -> None))
  in
  let enclosed =
    Array.to_list Core.Facts.(facts.blocks)
    |> List.concat_map ~f:(fun (block : Core.Block.def) ->
      Array.to_list block.postfix
      |> List.filter_map ~f:(fun (postfix : Core.Block.postfix) ->
        match postfix.p_body with
        | Core.Block.Nothing | Core.Block.Then _ -> None
        | Core.Block.Enclosed { content; _ } ->
          let kinds =
            match content with
            | Core.Block.One kinds -> kinds
            | Core.Block.Many { elem; _ } -> elem
          in
          let visited = fresh () in
          let members =
            match
              Array.to_list kinds |> List.filter_map ~f:(Core.Facts.rule_of_kind facts)
            with
            | [] -> block_members ~visited block
            | targets -> List.concat_map targets ~f:(include_ ~visited)
          in
          Some (Enclosed postfix.p_rule, members)))
  in
  root @ regions @ enclosed
;;

(* -- tying a position down ------------------------------------------------- *)

let literal_at (facts : Core.Facts.t) (rule : Core.Rule.def) ~(from : int) ~(at : int)
  : Core.Token.def option
  =
  if at < from || at >= Array.length rule.children
  then None
  else (
    let child = rule.children.(at) in
    match child.modifier with
    | Core.Grammar.Exactly_one ->
      (match Shape.token_of_child facts child with
       | Some token when Core.Token.text token <> None -> Some token
       | _ -> None)
    | Core.Grammar.Zero_or_one | Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _
      -> None)
;;

(* The rules that splice [target] into their own list, each with the index
   of the child that does it. [None] where something else reaches it: the
   document root, an expression block, or an operator's body. None of those
   puts a literal in front of it. A single match that inlines the target is
   not a reference at all, since the target's list is never tried there. *)
let references (scopes : Scopes.t) (target : Core.Rule.def)
  : (Core.Rule.def * int) list option
  =
  let facts = Scopes.facts scopes in
  let is_target (kind : Core.Kind.t) : bool =
    match Core.Facts.rule_of_kind facts kind with
    | Some rule -> rule.id = target.id
    | None -> false
  in
  let from_blocks =
    Array.exists
      Core.Facts.(facts.blocks)
      ~f:(fun (block : Core.Block.def) ->
        Array.exists block.atoms ~f:is_target
        || Array.exists block.postfix ~f:(fun (postfix : Core.Block.postfix) ->
          match postfix.p_body with
          | Core.Block.Nothing -> false
          | Core.Block.Then kinds -> Array.exists kinds ~f:is_target
          | Core.Block.Enclosed { content = Core.Block.One kinds; _ } ->
            Array.exists kinds ~f:is_target
          | Core.Block.Enclosed { content = Core.Block.Many { elem; _ }; _ } ->
            Array.exists elem ~f:is_target))
  in
  let from_root = List.mem target.id ~set:Core.Facts.(facts.roots) in
  if from_blocks || from_root
  then None
  else
    Some
      (Array.to_list Core.Facts.(facts.rules)
       |> List.concat_map ~f:(fun (parent : Core.Rule.def) ->
         match parent.origin, Shape.of_rule facts scopes parent with
         | Core.Rule.User, (Shape.Flat | Shape.Region _ | Shape.Delimited _) ->
           List.init ~len:(Array.length parent.children) ~f:(fun index -> index)
           |> List.filter_map ~f:(fun (index : int) ->
             if Array.exists parent.children.(index).alts ~f:is_target
             then Some (parent, index)
             else None)
         | _ -> []))
;;

(* The literals that sit beside a position on every occurrence of it.

   A literal has to be required, or some occurrences lack it. It has to sit
   inside the list the position is emitted from, or a region's [begin] has
   already consumed it. The position may be absent, since a pair that does
   not match leaves the text to the rest of the place, but it may not
   repeat: a pair ties down only the occurrence beside the literal.

   The first child of a flat list has no sibling before it. The literal
   before it then sits in each rule that splices the list in: [from] before
   [Table], and [join] before it too. Each gives a pair of its own, and every
   reference has to have one. *)
let anchors_of (scopes : Scopes.t) (rule : Core.Rule.def) ~(index : int) : anchor list =
  let facts = Scopes.facts scopes in
  let shape = Shape.of_rule facts scopes rule in
  let from = Option.value (Shape.body_from rule shape) ~default:0 in
  let before (parent : Core.Rule.def) ~(at : int) : anchor option =
    let parent_from =
      Option.value (Shape.body_from parent (Shape.of_rule facts scopes parent)) ~default:0
    in
    match literal_at facts parent ~from:parent_from ~at:(at - 1) with
    | Some lead ->
      Some
        (Before
           { lead
           ; lead_scope =
               Scopes.resolve scopes ~rule:parent.id ~child:(at - 1) ~token:lead.id
           })
    | None -> None
  in
  match rule.children.(index).modifier with
  | Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _ -> []
  | Core.Grammar.Exactly_one | Core.Grammar.Zero_or_one ->
    (match before rule ~at:index with
     | Some anchor -> [ anchor ]
     | None ->
       (match literal_at facts rule ~from ~at:(index + 1) with
        | Some next -> [ After next ]
        | None ->
          (match shape, index, references scopes rule with
           | Shape.Flat, 0, Some (_ :: _ as parents) ->
             let leads = List.map parents ~f:(fun (parent, at) -> before parent ~at) in
             if List.for_all leads ~f:Option.is_some
             then List.filter_map leads ~f:(fun lead -> lead)
             else []
           | _ -> [])))
;;

(* -- finding the clashes --------------------------------------------------- *)

(* A claim clashes with another member of its place where the two can match
   the same text and come out differently. An occurrence the grammar puts
   there clashes wherever it sits in the order: an earlier one takes the
   claim's text, and a later one has its own text taken. The grammar-wide
   list clashes only from in front. Behind the claim it is a fallback, and
   taking text from a fallback is what a per-position scope is for. *)
let of_scopes (scopes : Scopes.t) ~(raw : Core.Rule.id -> bool) : t =
  let facts = Scopes.facts scopes in
  let places = places scopes ~raw in
  let clashing = Hashtbl.create 16 in
  let wanted = Hashtbl.create 16 in
  List.iter places ~f:(fun (_, members) ->
    let members = Array.of_list members in
    Array.iteri members ~f:(fun k (claim : member) ->
      match claim.at with
      | None -> ()
      | Some (rule, child) ->
        let key = rule, child, claim.token.id in
        if not (same_scope claim.wanted claim.scope) then Hashtbl.replace wanted key ();
        let claimed = not (same_scope claim.scope (Scopes.token scopes claim.token.id)) in
        if claimed
        then
          Array.iteri members ~f:(fun m (other : member) ->
            if
              (not (Hashtbl.mem clashing key))
              && other.at <> claim.at
              && ((not other.fallback) || m < k)
              && (not (same_scope claim.scope other.scope))
              && overlaps claim.token other.token
            then Hashtbl.replace clashing key other.at)));
  let anchors = Hashtbl.create 16 in
  let anchor_list ((rule, child, _) : Core.Rule.id * int * Core.Token.id) : anchor list =
    anchors_of scopes (Core.Facts.rule facts rule) ~index:child
  in
  Hashtbl.iter (fun key _ -> Hashtbl.replace anchors key (anchor_list key)) clashing;
  Hashtbl.iter (fun key () -> Hashtbl.replace anchors key (anchor_list key)) wanted;
  let paired = Hashtbl.create 16 in
  Hashtbl.iter
    (fun key (found : anchor list) -> if found <> [] then Hashtbl.replace paired key ())
    anchors;
  let pairs =
    List.map places ~f:(fun (place, members) ->
      let seen = Hashtbl.create 8 in
      ( place
      , List.concat_map members ~f:(fun (member : member) ->
          match member.at, member.wanted with
          | Some (rule, child), Some scope ->
            let key = rule, child, member.token.id in
            if Hashtbl.mem seen key || not (Hashtbl.mem paired key)
            then []
            else (
              Hashtbl.replace seen key ();
              List.map (Hashtbl.find anchors key) ~f:(fun anchor ->
                { rule; child; token = member.token; scope; anchor }))
          | _ -> []) ))
  in
  let unanchored =
    Hashtbl.fold
      (fun (rule, child, token) against acc ->
         if Hashtbl.mem paired (rule, child, token)
         then acc
         else { rule; child; token; against } :: acc)
      clashing
      []
    |> List.sort ~cmp:compare
  in
  { pairs; paired; unanchored }
;;

let is_paired (t : t) ~(rule : Core.Rule.id) ~(child : int) ~(token : Core.Token.id)
  : bool
  =
  Hashtbl.mem t.paired (rule, child, token)
;;

let pairs (t : t) (place : place) : pair list =
  Option.value (List.assoc_opt place t.pairs) ~default:[]
;;

let unanchored (t : t) : unanchored list = t.unanchored
