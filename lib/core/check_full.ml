open StdLabels

(* -- left recursion, as an analysis ---------------------------------------- *)

(* What a rule's parser reaches without taking a token first. *)
type left_through =
  | Through_child of int (** The index of the child. *)
  | Through_atom

type left_edge =
  { target : int
  ; through : left_through
  }

type cycle =
  { members : int list (** Ascending, so in declaration order. *)
  ; path : (int * left_edge) list (** From the first member back to it. *)
  }

type recursion =
  { cycles : cycle list
  ; edge_children : (int * int, unit) Hashtbl.t
    (** A rule and a child it reaches another member of its cycle through. *)
  ; blocks : (int, unit) Hashtbl.t (** Blocks on a cycle, by rule id. *)
  }

(* [r -> s] where r's parser reaches s without taking a token first. A cycle
   here is a left recursion.

   A delimited production takes its opener before any rule call, so it gives
   no edges. An expression block reaches its atoms without taking a token,
   so a cycle through a block counts. *)
let left_edges (shape : Stage.shape) (fixpoint_reader : Fixpoint.Reader.t)
  : left_edge list array
  =
  let edges = Array.make (Array.length shape.rules) [] in
  let add (r : int) (edge : left_edge) = edges.(r) <- edge :: edges.(r) in
  Array.iter shape.rules ~f:(fun (d : Rule.def) ->
    match d.origin, d.frame with
    | Rule.Pratt_role _, _ | _, Rule.Delimited _ -> ()
    | (Rule.Pratt_block | Rule.User), (Rule.Plain | Rule.Committed _ | Rule.Separated _)
      ->
      let count = Array.length d.children in
      let rec walk (i : int) =
        if i < count
        then (
          let ch = d.children.(i) in
          Array.iter ch.alts ~f:(fun k ->
            let t = Fixpoint.Reader.rule_of_kind fixpoint_reader k in
            if t >= 0 then add d.id { target = t; through = Through_child i });
          if Fixpoint.Reader.child_nullable fixpoint_reader ch then walk (i + 1))
      in
      walk 0);
  Array.iter shape.blocks ~f:(fun (b : Block.def) ->
    Array.iter b.atoms ~f:(fun k ->
      let t = Fixpoint.Reader.rule_of_kind fixpoint_reader k in
      if t >= 0 then add b.rule_id { target = t; through = Through_atom }));
  Array.map edges ~f:List.rev
;;

(* Tarjan, one pass over the edge set. A component names a left recursion
   once.

   Cycles do not. A cycle has one rotation per node on it, so reporting
   cycles reports the same left recursion once per member. Something then
   has to walk the reports and recognise the rotations as one.

   Every cycle lies inside a strongly connected component, and a component
   of more than one rule holds a cycle. So the components to report are the
   ones with more than one rule, plus any rule that reaches itself. *)
let cyclic_components (edges : left_edge list array) : int list list =
  let succs (r : int) : int list =
    List.map edges.(r) ~f:(fun (e : left_edge) -> e.target)
  in
  let n = Array.length edges in
  let index = Array.make n (-1) in
  let low = Array.make n 0 in
  let stacked = Array.make n false in
  let stack = ref [] in
  let next = ref 0 in
  let components = ref [] in
  let rec visit (v : int) =
    index.(v) <- !next;
    low.(v) <- !next;
    incr next;
    stack := v :: !stack;
    stacked.(v) <- true;
    List.iter (succs v) ~f:(fun w ->
      if index.(w) < 0
      then (
        visit w;
        low.(v) <- min low.(v) low.(w))
      else if stacked.(w)
      then low.(v) <- min low.(v) index.(w));
    if low.(v) = index.(v)
    then (
      let rec pop acc =
        match !stack with
        | [] -> acc
        | w :: rest ->
          stack := rest;
          stacked.(w) <- false;
          if w = v then w :: acc else pop (w :: acc)
      in
      components := pop [] :: !components)
  in
  for v = 0 to n - 1 do
    if index.(v) < 0 then visit v
  done;
  (* Rule ids are assigned in declaration order. Sorting the members puts
     them in the author's order, and fixes which site the finding is filed
     at. The traversal can enter the component anywhere and the order comes
     out the same. *)
  List.filter_map !components ~f:(fun members ->
    match List.sort members ~cmp:compare with
    | [ r ] when not (List.mem r ~set:(succs r)) -> None
    | ms -> Some ms)
  |> List.sort ~cmp:compare
;;

(* The shortest way round from the first member back to it, breadth first
   over the edges that stay inside the component. The report shows the
   author this path, so a short one is the one to show. *)
let cycle_path (edges : left_edge list array) (members : int list)
  : (int * left_edge) list
  =
  let start = List.hd members in
  let came_from : (int, int * left_edge) Hashtbl.t = Hashtbl.create 8 in
  let queue = Queue.create () in
  Queue.add start queue;
  let closing = ref None in
  while Option.is_none !closing && not (Queue.is_empty queue) do
    let r = Queue.pop queue in
    List.iter edges.(r) ~f:(fun (e : left_edge) ->
      if Option.is_none !closing && List.mem e.target ~set:members
      then
        if e.target = start
        then closing := Some (r, e)
        else if not (Hashtbl.mem came_from e.target)
        then (
          Hashtbl.add came_from e.target (r, e);
          Queue.add e.target queue))
  done;
  let rec back (r : int) (acc : (int * left_edge) list) =
    if r = start
    then acc
    else (
      let from, e = Hashtbl.find came_from r in
      back from ((from, e) :: acc))
  in
  match !closing with
  | None -> []
  | Some (last, e) -> back last [] @ [ last, e ]
;;

(* A left recursion causes conflicts of its own. A rule that begins with
   itself also begins with whatever it takes first, so the child carrying the
   recursion overlaps a sibling or what follows it. A block on the cycle has an
   atom whose FIRST holds the others'. Those findings name the symptom, so the
   conflict checks skip them and the left recursion is reported on its own. *)
let recursion_of (shape : Stage.shape) (fixpoint_reader : Fixpoint.Reader.t) : recursion =
  let edges = left_edges shape fixpoint_reader in
  let edge_children = Hashtbl.create 8 in
  let blocks = Hashtbl.create 4 in
  let cycles =
    List.map (cyclic_components edges) ~f:(fun members ->
      List.iter members ~f:(fun r ->
        if shape.rules.(r).Rule.origin = Rule.Pratt_block then Hashtbl.replace blocks r ();
        List.iter edges.(r) ~f:(fun (e : left_edge) ->
          match e.through with
          | Through_child i when List.mem e.target ~set:members ->
            Hashtbl.replace edge_children (r, i) ()
          | Through_child _ | Through_atom -> ()));
      { members; path = cycle_path edges members })
  in
  { cycles; edge_children; blocks }
;;

type ctx =
  { shape : Stage.shape
  ; fixpoint_tables : Fixpoint.tables
  ; fixpoint_reader : Fixpoint.Reader.t
  ; kind_table : Kind.Table.t
  ; recursion : recursion
  ; repeated_empty : (int, unit) Hashtbl.t
    (** Rules that can match nothing and are an element of a repeated child. *)
  }

let kind_refs (ctx : ctx) (set : Kind.Set.t) : Error.kind_ref list =
  Error.kind_refs ctx.kind_table (Kind.Set.elements set)
;;

let user_rules (ctx : ctx) : Rule.def list =
  List.filter (Array.to_list ctx.shape.rules) ~f:(fun (d : Rule.def) ->
    d.origin = Rule.User)
;;

(* The rewrite to suggest. A rule that begins with itself through one child
   repeats what follows that child. A rule that is an atom of a block and
   begins with the block, then a token, is that block's operator. *)
let left_rewrite (ctx : ctx) (cycle : cycle) : Error.left_rewrite =
  let rules = ctx.shape.rules in
  let single_token (c : Rule.child) : Kind.t option =
    match c.modifier, c.alts with
    | Grammar.Exactly_one, [| k |]
      when Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k < 0 -> Some k
    | _ -> None
  in
  let names_block (c : Rule.child) (block : int) : bool =
    match c.modifier, c.alts with
    | Grammar.Exactly_one, [| k |] ->
      Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k = block
    | _ -> false
  in
  let operator ((r : int), (e : left_edge)) : Error.left_rewrite option =
    match e.through with
    | Through_atom -> None
    | Through_child i ->
      let d = rules.(r) in
      let atom_of_target =
        Array.exists ctx.shape.blocks ~f:(fun (b : Block.def) ->
          b.rule_id = e.target
          && Array.exists b.atoms ~f:(fun k ->
            Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k = r))
      in
      if (not atom_of_target) || i + 1 >= Array.length d.children
      then None
      else (
        match single_token d.children.(i + 1) with
        | None -> None
        | Some lead ->
          let last = Array.length d.children - 1 in
          Some
            (Error.Operator
               { block = rules.(e.target).name
               ; rule = d.name
               ; lead =
                   ctx.shape.names.tokens.(ctx.shape.names.kind_token.(Kind.to_int lead))
                     .name
               ; infix = last = i + 2 && names_block d.children.(last) e.target
               }))
  in
  match cycle.path with
  | [ (r, { target; through = Through_child i }) ]
    when target = r && rules.(r).origin = Rule.User ->
    Error.Repeat { rule = rules.(r).name; child = rules.(r).children.(i).child_name }
  | path ->
    (match List.find_map path ~f:operator with
     | Some rewrite -> rewrite
     | None -> Error.Break_cycle)
;;

(* -- first/first over one child's alternatives ----------------------------- *)

(* Alternative dispatch is a cascade. It takes the first arm whose FIRST
   admits the cursor. A leading token shared by two arms therefore always
   goes to the first of them. *)
let first_first (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    List.fold_left
      (List.mapi (Array.to_list d.children) ~f:(fun i ch -> i, ch))
      ~init:acc
      ~f:(fun acc ((i : int), (ch : Rule.child)) ->
        if Array.length ch.alts < 2 || Hashtbl.mem ctx.recursion.edge_children (d.id, i)
        then acc
        else (
          let firsts =
            Array.to_list
              (Array.map ~f:(Fixpoint.Reader.kind_first ctx.fixpoint_reader) ch.alts)
          in
          let rec overlap acc = function
            | [] | [ _ ] -> acc
            | x :: rest ->
              overlap
                (List.fold_left rest ~init:acc ~f:(fun a y ->
                   Kind.Set.union a (Kind.Set.inter x y)))
                rest
          in
          let common = overlap Kind.Set.empty firsts in
          if Kind.Set.is_empty common
          then acc
          else
            Error.make
              ~detail:(Error.First_first_conflict { common = kind_refs ctx common })
              (Error.At_child { production = d.name; child = ch.child_name })
            :: acc)))
;;

(* -- first/follow at a skippable position ---------------------------------- *)

let first_follow (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    let cs = d.children in
    let len = Array.length cs in
    let sfirst, spasses = Fixpoint.Reader.suffix_first ctx.fixpoint_reader cs in
    let acc = ref acc in
    for idx = 0 to len - 1 do
      let ch = cs.(idx) in
      (* The parser can pass over an optional or repeated slot. It can also pass
         over a required one whose target rule derives empty. Those are the
         positions where it has to choose. *)
      let skippable =
        match ch.modifier with
        | Grammar.Zero_or_one | Grammar.Zero_or_more _ -> true
        | Grammar.Exactly_one | Grammar.One_or_more _ ->
          Array.exists ~f:(Fixpoint.Reader.kind_nullable ctx.fixpoint_reader) ch.alts
      in
      if skippable
      then (
        let rest_first = sfirst.(idx + 1) in
        let rest_passes = spasses.(idx + 1) in
        let tail = idx = len - 1 in
        let delim_addition =
          if not tail
          then Kind.Set.empty
          else (
            match d.frame with
            | Rule.Delimited { close; sep; _ } ->
              let s = Kind.Set.singleton close in
              (match sep with
               | Some { sep_tok; _ } -> Kind.Set.add sep_tok s
               | None -> s)
            | Rule.Separated _ | Rule.Plain | Rule.Committed _ -> Kind.Set.empty)
        in
        (* In a delimited production, the close token sits between the tail position
           and the parent's continuation. So the parent's FOLLOW lies past it and
           does not apply here. *)
        let in_delimited_tail =
          tail
          &&
          match d.frame with
          | Rule.Delimited _ -> true
          | _ -> false
        in
        let parent_follow =
          if rest_passes && not in_delimited_tail
          then ctx.fixpoint_tables.follow.(d.id)
          else Kind.Set.empty
        in
        let following =
          match d.frame with
          (* A separated list runs its loop off the separator. It takes a separator,
             then re-enters the element. It leaves the loop when the cursor holds
             anything but a separator.

             So the question here is whether the separator appears in FIRST of the
             element. *)
          | Rule.Separated { sep_tok; _ } -> Kind.Set.singleton sep_tok
          | Rule.Delimited _ | Rule.Plain | Rule.Committed _ ->
            Kind.Set.unions [ rest_first; delim_addition; parent_follow ]
        in
        let common =
          Kind.Set.inter (Fixpoint.Reader.alts_first ctx.fixpoint_reader ch) following
        in
        (* A rule that can match nothing and repeats can follow itself, so
           what it begins with is in its own FOLLOW. That conflict is the
           repetition's, and [nullable_repeated] or [ambiguous_empty] reports
           it. *)
        let from_repetition =
          Hashtbl.mem ctx.repeated_empty d.id
          && Kind.Set.subset common ctx.fixpoint_tables.first.(d.id)
        in
        if
          not
            (Kind.Set.is_empty common
             || from_repetition
             || Hashtbl.mem ctx.recursion.edge_children (d.id, idx))
        then
          acc
          := Error.make
               ~detail:(Error.First_follow_conflict { common = kind_refs ctx common })
               (Error.At_child { production = d.name; child = ch.child_name })
             :: !acc)
    done;
    !acc)
;;

(* -- an operator after an expression ---------------------------------------- *)

(* An expression goes on while the cursor holds an infix operator or a
   postfix lead of its block, since a child parses it from binding power 0.
   So those tokens cannot also follow the child. Where one can, the
   expression takes it and the rest of the rule never sees it.

   What follows is read from [follow_outside]. An atom that ends in an
   expression is followed by its own block's operators, and the expression
   taking them is the binding power at work. *)
let operator_follow (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    let cs = d.children in
    let sfirst, spasses = Fixpoint.Reader.suffix_first ctx.fixpoint_reader cs in
    let after =
      match d.frame with
      | Rule.Delimited { close; sep = Some { sep_tok; _ }; _ } ->
        Kind.Set.of_list [ close; sep_tok ]
      | Rule.Delimited { close; sep = None; _ } -> Kind.Set.singleton close
      | Rule.Plain | Rule.Committed _ | Rule.Separated _ ->
        ctx.fixpoint_tables.follow_outside.(d.id)
    in
    let acc = ref acc in
    Array.iteri cs ~f:(fun idx (ch : Rule.child) ->
      let blocks =
        List.filter (Array.to_list ctx.shape.blocks) ~f:(fun (b : Block.def) ->
          Array.exists ch.alts ~f:(fun k ->
            Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k = b.rule_id))
      in
      let again =
        match ch.modifier, d.frame with
        | (Grammar.Exactly_one | Grammar.Zero_or_one), _ -> Kind.Set.empty
        | ( (Grammar.Zero_or_more _ | Grammar.One_or_more _)
          , ( Rule.Separated { sep_tok; _ }
            | Rule.Delimited { sep = Some { sep_tok; _ }; _ } ) ) ->
          Kind.Set.singleton sep_tok
        | (Grammar.Zero_or_more _ | Grammar.One_or_more _), _ ->
          Fixpoint.Reader.alts_first ctx.fixpoint_reader ch
      in
      let following =
        Kind.Set.unions
          [ sfirst.(idx + 1)
          ; again
          ; (if spasses.(idx + 1) then after else Kind.Set.empty)
          ]
      in
      List.iter blocks ~f:(fun (b : Block.def) ->
        let operators =
          Kind.Set.union
            (Kind.Set.of_list
               (List.map (Array.to_list b.infix) ~f:(fun (o : Block.op) -> o.op_kind)))
            (Kind.Set.of_list
               (List.map (Array.to_list b.postfix) ~f:(fun (p : Block.postfix) ->
                  p.p_lead)))
        in
        let common = Kind.Set.inter operators following in
        if
          not
            (Kind.Set.is_empty common
             || Hashtbl.mem ctx.recursion.edge_children (d.id, idx))
        then
          acc
          := Error.make
               ~detail:
                 (Error.Operator_follow_conflict
                    { block = b.name; common = kind_refs ctx common })
               (Error.At_child { production = d.name; child = ch.child_name })
             :: !acc));
    !acc)
;;

(* -- overrides nothing reads ------------------------------------------------- *)

(* A message is read where the child is reported missing, and a recovery set
   where the parse skips after that. An optional or repeated child is never
   missing, and neither is one that can match nothing. A missing token is
   reported where it is, and nothing skips, so its recovery set is never
   read. *)
let unused_overrides (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    Array.fold_left d.children ~init:acc ~f:(fun acc (ch : Rule.child) ->
      let never_missing : Error.unused_reason option =
        match ch.modifier with
        | Grammar.Zero_or_one | Grammar.Zero_or_more _ -> Some Error.Not_required
        | Grammar.Exactly_one | Grammar.One_or_more _ ->
          if Array.exists ch.alts ~f:(Fixpoint.Reader.kind_nullable ctx.fixpoint_reader)
          then Some Error.Matches_nothing
          else None
      in
      let one_token =
        match ch.alts with
        | [| k |] -> Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k < 0
        | _ -> false
      in
      let where = Error.At_child { production = d.name; child = ch.child_name } in
      let acc =
        match never_missing with
        | Some why
          when Array.exists d.messages ~f:(fun (name, _) ->
                 Grammar.Name.Child.equal name ch.child_name) ->
          Error.make
            ~detail:(Error.Unused_message_child { name = ch.child_name; why })
            where
          :: acc
        | Some _ | None -> acc
      in
      match ch.recover_to, never_missing with
      | None, _ -> acc
      | Some _, Some why ->
        Error.make ~detail:(Error.Unused_recover_to { name = ch.child_name; why }) where
        :: acc
      | Some _, None when one_token ->
        Error.make
          ~detail:
            (Error.Unused_recover_to { name = ch.child_name; why = Error.One_token })
          where
        :: acc
      | Some _, None -> acc))
;;

(* -- left recursion -------------------------------------------------------- *)

(* Joins names for a sentence: ["A"], then ["A and B"], then ["A, B and C"].
   The members of a left recursion read as a list in prose. A token set
   reads as a set in braces instead. *)
let rec join_names = function
  | [] -> ""
  | [ x ] -> x
  | [ x; y ] -> x ^ " and " ^ y
  | x :: rest -> x ^ ", " ^ join_names rest
;;

(* Writes up each left recursion the analysis found. *)
let left_recursion (ctx : ctx) (acc : Error.t list) : Error.t list =
  let name (r : int) : Grammar.Name.Rule.t = ctx.shape.rules.(r).Rule.name in
  List.fold_left (List.rev ctx.recursion.cycles) ~init:acc ~f:(fun acc (cycle : cycle) ->
    let steps =
      List.map cycle.path ~f:(fun ((r : int), (e : left_edge)) : Error.left_step ->
        { rule = name r
        ; reaches = name e.target
        ; through =
            (match e.through with
             | Through_child i ->
               Error.Through_child ctx.shape.rules.(r).children.(i).child_name
             | Through_atom -> Error.Through_atom)
        })
    in
    let members = List.map cycle.members ~f:name in
    Error.make
      ~detail:(Error.Left_recursion { members; steps; rewrite = left_rewrite ctx cycle })
      (Error.At_production (List.hd members))
    :: acc)
;;

(* -- nullability hazards --------------------------------------------------- *)

(* A repeated child needs one arm that takes a token, or the loop can never
   make progress.

   A separated production is skipped here. Its loop runs off the separator
   and never off element FIRST, so a nullable element still lets the loop
   advance. That element is rejected anyway, by
   [nullable_pratt_and_separated] below, for a different reason. *)
let nullable_repeated (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    match d.frame with
    | Rule.Separated _ -> acc
    | _ ->
      Array.fold_left d.children ~init:acc ~f:(fun acc (ch : Rule.child) ->
        if
          match ch.modifier with
          | Grammar.Zero_or_more _ -> false
          | Grammar.Exactly_one | Grammar.Zero_or_one | Grammar.One_or_more _ -> true
        then acc
        else if
          Array.length ch.alts = 0
          || not
               (Array.for_all
                  ~f:(Fixpoint.Reader.kind_nullable ctx.fixpoint_reader)
                  ch.alts)
        then acc
        else (
          let rep =
            ctx.shape.rules.(Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader ch.alts.(0))
              .Rule.name
          in
          Error.make
            ~detail:(Error.Nullable_repeated { rule = rep })
            (Error.At_child { production = d.name; child = ch.child_name })
          :: acc)))
;;

(* A child may match nothing in one way at most. A second way gives an empty
   child two trees, and the parser builds one of them without saying so. The
   second way is another alternative that can match nothing, or the child
   being optional, or the child repeating.

   Two cases have a check of their own. [nullable_repeated] reports a
   repeated child whose every alternative can match nothing.
   [nullable_pratt_and_separated] reports a separated element that can. This
   check skips both, so each shape gets one finding. *)
let ambiguous_empty (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    match d.frame with
    | Rule.Separated _ -> acc
    | Rule.Plain | Rule.Committed _ | Rule.Delimited _ ->
      Array.fold_left d.children ~init:acc ~f:(fun acc (ch : Rule.child) ->
        let nullable =
          List.filter
            (Array.to_list ch.alts)
            ~f:(Fixpoint.Reader.kind_nullable ctx.fixpoint_reader)
        in
        let every = List.length nullable = Array.length ch.alts in
        let how : Error.ambiguous_empty option =
          match ch.modifier, nullable with
          | _, [] -> None
          | Grammar.Exactly_one, [ _ ] -> None
          | Grammar.Exactly_one, _ -> Some Error.Another_alternative
          | Grammar.Zero_or_one, _ -> Some Error.Absent
          | Grammar.Zero_or_more _, _ when every -> None
          | (Grammar.Zero_or_more _ | Grammar.One_or_more _), _ -> Some Error.No_elements
        in
        match how with
        | None -> acc
        | Some how ->
          let rules =
            List.map nullable ~f:(fun k ->
              ctx.shape.rules.(Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k)
                .Rule.name)
          in
          Error.make
            ~detail:(Error.Ambiguous_empty { rules; how })
            (Error.At_child { production = d.name; child = ch.child_name })
          :: acc))
;;

(* [Fixpoint] works out nullability from a rule's children. It does not do
   that for an expression block or a separated production. Those two come out
   false whatever their children look like.

   That is right so long as every atom and every separated element consumes a
   token. An expression is then at least one atom or one prefix
   operator, and a separated list is at least one element. Neither can be
   empty.

   This check rejects a nullable atom and a nullable element, so it stays
   right.

   Without it the table would say a construct cannot derive empty when it
   can. FIRST and FOLLOW would come out short. [first_follow] would stop
   seeing a slot that holds the construct as skippable, so its test would
   never run there. *)
let nullable_pratt_and_separated (ctx : ctx) (acc : Error.t list) : Error.t list =
  let acc =
    Array.fold_left ctx.shape.blocks ~init:acc ~f:(fun acc (b : Block.def) ->
      Array.fold_left b.atoms ~init:acc ~f:(fun acc k ->
        let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
        if r >= 0 && ctx.fixpoint_tables.nullable.(r)
        then
          Error.make
            ~detail:
              (Error.Nullable_pratt_atom
                 { atom = Grammar.Name.Rule.to_string ctx.shape.rules.(r).Rule.name })
            (Error.At_block b.name)
          :: acc
        else acc))
  in
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    match d.frame with
    | Rule.Separated _ ->
      Array.fold_left d.children ~init:acc ~f:(fun acc (ch : Rule.child) ->
        Array.fold_left ch.alts ~init:acc ~f:(fun acc k ->
          let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
          if r >= 0 && ctx.fixpoint_tables.nullable.(r)
          then
            Error.make
              ~detail:
                (Error.Nullable_separated_element
                   { element = Grammar.Name.Rule.to_string ctx.shape.rules.(r).Rule.name })
              (Error.At_child { production = d.name; child = ch.child_name })
            :: acc
          else acc))
    | _ -> acc)
;;

(* -- a referenced rule with nothing to start it ---------------------------- *)

(* A rule with empty FIRST gives a [can_start] predicate that is always
   false. An optional or repeated reference to it is dropped. A required one
   is unreachable. *)
let empty_first_sets (ctx : ctx) (acc : Error.t list) : Error.t list =
  let sites = Hashtbl.create 16 in
  let note ~from k =
    let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
    if r >= 0
    then
      Hashtbl.replace
        sites
        r
        (from
         ::
         (try Hashtbl.find sites r with
          | Not_found -> []))
  in
  let () =
    List.iter (user_rules ctx) ~f:(fun (d : Rule.def) ->
      Array.iter
        ~f:(fun (ch : Rule.child) ->
          Array.iter
            ~f:
              (note
                 ~from:
                   (Grammar.Name.Rule.to_string d.name
                    ^ "."
                    ^ Grammar.Name.Child.to_string ch.child_name))
            ch.alts)
        d.children)
  in
  let () =
    Array.iter
      ~f:(fun (b : Block.def) ->
        Array.iter ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " atoms")) b.atoms;
        Array.iter
          ~f:(fun (p : Block.postfix) ->
            match p.p_body with
            | Block.Nothing -> ()
            | Block.Then rhs ->
              Array.iter
                ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " postfix rhs"))
                rhs
            | Block.Enclosed { content = Block.One s; _ } ->
              Array.iter
                ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " postfix body"))
                s
            | Block.Enclosed { content = Block.Many { elem; _ }; _ } ->
              Array.iter
                ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " postfix element"))
                elem)
          b.postfix)
      ctx.shape.blocks
  in
  Hashtbl.fold
    (fun r froms acc ->
       let d = ctx.shape.rules.(r) in
       if d.origin <> Rule.User
       then acc
       else if not (Kind.Set.is_empty ctx.fixpoint_tables.first.(r))
       then acc
       else
         Error.make
           ~detail:
             (Error.Empty_first_set
                { referenced_from = List.sort_uniq ~cmp:String.compare froms })
           (Error.At_production d.name)
         :: acc)
    sites
    acc
;;

(* -- operator conflicts that read first ------------------------------------ *)

let pratt_atom_first_first (ctx : ctx) (acc : Error.t list) : Error.t list =
  Array.fold_left ~init:acc ctx.shape.blocks ~f:(fun acc (b : Block.def) ->
    if Array.length b.atoms < 2 || Hashtbl.mem ctx.recursion.blocks b.rule_id
    then acc
    else (
      let firsts =
        Array.to_list
          (Array.map ~f:(Fixpoint.Reader.kind_first ctx.fixpoint_reader) b.atoms)
      in
      let rec overlap acc = function
        | [] | [ _ ] -> acc
        | x :: rest ->
          overlap
            (List.fold_left rest ~init:acc ~f:(fun a y ->
               Kind.Set.union a (Kind.Set.inter x y)))
            rest
      in
      let common = overlap Kind.Set.empty firsts in
      if Kind.Set.is_empty common
      then acc
      else
        Error.make
          ~detail:(Error.Pratt_atom_conflict { common = kind_refs ctx common })
          (Error.At_block b.name)
        :: acc))
;;

(* The left-hand-side dispatch reads the prefix table before the atoms. A
   prefix operator token that also appears in FIRST of an atom therefore always
   takes that token, and the atom never reads it. A token atom that is the
   operator itself goes the same way. *)
let prefix_atom_conflict (ctx : ctx) (acc : Error.t list) : Error.t list =
  Array.fold_left ctx.shape.blocks ~init:acc ~f:(fun acc (b : Block.def) ->
    if Hashtbl.mem ctx.recursion.blocks b.rule_id
    then acc
    else
      Array.fold_left b.prefix ~init:acc ~f:(fun acc (o : Block.op) ->
        Array.fold_left b.atoms ~init:acc ~f:(fun acc k ->
          let where =
            Error.At_operator
              { block = b.name
              ; token =
                  Grammar.Name.Token.of_string
                    (Kind.Name.to_string (Kind.Table.name ctx.kind_table o.op_kind))
              }
          in
          let report (how : Error.prefix_atom) =
            Error.make ~detail:(Error.Prefix_atom_conflict { how }) where :: acc
          in
          let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
          if r < 0
          then if Kind.equal k o.op_kind then report Error.Is_the_atom else acc
          else if ctx.shape.rules.(r).Rule.origin <> Rule.User
          then acc
          else if Kind.Set.mem ctx.fixpoint_tables.first.(r) o.op_kind
          then report (Error.Starts_the_atom { atom = ctx.shape.rules.(r).Rule.name })
          else acc)))
;;

(* -- token reachability ---------------------------------------------------- *)

(* A token whose case id never heads a state's accepts list wins max-munch
   from no state, so the lexer never emits it. There are two reasons that
   happens:

   1) the regex matches nothing; or
   2) an earlier-declared token beats it wherever it could match.

   For the second, the diagnostic names one token seen beating it, at one
   such state. *)
let token_reachability (ctx : ctx) (dfa : Redfa.Dfa.t) (acc : Error.t list) : Error.t list
  =
  let toks = ctx.shape.names.tokens in
  let n = Array.length toks in
  if n = 0
  then acc
  else (
    let head_seen = Array.make n false in
    let subsumer = Array.make n (-1) in
    Redfa.Dfa.iter_states dfa (fun st ->
      match Redfa.Dfa.accepts dfa st with
      | [] -> ()
      | head :: rest ->
        head_seen.(head) <- true;
        List.iter rest ~f:(fun cid ->
          if subsumer.(cid) < 0 || head < subsumer.(cid) then subsumer.(cid) <- head));
    let acc = ref acc in
    for i = n - 1 downto 0 do
      if not head_seen.(i)
      then (
        let t = toks.(i) in
        let reason : Error.token_unreachable_reason =
          if Redfa.Regex.is_empty_language t.regex
          then Error.Empty_language
          else if subsumer.(i) >= 0
          then Error.Subsumed_by toks.(subsumer.(i)).Token.name
          else Error.Empty_language
        in
        acc
        := Error.make ~detail:(Error.Token_unreachable { reason }) (Error.At_token t.name)
           :: !acc)
    done;
    !acc)
;;

(* A resync anchor ends a body before what continues it, so an anchor an
   element can start with ends the body before it ever takes one. shapes had
   exactly that: a block of declarations anchored on [let], which every
   declaration starts with. It read as correct until the body loop learned to
   recover. *)
let resync_anchor_conflict (ctx : ctx) (acc : Error.t list) : Error.t list =
  Array.fold_left ctx.shape.rules ~init:acc ~f:(fun acc (rule_def : Rule.def) ->
    match rule_def.origin, rule_def.frame, Rule.body_children rule_def with
    | Rule.User, Rule.Delimited _, [ ({ modifier = Grammar.Zero_or_more _; _ } as child) ]
      ->
      let first = Fixpoint.Reader.alts_first ctx.fixpoint_reader child in
      Kind.Set.fold
        (fun anchor acc ->
           if Kind.Set.mem first anchor
           then
             Error.make
               ~detail:
                 (Error.Resync_anchor_conflict
                    { anchor =
                        Grammar.Name.Token.of_string
                          (Kind.Name.to_string (Kind.Table.name ctx.kind_table anchor))
                    })
               (Error.At_production rule_def.name)
             :: acc
           else acc)
        rule_def.resync
        acc
    | _, _, _ -> acc)
;;

(* A metavariable stands where real text goes. A token that matches a string
   beginning with one would take it, at least in part, and the template would
   lex as something else. So every other token's language meets [m . any*]
   nowhere, for each metavariable [m], and the two metavariables meet each
   other nowhere either. *)
let metavariable_clash (ctx : ctx) (acc : Error.t list) : Error.t list =
  match ctx.shape.names.grammar.metavariables with
  | None -> acc
  | Some m ->
    let lowered (t : Grammar.token_def) : Redfa.Regex.t option =
      Result.to_option (Token.lower t.token_class)
    in
    let others =
      List.map (Array.to_list ctx.shape.names.tokens) ~f:(fun (t : Token.def) ->
        t.name, Some t.regex)
    in
    List.fold_left
      [ m.single, m.sequence; m.sequence, m.single ]
      ~init:acc
      ~f:(fun acc ((meta : Grammar.token_def), (other : Grammar.token_def)) ->
        match lowered meta with
        | None -> acc
        | Some regex ->
          let begins = Redfa.Regex.seq regex (Redfa.Regex.star Redfa.Regex.any) in
          List.fold_left
            ((other.token_name, lowered other) :: others)
            ~init:acc
            ~f:(fun acc ((name : Grammar.Name.Token.t), (r : Redfa.Regex.t option)) ->
              match r with
              | Some r
                when not (Redfa.Regex.is_empty_language (Redfa.Regex.inter r begins)) ->
                Error.make
                  ~detail:(Error.Metavariable_clash { token = name })
                  (Error.At_token meta.token_name)
                :: acc
              | Some _ | None -> acc))
;;

let run (shape : Stage.shape) (fixpoint_tables : Fixpoint.tables) (dfa : Redfa.Dfa.t)
  : Error.t list
  =
  let fixpoint_reader =
    Fixpoint.Reader.of_tables
      ~rules:shape.rules
      ~blocks:shape.blocks
      ~kind_rule:shape.names.kind_rule
      fixpoint_tables
  in
  let repeated_empty = Hashtbl.create 4 in
  Array.iter shape.rules ~f:(fun (d : Rule.def) ->
    match d.origin, d.frame with
    | Rule.User, (Rule.Plain | Rule.Committed _ | Rule.Delimited _) ->
      Array.iter d.children ~f:(fun (ch : Rule.child) ->
        match ch.modifier with
        | Grammar.Zero_or_more _ | Grammar.One_or_more _ ->
          Array.iter ch.alts ~f:(fun k ->
            let r = Fixpoint.Reader.rule_of_kind fixpoint_reader k in
            if r >= 0 && fixpoint_tables.nullable.(r)
            then Hashtbl.replace repeated_empty r ())
        | Grammar.Exactly_one | Grammar.Zero_or_one -> ())
    | _ -> ());
  let ctx =
    { shape
    ; fixpoint_tables
    ; fixpoint_reader
    ; kind_table = shape.names.kinds
    ; recursion = recursion_of shape fixpoint_reader
    ; repeated_empty
    }
  in
  first_first ctx []
  |> first_follow ctx
  |> operator_follow ctx
  |> left_recursion ctx
  |> nullable_repeated ctx
  |> ambiguous_empty ctx
  |> nullable_pratt_and_separated ctx
  |> empty_first_sets ctx
  |> resync_anchor_conflict ctx
  |> pratt_atom_first_first ctx
  |> prefix_atom_conflict ctx
  |> token_reachability ctx dfa
  |> metavariable_clash ctx
  |> unused_overrides ctx
;;
