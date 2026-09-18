open StdLabels

(* The key a view accessor is generated against. It is the child's symbol
   set, as spelled, in order.

   Two children with the same key share a slot family. The generated [skip]
   walks them in one ordering.

   Two children with different keys sit in separate buckets. Each counts its
   own occurrences from zero. That is why the same hazard takes three shapes
   below. *)
let kind_key (child : Rule.child) = Array.to_list (Array.map ~f:Kind.to_int child.alts)
let is_repeated (child : Rule.child) = child.modifier = Grammar.Repeated

let kind_refs (kind_table : Kind.Table.t) (kind_set : Kind.Set.t) : Error.kind_ref list =
  Error.kind_refs kind_table (Kind.Set.elements kind_set)
;;

let child_kinds (kind_table : Kind.Table.t) ({ alts; _ } : Rule.child)
  : Error.kind_ref list
  =
  Error.kind_refs kind_table (Array.to_list alts)
;;

let arity (shape : Stage.shape) (acc : Error.t list) : Error.t list =
  Array.fold_left shape.rules ~init:acc ~f:(fun acc (rule_def : Rule.def) ->
    match rule_def.origin with
    | Rule.Pratt_block | Rule.Pratt_role _ -> acc
    | Rule.User ->
      let body = Rule.body_children rule_def in
      let n = List.length body in
      (match rule_def.frame with
       | Rule.Plain | Rule.Committed _ -> acc
       | Rule.Delimited _ when n = 1 -> acc
       | Rule.Separated _ when n = 1 -> acc
       | Rule.Delimited _ ->
         (* The body loop runs until it reaches the close token. It does not
               consult element FIRST sets, so it loops over one element. *)
         Error.make
           ~detail:(Error.Delimited_arity { children = n })
           (Error.At_production rule_def.name)
         :: acc
       | Rule.Separated _ ->
         Error.make
           ~detail:(Error.Separated_arity { children = n })
           (Error.At_production rule_def.name)
         :: acc))
;;

(* The accessor for a required or optional child works in two steps. It
   filters the child nodes by kind, then picks one by a fixed occurrence
   rank. So it needs the run-time sequence of same-kind nodes to line up
   with that rank.

   Four shapes put the sequence and the rank out of step. Each returns the
   wrong node on a clean parse, with no diagnostic. *)
let view_hazards (shape : Stage.shape) acc =
  let tbl = shape.names.kinds in
  Array.fold_left shape.rules ~init:acc ~f:(fun acc (rule_def : Rule.def) ->
    match rule_def.origin with
    | Rule.Pratt_block | Rule.Pratt_role _ -> acc
    | Rule.User ->
      let children = Array.to_list rule_def.children in
      let indexed = List.mapi ~f:(fun i c -> i, c) children in
      let at = Error.At_production rule_def.name in
      (* Both children are in the same bucket. The repeated nodes pad the
            list that the single accessor indexes into. An earlier optional
            that is absent shifts every later occurrence by one. *)
      let buckets = Hashtbl.create 8 in
      List.iter
        ~f:(fun (i, c) ->
          let k = kind_key c in
          Hashtbl.replace
            buckets
            k
            ((i, c)
             ::
             (try Hashtbl.find buckets k with
              | Not_found -> [])))
        indexed;
      let acc =
        Hashtbl.fold
          (fun _ entries acc ->
             let ordered =
               List.sort ~cmp:(fun (i, _) (j, _) -> Int.compare i j) entries
             in
             let singles = List.filter ~f:(fun (_, c) -> not (is_repeated c)) ordered in
             let has_rep = List.exists ~f:(fun (_, c) -> is_repeated c) ordered in
             let named l = List.map ~f:(fun (_, (c : Rule.child)) -> c.child_name) l in
             let acc =
               if has_rep && singles <> []
               then
                 Error.make
                   ~detail:(Error.Repeated_vs_single { children = named ordered })
                   at
                 :: acc
               else acc
             in
             (* Where every slot is required, all of them are present on a clean parse.
                The two ranks then agree. *)
             let earlier_shadows =
               match List.rev singles with
               | [] | [ _ ] -> false
               | _last :: earlier ->
                 List.exists
                   ~f:(fun (_, (c : Rule.child)) -> c.modifier = Grammar.Optional)
                   earlier
             in
             if earlier_shadows
             then
               Error.make
                 ~detail:(Error.Ambiguous_same_kind_child { children = named singles })
                 at
               :: acc
             else acc)
          buckets
          acc
      in
      let shared (child1 : Rule.child) (child2 : Rule.child) =
        Kind.Set.inter child1.kinds child2.kinds
      in
      let reps = List.filter ~f:(fun (_, c) -> is_repeated c) indexed
      and singles = List.filter ~f:(fun (_, c) -> not (is_repeated c)) indexed in
      (* The two children are in different buckets, but their kinds overlap.
            The repeated child's [skip] passes over the single one, and both
            accessors admit the same kind at run time. *)
      let acc =
        List.fold_left reps ~init:acc ~f:(fun acc (_, (r : Rule.child)) ->
          List.fold_left singles ~init:acc ~f:(fun acc (_, (sg : Rule.child)) ->
            if kind_key r = kind_key sg
            then acc
            else if Kind.Set.is_empty (shared r sg)
            then acc
            else
              Error.make
                ~detail:
                  (Error.Repeated_vs_single_kinds
                     { repeated = r.child_name
                     ; single = sg.child_name
                     ; shared = kind_refs tbl (shared r sg)
                     })
                at
              :: acc))
      in
      (* Two single-valued children sit in different buckets, and their
            kinds intersect. Each one counts itself the first of its kind, so
            both accessors return the same node, whatever modifiers they
            carry. *)
      let rec pairs acc = function
        | [] -> acc
        | (_, (a : Rule.child)) :: rest ->
          let acc =
            List.fold_left rest ~init:acc ~f:(fun acc (_, (b : Rule.child)) ->
              if kind_key a = kind_key b
              then acc
              else if Kind.Set.is_empty (shared a b)
              then acc
              else
                Error.make
                  ~detail:
                    (Error.Overlapping_single_kinds
                       { a = a.child_name
                       ; a_kinds = child_kinds tbl a
                       ; b = b.child_name
                       ; b_kinds = child_kinds tbl b
                       })
                  at
                :: acc)
          in
          pairs acc rest
      in
      pairs acc singles)
;;

(* A resync anchor ends a repeated body before it reads past a token that
   belongs to an outer scope. Only a repeated child inside a matched pair has
   such a body. A production that repeats nothing has none to end, and a
   separated list has already ended wherever an anchor could sit, because its
   loop runs while the cursor is on its separator. Anchors anywhere else reach
   nothing, and used to do so in silence. *)
let resync (shape : Stage.shape) (acc : Error.t list) : Error.t list =
  Array.fold_left shape.rules ~init:acc ~f:(fun acc (rule_def : Rule.def) ->
    match rule_def.origin, Kind.Set.is_empty rule_def.resync with
    | (Rule.Pratt_block | Rule.Pratt_role _), _ | Rule.User, true -> acc
    | Rule.User, false ->
      let body : Error.resync_body option =
        match rule_def.frame, Rule.body_children rule_def with
        | Rule.Delimited _, [ { modifier = Grammar.Repeated; _ } ] -> None
        | Rule.Separated _, _ -> Some Error.Ends_at_its_separator
        | (Rule.Delimited _ | Rule.Plain | Rule.Committed _), _ ->
          Some Error.Repeats_nothing
      in
      (match body with
       | None -> acc
       | Some body ->
         Error.make
           ~detail:(Error.Unused_resync_anchors { body })
           (Error.At_production rule_def.name)
         :: acc))
;;

let run (s : Stage.shape) : Error.t list = [] |> arity s |> resync s |> view_hazards s
