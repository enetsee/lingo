open StdLabels

type tables =
  { first : Kind.Set.t array
  ; follow : Kind.Set.t array
  ; nullable : bool array
  ; enclosing : Kind.Set.t array
  }

(* Every table below is a least fixpoint of a monotone step. Visiting the
   rules in a different order gives the same answer. The order only changes
   how much work reaching that answer takes.

   A worklist recomputes a rule when something it reads has moved. Seed the
   queue with every rule. When one moves, wake the rules that read it. The
   cost then follows the edges of the dependency graph.

   [wakes r] names what to re-run when [r] moves. The pull-shaped tables and
   the push-shaped ones need different answers, so each caller supplies its
   own. *)
let solve n ~wakes ~step =
  let queued = Array.make n false in
  let q = Queue.create () in
  let enqueue i =
    if not queued.(i)
    then (
      queued.(i) <- true;
      Queue.add i q)
  in
  let moved r = List.iter ~f:enqueue (wakes r) in
  for i = 0 to n - 1 do
    enqueue i
  done;
  while not (Queue.is_empty q) do
    let i = Queue.pop q in
    queued.(i) <- false;
    step ~moved i
  done
;;

let pre_children (d : Rule.def) =
  Array.sub d.children ~pos:0 ~len:(min d.body_from (Array.length d.children))
;;

let body_children (d : Rule.def) =
  let len = Array.length d.children in
  let from = min d.body_from len in
  Array.sub d.children ~pos:from ~len:(len - from)
;;

(* The parts of a grammar every table below reads, gathered once.

   [readers.(r)] is the rules whose nullability and FIRST depend on [r]'s.
   Those are the rules that name it in a child, and the block rule that names
   it as an atom.

   [block_of_rule.(i)] is the block whose rule is [i], or [-1]. *)
type ctx =
  { rules : Rule.def array
  ; blocks : Block.def array
  ; n : int
  ; kind_rule : int array
  ; readers : int list array
  ; block_of_rule : int array
  }

let rule_of_kind (ctx : ctx) (k : Kind.t) : int =
  let i = Kind.to_int k in
  if i < Array.length ctx.kind_rule then ctx.kind_rule.(i) else -1
;;

let context ~(rules : Rule.def array) ~(blocks : Block.def array) ~(kind_rule : int array)
  : ctx
  =
  let n = Array.length rules in
  let ctx =
    { rules
    ; blocks
    ; n
    ; kind_rule
    ; readers = Array.make n []
    ; block_of_rule = Array.make n (-1)
    }
  in
  let reads ~(by : int) (k : Kind.t) : unit =
    let r = rule_of_kind ctx k in
    if r >= 0 && not (List.mem by ~set:ctx.readers.(r))
    then ctx.readers.(r) <- by :: ctx.readers.(r)
  in
  Array.iteri
    ~f:(fun i (d : Rule.def) ->
      Array.iter
        ~f:(fun (c : Rule.child) -> Array.iter ~f:(reads ~by:i) c.alts)
        d.children)
    rules;
  Array.iter
    ~f:(fun (b : Block.def) -> Array.iter ~f:(reads ~by:b.rule_id) b.atoms)
    blocks;
  Array.iteri ~f:(fun i (b : Block.def) -> ctx.block_of_rule.(b.rule_id) <- i) blocks;
  ctx
;;

(* -- helpers over a table, shared by more than one of the walks ------------ *)

(* A kind is nullable where it is a rule and that rule is. A token takes a
   byte: a token whose regex matches the empty string is rejected before
   this runs. *)
let kind_nullable (ctx : ctx) ~(nullable : bool array) (k : Kind.t) : bool =
  let r = rule_of_kind ctx k in
  r >= 0 && nullable.(r)
;;

let child_nullable (ctx : ctx) ~(nullable : bool array) (c : Rule.child) : bool =
  match c.modifier with
  | Grammar.Zero_or_one | Grammar.Zero_or_more -> true
  (* One or more takes its first, so it is nullable only where that one is. *)
  | Grammar.Exactly_one | Grammar.One_or_more ->
    Array.exists ~f:(kind_nullable ctx ~nullable) c.alts
;;

let kind_first (ctx : ctx) ~(first : Kind.Set.t array) (k : Kind.t) : Kind.Set.t =
  let r = rule_of_kind ctx k in
  if r >= 0 then first.(r) else Kind.Set.singleton k
;;

let alts_first (ctx : ctx) ~(first : Kind.Set.t array) (c : Rule.child) : Kind.Set.t =
  Array.fold_left c.alts ~init:Kind.Set.empty ~f:(fun acc k ->
    Kind.Set.union acc (kind_first ctx ~first k))
;;

let rule_targets (ctx : ctx) (c : Rule.child) : int list =
  Array.fold_left c.alts ~init:[] ~f:(fun acc k ->
    let r = rule_of_kind ctx k in
    if r >= 0 then r :: acc else acc)
;;

let frame_closers (f : Rule.frame) : Kind.Set.t =
  match f with
  | Rule.Delimited { close; sep; _ } ->
    let s = Kind.Set.singleton close in
    (match sep with
     | Some { sep_tok; _ } -> Kind.Set.add sep_tok s
     | None -> s)
  | Rule.Separated { sep_tok; _ } -> Kind.Set.singleton sep_tok
  | Rule.Plain | Rule.Committed _ -> Kind.Set.empty
;;

(* -- nullable -------------------------------------------------------------- *)

let nullable (ctx : ctx) : bool array =
  let nullable = Array.make ctx.n false in
  let all_nullable (cs : Rule.child array) : bool =
    Array.for_all ~f:(child_nullable ctx ~nullable) cs
  in
  let rule_nullable (d : Rule.def) : bool =
    match d.origin with
    (* An expression takes at least one atom or prefix operator. A nullable
       atom is rejected before this runs. *)
    | Rule.Pratt_block -> false
    | Rule.Pratt_role _ | Rule.User ->
      all_nullable (pre_children d)
      &&
        (match d.frame with
        (* A delimited body takes its open token whatever its children do. *)
        | Rule.Delimited _ -> false
        | Rule.Separated _ | Rule.Plain | Rule.Committed _ ->
          all_nullable (body_children d))
  in
  solve
    ctx.n
    ~wakes:(fun r -> ctx.readers.(r))
    ~step:(fun ~moved i ->
      let now = rule_nullable ctx.rules.(i) in
      if nullable.(i) <> now
      then (
        nullable.(i) <- now;
        moved i));
  nullable
;;

(* -- first ----------------------------------------------------------------- *)

let first (ctx : ctx) ~(nullable : bool array) : Kind.Set.t array =
  let first = Array.make ctx.n Kind.Set.empty in
  (* FIRST of a child sequence, and whether the whole of it can pass without
     consuming. *)
  let seq_first (cs : Rule.child array) : Kind.Set.t * bool =
    let rec go i acc =
      if i >= Array.length cs
      then acc, true
      else (
        let c = cs.(i) in
        let acc = Kind.Set.union acc (alts_first ctx ~first c) in
        if child_nullable ctx ~nullable c then go (i + 1) acc else acc, false)
    in
    go 0 Kind.Set.empty
  in
  let block_first (b : Block.def) : Kind.Set.t =
    let atoms =
      Array.fold_left b.atoms ~init:Kind.Set.empty ~f:(fun acc k ->
        Kind.Set.union acc (kind_first ctx ~first k))
    in
    Array.fold_left b.prefix ~init:atoms ~f:(fun acc (o : Block.op) ->
      Kind.Set.add o.op_kind acc)
  in
  let rule_first (i : int) (d : Rule.def) : Kind.Set.t =
    if ctx.block_of_rule.(i) >= 0
    then block_first ctx.blocks.(ctx.block_of_rule.(i))
    else (
      (* Children before [body_from] sit outside the frame: the operand a
         postfix applies to. The frame's opener reaches the front where they
         can all pass without consuming. For a production [body_from] is 0
         and this is the frame's own rule. *)
      let pre, pre_passes = seq_first (pre_children d) in
      if not pre_passes
      then pre
      else (
        let body = body_children d in
        Kind.Set.union
          pre
          (match d.frame with
           | Rule.Delimited { open_; _ } -> Kind.Set.singleton open_
           | Rule.Separated _ ->
             (* A separated body takes one element, then enters the separator loop. So
                FIRST is the element's FIRST, whatever modifier the child slot carries. *)
             if Array.length body = 0
             then Kind.Set.empty
             else alts_first ctx ~first body.(0)
           | Rule.Plain | Rule.Committed _ -> fst (seq_first body))))
  in
  solve
    ctx.n
    ~wakes:(fun r -> ctx.readers.(r))
    ~step:(fun ~moved i ->
      let now = rule_first i ctx.rules.(i) in
      if not (Kind.Set.equal first.(i) now)
      then (
        first.(i) <- now;
        moved i));
  first
;;

(* -- follow ---------------------------------------------------------------- *)

let follow (ctx : ctx) ~(nullable : bool array) ~(first : Kind.Set.t array)
  : Kind.Set.t array
  =
  let follow = Array.make ctx.n Kind.Set.empty in
  let propagate (r : int) (added : Kind.Set.t) : bool =
    let cur = follow.(r) in
    let next = Kind.Set.union cur added in
    if not (Kind.Set.equal cur next)
    then (
      follow.(r) <- next;
      true)
    else false
  in
  (* What can follow a block's own expression position.

     An expression sits immediately left of every infix token and every
     postfix lead. Inside an enclosed postfix it also sits left of that
     form's separator and close. *)
  let block_own_ops (b : Block.def) : Kind.Set.t =
    let init =
      Array.fold_left b.infix ~init:Kind.Set.empty ~f:(fun acc (o : Block.op) ->
        Kind.Set.add o.op_kind acc)
    in
    Array.fold_left b.postfix ~init ~f:(fun acc (p : Block.postfix) ->
      let acc = Kind.Set.add p.p_lead acc in
      match p.p_body with
      | Block.Nothing | Block.Then _ -> acc
      | Block.Enclosed { close; content } ->
        let acc = Kind.Set.add close acc in
        (match content with
         | Block.One _ -> acc
         | Block.Many { sep = Some { sep_tok; _ }; _ } -> Kind.Set.add sep_tok acc
         | Block.Many { sep = None; _ } -> acc))
  in
  let suffixes =
    Array.map
      ~f:(fun (d : Rule.def) ->
        let cs = d.children in
        let len = Array.length cs in
        let sfirst = Array.make (len + 1) Kind.Set.empty in
        let spasses = Array.make (len + 1) true in
        for k = len - 1 downto 0 do
          let c = cs.(k) in
          let here = alts_first ctx ~first c in
          if child_nullable ctx ~nullable c
          then (
            sfirst.(k) <- Kind.Set.union here sfirst.(k + 1);
            spasses.(k) <- spasses.(k + 1))
          else (
            sfirst.(k) <- here;
            spasses.(k) <- false)
        done;
        sfirst, spasses)
      ctx.rules
  in
  (* A rule's step reads its own FOLLOW and writes its children's targets'.
     A block's clause reads the block rule's FOLLOW. Nothing else reads
     either one.

     So a change to one rule's FOLLOW wakes that rule alone. The two clauses
     are one step, because they read the same value. *)
  solve
    ctx.n
    ~wakes:(fun r -> [ r ])
    ~step:(fun ~moved i ->
      let push r added = if propagate r added then moved r in
      let d = ctx.rules.(i) in
      (* Role rules take no part in this walk. The block clause below carries
         their contributions instead. *)
      if not (Rule.is_synthetic d)
      then (
        let parent_follow = follow.(i) in
        let trailing_follow =
          match d.frame with
          (* Inside a delimited body, the close token bounds what can follow. The
             parent's FOLLOW sits past the closer, so it does not apply here. *)
          | Rule.Delimited { close; _ } -> Kind.Set.singleton close
          | Rule.Separated _ | Rule.Plain | Rule.Committed _ -> parent_follow
        in
        let cs = d.children in
        let len = Array.length cs in
        let sfirst, spasses = suffixes.(i) in
        for idx = 0 to len - 1 do
          let c = cs.(idx) in
          let rest_first = sfirst.(idx + 1) in
          let rest_passes = spasses.(idx + 1) in
          (* Another repeat can follow this one. So the next token is the next
             element's FIRST, or the separator if the frame has one. *)
          let rest_first =
            match c.modifier with
            | Grammar.Exactly_one | Grammar.Zero_or_one -> rest_first
            | Grammar.Zero_or_more | Grammar.One_or_more ->
              (match d.frame with
               | Rule.Delimited { sep = Some { sep_tok; _ }; _ }
               | Rule.Separated { sep_tok; _ } -> Kind.Set.add sep_tok rest_first
               | _ -> Kind.Set.union rest_first (alts_first ctx ~first c))
          in
          List.iter
            ~f:(fun r ->
              push r rest_first;
              if rest_passes then push r trailing_follow)
            (rule_targets ctx c)
        done);
      if ctx.block_of_rule.(i) >= 0
      then (
        let b = ctx.blocks.(ctx.block_of_rule.(i)) in
        push b.rule_id (block_own_ops b);
        let efollow = follow.(b.rule_id) in
        let to_kinds set ks =
          Array.iter
            ~f:(fun k ->
              let r = rule_of_kind ctx k in
              if r >= 0 then push r set)
            ks
        in
        (* An atom sits in the block's expression position, so what follows
            the block follows the atom. *)
        to_kinds efollow b.atoms;
        Array.iter
          ~f:(fun (p : Block.postfix) ->
            match p.p_body with
            | Block.Nothing -> ()
            (* The right-hand side of an access sits in expression
                 position. *)
            | Block.Then rhs -> to_kinds efollow rhs
            | Block.Enclosed { close; content } ->
              (* Inside the delimiters, the content is followed by the
                   closer, or by the separator where there is one. *)
              let inside =
                match content with
                | Block.Many { sep = Some { sep_tok; _ }; _ } ->
                  Kind.Set.of_list [ close; sep_tok ]
                | Block.One _ | Block.Many _ -> Kind.Set.singleton close
              in
              to_kinds
                inside
                (match content with
                 | Block.One s -> s
                 | Block.Many { elem; _ } -> elem))
          b.postfix));
  (* A role node sits in the block's expression position, so what follows the
     block follows it too. The copy below happens after the walk has finished,
     so that no role rule can feed a block's own set. *)
  Array.iteri
    ~f:(fun i (d : Rule.def) ->
      match d.origin with
      | Rule.Pratt_role { block; _ } -> follow.(i) <- follow.(block)
      | Rule.User | Rule.Pratt_block -> ())
    ctx.rules;
  follow
;;

(* -- enclosing ------------------------------------------------------------- *)

(* A rule is referenced from several sites, and the framing differs between
   them. Its enclosing set is the union over those sites. That makes it a
   fixpoint of the same shape as the FOLLOW walk above. *)
let enclosing (ctx : ctx) : Kind.Set.t array =
  let enclosing = Array.make ctx.n Kind.Set.empty in
  let widen (r : int) (added : Kind.Set.t) : bool =
    let cur = enclosing.(r) in
    let next = Kind.Set.union cur added in
    if not (Kind.Set.equal cur next)
    then (
      enclosing.(r) <- next;
      true)
    else false
  in
  (* This has the same shape as the FOLLOW walk, and the same reader relation.
     A rule's step reads its own enclosing set. A block's clause reads the
     block rule's. *)
  solve
    ctx.n
    ~wakes:(fun r -> [ r ])
    ~step:(fun ~moved i ->
      let push r added = if widen r added then moved r in
      let d = ctx.rules.(i) in
      (* Role rules take no part here either. The block clause below carries 
         them as it does in the walk above. *)
      if not (Rule.is_synthetic d)
      then (
        let inherited = enclosing.(i) in
        let own = frame_closers d.frame in
        Array.iteri
          ~f:(fun idx (c : Rule.child) ->
            (* A child from [body_from] on sits inside this rule's own frame. A 
               child before it is a postfix operand, which sits to the left of 
               the opener and outside the frame. Both sit inside whatever 
               encloses the rule itself. *)
            let here =
              if idx >= d.body_from then Kind.Set.union own inherited else inherited
            in
            List.iter ~f:(fun r -> push r here) (rule_targets ctx c))
          d.children);
      if ctx.block_of_rule.(i) >= 0
      then (
        let b = ctx.blocks.(ctx.block_of_rule.(i)) in
        let inherited = enclosing.(b.rule_id) in
        let to_kinds set ks =
          Array.iter
            ~f:(fun k ->
              let r = rule_of_kind ctx k in
              if r >= 0 then push r set)
            ks
        in
        (* An atom sits in the block's own expression position so the frames 
           around the block are the frames around it. *)
        to_kinds inherited b.atoms;
        Array.iter
          ~f:(fun (p : Block.postfix) ->
            match p.p_body with
            | Block.Nothing -> ()
            | Block.Then rhs -> to_kinds inherited rhs
            | Block.Enclosed { close; content } ->
              (* An enclosed postfix is a frame and its content sit inside it. *)
              let own =
                match content with
                | Block.Many { sep = Some { sep_tok; _ }; _ } ->
                  Kind.Set.of_list [ close; sep_tok ]
                | Block.One _ | Block.Many _ -> Kind.Set.singleton close
              in
              to_kinds
                (Kind.Set.union own inherited)
                (match content with
                 | Block.One s -> s
                 | Block.Many { elem; _ } -> elem))
          b.postfix));
  (* A role node sits where the block sits, so they are in the same frames.
     Set after the traversal for the reason FOLLOW is. *)
  Array.iteri
    ~f:(fun i (d : Rule.def) ->
      match d.origin with
      | Rule.Pratt_role { block; _ } -> enclosing.(i) <- enclosing.(block)
      | Rule.User | Rule.Pratt_block -> ())
    ctx.rules;
  enclosing
;;

let compute ~(rules : Rule.def array) ~(blocks : Block.def array) ~(kind_rule : int array)
  : tables
  =
  let ctx = context ~rules ~blocks ~kind_rule in
  let nullable = nullable ctx in
  let first = first ctx ~nullable in
  let follow = follow ctx ~nullable ~first in
  let enclosing = enclosing ctx in
  { first; follow; nullable; enclosing }
;;

(* -- reading the tables back ----------------------------------------------- *)

(* The same questions the walks above ask while building the tables, asked
   once they are built. A check over a finished [tables] would otherwise
   carry its own copy of each. *)
module Reader = struct
  type t =
    { ctx : ctx
    ; tables : tables
    }

  let of_tables
        ~(rules : Rule.def array)
        ~(blocks : Block.def array)
        ~(kind_rule : int array)
        (tables : tables)
    : t
    =
    { ctx = context ~rules ~blocks ~kind_rule; tables }
  ;;

  let rule_of_kind (r : t) (k : Kind.t) : int = rule_of_kind r.ctx k

  let kind_nullable (r : t) (k : Kind.t) : bool =
    kind_nullable r.ctx ~nullable:r.tables.nullable k
  ;;

  let child_nullable (r : t) (c : Rule.child) : bool =
    child_nullable r.ctx ~nullable:r.tables.nullable c
  ;;

  let kind_first (r : t) (k : Kind.t) : Kind.Set.t =
    kind_first r.ctx ~first:r.tables.first k
  ;;

  let alts_first (r : t) (c : Rule.child) : Kind.Set.t =
    alts_first r.ctx ~first:r.tables.first c
  ;;

  (* FIRST of every suffix of a child sequence, and whether the whole of each
     suffix can pass without consuming. Index [k] describes the children from
     [k]; index [len] is the empty suffix, which passes. One right-to-left
     pass gives them all. *)
  let suffix_first (r : t) (cs : Rule.child array) : Kind.Set.t array * bool array =
    let len = Array.length cs in
    let sfirst = Array.make (len + 1) Kind.Set.empty in
    let spasses = Array.make (len + 1) true in
    for k = len - 1 downto 0 do
      let c = cs.(k) in
      let here = alts_first r c in
      if child_nullable r c
      then (
        sfirst.(k) <- Kind.Set.union here sfirst.(k + 1);
        spasses.(k) <- spasses.(k + 1))
      else (
        sfirst.(k) <- here;
        spasses.(k) <- false)
    done;
    sfirst, spasses
  ;;
end
