open StdLabels

(* One step of a path, inside one activation. A step exists for every place
   the interpreter's walk can descend to, and for nothing else.

   [Operand] and [Climbing] are the Pratt steps: the level is the [min_bp] the
   parse is at, and the phase is whether the left side has been read. pigeon
   compiled a block into a table of those states, kept in step with the parser
   by hand. Here they are the path. *)
type step =
  | Enter of int
  | Item of int
  | Arm of int
  | Child
  | Loop of int
  | Emits of
      { state : int
      ; goto : int
      }
  | Operand of int
  | Climbing of int
  | Postfix of int

module State = struct
  (* Steps, innermost first. An [Enter] starts a frame, and the steps in front
     of it are the path into that rule's body.

     A walk pushes a step at every descent, so a cons is the whole cost.
     Rebuilding a frame record beside it ran a fifth slower on the bench's most
     deeply nested row. *)
  type t = step list

  let enter (rule : int) : t = [ Enter rule ]
  let call (t : t) (rule : int) : t = Enter rule :: t
  let item (t : t) (index : int) : t = Item index :: t
  let arm (t : t) (index : int) : t = Arm index :: t
  let child (t : t) : t = Child :: t
  let loop (t : t) (state : int) : t = Loop state :: t
  let emits (t : t) ~(state : int) ~(goto : int) : t = Emits { state; goto } :: t
  let operand (t : t) (min_bp : int) : t = Operand min_bp :: t
  let climbing (t : t) (min_bp : int) : t = Climbing min_bp :: t
  let postfix (t : t) (index : int) : t = Postfix index :: t
  let equal (t : t) (other : t) : bool = t = other

  (* The steps down to and including the [Enter] that opened the frame. A
     walk pushes [Enter] at every call, so the first one is the innermost. *)
  let site (t : t) : t =
    let rec go (steps : t) (acc : t) : t =
      match steps with
      | [] -> List.rev acc
      | (Enter _ as step) :: _ -> List.rev (step :: acc)
      | step :: rest -> go rest (step :: acc)
    in
    go t []
  ;;

  (* The steps of a path come back in the order the walk took them, and the
     frames innermost first. *)
  let frames (t : t) : (int * step list) list =
    let rec go (path : step list) (steps : t) (acc : (int * step list) list) =
      match steps with
      | [] -> List.rev acc
      | Enter rule :: rest -> go [] rest ((rule, path) :: acc)
      | step :: rest -> go (step :: path) rest acc
    in
    go [] t []
  ;;

  let pp_step (ppf : Format.formatter) (step : step) : unit =
    match step with
    | Enter rule -> Format.fprintf ppf "rule %d" rule
    | Item index -> Format.fprintf ppf "item %d" index
    | Arm index -> Format.fprintf ppf "arm %d" index
    | Child -> Format.pp_print_string ppf "child"
    | Loop state -> Format.fprintf ppf "loop %d" state
    | Emits e -> Format.fprintf ppf "emits %d goto %d" e.state e.goto
    | Operand min_bp -> Format.fprintf ppf "operand %d" min_bp
    | Climbing min_bp -> Format.fprintf ppf "climbing %d" min_bp
    | Postfix index -> Format.fprintf ppf "postfix %d" index
  ;;

  let pp_frame (ppf : Format.formatter) ((rule, path) : int * step list) : unit =
    Format.fprintf ppf "rule %d" rule;
    List.iter path ~f:(fun step -> Format.fprintf ppf "/%a" pp_step step)
  ;;

  (* Prints outermost first, so a reader follows the parse down. *)
  let pp (ppf : Format.formatter) (t : t) : unit =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf " > ")
      pp_frame
      ppf
      (List.rev (frames t))
  ;;
end

(* -- the sets an instruction dispatches on --------------------------------- *)

let arms_gate (arms : (Kind.t array * Plan.instr) array) : Kind.t list =
  Array.fold_left arms ~init:[] ~f:(fun acc (on, _) -> Array.to_list on @ acc)
;;

let accepts (state : Plan.loop_state) : Kind.t list =
  Array.fold_left state.accepts ~init:[] ~f:(fun acc (on, _) -> Array.to_list on @ acc)
;;

(* A state that reports on the way out is one the grammar does not let a body
   end at. A separated list forbidding a trailing separator is the case: it
   ends after its separator only by taking bytes the source had and reporting
   them. *)
let ends (state : Plan.loop_state) : bool =
  match state.exit with
  | Plan.May_exit -> true
  | Plan.May_exit_reporting _ -> false
;;

(* A state runs one instruction whichever transition fires, so where it went
   is the only record of what got in. *)
let taken (state : Plan.loop_state) ~(goto : int) : Kind.t list =
  Array.fold_left state.accepts ~init:[] ~f:(fun acc (on, dest) ->
    if Int.equal dest goto then Array.to_list on @ acc else acc)
;;

let head_set (block : Plan.block) : Kind.t list =
  Array.fold_left block.atoms ~init:[] ~f:(fun acc (on, _) -> Array.to_list on @ acc)
  @ Array.to_list (Array.map block.prefix ~f:fst)
;;

let climb_set (block : Plan.block) ~(min_bp : int) : Kind.t list =
  Array.fold_left block.infix ~init:[] ~f:(fun acc (tok, (left_bp, _)) ->
    if left_bp >= min_bp then tok :: acc else acc)
  @ Array.fold_left block.postfix ~init:[] ~f:(fun acc (q : Plan.postfix) ->
    if q.bp >= min_bp then q.lead :: acc else acc)
;;

(* -- what is left of an instruction ---------------------------------------- *)

let rec whole (plan : Plan.t) (null : bool array) (instr : Plan.instr)
  : Kind.t list * bool
  =
  match instr with
  | Plan.Seq instrs -> from plan null instrs 0
  | Plan.Open _ | Plan.Close | Plan.Trivia -> [], true
  (* A bump takes whatever is under the cursor and names no kind of its own.
     The kinds it can take are the ones the dispatch above let through, and
     [entered] uses that set, and it runs before this is reached. *)
  | Plan.Bump -> [], false
  (* Nothing may appear where a drain is, and it takes nothing where nothing
     is left. *)
  | Plan.Drain _ -> [], true
  | Plan.Expect e -> [ e.tok ], false
  | Plan.Call rule -> Array.to_list plan.rules.(rule).first, null.(rule)
  | Plan.Pratt p -> head_set plan.blocks.(p.block), false
  (* An alt that finds no arm runs nothing, so it can always be left out. *)
  | Plan.Alt a -> arms_gate a.arms, true
  | Plan.Commit c -> Array.to_list c.first, false
  (* [ends_on] is missing from this on purpose. It holds the closer, which the
     instruction after the loop takes anyway, and the resync anchors, which
     are what recovery stops at rather than what the grammar admits. *)
  | Plan.Loop l -> accepts l.states.(l.entry), ends l.states.(l.entry)

and from (plan : Plan.t) (null : bool array) (instrs : Plan.instr array) (index : int)
  : Kind.t list * bool
  =
  if index >= Array.length instrs
  then [], true
  else (
    let first, nullable = whole plan null instrs.(index) in
    if nullable
    then (
      let after, after_nullable = from plan null instrs (index + 1) in
      first @ after, after_nullable)
    else first, false)
;;

(* A rule is nullable where its body can complete without taking a token. A
   [Call] makes that a fixpoint: reading one needs the result for the rule it
   names, and a grammar may be recursive. The approximation only grows, so
   the loop settles. *)
let nullable_rules (plan : Plan.t) : bool array =
  let null = Array.make (Array.length plan.rules) false in
  let settled = ref false in
  while not !settled do
    settled := true;
    Array.iteri plan.rules ~f:(fun index (rule : Plan.rule) ->
      if (not null.(index)) && snd (whole plan null rule.body)
      then (
        null.(index) <- true;
        settled := false))
  done;
  null
;;

(* A state carried to a plan it was not built from. Every index checked this
   way comes off the state. The plan's own indices are [Plan.Check]'s. *)
let within (what : string) (count : int) (index : int) : int =
  if index < 0 || index >= count
  then
    invalid_arg
      (Printf.sprintf "Residual.at: no %s %d; the plan holds %d" what index count);
  index
;;

let mismatch (step : step) (instr : Plan.instr) : 'a =
  let form =
    match instr with
    | Plan.Seq _ -> "seq"
    | Plan.Open _ -> "open"
    | Plan.Close -> "close"
    | Plan.Trivia -> "trivia"
    | Plan.Bump -> "bump"
    | Plan.Expect _ -> "expect"
    | Plan.Call _ -> "call"
    | Plan.Pratt _ -> "pratt"
    | Plan.Alt _ -> "alt"
    | Plan.Commit _ -> "commit"
    | Plan.Loop _ -> "loop"
    | Plan.Drain _ -> "drain"
  in
  invalid_arg
    (Format.asprintf "Residual.at: %a does not step into a %s" State.pp_step step form)
;;

(* What is left of [instr] from the position [steps] names: the kinds that may
   appear, and whether all of it can be left out.

   [inclusive] says whether the instruction the steps end at is still to run.
   The innermost frame is the parse's own position, so it is. A frame above is
   waiting on the frame below it, and what that frame does is counted there
   already, so it is not. *)
let rec remains
          (plan : Plan.t)
          (null : bool array)
          ~(inclusive : bool)
          (instr : Plan.instr)
          (steps : step list)
  : Kind.t list * bool
  =
  match steps, instr with
  | [], _ -> if inclusive then whole plan null instr else [], true
  | Item index :: rest, Plan.Seq instrs ->
    let index = within "instruction" (Array.length instrs) index in
    let first, nullable = remains plan null ~inclusive instrs.(index) rest in
    if nullable
    then (
      let after, after_nullable = from plan null instrs (index + 1) in
      first @ after, after_nullable)
    else first, false
  (* Finishing an arm finishes the alt, so nothing here follows it. *)
  | Arm index :: rest, Plan.Alt a ->
    let on, body = a.arms.(within "arm" (Array.length a.arms) index) in
    entered plan null ~inclusive ~gate:(Array.to_list on) body rest
  | Child :: rest, Plan.Commit c ->
    entered plan null ~inclusive ~gate:(Array.to_list c.first) c.body rest
  | Loop state :: [], Plan.Loop l ->
    let state = l.states.(within "loop state" (Array.length l.states) state) in
    accepts state, ends state
  (* A loop state is where a path ends. What the loop runs is reached through
     [Emits], which names the state it goes on to. *)
  | Loop _ :: _ :: _, Plan.Loop _ ->
    invalid_arg "Residual.at: a path does not carry on past a loop state"
  | Emits e :: rest, Plan.Loop l ->
    let count = Array.length l.states in
    let goto = within "loop state" count e.goto in
    let state = l.states.(within "loop state" count e.state) in
    let first, nullable =
      entered plan null ~inclusive ~gate:(taken state ~goto) state.emits rest
    in
    if nullable
    then first @ accepts l.states.(goto), ends l.states.(goto)
    else first, false
  | ((Operand _ | Climbing _ | Postfix _) :: _ as steps), Plan.Pratt p ->
    expression plan null ~inclusive plan.blocks.(p.block) steps
  | step :: _, instr -> mismatch step instr

(* A body a dispatch chose, still to run. The dispatch read the cursor and
   found one of [gate] there, so those are the kinds that may appear and the
   body is about to take one. Reading the body instead makes an alternation
   look skippable: an alt with no arm for the kind under the cursor runs
   nothing, and the dispatch has just ruled that out. *)
and entered
      (plan : Plan.t)
      (null : bool array)
      ~(inclusive : bool)
      ~(gate : Kind.t list)
      (body : Plan.instr)
      (steps : step list)
  : Kind.t list * bool
  =
  match steps, inclusive with
  | [], true -> gate, false
  | [], false -> [], true
  | _ :: _, _ -> remains plan null ~inclusive body steps

(* An expression, at the phase and the level the steps name. A climb may end
   wherever it is and an operand may not. That is the whole difference between
   the two phases here. *)
and expression
      (plan : Plan.t)
      (null : bool array)
      ~(inclusive : bool)
      (block : Plan.block)
      (steps : step list)
  : Kind.t list * bool
  =
  match steps with
  | Operand min_bp :: rest ->
    let first, nullable =
      match rest with
      | [] -> if inclusive then head_set block, false else [], true
      | _ -> expression plan null ~inclusive block rest
    in
    if nullable then first @ climb_set block ~min_bp, true else first, false
  | Climbing min_bp :: rest ->
    let first, nullable =
      match rest with
      | [] -> [], true
      | _ -> expression plan null ~inclusive block rest
    in
    if nullable then first @ climb_set block ~min_bp, true else first, false
  | Postfix index :: rest ->
    let q =
      block.postfix.(within "postfix operator" (Array.length block.postfix) index)
    in
    remains plan null ~inclusive q.body rest
  | (Enter _ | Item _ | Arm _ | Child | Loop _ | Emits _) :: _ | [] ->
    invalid_arg "Residual.at: an expression takes an operand, a climb or a postfix"
;;

let canonical (kinds : Kind.t list) : Kind.t array =
  Array.of_list (List.sort_uniq ~cmp:Int.compare kinds)
;;

let at (plan : Plan.t) (state : State.t) : Kind.t array =
  let null = nullable_rules plan in
  let rec walk
            ~(inclusive : bool)
            ((rule, path) : int * step list)
            (outer : (int * step list) list)
    : Kind.t list
    =
    let rule = within "rule" (Array.length plan.rules) rule in
    let first, nullable = remains plan null ~inclusive plan.rules.(rule).body path in
    match nullable, outer with
    | false, _ | true, [] -> first
    | true, above :: beyond -> first @ walk ~inclusive:false above beyond
  in
  match State.frames state with
  | [] -> [||]
  | frame :: outer -> canonical (walk ~inclusive:true frame outer)
;;

(* -- the points of one rule ------------------------------------------------ *)

(* Where a parse can be inside a rule, as an automaton over the node's green
   children. Each point carries its own set, so a walk over these needs no
   plan:
   an emitter writes them out and the walk reads them. *)
module Table = struct
  type point =
    { first : Kind.t array
    ; may_end : bool
    ; on : (Kind.t array * int) array
    }

  (* An atom that is a rule builds its own node and the block wraps nothing
     round it, so that rule's kind lands in the tree beside the block's own
     roles. *)
  let expression_kinds (plan : Plan.t) (block : Plan.block) : Kind.t list =
    (block.base_kind
     :: block.hole_kind
     :: Array.to_list (Array.map block.postfix ~f:(fun (q : Plan.postfix) -> q.kind)))
    @ List.filter_map [ block.prefix_kind; block.infix_kind ] ~f:(fun kind -> kind)
    @ Array.fold_left block.atoms ~init:[] ~f:(fun acc (_, atom) ->
      match atom with
      | Plan.Atom_rule rule -> plan.rules.(rule).kind :: acc
      | Plan.Atom_token -> acc)
  ;;

  (* A block rule builds no node of its own, so a call to one leaves an
     expression behind. *)
  let leaves (plan : Plan.t) (rule : int) : Kind.t list =
    match plan.rules.(rule).body with
    | Plan.Seq [| Plan.Pratt p |] -> expression_kinds plan plan.blocks.(p.block)
    | _ -> [ plan.rules.(rule).kind ]
  ;;

  (* [gate] is the only thing that says what a bare [Bump] leaves, because the
     instruction names no kind of its own. *)
  let rec children (plan : Plan.t) ~(gate : Kind.t list) (instr : Plan.instr)
    : Kind.t list
    =
    match instr with
    | Plan.Bump -> gate
    | Plan.Expect e ->
      e.tok
      ::
      (match e.placeholder with
       | None -> []
       | Some kind -> [ kind ])
    | Plan.Call rule -> leaves plan rule
    | Plan.Commit c -> c.placeholder :: children plan ~gate:(Array.to_list c.first) c.body
    | Plan.Pratt p -> expression_kinds plan plan.blocks.(p.block)
    | Plan.Alt a ->
      Array.fold_left a.arms ~init:[] ~f:(fun acc (on, body) ->
        children plan ~gate:(Array.to_list on) body @ acc)
    | Plan.Seq _ | Plan.Open _ | Plan.Close | Plan.Trivia | Plan.Drain _ | Plan.Loop _ ->
      []
  ;;

  let takes_nothing (instr : Plan.instr) : bool =
    match instr with
    | Plan.Open _ | Plan.Close | Plan.Trivia | Plan.Drain _ | Plan.Seq [||] -> true
    | _ -> false
  ;;

  let rec reached (instr : Plan.instr) (steps : step list) : Plan.instr =
    match steps, instr with
    | [], _ -> instr
    | Item index :: rest, Plan.Seq instrs -> reached instrs.(index) rest
    | Emits e :: rest, Plan.Loop l -> reached l.states.(e.state).emits rest
    | _ -> instr
  ;;

  let rec after (instr : Plan.instr) (steps : step list) : step list option =
    match steps, instr with
    | [], _ -> None
    | Item index :: rest, Plan.Seq instrs ->
      (match after instrs.(index) rest with
       | Some inner -> Some (Item index :: inner)
       | None ->
         if index + 1 < Array.length instrs then Some [ Item (index + 1) ] else None)
    (* A loop may end wherever it is, so the loop itself is what finishes. *)
    | Loop _ :: _, Plan.Loop _ -> None
    | Emits e :: rest, Plan.Loop l ->
      (match after l.states.(e.state).emits rest with
       | Some inner -> Some (Emits e :: inner)
       | None -> Some [ Loop e.goto ])
    | _ -> None
  ;;

  let settle (body : Plan.instr) (path : step list) : step list =
    let rec go (path : step list) : step list =
      match List.rev path with
      | Loop _ :: _ -> path
      | _ ->
        (match reached body path with
         | Plan.Seq [||] -> path
         | Plan.Seq _ -> go (path @ [ Item 0 ])
         | Plan.Loop l -> go (path @ [ Loop l.entry ])
         | instr when takes_nothing instr ->
           (match after body path with
            | Some next -> go next
            | None -> path)
         | _ -> path)
    in
    go path
  ;;

  (* The positions after this one are here because a child can be missing from
     the tree. A required token whose [Expect] failed leaves nothing behind, and
     the next child is then the one after it. *)
  let rec onward (plan : Plan.t) (body : Plan.instr) (path : step list)
    : (Kind.t list * step list option) list
    =
    let next = after body path in
    let past =
      match next with
      | None -> []
      | Some next -> onward plan body (settle body next)
    in
    match List.rev path, reached body path with
    | Loop state :: _, Plan.Loop l ->
      let stay (dest : int) : step list =
        List.rev (Loop dest :: List.tl (List.rev path))
      in
      (* A loop that recovers sweeps what it cannot take into an error node and
         starts again at its entry, so the error node is a child like any other
         and it moves the automaton there. A loop that ends instead never
         recovers and never leaves one behind. *)
      let recovered =
        match l.ends_on with
        | None -> past
        | Some _ -> ([ plan.error_kind ], Some (settle body (stay l.entry))) :: past
      in
      Array.fold_left l.states.(state).accepts ~init:recovered ~f:(fun acc (gate, dest) ->
        ( children plan ~gate:(Array.to_list gate) l.states.(state).emits
        , Some (settle body (stay dest)) )
        :: acc)
    | _, instr ->
      (match children plan ~gate:[] instr with
       | [] -> past
       | kinds -> (kinds, Option.map (settle body) next) :: past)
  ;;

  (* An atom that is a rule, with the operators that may extend it.

     Every other operand shape leaves a role in the tree, and a role's last
     point carries the climb. A rule atom leaves its own node instead, because
     the block wraps nothing round it, and that node's automaton cannot carry
     the climb: the same node is a function's body elsewhere and nothing may
     follow it there.

     So the edge that takes one carries it. The edge rather than the point,
     because the other shapes reach the same point -- a hole among them, which
     is a child the parse never read and which nothing may follow.

     [base_kind] gates it, and has to. The same rule is an atom in one slot and
     an ordinary child in another: rust's [Block] is an expression and it is
     also a function's body, and nothing follows it there. An edge that leaves
     an expression behind lists every operand shape, [base_kind] among them,
     and an edge that names the rule alone does not. *)
  let rule_atoms (plan : Plan.t) : (Kind.t * Kind.t list * Kind.t list) list =
    Array.fold_left plan.blocks ~init:[] ~f:(fun acc (block : Plan.block) ->
      match
        Array.fold_left block.atoms ~init:[] ~f:(fun acc (_, atom) ->
          match atom with
          | Plan.Atom_rule rule -> plan.rules.(rule).kind :: acc
          | Plan.Atom_token -> acc)
      with
      | [] -> acc
      | atoms -> (block.base_kind, atoms, climb_set block ~min_bp:0) :: acc)
  ;;

  (* [None] is the point a body finishes at. A rule body ends on its [Close],
     which admits nothing and may end, so it never needs one. A postfix
     operator's body ends on its last child and does. *)
  let of_body (plan : Plan.t) (null : bool array) (body : Plan.instr) : point array =
    let seen : (step list option, int) Hashtbl.t = Hashtbl.create 16 in
    let points = ref [] in
    let rec visit (path : step list option) : int =
      match Hashtbl.find_opt seen path with
      | Some index -> index
      | None ->
        let index = Hashtbl.length seen in
        Hashtbl.add seen path index;
        points := (index, path) :: !points;
        (match path with
         | None -> ()
         | Some path ->
           List.iter (onward plan body path) ~f:(fun (_, target) -> ignore (visit target)));
        index
    in
    ignore (visit (Some (settle body [])));
    (* A destination reached by a rule atom, with the climb that atom left
       behind. Allocated after the points the paths gave, so an index into
       those still means what it did. *)
    let variants : (int * Kind.t list, int) Hashtbl.t = Hashtbl.create 8 in
    let base = Hashtbl.length seen in
    let variant (at : int) (climb : Kind.t list) : int =
      match Hashtbl.find_opt variants (at, climb) with
      | Some index -> index
      | None ->
        let index = base + Hashtbl.length variants in
        Hashtbl.add variants (at, climb) index;
        index
    in
    let atoms = rule_atoms plan in
    (* The atom edge comes first, because a walk takes the first entry holding
       the child's kind. *)
    let edges (path : step list) : (Kind.t array * int) list =
      List.concat_map (onward plan body path) ~f:(fun (kinds, target) ->
        let at = Hashtbl.find seen target in
        let split =
          List.filter_map atoms ~f:(fun (base_kind, atom_kinds, climb) ->
            if not (List.mem base_kind ~set:kinds)
            then None
            else (
              match List.filter atom_kinds ~f:(fun k -> List.mem k ~set:kinds) with
              | [] -> None
              | here -> Some (canonical here, variant at (Array.to_list (canonical climb)))))
        in
        split @ [ canonical kinds, at ])
    in
    let ordered = List.sort ~cmp:(fun (a, _) (b, _) -> Int.compare a b) !points in
    let settled =
      List.map ordered ~f:(fun (_, path) ->
        match path with
        | None -> { first = [||]; may_end = true; on = [||] }
        | Some path ->
          let first, may_end = remains plan null ~inclusive:true body path in
          { first = canonical first; may_end; on = Array.of_list (edges path) })
    in
    let settled = Array.of_list settled in
    let extra =
      Array.make (Hashtbl.length variants) { first = [||]; may_end = true; on = [||] }
    in
    Hashtbl.iter
      (fun (at, climb) index ->
         let p = settled.(at) in
         extra.(index - base)
         <- { p with first = canonical (Array.to_list p.first @ climb) })
      variants;
    Array.append settled extra
  ;;

  (* No instruction describes an expression node, because a block rule builds
     none. The shapes come from the block: a base wraps its atom, a prefix is
     its operator and an operand, an infix is operand, operator, operand, and a
     postfix is its operand, its lead token, and what the operator's body
     reads.

     Binding power does not come into it. An operator one level refuses is
     taken by the level above, so every point that has read an operand admits
     the same set. *)
  let expression (plan : Plan.t) (null : bool array) (block : Plan.block)
    : (Kind.t * point array) list
    =
    let head = canonical (head_set block) in
    let climb = canonical (climb_set block ~min_bp:0) in
    let operands = canonical (expression_kinds plan block) in
    let atom_tokens =
      canonical
        (Array.fold_left block.atoms ~init:[] ~f:(fun acc (on, atom) ->
           match atom with
           | Plan.Atom_token -> Array.to_list on @ acc
           | Plan.Atom_rule _ -> acc))
    in
    let operand (on : (Kind.t array * int) array) : point =
      { first = head; may_end = false; on }
    in
    let climbed (on : (Kind.t array * int) array) : point =
      { first = climb; may_end = true; on }
    in
    let infix_tokens = canonical (Array.to_list (Array.map block.infix ~f:fst)) in
    let prefix_tokens = canonical (Array.to_list (Array.map block.prefix ~f:fst)) in
    let of_option (kind : Kind.t option) (points : point array) =
      match kind with
      | None -> []
      | Some kind -> [ kind, points ]
    in
    (* The body reads on from the lead token, so its points carry on from
       there. Where the body may end the operator may too, and the operators
       that extend it join in. *)
    let postfix (q : Plan.postfix) : Kind.t * point array =
      let body = of_body plan null q.body in
      let shift (at : int) : int = at + 2 in
      let carried =
        Array.map body ~f:(fun (p : point) ->
          { first =
              (if p.may_end
               then canonical (Array.to_list p.first @ Array.to_list climb)
               else p.first)
          ; may_end = p.may_end
          ; on = Array.map p.on ~f:(fun (kinds, dest) -> kinds, shift dest)
          })
      in
      ( q.kind
      , Array.append
          [| operand [| operands, 1 |]; climbed [| [| q.lead |], 2 |] |]
          carried )
    in
    (((block.base_kind, [| operand [| atom_tokens, 1 |]; climbed [||] |])
      :: (block.hole_kind, [| operand [||] |])
      :: of_option
           block.prefix_kind
           [| operand [| prefix_tokens, 1 |]; operand [| operands, 2 |]; climbed [||] |])
     @ of_option
         block.infix_kind
         [| operand [| operands, 1 |]
          ; climbed [| infix_tokens, 2 |]
          ; operand [| operands, 3 |]
          ; climbed [||]
         |])
    @ Array.to_list (Array.map block.postfix ~f:postfix)
  ;;

  (* A block's roles come first. Where a block has no role of its own for an
     atom it builds the block rule's kind, and the expression shape is the one
     the tree holds. *)
  let of_plan (plan : Plan.t) : (Kind.t * point array) list =
    let null = nullable_rules plan in
    let blocks = Array.to_list plan.blocks |> List.concat_map ~f:(expression plan null) in
    let rules =
      Array.to_list plan.rules
      |> List.filter ~f:(fun (rule : Plan.rule) ->
        match rule.body with
        | Plan.Seq [||] | Plan.Seq [| Plan.Pratt _ |] -> false
        | _ -> true)
      |> List.map ~f:(fun (rule : Plan.rule) -> rule.kind, of_body plan null rule.body)
    in
    let taken = List.map blocks ~f:fst in
    blocks @ List.filter rules ~f:(fun (kind, _) -> not (List.mem kind ~set:taken))
  ;;
end
