open StdLabels
module Cursor = Lingo_runtime.Cursor
module Build = Lingo_runtime.Build
module Recover = Lingo_runtime.Recover
module Diagnostic = Lingo_runtime.Diagnostic

(* -- recovery sets --------------------------------------------------------- *)

let holds_array (set : Ir.Kind.t array) (k : Ir.Kind.t) =
  Array.exists set ~f:(fun x -> x = k)
;;

let add_all (set : Ir.Kind.t list) (ks : Ir.Kind.t array) =
  Array.fold_left ks ~init:set ~f:(fun acc x ->
    if List.mem x ~set:acc then acc else x :: acc)
;;

(* -- the balanced skip ----------------------------------------------------- *)

(* Answers the closer a kind opens, where the plan pairs it with one. *)
let closes (plan : Ir.Plan.t) (kind : Ir.Kind.t) =
  Array.find_map plan.pairs ~f:(fun (kind_open, kind_close) ->
    if Int.equal kind_open kind then Some kind_close else None)
;;

let is_a_closer (plan : Ir.Plan.t) (kind : Ir.Kind.t) =
  Array.exists plan.pairs ~f:(fun (_, kind_close) -> Int.equal kind_close kind)
;;

(* Skips forward to a token the parse can carry on from, and puts what it
   skipped in a node of the plan's error kind.

   It balances as it goes. A pair it opens itself is one it closes itself, so
   a stop token inside that pair belongs to the pair and not to the caller.

   It ends at a closer it cannot match, whichever pair that closer belongs to.
   Take [{ def [ x { ] }]: the stray [{] meets the []] first, and no [{] can be
   closed by a []], so the skip stops there. A skip that ran on to its own [}]
   would take the []] from the frame waiting for it, and two frames would end
   up with no close.

   It takes the closer it stopped at only where no frame is waiting for that
   kind. Reparsing [{{}], the stray [{] would otherwise take the [}] the open
   frame wants, that frame's close goes missing again, and the text grows a [}]
   on every pass. *)
let skip (plan : Ir.Plan.t) (cursor : Cursor.t) (stop_on : Ir.Kind.t list) : unit =
  if (not (Cursor.eof cursor)) && not (List.mem ~set:stop_on (Cursor.current cursor))
  then (
    let from = fst (Cursor.range cursor) in
    Build.start_node cursor plan.error_kind;
    let open_closers = ref [] in
    let running = ref true in
    while !running do
      let k = Cursor.current cursor in
      if Cursor.eof cursor
      then running := false
      else (
        match !open_closers with
        | want :: rest when want = k ->
          (* The closer of a pair this skip opened. *)
          open_closers := rest;
          Cursor.bump cursor
        | _ ->
          if is_a_closer plan k
          then (
            (* A closer this skip cannot match. It belongs to a frame above. *)
            if not (List.mem ~set:stop_on k) then Cursor.bump cursor;
            running := false)
          else if !open_closers = [] && List.mem ~set:stop_on k
          then running := false
          else (
            (match closes plan k with
             | Some close -> open_closers := close :: !open_closers
             | None -> ());
            Cursor.bump cursor))
    done;
    Build.finish_node cursor;
    Cursor.report_at cursor (from, Cursor.offset cursor) Diagnostic.Unexpected)
;;

(* -- execution ------------------------------------------------------------- *)

(* What every step of the walk needs. [trace] is called with the name of each
   instruction as it runs, and [at] with the position wherever the walk reads
   the cursor. *)
type t =
  { plan : Ir.Plan.t
  ; cursor : Cursor.t
  ; trace : string -> unit
  ; at : Ir.Residual.State.t -> index:int -> reported:int -> unit
  }

let reached (t : t) (where : Ir.Residual.State.t) : unit =
  t.at where ~index:(Cursor.position t.cursor) ~reported:(Cursor.reports t.cursor)
;;

let first_index (count : int) ~(holds : int -> bool) : int option =
  let rec go (index : int) : int option =
    if index >= count then None else if holds index then Some index else go (index + 1)
  in
  go 0
;;

(* [recover] is what a failure here can resume on: the closers of every frame
   open above this point. A [Commit] unions its own recovery set into that
   before it skips, so what a position contributes is added where it is used
   rather than carried down.

   [passed_down] is [recover] plus this rule's own closers. That is what a
   [Call] hands the rule it calls. *)
let rec exec
          (t : t)
          ~(recover : Ir.Kind.t list)
          ~(passed_down : Ir.Kind.t list)
          ~(where : Ir.Residual.State.t)
          (instr : Ir.Plan.instr)
  =
  match instr with
  | Seq xs ->
    t.trace "seq";
    Array.iteri xs ~f:(fun index instruction ->
      exec t ~recover ~passed_down ~where:(Ir.Residual.State.item where index) instruction)
  | Open k ->
    t.trace "open";
    Build.start_node t.cursor k
  | Close ->
    t.trace "close";
    Build.finish_node t.cursor
  | Trivia ->
    t.trace "trivia";
    Cursor.skip_trivia t.cursor
  | Bump ->
    t.trace "bump";
    reached t where;
    Cursor.bump t.cursor
  | Drain id ->
    t.trace "drain";
    reached t where;
    drain t.cursor id
  | Expect e ->
    t.trace "expect";
    reached t where;
    Recover.expect
      ?at_child:e.at_child
      ?hole_kind:e.hole
      ?placeholder:e.placeholder
      t.cursor
      e.tok
      e.message
  | Call r ->
    t.trace "call";
    call t passed_down r ~where:(Ir.Residual.State.call where r)
  | Pratt pr ->
    t.trace "pratt";
    pratt t ~recover ~passed_down ~where ~block:pr.block ~min_bp:pr.min_bp
  | Alt a ->
    t.trace "alt";
    reached t where;
    (match
       first_index (Array.length a.arms) ~holds:(fun index ->
         holds_array (fst a.arms.(index)) (Cursor.current t.cursor))
     with
     | Some index ->
       exec
         t
         ~recover
         ~passed_down
         ~where:(Ir.Residual.State.arm where index)
         (snd a.arms.(index))
     | None -> ())
  | Commit cm ->
    t.trace "commit";
    reached t where;
    if holds_array cm.first (Cursor.current t.cursor)
    then exec t ~recover ~passed_down ~where:(Ir.Residual.State.child where) cm.body
    else (
      let id =
        Cursor.report_id
          t.cursor
          (Diagnostic.Missing
             { at_child = Some cm.at_child
             ; expected = cm.message
             ; expected_kinds = Array.to_list cm.first
             ; hole_kind = cm.hole
             })
      in
      Build.missing_node ~payload:id t.cursor cm.placeholder;
      (* Where a later child can take what is under the cursor, a skip would
         eat it. *)
      let rest_can_take =
        match cm.resume with
        | None -> false
        | Some rs -> holds_array rs (Cursor.current t.cursor)
      in
      if not rest_can_take then skip t.plan t.cursor (add_all recover cm.recover))
  | Loop l ->
    t.trace "loop";
    loop t ~recover ~passed_down ~where l.states l.entry l.ends_on

and call (t : t) (inbound : Ir.Kind.t list) (r : int) ~(where : Ir.Residual.State.t) =
  let rule = t.plan.rules.(r) in
  (* A boundary rule starts from nothing, so a failure inside it stops at its
     own delimiters. Recovery then cannot leave a scope it was never in. *)
  let recover = if rule.boundary then [] else inbound in
  exec t ~recover ~passed_down:(add_all recover rule.adds) ~where rule.body

and loop
      (t : t)
      ~(recover : Ir.Kind.t list)
      ~(passed_down : Ir.Kind.t list)
      ~(where : Ir.Residual.State.t)
      (states : Ir.Plan.loop_state array)
      (entry : int)
      (ends_on : Ir.Kind.t array option)
  =
  let state = ref entry in
  (* The range of what the last transition took. Ending at a state that
     reports needs it, because by then the cursor has moved past it. *)
  let last_taken = ref None in
  let target () =
    Array.find_map states.(!state).accepts ~f:(fun (on, dest) ->
      if holds_array on (Cursor.current t.cursor) then Some dest else None)
  in
  (* Where a body can carry on from: what ends it, what any of its states
     could take next, and whatever the frames above are waiting for.

     Two things read this. The sweep stops here, so a body that meets junk
     picks up at its next element rather than running to the closer. And an
     element is parsed with it, so a failure nested inside one stops at the
     next element too rather than escaping further out. A body with no closer
     has nothing else to offer, which is why a root needs it most. *)
  let stop_on =
    match ends_on with
    | None -> []
    | Some ks ->
      Array.fold_left states ~init:(add_all recover ks) ~f:(fun acc st ->
        Array.fold_left st.Ir.Plan.accepts ~init:acc ~f:(fun acc (on, _) ->
          add_all acc on))
  in
  (* What ends the body is asked before what continues it. An anchor is a
     token that ought to end the body even where an element could start with
     one, which is the whole of what declaring an anchor buys. *)
  let ending () =
    match ends_on with
    | None -> false
    | Some ks -> Cursor.eof t.cursor || holds_array ks (Cursor.current t.cursor)
  in
  let running = ref true in
  while !running do
    let before = Cursor.position t.cursor in
    reached t (Ir.Residual.State.loop where !state);
    if ending ()
    then running := false
    else (
      match target () with
      | Some dest ->
        let from = fst (Cursor.range t.cursor) in
        exec
          t
          ~recover
          ~passed_down:(add_all passed_down (Array.of_list stop_on))
          ~where:(Ir.Residual.State.emits where ~state:!state ~goto:dest)
          states.(!state).emits;
        last_taken := Some (from, Cursor.offset t.cursor);
        state := dest
      | None ->
        (* Nothing this position accepts, and the body is not ending. Either
           something is missing here, or what is under the cursor is junk.

           Some state accepting it is what tells the two apart. A body that
           could take this token somewhere has a gap at this position; a body
           that could take it nowhere is looking at junk. *)
        let could_continue =
          Array.exists states ~f:(fun (st : Ir.Plan.loop_state) ->
            Array.exists st.accepts ~f:(fun (on, _) ->
              holds_array on (Cursor.current t.cursor)))
        in
        (match states.(!state).when_missing, ends_on with
         | Some m, _ when could_continue ->
           t.trace "loop-missing";
           Recover.expect t.cursor m.tok m.message;
           state := m.goto
         | (Some _ | None), None -> running := false
         | (Some _ | None), Some _ ->
           t.trace "loop-recover";
           skip t.plan t.cursor stop_on;
           (* The broken span is behind us, so the body starts again. Staying
              in the state the failure left would keep a body that has just
              thrown away a separator waiting for one. *)
           state := entry));
    if !running && Cursor.position t.cursor = before then running := false
  done;
  match states.(!state).exit, !last_taken with
  | Ir.Plan.May_exit_reporting id, Some range ->
    t.trace "exit-reporting";
    Cursor.report_at t.cursor range (Diagnostic.Extra id)
  | (Ir.Plan.May_exit | Ir.Plan.May_exit_reporting _), _ -> ()

(* An expression, by precedence climbing.

   The checkpoint is taken before anything is read, so an operator that has
   already read its left side can wrap it. [Build.mark] takes the trivia under
   the cursor into the enclosing frame first, which is what keeps a space
   before an expression out of the expression's own node.

   Associativity is the binding powers and nothing else. A left operator
   carries [(bp, bp + 1)] and a right one [(bp, bp)], so the one
   [left_bp >= min_bp] test below tells them apart. *)
and pratt
      (t : t)
      ~(recover : Ir.Kind.t list)
      ~(passed_down : Ir.Kind.t list)
      ~(where : Ir.Residual.State.t)
      ~(block : int)
      ~(min_bp : int)
  =
  let b = t.plan.blocks.(block) in
  let start = Build.mark t.cursor in
  head
    t
    ~recover
    ~passed_down
    ~where:(Ir.Residual.State.operand where min_bp)
    block
    b
    start;
  let climbing = Ir.Residual.State.climbing where min_bp in
  let running = ref true in
  while !running do
    reached t climbing;
    let k = Cursor.current t.cursor in
    match
      first_index (Array.length b.postfix) ~holds:(fun index ->
        let q : Ir.Plan.postfix = b.postfix.(index) in
        q.lead = k && q.bp >= min_bp)
    with
    | Some index ->
      let q = b.postfix.(index) in
      t.trace "postfix";
      Cursor.bump t.cursor;
      exec
        t
        ~recover
        ~passed_down
        ~where:(Ir.Residual.State.postfix climbing index)
        q.body;
      Build.start_node_at t.cursor start q.kind;
      Build.finish_node t.cursor
    | None ->
      (match
         Array.find_opt b.infix ~f:(fun (tok, (left_bp, _)) ->
           tok = k && left_bp >= min_bp)
       with
       | Some (_, (_, right_bp)) ->
         t.trace "infix";
         Cursor.bump t.cursor;
         pratt t ~recover ~passed_down ~where:climbing ~block ~min_bp:right_bp;
         (match b.infix_kind with
          | Some kind ->
            Build.start_node_at t.cursor start kind;
            Build.finish_node t.cursor
          | None -> ())
       | None -> running := false)
  done

(* What an expression starts with: a prefix operator and its operand or an atom *)
and head
      (t : t)
      ~(recover : Ir.Kind.t list)
      ~(passed_down : Ir.Kind.t list)
      ~(where : Ir.Residual.State.t)
      (block : int)
      (b : Ir.Plan.block)
      (start : Siesta.Builder.checkpoint)
  : unit
  =
  reached t where;
  let k = Cursor.current t.cursor in
  match Array.find_opt b.prefix ~f:(fun (tok, _) -> tok = k) with
  | Some (_, right_bp) ->
    t.trace "prefix";
    Cursor.bump t.cursor;
    pratt t ~recover ~passed_down ~where ~block ~min_bp:right_bp;
    (match b.prefix_kind with
     | Some kind ->
       Build.start_node_at t.cursor start kind;
       Build.finish_node t.cursor
     | None -> ())
  | None ->
    (match
       Array.find_map b.atoms ~f:(fun (on, atom) ->
         if holds_array on k then Some atom else None)
     with
     (* The token is the whole atom, and the node wraps just it. *)
     | Some Ir.Plan.Atom_token ->
       t.trace "atom-token";
       Cursor.bump t.cursor;
       Build.start_node_at t.cursor start b.base_kind;
       Build.finish_node t.cursor
     (* A rule builds its own node, so this wraps nothing. *)
     | Some (Ir.Plan.Atom_rule r) ->
       t.trace "atom-rule";
       call t passed_down r ~where:(Ir.Residual.State.call where r)
     | None -> missing_atom t.cursor b)

(* No atom where one was needed. The hole stands in for the expression, so the
   tree keeps a node at the position the source left empty. *)
and missing_atom (cursor : Cursor.t) (block : Ir.Plan.block) : unit =
  let id =
    Cursor.report_id
      cursor
      (Diagnostic.Missing
         { at_child = None
         ; expected = block.message
         ; expected_kinds = Array.to_list block.expected
         ; hole_kind = Some block.hole_kind
         })
  in
  Build.missing_node ~payload:id cursor block.hole_kind

(* Sweeps what is left of the input into the open node. A root ends with this,
   so trivia past the last child reaches the tree instead of falling off the
   end, and a meaningful token still there is reported rather than dropped.

   [Cursor.eof] looks past trivia, so reaching this test means a meaningful
   token is left and the sweep has something to report. *)
and drain (cursor : Cursor.t) (message_id : Ir.Message.id) : unit =
  if not (Cursor.eof cursor)
  then (
    let from, _ = Cursor.range cursor in
    while not (Cursor.eof cursor) do
      Cursor.bump cursor
    done;
    Cursor.report_at cursor (from, Cursor.offset cursor) (Diagnostic.Extra message_id));
  Cursor.skip_trivia cursor
;;

(* A rule with an empty body opens no node, so a parse entered there builds
   nothing and [Build.finish] raises. Its message is about the builder, which
   tells a caller nothing about the entry they passed.

   The rules with empty bodies are an expression block's roles. Nothing calls
   them. They are in the plan so that a rule's index here matches its index in
   the facts, which is what lets [Call] carry one unchanged. *)
let run
      ?(trace = fun (_ : string) -> ())
      ?(at = fun (_ : Ir.Residual.State.t) ~index:(_ : int) ~reported:(_ : int) -> ())
      (plan : Ir.Plan.t)
      (entry : int)
      (tokens : Lingo_runtime.Token.t array)
  =
  if entry < 0 || entry >= Array.length plan.rules
  then
    invalid_arg
      (Printf.sprintf
         "Interp.run: no rule %d; the plan holds %d"
         entry
         (Array.length plan.rules));
  (match plan.rules.(entry).body with
   | Ir.Plan.Seq [||] ->
     invalid_arg
       (Printf.sprintf
          "Interp.run: rule %d (%s) has an empty body, so it builds no tree"
          entry
          plan.rules.(entry).name)
   | _ -> ());
  let cursor =
    Cursor.create
      ~cache:(Siesta.Cache.create_plain ())
      ~trivia_kinds:(Array.to_list plan.trivia)
      tokens
  in
  call { plan; cursor; trace; at } [] entry ~where:(Ir.Residual.State.enter entry);
  Build.finish cursor
;;
