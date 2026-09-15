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

(* The three things every step of the walk needs. [trace] is called with the
   name of each instruction as it runs. *)
type t =
  { plan : Ir.Plan.t
  ; cursor : Cursor.t
  ; trace : string -> unit
  }

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
          (instr : Ir.Plan.instr)
  =
  match instr with
  | Seq xs ->
    t.trace "seq";
    Array.iter xs ~f:(exec t ~recover ~passed_down)
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
    Cursor.bump t.cursor
  | Drain id ->
    t.trace "drain";
    drain t.cursor id
  | Expect e ->
    t.trace "expect";
    Recover.expect
      ?at_child:e.at_child
      ?hole_kind:e.hole
      ?placeholder:e.placeholder
      t.cursor
      e.tok
      e.message
  | Call r ->
    t.trace "call";
    call t passed_down r
  | Pratt pr ->
    t.trace "pratt";
    pratt t ~recover ~passed_down ~block:pr.block ~min_bp:pr.min_bp
  | Alt a ->
    t.trace "alt";
    (match
       Array.find_map a.arms ~f:(fun (on, body) ->
         if holds_array on (Cursor.current t.cursor) then Some body else None)
     with
     | Some body -> exec t ~recover ~passed_down body
     | None -> ())
  | Commit cm ->
    t.trace "commit";
    if holds_array cm.first (Cursor.current t.cursor)
    then exec t ~recover ~passed_down cm.body
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
    loop t ~recover ~passed_down l.states l.entry

and call (t : t) (inbound : Ir.Kind.t list) (r : int) =
  let rule = t.plan.rules.(r) in
  (* A boundary rule starts from nothing, so a failure inside it stops at its
     own delimiters. Recovery then cannot leave a scope it was never in. *)
  let recover = if rule.boundary then [] else inbound in
  exec t ~recover ~passed_down:(add_all recover rule.adds) rule.body

and loop
      (t : t)
      ~(recover : Ir.Kind.t list)
      ~(passed_down : Ir.Kind.t list)
      (states : Ir.Plan.loop_state array)
      (entry : int)
  =
  let state = ref entry in
  (* The range of what the last transition took. Ending at a state that
     reports needs it, because by then the cursor has moved past it. *)
  let last_taken = ref None in
  let target () =
    Array.find_map states.(!state).accepts ~f:(fun (on, dest) ->
      if holds_array on (Cursor.current t.cursor) then Some dest else None)
  in
  (* A repeated child whose rule is nullable leaves the same kind under the
     cursor at the same position, and so does a body that emits a hole and
     takes nothing. The position test is what ends the loop in both cases. *)
  Cursor.while_progress
    t.cursor
    (fun () -> target () <> None)
    (fun () ->
       match target () with
       | None -> ()
       | Some dest ->
         let from = fst (Cursor.range t.cursor) in
         exec t ~recover ~passed_down states.(!state).emits;
         last_taken := Some (from, Cursor.offset t.cursor);
         state := dest);
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
      ~(block : int)
      ~(min_bp : int)
  =
  let b = t.plan.blocks.(block) in
  let start = Build.mark t.cursor in
  head t ~recover ~passed_down block b start;
  let climbing = ref true in
  while !climbing do
    let k = Cursor.current t.cursor in
    match
      Array.find_opt b.postfix ~f:(fun (q : Ir.Plan.postfix) ->
        q.lead = k && q.bp >= min_bp)
    with
    | Some q ->
      t.trace "postfix";
      Cursor.bump t.cursor;
      exec t ~recover ~passed_down q.body;
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
         pratt t ~recover ~passed_down ~block ~min_bp:right_bp;
         (match b.infix_kind with
          | Some kind ->
            Build.start_node_at t.cursor start kind;
            Build.finish_node t.cursor
          | None -> ())
       | None -> climbing := false)
  done

(* What an expression starts with: a prefix operator and its operand or an atom *)
and head
      (t : t)
      ~(recover : Ir.Kind.t list)
      ~(passed_down : Ir.Kind.t list)
      (block : int)
      (b : Ir.Plan.block)
      (start : Siesta.Builder.checkpoint)
  : unit
  =
  let k = Cursor.current t.cursor in
  match Array.find_opt b.prefix ~f:(fun (tok, _) -> tok = k) with
  | Some (_, right_bp) ->
    t.trace "prefix";
    Cursor.bump t.cursor;
    pratt t ~recover ~passed_down ~block ~min_bp:right_bp;
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
       call t passed_down r
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
  call { plan; cursor; trace } [] entry;
  Build.finish cursor
;;
