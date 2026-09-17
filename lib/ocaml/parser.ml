open StdLabels

let runtime (path : string) : Emit.expr = Emit.evar ("Lingo_runtime." ^ path)

let call (path : string) (args : Emit.expr list) : Emit.expr =
  Emit.eapply (runtime path) args
;;

let cursor : Emit.expr = Emit.evar "cursor"
let current : Emit.expr = call "Cursor.current" [ cursor ]

let message_id (id : Ir.Message.id) : Emit.expr =
  call "Message.of_int" [ Emit.eint (Ir.Message.to_int id) ]
;;

let kind_list (kinds : Ir.Kind.t list) : Emit.expr =
  Emit.elist (List.map kinds ~f:Emit.eint)
;;

let union (sets : Ir.Kind.t list list) : Ir.Kind.t list =
  List.sort_uniq ~cmp:Int.compare (List.concat sets)
;;

let arm_pattern (kinds : Ir.Kind.t list) : Emit.pat option =
  match kinds with
  | [] -> None
  | first :: rest -> Some (Emit.por (Emit.pint first) (List.map rest ~f:Emit.pint))
;;

let dispatch_on
      (scrutinee : Emit.expr)
      (arms : (Ir.Kind.t list * Emit.expr) list)
      ~(default : Emit.expr)
  : Emit.expr
  =
  let cases =
    List.filter_map arms ~f:(fun ((kinds, body) : Ir.Kind.t list * Emit.expr) ->
      Option.map (fun (pattern : Emit.pat) -> Emit.ecase pattern body) (arm_pattern kinds))
  in
  match cases with
  | [] -> default
  | cases -> Emit.ematch scrutinee (cases @ [ Emit.ecase Emit.pany default ])
;;

let dispatch (arms : (Ir.Kind.t list * Emit.expr) list) ~(default : Emit.expr) : Emit.expr
  =
  dispatch_on current arms ~default
;;

let holds (kinds : Ir.Kind.t list) : Emit.expr =
  dispatch [ kinds, Emit.ebool true ] ~default:(Emit.ebool false)
;;

let add_all ~(set : Emit.expr) (kinds : Emit.expr) : Emit.expr =
  Emit.ecall "add_all" [ set; kinds ]
;;

let extend ~(set : Emit.expr) (kinds : Ir.Kind.t list) : Emit.expr =
  match kinds with
  | [] -> set
  | kinds -> add_all ~set (kind_list kinds)
;;

(* -- what a body reads ----------------------------------------------------- *)

(* Which of the two recovery sets a body names. *)
type needs =
  { recover : bool
  ; passed_down : bool
  }

let needs_nothing : needs = { recover = false; passed_down = false }
let needs_recover : needs = { recover = true; passed_down = false }
let needs_passed_down : needs = { recover = false; passed_down = true }
let needs_both : needs = { recover = true; passed_down = true }

let together (left : needs) (right : needs) : needs =
  { recover = left.recover || right.recover
  ; passed_down = left.passed_down || right.passed_down
  }
;;

let all_of (each : needs list) : needs =
  List.fold_left each ~init:needs_nothing ~f:together
;;

(* -- instructions ---------------------------------------------------------- *)

let start_node (kind : Ir.Kind.t) : Emit.expr =
  call "Build.start_node" [ cursor; Emit.eint kind ]
;;

let finish_node : Emit.expr = call "Build.finish_node" [ cursor ]

let assign (name : string) (value : Emit.expr) : Emit.expr =
  Emit.ecall ":=" [ Emit.evar name; value ]
;;

let read (name : string) : Emit.expr = Emit.ecall "!" [ Emit.evar name ]

let optional_kind (kind : Ir.Kind.t option) : Emit.expr =
  match kind with
  | None -> Emit.econstruct "None" []
  | Some kind -> Emit.econstruct "Some" [ Emit.eint kind ]
;;

let missing_diagnostic
      ~(at_child : string option)
      ~(report : Ir.Message.id)
      ~(expected : Ir.Kind.t list)
      ~(hole : Ir.Kind.t option)
  : Emit.expr
  =
  Emit.econstruct
    "Lingo_runtime.Diagnostic.Missing"
    [ Emit.erecord
        [ ( "Lingo_runtime.Diagnostic.at_child"
          , match at_child with
            | None -> Emit.econstruct "None" []
            | Some name -> Emit.econstruct "Some" [ Emit.estr name ] )
        ; "expected", message_id report
        ; "expected_kinds", kind_list expected
        ; "hole_kind", optional_kind hole
        ]
    ]
;;

(* Emits the hole a failed position leaves, then [rest]. *)
let hole_node
      ~(diagnostic : Emit.expr)
      ~(placeholder : Ir.Kind.t)
      ~(rest : Emit.expr list)
  : Emit.expr
  =
  Emit.elet
    "id"
    ~body:(call "Cursor.report_id" [ cursor; diagnostic ])
    ~rest:
      (Emit.eseq
         (Emit.eapply_labelled
            (runtime "Build.missing_node")
            [ Ppxlib.Labelled "payload", Emit.evar "id"
            ; Ppxlib.Nolabel, cursor
            ; Ppxlib.Nolabel, Emit.eint placeholder
            ]
          :: rest))
;;

let optional_arg (label : string) (value : Emit.expr option)
  : (Ppxlib.arg_label * Emit.expr) list
  =
  match value with
  | None -> []
  | Some value -> [ Ppxlib.Labelled label, value ]
;;

let parse_call (plan : Ir.Plan.t) (rule : int) : Emit.expr =
  Emit.eapply_labelled
    (Emit.evar (Core.Manifest.parse_fn plan.rules.(rule).name))
    [ Ppxlib.Nolabel, cursor; Ppxlib.Labelled "inbound", Emit.evar "passed_down" ]
;;

let rec instr (plan : Ir.Plan.t) (instruction : Ir.Plan.instr) : Emit.expr * needs =
  match instruction with
  | Ir.Plan.Seq instructions ->
    let each = List.map (Array.to_list instructions) ~f:(instr plan) in
    Emit.eseq (List.map each ~f:fst), all_of (List.map each ~f:snd)
  | Open kind -> start_node kind, needs_nothing
  | Close -> finish_node, needs_nothing
  | Trivia -> call "Cursor.skip_trivia" [ cursor ], needs_nothing
  | Bump -> call "Cursor.bump" [ cursor ], needs_nothing
  | Drain report -> drain report, needs_nothing
  | Expect { tok; message; at_child; hole; placeholder } ->
    ( Emit.eapply_labelled
        (runtime "Recover.expect")
        (optional_arg "at_child" (Option.map Emit.estr at_child)
         @ optional_arg "hole_kind" (Option.map Emit.eint hole)
         @ optional_arg "placeholder" (Option.map Emit.eint placeholder)
         @ [ Ppxlib.Nolabel, cursor
           ; Ppxlib.Nolabel, Emit.eint tok
           ; Ppxlib.Nolabel, message_id message
           ])
    , needs_nothing )
  | Call rule -> parse_call plan rule, needs_passed_down
  | Pratt { block; min_bp } -> enter_block plan block ~min_bp, needs_both
  | Alt { arms } ->
    let each =
      List.map
        (Array.to_list arms)
        ~f:(fun ((on, body) : Ir.Kind.t array * Ir.Plan.instr) ->
          let body, needed = instr plan body in
          (Array.to_list on, body), needed)
    in
    dispatch (List.map each ~f:fst) ~default:Emit.eunit, all_of (List.map each ~f:snd)
  | Commit { first; recover; at_child; message; hole; placeholder; resume; body } ->
    let first = Array.to_list first in
    commit
      plan
      ~first
      ~local:(Array.to_list recover)
      ~diagnostic:
        (missing_diagnostic
           ~at_child:(Some at_child)
           ~report:message
           ~expected:first
           ~hole)
      ~placeholder
      ~resume:(Option.map Array.to_list resume)
      ~body
  | Loop { states; entry; ends_on } ->
    loop plan ~states ~entry ~ends_on:(Option.map Array.to_list ends_on)

(* Sweeps what is left of the input into the open node. [Cursor.eof] looks
   past trivia, so reaching the report means a meaningful token is left. The
   skip at the end puts trailing trivia in the tree. *)
and drain (report : Ir.Message.id) : Emit.expr =
  let not_eof = Emit.enot (call "Cursor.eof" [ cursor ]) in
  Emit.eseq
    [ Emit.ewhen
        ~condition:not_eof
        ~then_:
          (Emit.elet
             "from"
             ~body:(Emit.ecall "fst" [ call "Cursor.range" [ cursor ] ])
             ~rest:
               (Emit.eseq
                  [ Emit.ewhile ~condition:not_eof ~body:(call "Cursor.bump" [ cursor ])
                  ; call
                      "Cursor.report_at"
                      [ cursor
                      ; Emit.etuple [ Emit.evar "from"; call "Cursor.offset" [ cursor ] ]
                      ; Emit.econstruct
                          "Lingo_runtime.Diagnostic.Extra"
                          [ message_id report ]
                      ]
                  ]))
    ; call "Cursor.skip_trivia" [ cursor ]
    ]

(* Emits a required child, and the hole and the skip that stand in for it. *)
and commit
      (plan : Ir.Plan.t)
      ~(first : Ir.Kind.t list)
      ~(local : Ir.Kind.t list)
      ~(diagnostic : Emit.expr)
      ~(placeholder : Ir.Kind.t)
      ~(resume : Ir.Kind.t list option)
      ~(body : Ir.Plan.instr)
  : Emit.expr * needs
  =
  (* The skip names [recover]. *)
  let skip = Emit.ecall "skip" [ cursor; extend ~set:(Emit.evar "recover") local ] in
  let resumed =
    match resume with
    | None -> skip
    | Some later -> dispatch [ later, Emit.eunit ] ~default:skip
  in
  let failed = hole_node ~diagnostic ~placeholder ~rest:[ resumed ] in
  let body, needed = instr plan body in
  dispatch [ first, body ] ~default:failed, together needs_recover needed

(* Emits a body of repeated elements, with the states as an automaton. *)
and loop
      (plan : Ir.Plan.t)
      ~(states : Ir.Plan.loop_state array)
      ~(entry : int)
      ~(ends_on : Ir.Kind.t list option)
  : Emit.expr * needs
  =
  let emitted =
    Array.map states ~f:(fun (state : Ir.Plan.loop_state) -> instr plan state.emits)
  in
  let inner = all_of (List.map (Array.to_list emitted) ~f:snd) in
  let reports =
    Array.exists states ~f:(fun (state : Ir.Plan.loop_state) ->
      match state.exit with
      | Ir.Plan.May_exit_reporting _ -> true
      | May_exit -> false)
  in
  let accepts_of (state : Ir.Plan.loop_state) : Ir.Kind.t list =
    union (List.map (Array.to_list state.accepts) ~f:(fun (on, _) -> Array.to_list on))
  in
  let continues = union (List.map (Array.to_list states) ~f:accepts_of) in
  let stops =
    Option.map (fun (ends : Ir.Kind.t list) -> union [ ends; continues ]) ends_on
  in
  let swept =
    match stops with
    | None -> Emit.eunit
    | Some _ ->
      Emit.eseq
        [ Emit.ecall "skip" [ cursor; Emit.evar "stop_on" ]
        ; assign "state" (Emit.eint entry)
        ]
  in
  let kind = Emit.evar "kind" in
  let stuck (state : Ir.Plan.loop_state) : Emit.expr =
    match state.when_missing with
    | None -> swept
    | Some wanted ->
      dispatch_on
        kind
        [ ( continues
          , Emit.eseq
              [ call
                  "Recover.expect"
                  [ cursor; Emit.eint wanted.tok; message_id wanted.message ]
              ; assign "state" (Emit.eint wanted.goto)
              ] )
        ]
        ~default:swept
  in
  let take (index : int) (destination : int) : Emit.expr =
    let moved =
      (fst emitted.(index)
       ::
       (if reports
        then
          [ assign
              "last_taken"
              (Emit.econstruct
                 "Some"
                 [ Emit.etuple [ Emit.evar "from"; call "Cursor.offset" [ cursor ] ] ])
          ]
        else []))
      @ [ assign "state" (Emit.eint destination) ]
    in
    if reports
    then
      Emit.elet
        "from"
        ~body:(Emit.ecall "fst" [ call "Cursor.range" [ cursor ] ])
        ~rest:(Emit.eseq moved)
    else Emit.eseq moved
  in
  let over_states (arm : int -> Ir.Plan.loop_state -> Emit.case option) : Emit.expr =
    Emit.ematch
      (read "state")
      (List.filter_map
         (List.mapi (Array.to_list states) ~f:(fun (index : int) state -> arm index state))
         ~f:Fun.id
       @ [ Emit.ecase Emit.pany Emit.eunit ])
  in
  let stepping =
    over_states (fun (index : int) (state : Ir.Plan.loop_state) ->
      Some
        (Emit.ecase
           (Emit.pint index)
           (dispatch_on
              kind
              (List.map (Array.to_list state.accepts) ~f:(fun (on, destination) ->
                 Array.to_list on, take index destination))
              ~default:(stuck state))))
  in
  let step =
    match continues with
    | [] -> stepping
    | _ :: _ -> Emit.elet "kind" ~body:current ~rest:stepping
  in
  let carrying_on =
    match ends_on with
    | None -> Emit.ebool true
    | Some ends ->
      dispatch [ Ir.Kind.none :: ends, Emit.ebool false ] ~default:(Emit.ebool true)
  in
  let exiting =
    if not reports
    then []
    else
      [ over_states (fun (index : int) (state : Ir.Plan.loop_state) ->
          match state.exit with
          | Ir.Plan.May_exit -> None
          | May_exit_reporting report ->
            Some
              (Emit.ecase
                 (Emit.pint index)
                 (Emit.ematch
                    (read "last_taken")
                    [ Emit.ecase
                        (Emit.pconstruct "Some" [ Emit.pvar "taken" ])
                        (call
                           "Cursor.report_at"
                           [ cursor
                           ; Emit.evar "taken"
                           ; Emit.econstruct
                               "Lingo_runtime.Diagnostic.Extra"
                               [ message_id report ]
                           ])
                    ; Emit.ecase (Emit.pconstruct "None" []) Emit.eunit
                    ])))
      ]
  in
  let running =
    Emit.eseq
      (Emit.elet
         "going"
         ~body:(Emit.ecall "ref" [ Emit.ebool true ])
         ~rest:
           (Emit.ewhile
              ~condition:(Emit.eand ~left:(read "going") ~right:carrying_on)
              ~body:
                (Emit.elet
                   "before"
                   ~body:(call "Cursor.position" [ cursor ])
                   ~rest:
                     (Emit.eseq
                        [ step
                        ; Emit.ewhen
                            ~condition:
                              (Emit.eequal
                                 ~left:(call "Cursor.position" [ cursor ])
                                 ~right:(Emit.evar "before"))
                            ~then_:(assign "going" (Emit.ebool false))
                        ])))
       :: exiting)
  in
  let running =
    if reports
    then
      Emit.elet
        "last_taken"
        ~body:(Emit.ecall "ref" [ Emit.econstruct "None" [] ])
        ~rest:running
    else running
  in
  let running =
    Emit.elet "state" ~body:(Emit.ecall "ref" [ Emit.eint entry ]) ~rest:running
  in
  match stops with
  | None -> running, inner
  | Some stops ->
    let running =
      if inner.passed_down
      then
        Emit.elet
          "passed_down"
          ~body:(add_all ~set:(Emit.evar "passed_down") (Emit.evar "stop_on"))
          ~rest:running
      else running
    in
    (* The stopping set is built from [recover]. *)
    ( Emit.elet "stop_on" ~body:(extend ~set:(Emit.evar "recover") stops) ~rest:running
    , together needs_recover inner )

(* Emits an entry into an expression block.

   The checkpoint comes before anything is read, so an operator that has
   already read its left side can wrap it.

   The emitted [let] shadows any [start] already in scope, so a nested
   expression wraps from its own checkpoint. The outer one is back in scope
   after the [let] ends. *)
and enter_block (plan : Ir.Plan.t) (block : int) ~(min_bp : int) : Emit.expr =
  let name = plan.blocks.(block).name in
  let carried =
    [ Ppxlib.Nolabel, cursor
    ; Ppxlib.Labelled "recover", Emit.evar "recover"
    ; Ppxlib.Labelled "passed_down", Emit.evar "passed_down"
    ; Ppxlib.Labelled "start", Emit.evar "start"
    ]
  in
  Emit.elet
    "start"
    ~body:(call "Build.mark" [ cursor ])
    ~rest:
      (Emit.eseq
         [ Emit.eapply_labelled (Emit.evar (Core.Manifest.pratt_lhs_fn name)) carried
         ; Emit.eapply_labelled
             (Emit.evar (Core.Manifest.pratt_infix_fn name))
             (carried @ [ Ppxlib.Labelled "min_bp", Emit.eint min_bp ])
         ])
;;

(* -- an expression block ---------------------------------------------------- *)

let wrap (kind : Ir.Kind.t) : Emit.expr list =
  [ call "Build.start_node_at" [ cursor; Emit.evar "start"; Emit.eint kind ]
  ; finish_node
  ]
;;

(* A kind in both the prefix and the atom table takes the prefix arm, and is
   dropped from the atom's set, because two arms for one kind do not
   compile. *)
let lhs_body (plan : Ir.Plan.t) (block : int) : Emit.expr =
  let definition = plan.blocks.(block) in
  let prefixes = List.map (Array.to_list definition.prefix) ~f:fst in
  let prefix_arms =
    List.map
      (Array.to_list definition.prefix)
      ~f:(fun ((token, right_bp) : Ir.Kind.t * int) ->
        ( [ token ]
        , Emit.eseq
            ([ call "Cursor.bump" [ cursor ]; enter_block plan block ~min_bp:right_bp ]
             @
             match definition.prefix_kind with
             | None -> []
             | Some kind -> wrap kind) ))
  in
  let atom_arms =
    List.map
      (Array.to_list definition.atoms)
      ~f:(fun ((on, atom) : Ir.Kind.t array * Ir.Plan.atom) ->
        ( List.filter (Array.to_list on) ~f:(fun (k : Ir.Kind.t) ->
            not (List.mem k ~set:prefixes))
        , match atom with
          (* The token is the whole atom, and the node wraps just it. *)
          | Ir.Plan.Atom_token ->
            Emit.eseq (call "Cursor.bump" [ cursor ] :: wrap definition.base_kind)
          (* A rule builds its own node, so this wraps nothing. *)
          | Atom_rule rule -> parse_call plan rule ))
  in
  (* No atom where one was needed. The hole stands in for the expression, so
     the tree keeps a node at the position the source left empty. *)
  let no_atom =
    hole_node
      ~diagnostic:
        (missing_diagnostic
           ~at_child:None
           ~report:definition.message
           ~expected:(Array.to_list definition.expected)
           ~hole:(Some definition.hole_kind))
      ~placeholder:definition.hole_kind
      ~rest:[]
  in
  dispatch (prefix_arms @ atom_arms) ~default:no_atom
;;

(* Associativity is the binding powers and nothing else. A left operator
   carries [(bp, bp + 1)] and a right one [(bp, bp)], so the one
   [left_bp >= min_bp] guard below tells them apart.

   The postfix arms come before the infix ones, and an operator that binds
   too loosely falls out of its own arm into whatever follows it. *)
let infix_body (plan : Ir.Plan.t) (block : int) : Emit.expr =
  let definition = plan.blocks.(block) in
  let binds (bp : int) : Emit.expr =
    Emit.egreater_equal ~left:(Emit.eint bp) ~right:(Emit.evar "min_bp")
  in
  let postfix_cases =
    List.map (Array.to_list definition.postfix) ~f:(fun (operator : Ir.Plan.postfix) ->
      Emit.ecase
        ~guard:(binds operator.bp)
        (Emit.pint operator.lead)
        (Emit.eseq
           ((call "Cursor.bump" [ cursor ]
             ::
             (match operator.body with
              | Ir.Plan.Seq [||] -> []
              (* [recover] and [passed_down] are this function's own
                 parameters, so whatever the body reads is already in
                 scope. *)
              | body -> [ fst (instr plan body) ]))
            @ wrap operator.kind)))
  in
  let infix_cases =
    List.map
      (Array.to_list definition.infix)
      ~f:(fun ((token, (left_bp, right_bp)) : Ir.Kind.t * (int * int)) ->
        Emit.ecase
          ~guard:(binds left_bp)
          (Emit.pint token)
          (Emit.eseq
             ([ call "Cursor.bump" [ cursor ]; enter_block plan block ~min_bp:right_bp ]
              @
              match definition.infix_kind with
              | None -> []
              | Some kind -> wrap kind)))
  in
  Emit.elet
    "climbing"
    ~body:(Emit.ecall "ref" [ Emit.ebool true ])
    ~rest:
      (Emit.ewhile
         ~condition:(read "climbing")
         ~body:
           (Emit.ematch
              current
              (postfix_cases
               @ infix_cases
               @ [ Emit.ecase Emit.pany (assign "climbing" (Emit.ebool false)) ])))
;;

(* -- the cluster ------------------------------------------------------------ *)

let cursor_arg : Emit.arg =
  Emit.arg_typed ~arg_name:"cursor" ~type_path:"Lingo_runtime.Cursor.t"
;;

let rule_binding (plan : Ir.Plan.t) (rule : Ir.Plan.rule)
  : string * Emit.arg list * Emit.expr
  =
  (* A boundary rule starts from nothing, so a failure inside it stops at its
     own delimiters and recovery cannot leave a scope it was never in. *)
  let inbound = if rule.boundary then Emit.elist [] else Emit.evar "inbound" in
  let body, needed = instr plan rule.body in
  let body =
    if needed.passed_down
    then
      Emit.elet
        "passed_down"
        ~body:
          (extend
             ~set:(if needed.recover then Emit.evar "recover" else inbound)
             (Array.to_list rule.adds))
        ~rest:body
    else body
  in
  let body =
    if needed.recover then Emit.elet "recover" ~body:inbound ~rest:body else body
  in
  Core.Manifest.parse_fn rule.name, [ cursor_arg; Emit.Named "inbound" ], body
;;

let block_bindings (plan : Ir.Plan.t) (block : int)
  : (string * Emit.arg list * Emit.expr) list
  =
  let name = plan.blocks.(block).name in
  let carried =
    [ cursor_arg; Emit.Named "recover"; Emit.Named "passed_down"; Emit.Named "start" ]
  in
  [ Core.Manifest.pratt_lhs_fn name, carried, lhs_body plan block
  ; ( Core.Manifest.pratt_infix_fn name
    , carried @ [ Emit.Named "min_bp" ]
    , infix_body plan block )
  ]
;;

let cluster (plan : Ir.Plan.t) : Emit.item =
  Emit.ilet_rec
    (List.map (Array.to_list plan.rules) ~f:(rule_binding plan)
     @ List.concat_map
         (List.init ~len:(Array.length plan.blocks) ~f:Fun.id)
         ~f:(block_bindings plan))
;;

(* -- the preamble ----------------------------------------------------------- *)

(* A recovery set is a list of kinds. The emitted parser does two things with
   one: this union, and a membership test. *)
let add_all_item : Emit.item =
  Emit.ilet
    ~args:[ Emit.arg_var "set"; Emit.arg_var "kinds" ]
    "add_all"
    (Emit.ecall
       "List.fold_left"
       [ Emit.elambda
           [ Emit.arg_var "acc"; Emit.arg_var "kind" ]
           (Emit.eif
              ~condition:(Emit.ecall "List.mem" [ Emit.evar "kind"; Emit.evar "acc" ])
              ~then_:(Emit.evar "acc")
              ~else_:(Emit.econstruct "::" [ Emit.evar "kind"; Emit.evar "acc" ]))
       ; Emit.evar "set"
       ; Emit.evar "kinds"
       ])
;;

(* Skips forward to a token the parse can carry on from, and puts what it
   skipped in a node of the plan's error kind.

   It balances as it goes, and two rules keep a stray opener inside the error
   span from walking off with a delimiter a real production needs. It ends at
   the next closer of any pair, not just its own. And it takes that closer
   only where no frame above is waiting for the kind.

   lib/interp/interp.ml works the same way, and its comment carries the
   inputs that earned both rules. *)
let skip_item (plan : Ir.Plan.t) : Emit.item =
  let stop_on = Emit.evar "stop_on" in
  let kind = Emit.evar "kind" in
  let bump = call "Cursor.bump" [ cursor ] in
  let halt = assign "running" (Emit.ebool false) in
  (* [waited_for] is bound once in the emitted code. Every arm below tests
     it, and [List.mem] walks the set. [reserved] adds that no pair of this
     skip's own is still open. *)
  let waited_for = Emit.evar "waited_for" in
  let reserved =
    Emit.eand
      ~left:(Emit.eequal ~left:(read "open_closers") ~right:(Emit.elist []))
      ~right:waited_for
  in
  let closers =
    List.sort_uniq ~cmp:Int.compare (List.map (Array.to_list plan.pairs) ~f:snd)
  in
  let openers =
    List.filter (Array.to_list plan.pairs) ~f:(fun ((o, _) : Ir.Kind.t * Ir.Kind.t) ->
      not (List.mem o ~set:closers))
  in
  let unmatched_closer =
    Emit.eseq [ Emit.ewhen ~condition:(Emit.enot waited_for) ~then_:bump; halt ]
  in
  let opened (close : Ir.Kind.t) : Emit.expr =
    Emit.eif
      ~condition:reserved
      ~then_:halt
      ~else_:
        (Emit.eseq
           [ assign
               "open_closers"
               (Emit.econstruct "::" [ Emit.eint close; read "open_closers" ])
           ; bump
           ])
  in
  let on_kind =
    dispatch_on
      kind
      ((match closers with
        | [] -> []
        | closers -> [ closers, unmatched_closer ])
       @ List.map openers ~f:(fun ((o, close) : Ir.Kind.t * Ir.Kind.t) ->
         [ o ], opened close))
      ~default:(Emit.eif ~condition:reserved ~then_:halt ~else_:bump)
  in
  let one_token =
    Emit.ematch
      (read "open_closers")
      [ (* The closer of a pair this skip opened. It needs no set walked, so
           the binding below sits in the other arm. *)
        Emit.ecase
          ~guard:(Emit.eequal ~left:(Emit.evar "want") ~right:kind)
          (Emit.pconstruct "::" [ Emit.pvar "want"; Emit.pvar "rest" ])
          (Emit.eseq [ assign "open_closers" (Emit.evar "rest"); bump ])
      ; Emit.ecase
          Emit.pany
          (Emit.elet
             "waited_for"
             ~body:(Emit.ecall "List.mem" [ kind; stop_on ])
             ~rest:on_kind)
      ]
  in
  let sweep =
    Emit.ewhile
      ~condition:(read "running")
      ~body:
        (Emit.elet
           "kind"
           ~body:current
           ~rest:
             (Emit.eif
                ~condition:(Emit.eequal ~left:kind ~right:(Emit.eint Ir.Kind.none))
                ~then_:halt
                ~else_:one_token))
  in
  let sweeping =
    Emit.elet
      "from"
      ~body:(Emit.ecall "fst" [ call "Cursor.range" [ cursor ] ])
      ~rest:
        (Emit.eseq
           [ start_node plan.error_kind
           ; Emit.elet
               "open_closers"
               ~body:(Emit.ecall "ref" [ Emit.elist [] ])
               ~rest:
                 (Emit.elet
                    "running"
                    ~body:(Emit.ecall "ref" [ Emit.ebool true ])
                    ~rest:sweep)
           ; finish_node
           ; call
               "Cursor.report_at"
               [ cursor
               ; Emit.etuple [ Emit.evar "from"; call "Cursor.offset" [ cursor ] ]
               ; Emit.econstruct "Lingo_runtime.Diagnostic.Unexpected" []
               ]
           ])
  in
  Emit.ilet
    ~args:[ cursor_arg; Emit.arg_var "stop_on" ]
    "skip"
    (Emit.elet
       "kind"
       ~body:current
       ~rest:
         (Emit.ewhen
            ~condition:
              (Emit.eand
                 ~left:(Emit.enot_equal ~left:kind ~right:(Emit.eint Ir.Kind.none))
                 ~right:(Emit.enot (Emit.ecall "List.mem" [ kind; stop_on ])))
            ~then_:sweeping))
;;

(* -- entry points ----------------------------------------------------------- *)

(* A parse starts with nothing open above it, so the rule it enters is given
   an empty recovery set. *)
let entry_point (plan : Ir.Plan.t) (root : int) : Emit.item =
  let rule = plan.rules.(root) in
  Emit.ilet
    ~args:[ Emit.Opt ("cache", None); Emit.arg_var "tokens" ]
    (Core.Manifest.entry_point_fn rule.name)
    (Emit.elet
       "cursor"
       ~body:
         (Emit.eapply_labelled
            (runtime "Cursor.create")
            [ Ppxlib.Optional "cache", Emit.evar "cache"
            ; Ppxlib.Labelled "trivia_kinds", kind_list (Array.to_list plan.trivia)
            ; Ppxlib.Nolabel, Emit.evar "tokens"
            ])
       ~rest:
         (Emit.eseq
            [ Emit.eapply_labelled
                (Emit.evar (Core.Manifest.parse_fn rule.name))
                [ Ppxlib.Nolabel, cursor; Ppxlib.Labelled "inbound", Emit.elist [] ]
            ; call "Build.finish" [ cursor ]
            ]))
;;

let entry_points (plan : Ir.Plan.t) : Emit.item list =
  List.map (Array.to_list plan.roots) ~f:(entry_point plan)
  @
  match Array.to_list plan.roots with
  | [] -> []
  | first :: _ ->
    [ Emit.ilet
        "parse_tokens"
        (Emit.evar (Core.Manifest.entry_point_fn plan.rules.(first).name))
    ]
;;

(* -- the module ------------------------------------------------------------- *)

let generate (plan : Ir.Plan.t) : Emit.item list =
  [ add_all_item; skip_item plan; cluster plan ] @ entry_points plan
;;

let entry_type : Emit.ty =
  Emit.tarrow_optional
    "cache"
    ~domain:(Emit.tcon "Siesta.Cache.t" [])
    ~codomain:
      (Emit.tarrow
         ~domain:(Emit.tcon "array" [ Emit.tcon "Lingo_runtime.Token.t" [] ])
         ~codomain:
           (Emit.ttuple
              [ Emit.tcon "Siesta.Green.node" []
              ; Emit.tcon "list" [ Emit.tcon "Lingo_runtime.Diagnostic.t" [] ]
              ]))
;;

let signature (plan : Ir.Plan.t) : Emit.sig_item list =
  List.map (Array.to_list plan.roots) ~f:(fun (root : int) ->
    Emit.sval (Core.Manifest.entry_point_fn plan.rules.(root).name) entry_type)
  @
  match Array.to_list plan.roots with
  | [] -> []
  | _ :: _ -> [ Emit.sval "parse_tokens" entry_type ]
;;
