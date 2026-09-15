open StdLabels

(* [Core.Kind.Set.elements] is ascending and [Core.Kind.to_int] is the
   identity, so what comes back is what [Plan.Check] wants: ascending, no
   repeats. *)
let kset (s : Core.Kind.Set.t) : Ir.Kind.t array =
  Array.of_list (List.map (Core.Kind.Set.elements s) ~f:Core.Kind.to_int)
;;

(* Names a kind for a diagnostic. A keyword or a punctuation literal is
   printed as its own text. Everything else is printed as the name the author
   wrote. *)
let diagnostic_string (facts : Core.Facts.t) (kind : Core.Kind.t) : string =
  match Core.Facts.token_of_kind facts kind with
  | Some t ->
    (match Core.Token.text t with
     | Some s -> "\"" ^ s ^ "\""
     | None -> Core.Grammar.Name.Token.to_string t.name)
  | None ->
    (match Core.Facts.rule_of_kind facts kind with
     | Some r -> Core.Grammar.Name.Rule.to_string r.name
     | None -> Core.Kind.Name.to_string (Core.Facts.kind_name facts kind))
;;

let default_text (facts : Core.Facts.t) (kind_set : Core.Kind.Set.t) : string =
  match List.map (Core.Kind.Set.elements kind_set) ~f:(diagnostic_string facts) with
  | [] -> "expected something else here"
  | words -> "expected " ^ String.concat ~sep:" or " words
;;

let message
      (facts : Core.Facts.t)
      (msgs : Messages.Builder.t)
      (rule_def : Core.Rule.def)
      (child : Core.Rule.child)
      (expected : Core.Kind.Set.t)
  : Ir.Message.id
  =
  let override =
    Array.find_map rule_def.messages ~f:(fun (name, text) ->
      if Core.Grammar.Name.Child.equal name child.child_name then Some text else None)
  in
  Messages.Builder.intern
    msgs
    (match override with
     | Some text -> text
     | None -> default_text facts expected)
;;

let child_first (facts : Core.Facts.t) (child : Core.Rule.child) : Core.Kind.Set.t =
  Core.Kind.Set.unions
    (List.map (Array.to_list child.alts) ~f:(fun kind ->
       Core.Facts.first_of_kind facts kind))
;;

(* -- a child's body -------------------------------------------------------- *)

let rec body_of_alts (facts : Core.Facts.t) (alts : Core.Kind.t array) : Ir.Plan.instr =
  match alts with
  | [| kind |] -> body_of_alt facts kind
  | _ ->
    Ir.Plan.Alt
      { arms =
          Array.map alts ~f:(fun kind ->
            kset (Core.Facts.first_of_kind facts kind), body_of_alt facts kind)
      }

and body_of_alt (facts : Core.Facts.t) (kind : Core.Kind.t) : Ir.Plan.instr =
  let r = facts.kind_rule.(Core.Kind.to_int kind) in
  if r >= 0 then Ir.Plan.Call r else Ir.Plan.Bump
;;

(* A repeated child as a one-state loop. The trivia sweep keeps the space 
   between the last element and whatever closes the body from leaking up to the 
   next thing that skips trivia. *)
let repetition (first : Ir.Kind.t array) (body : Ir.Plan.instr) : Ir.Plan.instr =
  Ir.Plan.Seq
    [| Ir.Plan.Loop
         { entry = 0
         ; states =
             [| { accepts = [| first, 0 |]; exit = Ir.Plan.May_exit; emits = body } |]
         }
     ; Ir.Plan.Trivia
    |]
;;

(* Lowers one child of a rule. [resume] holds what a later child can start
   with. Pass [None] where the child is the last thing in the rule. *)
let instr_of_child
      (facts : Core.Facts.t)
      (msgs : Messages.Builder.t)
      (rule_def : Core.Rule.def)
      (index : int)
      (resume : Ir.Kind.t array option)
      (child : Core.Rule.child)
  : Ir.Plan.instr
  =
  let first = child_first facts child in
  let at_child = Core.Grammar.Name.Child.to_string child.child_name in
  let id = message facts msgs rule_def child first in
  match child.modifier with
  | Core.Grammar.Optional ->
    Ir.Plan.Alt { arms = [| kset first, body_of_alts facts child.alts |] }
  | Core.Grammar.Repeated -> repetition (kset first) (body_of_alts facts child.alts)
  | Core.Grammar.Required ->
    (match child.alts with
     | [| kind |] when facts.kind_rule.(Core.Kind.to_int kind) < 0 ->
       Ir.Plan.Expect
         { tok = Core.Kind.to_int kind
         ; message = id
         ; at_child = Some at_child
         ; hole = Option.map Core.Kind.to_int rule_def.hole
         ; placeholder = None
         }
     | alts ->
       let recover =
         match child.recover_to with
         | Some s -> s
         | None -> Core.Facts.local_recovery_set facts rule_def.id ~child:index
       in
       Ir.Plan.Commit
         { first = kset first
         ; recover = kset recover
         ; at_child
         ; message = id
         ; expected = kset first
         ; hole = Option.map Core.Kind.to_int rule_def.hole
         ; placeholder =
             (match rule_def.hole with
              | Some kind -> Core.Kind.to_int kind
              | None -> Core.Kind.to_int facts.missing_kind)
         ; resume
         ; body = body_of_alts facts alts
         })
;;

(* -- a body loop ----------------------------------------------------------- *)

(* While a parser reads a separated body it can be in one of three places, and
   what may come next differs in each. So each place gets its own state.

   State 0 is just past the opener. An element may come next, or the body may
   end.

   State 1 is just past an element. The separator may come next, or the body
   may end.

   State 2 is just past a separator. An element must come next.

   Ending at state 2 means the separator was trailing. [after_sep] says what
   the grammar makes of that. The parser takes the separator either way,
   because it is bytes the source had. *)
let separated_loop
      (elem_first : Ir.Kind.t array)
      (sep : Ir.Kind.t)
      (body : Ir.Plan.instr)
      (after_sep : Ir.Plan.exit_policy)
  : Ir.Plan.instr
  =
  Ir.Plan.Loop
    { entry = 0
    ; states =
        [| { accepts = [| elem_first, 1 |]; exit = Ir.Plan.May_exit; emits = body }
         ; { accepts = [| [| sep |], 2 |]; exit = Ir.Plan.May_exit; emits = Ir.Plan.Bump }
         ; { accepts = [| elem_first, 1 |]; exit = after_sep; emits = body }
        |]
    }
;;

(* Answers what ending after a separator means. A trailing separator the
   grammar forbids is still taken, and it is reported as well. *)
let trailing_exit (facts : Core.Facts.t) (msgs : Messages.Builder.t) (sep : Core.Rule.sep)
  : Ir.Plan.exit_policy
  =
  match sep.trailing with
  | Core.Grammar.Never ->
    Ir.Plan.May_exit_reporting
      (Messages.Builder.intern msgs ("a trailing " ^ diagnostic_string facts sep.sep_tok))
  | Core.Grammar.On_break | Core.Grammar.Always -> Ir.Plan.May_exit
;;

(* Lowers the children a frame wraps. A separator only applies to a repeated
   child. So where the body is a sequence of required children, it lowers the
   way a plain body does. *)
let body_instrs
      (facts : Core.Facts.t)
      (msgs : Messages.Builder.t)
      (rule_def : Core.Rule.def)
      (sep_opt : Core.Rule.sep option)
      (resume_of : int -> int array option)
  : Ir.Plan.instr list
  =
  let body = Core.Rule.body_children rule_def in
  match sep_opt, body with
  | Some sep, [ ({ modifier = Core.Grammar.Repeated; _ } as child) ] ->
    let elem_first = kset (child_first facts child)
    and sep = Core.Kind.to_int sep.sep_tok
    and body = body_of_alts facts child.alts
    and after_sep = trailing_exit facts msgs sep in
    [ separated_loop elem_first sep body after_sep; Ir.Plan.Trivia ]
  | _ ->
    List.mapi body ~f:(fun i c ->
      let index = rule_def.body_from + i in
      instr_of_child facts msgs rule_def index (resume_of index) c)
;;

(* -- a rule ---------------------------------------------------------------- *)

(* Answers what a later child of the rule can start with.

   A parser that fails at a child skips recovery when the cursor already sits
   on one of these, because the rest of the rule can take that token. Where
   nothing follows the child there is nothing to skip for, and the answer is
   [None]. *)
let resume_after (facts : Core.Facts.t) (rule_def : Core.Rule.def) (index : int)
  : int array option
  =
  let later = ref Core.Kind.Set.empty in
  Array.iteri rule_def.children ~f:(fun child_index child ->
    if child_index > index
    then later := Core.Kind.Set.union !later (child_first facts child));
  (match rule_def.frame with
   | Core.Rule.Delimited d -> later := Core.Kind.Set.add d.close !later
   | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> ());
  if Core.Kind.Set.is_empty !later then None else Some (kset !later)
;;

(* Answers what the rule's own frame contributes to the recovery set it passes
   down: its closer, and its separator where it has one. *)
let adds_of ({ frame; _ } : Core.Rule.def) : Core.Kind.Set.t =
  match frame with
  | Core.Rule.Plain | Core.Rule.Committed _ -> Core.Kind.Set.empty
  | Core.Rule.Separated s -> Core.Kind.Set.singleton s.sep_tok
  | Core.Rule.Delimited d ->
    let kind_set = Core.Kind.Set.singleton d.close in
    Option.fold d.sep ~none:kind_set ~some:(fun (sep : Core.Rule.sep) ->
      Core.Kind.Set.add sep.sep_tok kind_set)
;;

let boundary_of ({ frame; _ } : Core.Rule.def) : bool =
  match frame with
  | Core.Rule.Plain -> false
  | Core.Rule.Committed c -> c.boundary
  | Core.Rule.Delimited d -> d.boundary
  | Core.Rule.Separated s -> s.boundary
;;

let rule_of
      (facts : Core.Facts.t)
      (msgs : Messages.Builder.t)
      (rule_def : Core.Rule.def)
      (block : int)
      (is_root : bool)
  : Ir.Plan.rule
  =
  let resume_of = resume_after facts rule_def in
  let inner =
    match rule_def.origin with
    (* The block's parse builds its own nodes, so the rule other rules
       reference is the parse and nothing around it. *)
    | Core.Rule.Pratt_block -> [ Ir.Plan.Pratt { block; min_bp = 0 } ]
    (* A role describes the shape of a node the block emits, and nothing calls
       it. It gets an empty body so that a [Call] can carry a rule id
       unchanged. *)
    | Core.Rule.Pratt_role _ -> []
    | Core.Rule.User ->
      let before =
        List.init ~len:rule_def.body_from ~f:(fun index ->
          instr_of_child
            facts
            msgs
            rule_def
            index
            (resume_of index)
            rule_def.children.(index))
      in
      let sep =
        match rule_def.frame with
        | Core.Rule.Delimited d -> d.sep
        | Core.Rule.Separated s ->
          Some { Core.Rule.sep_tok = s.sep_tok; trailing = s.trailing }
        | Core.Rule.Plain | Core.Rule.Committed _ -> None
      in
      let inside = body_instrs facts msgs rule_def sep resume_of in
      (match rule_def.frame with
       | Core.Rule.Delimited d ->
         let id =
           Messages.Builder.intern
             msgs
             (default_text facts (Core.Kind.Set.singleton d.close))
         in
         before
         @ [ Ir.Plan.Expect
               { tok = Core.Kind.to_int d.open_
               ; message =
                   Messages.Builder.intern
                     msgs
                     (default_text facts (Core.Kind.Set.singleton d.open_))
               ; at_child = None
               ; hole = None
               ; placeholder = None
               }
           ]
         @ inside
         (* Record a missing close as a node of the close's own kind. The
            formatter materialises that node, so one pass closes every level
            that is open. Without it the formatter emits the enclosing
            group's close instead, and each reparse adds one more. *)
         @ [ Ir.Plan.Expect
               { tok = Core.Kind.to_int d.close
               ; message = id
               ; at_child = None
               ; hole = None
               ; placeholder = Some (Core.Kind.to_int d.close)
               }
           ]
       | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ ->
         before @ inside)
  in
  let wrapped =
    match rule_def.origin with
    | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> inner
    | Core.Rule.User ->
      (* A root drains what is left of the input. Without that, trivia past
         the last child falls off the end and is lost, and any meaningful
         token still there is dropped with it. *)
      let tail = if is_root then [ Ir.Plan.Drain ] else [] in
      (Ir.Plan.Open (Core.Kind.to_int rule_def.kind) :: inner) @ tail @ [ Ir.Plan.Close ]
  in
  { name = Core.Grammar.Name.Rule.to_string rule_def.name
  ; kind = Core.Kind.to_int rule_def.kind
  ; first =
      (if Core.Rule.is_synthetic rule_def
       then [||]
       else kset (Core.Facts.first_of facts rule_def.id))
  ; adds = kset (adds_of rule_def)
  ; boundary = boundary_of rule_def
  ; body = Ir.Plan.Seq (Array.of_list wrapped)
  }
;;

(* -- an expression block --------------------------------------------------- *)

(* The kind of the node a role builds. An inactive role mints no rule, which
   is what the [option] on a block's prefix and infix kinds is for. *)
let role_kind (f : Core.Facts.t) (block_rule : Core.Rule.id) (want : Core.Role.t) =
  Array.find_map f.rules ~f:(fun (r : Core.Rule.def) ->
    match r.origin with
    | Core.Rule.Pratt_role p when p.block = block_rule && Core.Role.equal p.role want ->
      Some (Core.Kind.to_int r.kind)
    | Core.Rule.Pratt_role _ | Core.Rule.Pratt_block | Core.Rule.User -> None)
;;

let first_of_alts (f : Core.Facts.t) (alts : Core.Kind.t array) =
  Core.Kind.Set.unions
    (List.map (Array.to_list alts) ~f:(fun a -> Core.Facts.first_of_kind f a))
;;

let content_of (f : Core.Facts.t) msgs (c : Core.Block.content) : Ir.Plan.instr =
  match c with
  | Core.Block.One alts -> body_of_alts f alts
  | Core.Block.Many m ->
    let first = kset (first_of_alts f m.elem) in
    let body = body_of_alts f m.elem in
    (match m.sep with
     | None -> repetition first body
     | Some s ->
       let sep = Core.Kind.to_int s.sep_tok
       and after_sep = trailing_exit f msgs s in
       Ir.Plan.Seq [| separated_loop first sep body after_sep; Ir.Plan.Trivia |])
;;

let postfix_of (f : Core.Facts.t) msgs (p : Core.Block.postfix) : Ir.Plan.postfix =
  { lead = Core.Kind.to_int p.p_lead
  ; bp = p.p_bp
  ; kind = Core.Kind.to_int (Core.Facts.rule f p.p_rule).kind
  ; body =
      (match p.p_body with
       | Core.Block.Nothing -> Ir.Plan.Nothing
       | Core.Block.Then alts -> Ir.Plan.Then (kset (first_of_alts f alts))
       | Core.Block.Enclosed e ->
         Ir.Plan.Enclosed
           { close = Core.Kind.to_int e.close; body = content_of f msgs e.content })
  }
;;

let block_of (f : Core.Facts.t) msgs (b : Core.Block.def) : Ir.Plan.block =
  let atoms_first = first_of_alts f b.atoms in
  { infix =
      Array.map b.infix ~f:(fun (o : Core.Block.op) ->
        Core.Kind.to_int o.op_kind, Core.Block.op_bps o)
  ; prefix =
      Array.map b.prefix ~f:(fun (o : Core.Block.op) ->
        Core.Kind.to_int o.op_kind, snd (Core.Block.op_bps o))
  ; postfix = Array.map b.postfix ~f:(postfix_of f msgs)
  ; atoms =
      Array.map b.atoms ~f:(fun a ->
        let r = f.kind_rule.(Core.Kind.to_int a) in
        ( kset (Core.Facts.first_of_kind f a)
        , if r >= 0 then Ir.Plan.Atom_rule r else Ir.Plan.Atom_token ))
  ; base_kind =
      (match role_kind f b.rule_id Core.Role.Base with
       | Some x -> x
       | None -> Core.Kind.to_int b.kind)
  ; prefix_kind = role_kind f b.rule_id Core.Role.Prefix
  ; infix_kind = role_kind f b.rule_id Core.Role.Bin
  ; hole_kind = Core.Kind.to_int b.hole_kind
  ; expected = kset atoms_first
  ; message = Messages.Builder.intern msgs (default_text f atoms_first)
  }
;;

(* -- the plan -------------------------------------------------------------- *)

let of_facts (facts : Core.Facts.t) : Ir.Plan.t * Messages.t =
  let msgs = Messages.Builder.create () in
  let block_of_rule = Array.make (Array.length facts.rules) (-1) in
  Array.iteri facts.blocks ~f:(fun block_index (block_def : Core.Block.def) ->
    block_of_rule.(block_def.rule_id) <- block_index);
  let roots = Array.of_list facts.roots in
  let is_root id = Array.exists roots ~f:(fun root -> Int.equal root id) in
  let rules =
    Array.map facts.rules ~f:(fun (rule_def : Core.Rule.def) ->
      let block = block_of_rule.(rule_def.id)
      and is_root = is_root rule_def.id in
      rule_of facts msgs rule_def block is_root)
  in
  let blocks = Array.map facts.blocks ~f:(block_of facts msgs) in
  let pairs =
    Array.of_list
      (List.map (Core.Facts.delimiter_pairs facts) ~f:(fun (kind_open, kind_close) ->
         Core.Kind.to_int kind_open, Core.Kind.to_int kind_close))
  and trivia = kset facts.trivia
  and error_kind = Core.Kind.to_int facts.error_kind
  and missing_kind = Core.Kind.to_int facts.missing_kind in
  ( Ir.Plan.{ rules; blocks; roots; pairs; trivia; error_kind; missing_kind }
  , Messages.Builder.finish msgs )
;;
