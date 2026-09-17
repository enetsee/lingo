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
let repetition
      (first : Ir.Kind.t array)
      (body : Ir.Plan.instr)
      (ends_on : Ir.Kind.t array option)
  : Ir.Plan.instr
  =
  Ir.Plan.Seq
    [| Ir.Plan.Loop
         { entry = 0
         ; ends_on
         ; states =
             [| { accepts = [| first, 0 |]
                ; exit = Ir.Plan.May_exit
                ; when_missing = None
                ; emits = body
                }
             |]
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
      (repeat_ends_on : Ir.Kind.t array option)
      (child : Core.Rule.child)
  : Ir.Plan.instr
  =
  let first = child_first facts child in
  match child.modifier with
  (* Neither of these reports, so neither asks for a message. Asking here
     would put wording in the catalogue that no instruction ever names. *)
  | Core.Grammar.Optional ->
    Ir.Plan.Alt { arms = [| kset first, body_of_alts facts child.alts |] }
  (* A repeated child outside a frame ends where no element can start. Only a
     frame's own body recovers, because only a frame has a closer to stop at.
     See [body_instrs]. *)
  | Core.Grammar.Repeated ->
    repetition (kset first) (body_of_alts facts child.alts) repeat_ends_on
  | Core.Grammar.Required ->
    let at_child = Core.Grammar.Name.Child.to_string child.child_name in
    let id = message facts msgs rule_def child first in
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
      (ends_on : Ir.Kind.t array option)
      (no_sep : Ir.Message.id)
  : Ir.Plan.instr
  =
  Ir.Plan.Loop
    { entry = 0
    ; ends_on
    ; states =
        [| { accepts = [| elem_first, 1 |]
           ; exit = Ir.Plan.May_exit
           ; when_missing = None
           ; emits = body
           }
           (* Just past an element. A separator is what carries the body on, so
              a body that has none here is missing one rather than holding
              junk, and [goto] is where taking one would have led. *)
         ; { accepts = [| [| sep |], 2 |]
           ; exit = Ir.Plan.May_exit
           ; when_missing = Some { tok = sep; message = no_sep; goto = 2 }
           ; emits = Ir.Plan.Bump
           }
         ; { accepts = [| elem_first, 1 |]
           ; exit = after_sep
           ; when_missing = None
           ; emits = body
           }
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
(* What ends a body rather than being recovered past: the frame's closer, and
   the anchors the grammar declared. Everything else a body meets and cannot
   use is swept into an error node, and the body carries on.

   A separated list answers [None]. It has no closer, so it ends where the
   cursor leaves the separator and what follows is the caller's business. *)
let ends_on_of (rule_def : Core.Rule.def) : Ir.Kind.t array option =
  match rule_def.frame with
  | Core.Rule.Delimited d -> Some (kset (Core.Kind.Set.add d.close rule_def.resync))
  | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> None
;;

(* A root whose last child is repeated is a file of items, and it has no
   closer. Such a body recovers to the end of the input rather than ending at
   the first token it cannot use, so one stray token at the top level costs a
   diagnostic instead of every item after it.

   [Some [||]] is what says so: recover, and let nothing but the end of the
   input end it. *)
let repeat_ends_on_of (rule_def : Core.Rule.def) ~(is_root : bool) (index : int) =
  let last = Array.length rule_def.children - 1 in
  match is_root && index = last && index >= 0 with
  | true ->
    (match rule_def.children.(index).modifier with
     | Core.Grammar.Repeated -> Some [||]
     | Core.Grammar.Required | Core.Grammar.Optional -> None)
  | false -> None
;;

let body_instrs
      (facts : Core.Facts.t)
      (msgs : Messages.Builder.t)
      (rule_def : Core.Rule.def)
      (sep_opt : Core.Rule.sep option)
      (resume_of : int -> int array option)
      (is_root : bool)
  : Ir.Plan.instr list
  =
  let body = Core.Rule.body_children rule_def in
  match sep_opt, body with
  | Some sep, [ ({ modifier = Core.Grammar.Repeated; _ } as child) ] ->
    let elem_first = kset (child_first facts child)
    and sep_kind = Core.Kind.to_int sep.sep_tok
    and body = body_of_alts facts child.alts
    and after_sep = trailing_exit facts msgs sep in
    let no_sep =
      Messages.Builder.intern
        msgs
        (default_text facts (Core.Kind.Set.singleton sep.sep_tok))
    in
    [ separated_loop elem_first sep_kind body after_sep (ends_on_of rule_def) no_sep
    ; Ir.Plan.Trivia
    ]
  (* A delimited body with no separator. It recovers like the separated one,
     because it has the same closer to stop at. *)
  | None, [ ({ modifier = Core.Grammar.Repeated; _ } as child) ]
    when ends_on_of rule_def <> None ->
    [ repetition
        (kset (child_first facts child))
        (body_of_alts facts child.alts)
        (ends_on_of rule_def)
    ]
  | _ ->
    List.mapi body ~f:(fun i c ->
      let index = rule_def.body_from + i in
      instr_of_child
        facts
        msgs
        rule_def
        index
        (resume_of index)
        (repeat_ends_on_of rule_def ~is_root index)
        c)
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
   down: its closer, and its separator where it has one.

   The resync anchors are not here. [ends_on_of] carries them, and a
   delimited body's loop unions its stopping set into what it hands an
   element, so an anchor reaches a nested call that way. A rule with anchors
   and no delimited frame hands them nowhere: [ends_on_of] is the only reader
   of [rule.resync], and it answers [None] for every other frame. *)
let adds_of ({ frame; _ } : Core.Rule.def) : Core.Kind.Set.t =
  let framing =
    match frame with
    | Core.Rule.Plain | Core.Rule.Committed _ -> Core.Kind.Set.empty
    | Core.Rule.Separated s -> Core.Kind.Set.singleton s.sep_tok
    | Core.Rule.Delimited d ->
      let kind_set = Core.Kind.Set.singleton d.close in
      Option.fold d.sep ~none:kind_set ~some:(fun (sep : Core.Rule.sep) ->
        Core.Kind.Set.add sep.sep_tok kind_set)
  in
  framing
;;

let boundary_of ({ frame; _ } : Core.Rule.def) : bool =
  match frame with
  | Core.Rule.Plain -> false
  | Core.Rule.Committed c -> c.boundary
  | Core.Rule.Delimited d -> d.boundary
  | Core.Rule.Separated s -> s.boundary
;;

(* Everything inside a delimited frame: the body, then the closer. The opener
   is the caller's, because a production takes it as its first token and a
   postfix operator took it as its lead.

   A missing close is recorded as a node of the close's own kind. The formatter
   materialises that node, so one pass closes every level that is open. Without
   it the formatter emits the enclosing group's close instead, and each reparse
   adds one more. *)
let delimited_tail
      (facts : Core.Facts.t)
      (msgs : Messages.Builder.t)
      (rule_def : Core.Rule.def)
      resume_of
      (close : Core.Kind.t)
      (sep : Core.Rule.sep option)
  : Ir.Plan.instr list
  =
  body_instrs facts msgs rule_def sep resume_of false
  @ [ Ir.Plan.Expect
        { tok = Core.Kind.to_int close
        ; message =
            Messages.Builder.intern
              msgs
              (default_text facts (Core.Kind.Set.singleton close))
        ; at_child = None
        ; hole = None
        ; placeholder = Some (Core.Kind.to_int close)
        }
    ]
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
            None
            rule_def.children.(index))
      in
      let sep =
        match rule_def.frame with
        | Core.Rule.Delimited d -> d.sep
        | Core.Rule.Separated s ->
          Some { Core.Rule.sep_tok = s.sep_tok; trailing = s.trailing }
        | Core.Rule.Plain | Core.Rule.Committed _ -> None
      in
      (match rule_def.frame with
       | Core.Rule.Delimited d ->
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
         @ delimited_tail facts msgs rule_def resume_of d.close sep
       | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ ->
         before @ body_instrs facts msgs rule_def sep resume_of is_root)
  in
  let wrapped =
    match rule_def.origin with
    | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> inner
    | Core.Rule.User ->
      (* A root drains what is left of the input. Without that, trivia past
         the last child falls off the end and is lost, and any meaningful
         token still there is dropped with it. *)
      let tail =
        if is_root
        then [ Ir.Plan.Drain (Messages.Builder.intern msgs "trailing tokens") ]
        else []
      in
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

(* The kind of the node a role builds. An inactive role has no rule, which
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

(* A postfix operator's body. The Pratt loop takes the lead token before this
   runs, so an enclosed form starts after its opener.

   The shapes come from the rule the block desugared to. That rule carries the
   child names, the messages and the frame, so a postfix body lowers through
   the same path a production's body does. *)
let postfix_of (facts : Core.Facts.t) (msgs : Messages.Builder.t) (p : Core.Block.postfix)
  : Ir.Plan.postfix
  =
  let rule_def = Core.Facts.rule facts p.p_rule in
  let resume_of = resume_after facts rule_def in
  { lead = Core.Kind.to_int p.p_lead
  ; bp = p.p_bp
  ; kind = Core.Kind.to_int rule_def.kind
  ; body =
      (match p.p_body with
       (* The lead token is the whole operator. *)
       | Core.Block.Nothing -> Ir.Plan.Seq [||]
       (* The child that follows the lead is the rule's last one. *)
       | Core.Block.Then _ ->
         let index = Array.length rule_def.children - 1 in
         instr_of_child
           facts
           msgs
           rule_def
           index
           (resume_of index)
           None
           rule_def.children.(index)
       | Core.Block.Enclosed e ->
         let sep =
           match e.content with
           | Core.Block.One _ -> None
           | Core.Block.Many m -> m.sep
         in
         Ir.Plan.Seq
           (Array.of_list (delimited_tail facts msgs rule_def resume_of e.close sep)))
  }
;;

let block_of (f : Core.Facts.t) msgs (b : Core.Block.def) : Ir.Plan.block =
  let atoms_first = first_of_alts f b.atoms in
  { name = Core.Grammar.Name.Rule.to_string f.rules.(b.rule_id).name
  ; infix =
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
