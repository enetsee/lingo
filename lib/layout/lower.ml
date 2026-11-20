open StdLabels

let kind (k : Core.Kind.t) : Ir.Kind.t = Core.Kind.to_int k

let break (style : Grammar.break_style) : Ir.Layout.break =
  match style with
  | Never -> Flat
  | Fit -> Fit
  | Always n -> Hard (n :> int)
;;

let side (s : Grammar.side) : Ir.Layout.side =
  match s with
  | Hug -> Hug
  | Free -> Free
  | Space -> Space
;;

let optional_sep (t : Grammar.optional_sep) : Ir.Layout.optional_sep =
  match t with
  | Never -> Never
  | On_break -> On_break
  | Always -> Always
;;

let position (p : Grammar.operator_position) : Ir.Layout.position =
  match p with
  | Op_after -> Ends_line
  | Op_before -> Starts_line
;;

(* A separator with no fixed spelling cannot be written, so both policies drop
   to [Never]: the parser still reports one and the formatter adds none.
   A pattern token matched whatever it matched and there is nothing to write. *)
let sep (f : Core.Facts.t) (s : Core.Rule.sep) : Ir.Layout.sep =
  match Core.Facts.token_of_kind f s.sep_tok with
  | Some t ->
    (match Core.Token.text t with
     | Some text ->
       { sep_kind = kind s.sep_tok
       ; text
       ; leading = optional_sep s.leading
       ; trailing = optional_sep s.trailing
       ; position = position s.position
       }
     | None ->
       { sep_kind = kind s.sep_tok
       ; text = ""
       ; leading = Never
       ; trailing = Never
       ; position = position s.position
       })
  | None ->
    { sep_kind = kind s.sep_tok
    ; text = ""
    ; leading = Never
    ; trailing = Never
    ; position = position s.position
    }
;;

(* [open_space] is the gap between an enclosed postfix's operand and its
   opener. A production's opener is its first child, so nothing precedes it. *)
let frame (facts : Core.Facts.t) ~(open_space : bool) (f : Core.Rule.frame)
  : Ir.Layout.frame
  =
  match f with
  | Plain | Committed _ -> Plain
  | Delimited { open_; close; sep = s; pad; boundary = _ } ->
    Delimited
      { open_ = kind open_
      ; close = kind close
      ; sep =
          (match s with
           | None -> None
           | Some s -> Some (sep facts s))
      ; open_space
      ; pad
      }
  | Separated { sep_tok; leading; trailing; position; boundary = _ } ->
    Separated (sep facts { Core.Rule.sep_tok; leading; trailing; position })
;;

(* A child whose symbol is an expression block is filled by any node that
   block's parse builds. The block's own kind is one of several, so the slot
   holds the roles and the hole as well.

   Without this an infix operand fails to match its slot as soon as it is itself
   an application, which is every expression but the shallowest.

   And a rule atom, which is the other node that parse builds. A token atom is
   wrapped in the base role, so the roles cover it; a rule atom stands as
   itself. Without it the fold does not count one as an element: it writes the
   separator a body's policy adds in front of the atom rather than after it,
   the next parse reads that separator as a real one, and the two passes lay
   the body out differently. rust's [f(0, i, {})] is the case, where [{}] is
   the [Block] atom. *)
let expansion (f : Core.Facts.t) =
  let tbl = Hashtbl.create 8 in
  Array.iter f.blocks ~f:(fun (b : Core.Block.def) ->
    let ks = ref [ kind b.kind; kind b.hole_kind ] in
    Array.iter b.atoms ~f:(fun a ->
      if not (Core.Facts.is_token_kind f a) then ks := kind a :: !ks);
    Array.iter f.rules ~f:(fun (r : Core.Rule.def) ->
      match r.origin with
      | Core.Rule.Pratt_role { block; _ } when block = b.rule_id ->
        ks := kind r.kind :: !ks
      | Core.Rule.User | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> ());
    Hashtbl.replace tbl (kind b.kind) !ks);
  tbl
;;

let kinds_of (expand : (Ir.Kind.t, Ir.Kind.t list) Hashtbl.t) (c : Core.Rule.child)
  : Ir.Kind.t array
  =
  let all =
    Array.fold_left c.alts ~init:[] ~f:(fun acc k ->
      let k = kind k in
      match Hashtbl.find_opt expand k with
      | Some ks -> List.rev_append ks acc
      | None -> k :: acc)
  in
  Array.of_list (List.sort_uniq ~cmp:Stdlib.compare all)
;;

let repeats (m : Grammar.modifier) =
  match m with
  | Zero_or_more _ | One_or_more _ -> true
  | Exactly_one | Zero_or_one -> false
;;

(* A role rule's own format is the default one, because the author writes the
   layout on the block rather than on the rules it desugars into. *)
let block_of (f : Core.Facts.t) (r : Core.Rule.def) =
  match r.origin with
  | Core.Rule.Pratt_role { block; role } ->
    let b =
      Array.to_list f.blocks
      |> List.find_opt ~f:(fun (b : Core.Block.def) -> b.rule_id = block)
    in
    (match b with
     | Some b -> Some (b, role)
     | None -> None)
  | Core.Rule.User | Core.Rule.Pratt_block -> None
;;

(* The boundary in front of each slot, and the one between two children of a
   slot that takes more than one. Each comes off the child that declares it.

   Slot zero describes a boundary that is not there: nothing of this rule
   precedes its first child. It reads [Flat] so the caller's boundary stands,
   and the break the first child declares is read as the rule's [body] and
   [inner] instead. *)
let slots (expand : (Ir.Kind.t, Ir.Kind.t list) Hashtbl.t) (r : Core.Rule.def)
  : Ir.Layout.slot array
  =
  Array.mapi r.children ~f:(fun i (c : Core.Rule.child) ->
    let before = if i = 0 then Ir.Layout.Flat else break c.c_break in
    { Ir.Layout.kinds = kinds_of expand c
    ; repeats = repeats c.modifier
    ; before
      (* A slot that holds one child has one boundary, so both its breaks
         name it. {!Layout.Check.Single_slot_between} is that invariant. *)
    ; between =
        (match c.modifier with
         | Grammar.Zero_or_more b | Grammar.One_or_more b -> break b
         | Grammar.Exactly_one | Grammar.Zero_or_one -> before)
    ; after = break c.c_after
    ; nests = c.c_indent
    ; together = c.c_together
    ; handed = false
    ; space = c.c_space
    })
;;

(* A slot that holds one child has one boundary, so both its breaks name it. *)
let with_before (s : Ir.Layout.slot) (b : Ir.Layout.break) =
  { s with Ir.Layout.before = b; between = (if s.repeats then s.between else b) }
;;

(* An infix role breaks at one of the two boundaries around its operator, and
   [operator_position] settles which. Nothing else in the fold separates the
   two. *)
let operator_slots (e : Grammar.expr_format) (ss : Ir.Layout.slot array) =
  let at i b = if i < Array.length ss then ss.(i) <- with_before ss.(i) b in
  (match e.operator_position with
   | Op_after ->
     at 1 Ir.Layout.Flat;
     at 2 Ir.Layout.Fit
   | Op_before ->
     at 1 Ir.Layout.Fit;
     at 2 Ir.Layout.Flat);
  ss
;;

let rule
      (f : Core.Facts.t)
      (expand : (Ir.Kind.t, Ir.Kind.t list) Hashtbl.t)
      (r : Core.Rule.def)
  : Ir.Layout.rule
  =
  let indent =
    match block_of f r with
    | Some (b, _) -> b.format.continuation_indent
    | None -> r.format.indent_width
  in
  (* The boundary against the frame, and the one a stray child takes. Both sit
     in front of the body, which is the first child, so both come off it. A
     rule with no children has neither and the value is unread. *)
  let edge =
    match block_of f r with
    | Some _ -> Ir.Layout.Fit
    | None ->
      if Array.length r.children = 0 then Ir.Layout.Fit else break r.children.(0).c_break
  in
  let ss = slots expand r in
  let ss =
    match block_of f r with
    | Some (b, Core.Role.Bin) -> operator_slots b.format ss
    | Some (_, (Core.Role.Base | Core.Role.Prefix | Core.Role.Postfix _)) ->
      Array.map ss ~f:(fun s -> with_before s Ir.Layout.Flat)
    | None -> ss
  in
  let open_space =
    match block_of f r with
    | Some (b, Core.Role.Postfix i) -> b.postfix.(i).p_space
    | Some (_, (Core.Role.Base | Core.Role.Bin | Core.Role.Prefix)) | None -> false
  in
  { name = Grammar.Name.Rule.to_string r.name
  ; kind = kind r.kind
  ; frame = frame f ~open_space r.frame
  ; slots = ss
  ; body = edge
  ; inner = edge
  ; indent
  }
;;

let tokens (f : Core.Facts.t) =
  let out = Array.make (Core.Facts.kind_count f) None in
  Array.iter f.tokens ~f:(fun (t : Core.Token.def) ->
    out.(kind t.kind)
    <- Some
         { Ir.Layout.space_before = side t.format.space_before
         ; space_after = side t.format.space_after
         ; trivia =
             (match t.trivia with
              | None -> None
              | Some Reformat -> Some Ir.Layout.Reformat
              | Some Preserve -> Some Ir.Layout.Preserve)
         });
  (* The two the lexer makes rather than the grammar. Both hold bytes nobody
     described, and the best a formatter can do with those is leave them where
     the source had them, so neither takes a space. *)
  let raw = Some { Ir.Layout.space_before = Hug; space_after = Hug; trivia = None } in
  out.(kind f.error_token_kind) <- raw;
  out.(kind f.unterminated_kind) <- raw;
  out
;;

(* Whether everything [slots] may hold from [from] on is a token or a list,
   with one list at least. *)
let made_of_lists
      (is_list : int -> bool)
      (of_kind : int array)
      (slots : Ir.Layout.slot array)
      ~(from : int)
  : bool
  =
  let lists = ref 0 in
  let others = ref 0 in
  Array.iteri slots ~f:(fun i (s : Ir.Layout.slot) ->
    if i >= from
    then
      Array.iter s.kinds ~f:(fun (k : Ir.Kind.t) ->
        let held = of_kind.(k) in
        if held >= 0 then if is_list held then incr lists else incr others));
  !others = 0 && !lists > 0
;;

(* Which rules are a list with no brackets of its own: a separated rule, and
   a plain rule that may hold nothing but tokens and such rules, with one of
   them at least. [f(a, b; k)] holds two lists and a semicolon between its
   brackets, and the rule for all three is one of these.

   A rule that may hold itself is none. Neither is one that may hold a
   matched pair or an expression: what those do when the line does not fit is
   theirs to settle. *)
let lists (rules : Ir.Layout.rule array) (of_kind : int array) : bool array =
  let known : bool option array = Array.make (Array.length rules) None in
  let rec is_list (seen : int list) (ri : int) : bool =
    match known.(ri) with
    | Some found -> found
    | None ->
      let r = rules.(ri) in
      let found =
        match r.frame with
        | Separated _ -> true
        | Delimited _ -> false
        | Plain ->
          (not (List.mem ri ~set:seen))
          && made_of_lists (is_list (ri :: seen)) of_kind r.slots ~from:0
      in
      known.(ri) <- Some found;
      found
  in
  Array.mapi rules ~f:(fun ri (_ : Ir.Layout.rule) -> is_list [] ri)
;;

(* The slots a rule hands its decision through, which is
   {!Ir.Layout.slot.handed}.

   A matched pair with no separator of its own hands it on where everything it
   may hold is a token or a list. A pair with a separator holds elements, and
   an element decides for itself.

   An enclosed postfix has its operand in slot zero, in front of the opener.
   That is no part of what the pair holds.

   A plain list passes on what it was handed, so the slots of every one a
   marked slot may hold are marked as well. *)
let hand (f : Core.Facts.t) (rules : Ir.Layout.rule array) (of_kind : int array)
  : Ir.Layout.rule array
  =
  let is_list = lists rules of_kind in
  let marks =
    Array.map rules ~f:(fun (r : Ir.Layout.rule) ->
      Array.make (Array.length r.slots) false)
  in
  let rec mark (ri : int) ~(from : int) : unit =
    Array.iteri rules.(ri).slots ~f:(fun i (s : Ir.Layout.slot) ->
      if i >= from && not marks.(ri).(i)
      then
        Array.iter s.kinds ~f:(fun (k : Ir.Kind.t) ->
          let held = of_kind.(k) in
          if held >= 0
          then (
            marks.(ri).(i) <- true;
            match rules.(held).frame with
            | Plain -> mark held ~from:0
            | Delimited _ | Separated _ -> ())))
  in
  Array.iteri rules ~f:(fun ri (r : Ir.Layout.rule) ->
    match r.frame with
    | Delimited { sep = None; _ } ->
      let from =
        match block_of f f.rules.(ri) with
        | Some (_, Core.Role.Postfix _) -> 1
        | Some (_, (Core.Role.Base | Core.Role.Bin | Core.Role.Prefix)) | None -> 0
      in
      if made_of_lists (fun (held : int) -> is_list.(held)) of_kind r.slots ~from
      then mark ri ~from
    | Delimited { sep = Some _; _ } | Plain | Separated _ -> ());
  Array.mapi rules ~f:(fun ri (r : Ir.Layout.rule) ->
    { r with
      slots =
        Array.mapi r.slots ~f:(fun i (s : Ir.Layout.slot) ->
          { s with handed = marks.(ri).(i) })
    })
;;

let of_facts (f : Core.Facts.t) : Ir.Layout.t =
  let expand = expansion f in
  let of_kind = Array.copy f.kind_rule in
  { rules = hand f (Array.map f.rules ~f:(rule f expand)) of_kind
  ; of_kind
  ; tokens = tokens f
  }
;;
