open StdLabels

let kind (k : Core.Kind.t) : Ir.Kind.t = Core.Kind.to_int k

let break (style : Grammar.break_style) ~(lines : int) : Ir.Layout.break =
  match style with
  | Never -> Flat
  | Fit -> Fit
  | Always -> Hard (if lines < 1 then 1 else lines)
;;

let trailing (t : Grammar.trailing_sep) : Ir.Layout.trailing =
  match t with
  | Never -> Never
  | On_break -> On_break
  | Always -> Always
;;

(* A separator with no fixed spelling cannot be written, so the policy drops to
   [Never]: the parser still reports a trailing one and the formatter adds none.
   A pattern token matched whatever it matched and there is nothing to write. *)
let sep (f : Core.Facts.t) (s : Core.Rule.sep) : Ir.Layout.sep =
  match Core.Facts.token_of_kind f s.sep_tok with
  | Some t ->
    (match Core.Token.text t with
     | Some text -> { sep_kind = kind s.sep_tok; text; trailing = trailing s.trailing }
     | None -> { sep_kind = kind s.sep_tok; text = ""; trailing = Never })
  | None -> { sep_kind = kind s.sep_tok; text = ""; trailing = Never }
;;

let frame (facts : Core.Facts.t) (f : Core.Rule.frame) : Ir.Layout.frame =
  match f with
  | Plain | Committed _ -> Plain
  | Delimited { open_; close; sep = s; boundary = _ } ->
    Delimited
      { open_ = kind open_
      ; close = kind close
      ; sep =
          (match s with
           | None -> None
           | Some s -> Some (sep facts s))
      }
  | Separated ({ sep_tok = _; trailing = _; boundary = _ } as sp) ->
    Separated (sep facts { Core.Rule.sep_tok = sp.sep_tok; trailing = sp.trailing })
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
  | Zero_or_more | One_or_more -> true
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
   slot that takes more than one.

   Slot zero describes a boundary that is not there: nothing of this rule
   precedes its first child. It reads [Flat] so the caller's boundary stands. *)
let slots
      (expand : (Ir.Kind.t, Ir.Kind.t list) Hashtbl.t)
      (r : Core.Rule.def)
      ~(style : Grammar.break_style)
      ~(lines : int)
  : Ir.Layout.slot array
  =
  Array.mapi r.children ~f:(fun i (c : Core.Rule.child) ->
    let before = if i = 0 then Ir.Layout.Flat else break style ~lines:1 in
    let rep = repeats c.modifier in
    { Ir.Layout.kinds = kinds_of expand c
    ; repeats = rep
    ; before
    ; between = (if rep then break style ~lines else before)
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
  let style, indent, lines =
    match block_of f r with
    | Some (b, _) -> Grammar.Fit, b.format.continuation_indent, 1
    | None -> r.format.break_style, r.format.indent_width, r.format.separator_lines
  in
  let ss = slots expand r ~style ~lines in
  let ss =
    match block_of f r with
    | Some (b, Core.Role.Bin) -> operator_slots b.format ss
    | Some (_, (Core.Role.Base | Core.Role.Prefix | Core.Role.Postfix _)) ->
      Array.map ss ~f:(fun s -> with_before s Ir.Layout.Flat)
    | None -> ss
  in
  { name = Grammar.Name.Rule.to_string r.name
  ; kind = kind r.kind
  ; frame = frame f r.frame
  ; slots = ss
  ; body = break style ~lines:1
  ; inner = break style ~lines:1
  ; indent
  ; edge_before = r.edge_space_before
  ; edge_after = r.edge_space_after
  }
;;

let tokens (f : Core.Facts.t) =
  let out = Array.make (Core.Facts.kind_count f) None in
  Array.iter f.tokens ~f:(fun (t : Core.Token.def) ->
    out.(kind t.kind)
    <- Some
         { Ir.Layout.space_before = t.format.space_before
         ; space_after = t.format.space_after
         ; trivia =
             (match t.trivia with
              | None -> None
              | Some Reformat -> Some Ir.Layout.Reformat
              | Some Preserve -> Some Ir.Layout.Preserve)
         });
  (* The two the lexer makes rather than the grammar. Both hold bytes nobody
     described, and the best a formatter can do with those is leave them where
     the source had them, so neither takes a space. *)
  let raw = Some { Ir.Layout.space_before = false; space_after = false; trivia = None } in
  out.(kind f.error_token_kind) <- raw;
  out.(kind f.unterminated_kind) <- raw;
  out
;;

let of_facts (f : Core.Facts.t) : Ir.Layout.t =
  let expand = expansion f in
  { rules = Array.map f.rules ~f:(rule f expand)
  ; of_kind = Array.copy f.kind_rule
  ; tokens = tokens f
  }
;;
