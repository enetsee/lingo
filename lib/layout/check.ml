open StdLabels

type problem =
  | Rule_out_of_range of
      { at : string
      ; id : int
      }
  | Kind_out_of_range of
      { at : string
      ; kind : int
      }
  | Kind_not_its_rule of
      { at : string
      ; kind : int
      }
  | Kinds_unordered of
      { at : string
      ; kinds : int list
      }
  | Lead_slot_breaks of { at : string }
  | Single_slot_between of { at : string }
  | Empty_break of
      { at : string
      ; lines : int
      }
  | Negative_indent of
      { at : string
      ; indent : int
      }
  | Rule_kind_is_a_token of
      { at : string
      ; kind : int
      }

let pp_problem (ppf : Format.formatter) (p : problem) : unit =
  match p with
  | Rule_out_of_range { at; id } -> Format.fprintf ppf "%s: no rule at %d" at id
  | Kind_out_of_range { at; kind } -> Format.fprintf ppf "%s: no kind %d" at kind
  | Kind_not_its_rule { at; kind } ->
    Format.fprintf ppf "%s: kind %d does not index back to this rule" at kind
  | Kinds_unordered { at; kinds } ->
    Format.fprintf
      ppf
      "@[<h>%s: kinds not ascending and distinct:%a@]"
      at
      (fun ppf ks -> List.iter ks ~f:(fun k -> Format.fprintf ppf " %d" k))
      kinds
  | Lead_slot_breaks { at } -> Format.fprintf ppf "%s: the first slot names a break" at
  | Single_slot_between { at } ->
    Format.fprintf ppf "%s: a slot holding one child names a break between two" at
  | Empty_break { at; lines } ->
    Format.fprintf ppf "%s: a hard break of %d lines" at lines
  | Negative_indent { at; indent } -> Format.fprintf ppf "%s: an indent of %d" at indent
  | Rule_kind_is_a_token { at; kind } ->
    Format.fprintf ppf "%s: kind %d is a token as well as a rule" at kind
;;

let run (l : Ir.Layout.t) =
  let found = ref [] in
  let report p = found := p :: !found in
  let kinds = Array.length l.tokens in
  let in_range ~at k =
    if k < 0 || k >= kinds then report (Kind_out_of_range { at; kind = k })
  in
  let brk ~at (b : Ir.Layout.break) =
    match b with
    | Hard n when n < 1 -> report (Empty_break { at; lines = n })
    | Hard _ | Fit | Flat -> ()
  in
  Array.iteri l.of_kind ~f:(fun k id ->
    let at = Printf.sprintf "kind %d" k in
    if id >= Array.length l.rules then report (Rule_out_of_range { at; id }));
  Array.iteri l.rules ~f:(fun i (r : Ir.Layout.rule) ->
    let at = Printf.sprintf "rule %d %s" i r.name in
    in_range ~at r.kind;
    if r.kind >= 0 && r.kind < Array.length l.of_kind && l.of_kind.(r.kind) <> i
    then report (Kind_not_its_rule { at; kind = r.kind });
    if r.kind >= 0 && r.kind < kinds && l.tokens.(r.kind) <> None
    then report (Rule_kind_is_a_token { at; kind = r.kind });
    if r.indent < 0 then report (Negative_indent { at; indent = r.indent });
    brk ~at r.body;
    brk ~at r.inner;
    (match r.frame with
     | Plain -> ()
     | Delimited { open_; close; sep } ->
       in_range ~at:(at ^ " open") open_;
       in_range ~at:(at ^ " close") close;
       (match sep with
        | None -> ()
        | Some s -> in_range ~at:(at ^ " sep") s.sep_kind)
     | Separated s -> in_range ~at:(at ^ " sep") s.sep_kind);
    Array.iteri r.slots ~f:(fun j (s : Ir.Layout.slot) ->
      let at = Printf.sprintf "%s slot %d" at j in
      Array.iter s.kinds ~f:(in_range ~at);
      let ordered =
        let rec go i =
          i + 1 >= Array.length s.kinds || (s.kinds.(i) < s.kinds.(i + 1) && go (i + 1))
        in
        Array.length s.kinds = 0 || go 0
      in
      if not ordered then report (Kinds_unordered { at; kinds = Array.to_list s.kinds });
      brk ~at s.before;
      brk ~at s.between;
      if j = 0 && s.before <> Flat then report (Lead_slot_breaks { at });
      if (not s.repeats) && s.between <> s.before then report (Single_slot_between { at })));
  match List.rev !found with
  | [] -> Ok ()
  | ps -> Error ps
;;
