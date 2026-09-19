open StdLabels

let ints ppf ks = Array.iter ks ~f:(fun k -> Format.fprintf ppf " %d" k)

let break ppf (b : Ir.Layout.break) =
  match b with
  | Flat -> Format.pp_print_string ppf "flat"
  | Fit -> Format.pp_print_string ppf "fit"
  | Hard n -> Format.fprintf ppf "(hard %d)" n
;;

let trailing ppf (t : Ir.Layout.trailing) =
  Format.pp_print_string
    ppf
    (match t with
     | Never -> "never"
     | On_break -> "on-break"
     | Always -> "always")
;;

let sep ppf (s : Ir.Layout.sep) =
  Format.fprintf ppf "(sep %d %a)" s.sep_kind trailing s.trailing
;;

let frame ppf (f : Ir.Layout.frame) =
  match f with
  | Plain -> Format.pp_print_string ppf "(frame plain)"
  | Delimited { open_; close; sep = s } ->
    Format.fprintf ppf "(frame delimited %d %d" open_ close;
    (match s with
     | None -> ()
     | Some s -> Format.fprintf ppf " %a" sep s);
    Format.pp_print_string ppf ")"
  | Separated s -> Format.fprintf ppf "(frame separated %a)" sep s
;;

let token ppf (k : int) (t : Ir.Layout.token) =
  Format.fprintf
    ppf
    "  (token %d%s%s"
    k
    (if t.space_before then " before" else "")
    (if t.space_after then " after" else "");
  (match t.trivia with
   | None -> ()
   | Some Reformat -> Format.pp_print_string ppf " reformat"
   | Some Preserve -> Format.pp_print_string ppf " preserve");
  Format.pp_print_string ppf ")\n"
;;

let slot ppf (s : Ir.Layout.slot) =
  Format.fprintf
    ppf
    "    (slot (kinds%a)%s (before %a) (between %a))\n"
    ints
    s.kinds
    (if s.repeats then " repeats" else "")
    break
    s.before
    break
    s.between
;;

let rule ppf (i : int) (r : Ir.Layout.rule) =
  Format.fprintf ppf "  (rule %d %s %d\n" i r.name r.kind;
  Format.fprintf ppf "    %a\n" frame r.frame;
  Format.fprintf
    ppf
    "    (body %a) (inner %a) (indent %d)"
    break
    r.body
    break
    r.inner
    r.indent;
  (match r.edge_before with
   | None -> ()
   | Some b -> Format.fprintf ppf " (edge-before %b)" b);
  (match r.edge_after with
   | None -> ()
   | Some b -> Format.fprintf ppf " (edge-after %b)" b);
  Format.pp_print_string ppf "\n";
  Array.iter r.slots ~f:(slot ppf);
  Format.pp_print_string ppf "  )\n"
;;

(* A kind with no token entry is a node's, and a node's layout is its rule. So
   the token section runs one line per token and the rule section one block per
   rule. *)
let pp ppf (l : Ir.Layout.t) =
  Format.pp_print_string ppf "(layout\n";
  Array.iteri l.tokens ~f:(fun k t ->
    match t with
    | None -> ()
    | Some t -> token ppf k t);
  Array.iteri l.rules ~f:(rule ppf);
  Format.pp_print_string ppf ")\n"
;;
