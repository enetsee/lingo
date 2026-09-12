type missing =
  { at_child : string option
  ; expected : Message.id
  ; expected_kinds : Kind.t list
  ; hole_kind : Kind.t option
  }

type kind =
  | Missing of missing
  | Extra of Message.id
  | Unexpected

type t =
  { range : int * int
  ; kind : kind
  }

(* The message id prints as a number. Rendering it needs the catalogue, and a
   diagnostic does not carry one. *)
let pp fmt (d : t) =
  let lo, hi = d.range in
  match d.kind with
  | Missing m ->
    Format.fprintf
      fmt
      "@[<h>%d-%d missing %a%a%a@]"
      lo
      hi
      Message.pp
      m.expected
      (fun fmt -> function
         | None -> ()
         | Some c -> Format.fprintf fmt " at %s" c)
      m.at_child
      (fun fmt ks ->
         if ks <> []
         then
           Format.fprintf
             fmt
             " expecting %a"
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
                Format.pp_print_int)
             ks)
      m.expected_kinds
  | Extra id -> Format.fprintf fmt "@[<h>%d-%d extra %a@]" lo hi Message.pp id
  | Unexpected -> Format.fprintf fmt "@[<h>%d-%d unexpected@]" lo hi
;;

let pp_list fmt ds =
  Format.fprintf fmt "@[<v>%a@]" (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp) ds
;;
