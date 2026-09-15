open StdLabels

type t = { entries : string array } [@@ocaml.unboxed]

let entries ({ entries } : t) = entries
let count ({ entries } : t) = Array.length entries
let text ({ entries } : t) (id : Ir.Message.id) = entries.(Ir.Message.to_int id)

let pp fmt ({ entries } : t) =
  Array.iteri entries ~f:(fun i s -> Format.fprintf fmt "@[<h>%d %S@]@," i s)
;;

module Builder = struct
  type t =
    { seen : (string, Ir.Message.id) Hashtbl.t
    ; order : string Dynarray.t
    }

  let create () = { seen = Hashtbl.create 64; order = Dynarray.create () }

  let intern (t : t) (str : string) : Ir.Message.id =
    match Hashtbl.find_opt t.seen str with
    | Some id -> id
    | None ->
      let id = Ir.Message.of_int (Dynarray.length t.order) in
      Dynarray.add_last t.order str;
      Hashtbl.add t.seen str id;
      id
  ;;

  let finish ({ order; _ } : t) = { entries = Dynarray.to_array order }
end
