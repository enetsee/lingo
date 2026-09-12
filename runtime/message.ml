type id = int

let of_int (i : int) : id = i
let to_int (i : id) : int = i
let equal (a : id) (b : id) = a = b
let compare (a : id) (b : id) = Int.compare a b
let pp fmt (i : id) = Format.pp_print_int fmt i
