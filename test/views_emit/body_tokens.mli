(** [meaningful ~comments f tree] is the tree's tokens, kind and text, left
    to right. Whitespace is left out, and so is any separator at either end
    of a body. Comments are kept where [comments] is set. *)
val meaningful : comments:bool -> Core.Facts.t -> Siesta.Green.node -> (int * string) list
