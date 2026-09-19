(** The layout's printed form.

    A layout is data, so it has to be readable as a diff. The form is
    s-expressions with a fixed indentation, so the same layout gives the same
    bytes whatever the formatter's margin is set to. A golden that moves is
    then a statement of intent and never the width.

    Kinds print as integers. A caller wanting names prints a legend above the
    dump, the way the lexer's does. *)

val pp : Format.formatter -> Ir.Layout.t -> unit
