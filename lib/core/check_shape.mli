(** The checks that read resolved children and normalised framing.

    Everything here is a statement about kinds. How many children does a
    frame wrap? Do two child slots admit one node? Both questions are over
    resolved kinds. Over the author's spellings they would redo the
    resolution the first stage has already done.

    They run over rules the author wrote, and skip the desugared roles. The
    desugaring builds a role to a fixed shape, so it cannot get these wrong.
    An infix role has an [lhs] and an [rhs] that admit the same kind, and its
    accessors address them by position. *)

val run : Stage.shape -> Error.t list
