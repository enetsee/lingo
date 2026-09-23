(** How much of a plan a corpus reached.

    A {b point} is a place in the plan where a parse read the cursor. An
    {b edge} is a pair of consecutive points. The count that matters is the
    edges, and the reason is that the points saturate: how many there are
    follows from the plan, so once a corpus has reached the grammar the number
    stops moving and a run that explores nothing reads the same as a run that
    explores everything.

    The predecessor counted single decisions. That saturated at about a
    thousand iterations, and every sweep for ten issues after it was a million
    random samples with a flat number beside them. Pairing consecutive
    decisions took one grammar from 29 edges to 691 and one production's
    trailing separator from zero to 130.

    A point is {!Ir.Residual.State.site} rather than the whole parse stack. The
    stack grows with the input's nesting, so distinct stacks climb with depth
    whatever the parse explored, and a count over them measures the corpus's
    brackets. *)

type t

val create : unit -> t

(** Start a walk. The first point in a walk opens no edge, because it has no
    predecessor inside the walk, and pairing it with the last point of the
    previous parse would count an edge no plan has. *)
val walk : t -> unit

(** One point of the walk {!walk} opened. *)
val at : t -> Ir.Residual.State.t -> unit

(** Distinct points since {!create}. *)
val points : t -> int

(** Distinct edges since {!create}. *)
val edges : t -> int

(** Whether the walk in progress has opened an edge no earlier walk did. A
    coverage-guided corpus keeps the inputs this is true of and drops the rest,
    so a later mutation starts from something that reached somewhere new. *)
val fresh : t -> bool
