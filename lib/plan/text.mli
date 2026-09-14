(** The plan's printed form, and the reader that takes it back.

    A plan is data, so it has to be readable as a diff and editable by hand.
    The form is s-expressions, which is the smallest thing that prints and
    reads without an indentation-sensitive reader. The printer breaks a form
    that does not fit, so a dump still reads as a tree. *)

(** A dump. The same plan gives the same bytes at the same width, so a
    golden moving is a statement of intent rather than noise. The width comes
    from the formatter, and a form that does not fit it breaks, so a caller
    holding a golden sets one and keeps it. *)
val pp : Format.formatter -> Ir.Plan.t -> unit

(** Reads {!pp}'s output, and a hand-edited version of it: [;] starts a
    comment that runs to the end of its line.

    The round trip holds for a plan {!Check.run} accepts. An empty [resume]
    set prints the way [None] does, and that check is what rules the first
    one out. *)
val parse : string -> (Ir.Plan.t, string) result
