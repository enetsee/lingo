(** Lower [Facts.t] to a layout: where a formatter may end a line.

    {1 The rules line up}

    A layout holds the facts' rules at the same indices, so a rule id reads the
    same here as it does in a plan.

    {1 One frame, both spellings}

    A production's delimited framing and an expression block's enclosed
    postfix body are the same {!Core.Rule.type-frame} by the time the facts are
    built, so they lower to the same {!Ir.Layout.type-frame} and the fold has one
    shape to walk. The predecessor wrote the two out separately and the
    separator rule reached only one of them.

    {1 Where the operator goes}

    An infix role's [operator_position] is not a mode the fold has to read
    about. It is two boundaries: the one before the operator and the one after
    it, and the position settles which of them may end the line. So it lowers to
    the slots like everything else. *)

val of_facts : Core.Facts.t -> Ir.Layout.t
