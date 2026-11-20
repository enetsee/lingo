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
    the slots like everything else.

    {1 A list between brackets}

    A pair that owns its separator is one decision: where it opens, its
    elements go one to a line. A pair around a list that is a rule of its own
    is two rules, and was two decisions. {!Ir.Layout.slot.handed} makes it
    one. It is set on what a pair with no separator of its own may hold, where
    that is nothing but tokens and lists with no brackets of their own.

    Such a list is a separated rule, or a plain rule that may hold nothing but
    tokens and such rules, with one of them at least. [f(a, b; k)] holds two
    lists and a semicolon, and the rule for all three is one. A rule that may
    hold itself, a matched pair or an expression is none.

    A plain rule of that kind passes the decision on to the lists it holds, so
    its slots are marked as well, wherever a marked slot may hold it. *)

val of_facts : Core.Facts.t -> Ir.Layout.t
