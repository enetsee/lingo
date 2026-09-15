(** Lower [Facts.t] to a plan: the instructions a recursive-descent parser runs.

    {1 The rules line up}

    A plan holds the facts' rules at the same indices, so an [Ir.Plan.Call]
    carries a [Rule.id] unchanged.

    An expression block's roles are rules too, and nothing calls them, because
    the block's own parse builds those nodes. They are here with an empty body,
    to keep the indices lined up.

    {1 Recovery holds the local half}

    A parser that fails at a required child skips forward until it reaches a
    token it can resume on. Two things decide which tokens those are.

    The first is the child's own position: what can follow the child inside its
    rule, the rule's closer, and the rule's FOLLOW where the child is last. A
    [Commit]'s [recover] holds this, from {!Core.Facts.local_recovery_set}.

    The second is the closers of every frame open around the rule. The parser
    opened those frames itself, so it carries them down its own call stack and
    unions them in at each child.

    {!Core.Facts.recovery_set} answers both at once, from a table held per
    rule. A table indexed by rule cannot tell one call site from another. So a
    rule reached from inside [( … )] and from inside [\[ … \]] gets both
    closers at both sites, and a failure in the parenthesised call stops at a
    [\]] that no [\[] opened. *)

(** The plan, and the catalogue its message ids index. *)
val of_facts : Core.Facts.t -> Ir.Plan.t * Messages.t
