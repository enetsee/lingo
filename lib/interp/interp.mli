(** Run a plan over a token stream.

    This is the specification. The emitted parser is the deployment, and a
    differential test compares the two on every fuzz input. So where this and
    the emitter disagree, this one is right by definition and the emitter is
    the thing to fix.

    It shares no parse code with the emitter. Both call the runtime for the
    cursor, the builder and {!Lingo_runtime.Recover.expect}, and each brings
    its own dispatch, its own loops and its own balanced skip. A shared copy
    of any of those would put that code outside the comparison. *)

(** [run plan entry tokens] parses [tokens] with the rule at index [entry],
    and gives the tree and the diagnostics in the order they were reported.

    [entry] is an index into [plan.rules], and any rule will do: a fuzz
    harness enters at one that is not a root to read a fragment. Raises
    [Invalid_argument] where no rule sits at that index, and where the rule
    there has an empty body, since a rule that opens no node builds no tree.

    Every byte of [tokens] reaches the tree, trivia included, so
    [Siesta.Green.to_source] gives the input back.

    [?trace] is called with the name of each instruction form as it runs:
    ["seq"], ["commit"], ["postfix"] and so on. A law counts those to say
    which of them the corpus reaches, because a form no parse runs is a form
    the law says nothing about.

    [?at] is called wherever the parse reads the cursor before its next step:
    the position in the plan, the index into [tokens] the parse has
    reached with trivia counted, and how many times it has reported. Two calls
    and their [reported] say whether the step between them reported.

    Every hole is one of those positions. A hole is what a position leaves
    behind when it reads the cursor and what the position needs is not there.
    {!Ir.Residual.at} turns a position into the kinds the grammar admits
    there. *)
val run
  :  ?trace:(string -> unit)
  -> ?at:(Ir.Residual.State.t -> index:int -> reported:int -> unit)
  -> Ir.Plan.t
  -> int
  -> Lingo_runtime.Token.t array
  -> Siesta.Green.node * Lingo_runtime.Diagnostic.t list
