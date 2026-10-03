(** The typed views, as OCaml source.

    A view is a production's node with a type of its own, and an accessor per
    child. It is a [Siesta.Syntax.t] underneath, made [private] in the
    signature, so a caller coerces out of one with [:>] and builds one only
    through [cast].

    {1 What the module holds}

    - [<rule>_view] for every production and every role a block's parse builds.
    - [<block>_position], a variant over the nodes a block's parse builds: its
      roles, and its rule atoms.
    - [<rule>_<child>], a variant for a child with several symbols where at
      least one is a rule. A child of tokens alone reads as the token.
    - A module per variant, named after it, with [elem] for any arm and
      [syntax] where no arm is a token. A caller holding an [expr_position]
      gets its node without a match of its own.
    - A module per view, with [cast], [syntax] and the accessors.

    {1 What an accessor returns}

    A child that repeats gives a list, and any other child gives an option. A
    required child is an option too. Recovery can leave a placeholder where
    the parse found nothing, and the accessor reads that as [None].

    {1 Which child is which}

    The accessors do not count kinds. Each module sorts the node's children
    into its slots once, left to right, the way the parse filled them. A slot
    keeps taking children while it repeats, and otherwise the next child goes
    to the first later slot that admits it. A placeholder fills its slot. So
    [lhs] and [rhs] stay apart where both are expressions, and where one is
    missing.

    The emitted module links [Siesta] and nothing else.

    Takes the facts of a grammar the checks accepted. *)

(** Every type above, a [Slots] module of helpers, the variant modules, then
    the view modules. *)
val generate : Core.Facts.t -> Emit.item list

(** The same, without [Slots]. *)
val signature : Core.Facts.t -> Emit.sig_item list
