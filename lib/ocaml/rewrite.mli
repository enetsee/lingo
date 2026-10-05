(** The rewrite support for a grammar, as OCaml source.

    One module per view, with the same name, so [Json_rewrite.Member] sits
    beside [Json_views.Member]. Each holds [congr], a congruence: it takes a
    rule per named child, as an optional argument labelled with the child's
    accessor name, and runs on nodes of the view's kind.

    A child's argument has the type of what the child holds:

    - a rule over nodes, {!Lingo_runtime.Rewrite.t}, where every symbol is a
      rule or a block;
    - a token rule, {!Lingo_runtime.Rewrite.Token.t}, where every symbol is a
      token;
    - an {!Lingo_runtime.Rewrite.Elem.t} where the child has both;
    - any of the three under {!Lingo_runtime.Rewrite.Elems.t} where the child
      repeats.

    The module finds a node's children with the views module's [Slots.slots],
    so it links [Siesta], [Lingo_runtime] and the views module.

    Takes the facts of a grammar the checks accepted. *)

(** {1 Constructors}

    Each view's module also holds [make]. It takes a
    {!Siesta.Cache.t}, then the production's children, and gives the node as
    a view over a cursor of its own, or the reason it could not build one.

    - A token with fixed text is written by [make]. Where it is optional,
      [make] takes a flag, and where it repeats, a count.
    - Any other token takes its text.
    - A rule takes its view, and a block its position.
    - A child with several symbols takes a polymorphic variant with a tag
      per symbol, named after it, as in [`Number "1"].
    - A repeated child takes a list, and an optional one an optional
      argument.
    - A delimited production gets its delimiters, and a separated body its
      separators. A separator goes in front of the first element or after the
      last where the policy is [Always].

    [make] writes no whitespace, and the formatter writes all of it.

    [?replacing] comes straight after the cache. It is the node the new one
    replaces, and [make] keeps the comments that sit between that node's
    children. See {!Lingo_runtime.Rewrite.Construct} for where each one goes.

    A lone argument that is always given is positional, and it lets a caller
    leave [?replacing] out. Every other is labelled with its child's name,
    and [unit] closes the list.

    [make] fails where a child that must repeat at least once is given none,
    and where a delimiter or separator has no fixed text to write. The text
    given for a token is not checked against the token's pattern.

    A block's roles have a [make] too. It puts an operand in the block's
    bracketing atom where the parse would otherwise take it apart. *)

(** {1 Parentheses}

    Each block has a module named after its position type, such as
    [Expr_position], with two functions.

    - [parens cache e] puts [e] in the block's bracketing atom: a delimited
      production whose one required child is the block, and one of its
      atoms. A block without one gives an error.
    - [needs_parens ~at node] holds where [node] needs parentheses to sit
      where [at] sits, as an operand of a role. Anywhere else it is false.

    An operand needs them where the parse would take it apart. That is read
    from the binding powers, the way the parser reads them. The operand of an
    operator on the left needs them where something open on the operand's
    right edge binds no tighter than the operator does. An operand on the
    right needs them where something on its left edge binds less tightly than
    the operator asks for. An expression that ends in a slot of a block, such
    as ml's [if c then a else b], takes in any operator after it, so it is
    open on the right at binding power 0. *)

(** [generate ~views f] is the module. [views] names the views module, such
    as ["Json_views"].

    It also holds two functions the signature leaves out, so a test can run
    every congruence and constructor without knowing which module holds it.

    - [probe k slot ~node ~token] is the congruence for kind [k] with [node]
      and [token] given to the child at [slot], lifted to that child's type,
      or with no rules where [slot] is [None]. It gives [None] for a kind
      with no view and a slot the view does not have.
    - [rebuild cache node] reads a node through its view and builds it
      again with [make], replacing [node].
    - [needs_parens ~at node] is every block's [needs_parens], so a test
      need not know which block an operand belongs to. It gives [None] for a node of any other
      kind. *)
val generate : views:string -> Core.Facts.t -> Emit.item list

val signature : views:string -> Core.Facts.t -> Emit.sig_item list
