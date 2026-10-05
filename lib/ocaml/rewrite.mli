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

(** [generate ~views f] is the module. [views] names the views module, such
    as ["Json_views"].

    It also holds [probe], which the signature leaves out. [probe k slot
    ~node ~token] is the congruence for kind [k] with [node] and [token] given
    to the child at [slot], lifted to that child's type, or with no rules
    where [slot] is [None]. It gives [None] for a kind with no view and a
    slot the view does not have. It is here so a test can run every
    congruence without knowing which module holds it. *)
val generate : views:string -> Core.Facts.t -> Emit.item list

val signature : Core.Facts.t -> Emit.sig_item list
