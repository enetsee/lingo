(** The names the emitted grammar uses for its own nodes.

    A tree-sitter node name is a lowercase identifier, and a query matches
    on it: [(struct_body) @fold] names the rule called [struct_body]. So
    these names are part of the interface, and they are the author's names
    with one mangling applied.

    A rule and a token share this namespace, because a pattern token becomes
    a rule of its own so that a query can name it. Two that collide are a
    {!Check.problem}. *)

val of_rule : Core.Grammar.Name.Rule.t -> string
val of_token : Core.Grammar.Name.Token.t -> string

(** An expression block is two nodes here, and this is the second.

    lingo's own tree has the block's kind on a token atom and the role's
    kind on everything else, so a rule referring to an expression takes
    either. That is a choice between node types, and tree-sitter writes one
    as a rule. A leading underscore makes the rule hidden, so the choice
    adds no level to the tree and the shape stays lingo's.

    A hidden rule cannot be named in a query. The block's own node can, and
    an author scopes that one. *)
val dispatch : Core.Grammar.Name.Rule.t -> string

(** The rule tree-sitter starts at, where the grammar declares more than one
    root and one has to be made up. *)
val start : string
