(** A code example, coloured.

    The example is lexed. The facts carry the token automaton, so turning
    source into tokens needs nothing else, and every token has a scope
    already. That is enough for a page: a reader picks out a keyword, a
    string, a number and a comment, and the lexer settles all four.

    Colouring by position is out of reach. Whether an identifier is a type
    name or a parameter name is a question about the tree, and there is no
    tree here. A page with those colours needs a parse, and a parse is a
    dependency this library does not take. *)

(** The source laid out with each token annotated by its scope. Lines are
    kept as the source had them. An example in a manual has already been
    formatted, and re-flowing it would undo that. *)
val example : Scopes.t -> string -> Render.document
