(** The checks that read declarations and the kind table.

    Everything here reads two things: the names the author wrote, and the
    manifest of names a backend emits from them. Nothing here uses a resolved
    reference. Resolution is what half of these checks are for.

    Three checks look as though they belong at a later stage. Mixed operator
    roles, associativity consistency and duplicate operator tokens each
    compare tokens within one block. A token name is a declaration, so they
    can answer here.

    A nullable token is a question about that token's regex. The lexer
    automaton arrives two stages later and takes no part. So the question is
    asked here. A grammar whose lexer would loop at every position is then
    reported before two more stages of derivation run on it.

    [invalid-token-literal] keeps the checker total. A keyword or punctuation
    literal becomes a regex by decoding it as UTF-8, and malformed bytes have
    no decoding. This check reports them, which is why {!Facts.of_grammar}
    can answer with a [result]. *)

val run : Stage.names -> Error.t list
