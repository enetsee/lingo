(** Generated inputs, at a size you ask for.

    Every rule becomes a species of [token list] in a bolts system. The
    size unit is tokens: only a terminal is counted, so a sampler asked for
    200 draws 200 meaningful tokens. That is the unit the token-preservation
    law counts in, and the unit a width sweep measures against.

    {!system_of} builds the system and {!decode} turns a sample back into
    source text. Between them sits [bolts], which solves the system and draws
    from it.

    {1 What each construct becomes}

    - a keyword or a punctuation token is an [atom] carrying its own text,
      size 1;
    - a pattern token is a [prim], size 1, whose lexeme is an opaque draw;
    - a child is the sum of its symbols, under [option] where it is optional
      and [star] where it repeats;
    - a production is the product of its children inside its frame, with the
      frame's delimiters and separators in place;
    - a body carries a separator after its last element where the policy is
      not [Never], because that is the one policy under which the parser
      reports one;
    - an expression block is its operator table, stratified by binding power;
    - [Reformat] trivia is absent, because {!decode} writes the spacing;
    - [Preserve] trivia is a [gen0] of size 0. See {!system_of}'s [comments].

    A doc comment is none of those. It is a meaningful token declared in a
    production, so it is an ordinary [prim] of size 1 and it is counted.

    A pattern token is never expanded into the system. [[0-9]+] expanded is
    [10 z seq (10 z)], and numerals then swamp every other shape. One [prim]
    is one terminal, whatever the lexeme's length.

    A body with a separator reads [elem (sep elem)* sep?], and that is one
    derivation per token sequence rather than several. An element cannot begin
    with the separator, because {!Core.Facts.of_grammar} reports
    [first-follow-conflict] where it can, and an element cannot be empty
    either. So the separators between elements are settled by the tokens, and
    the one that may trail is the body's last token.

    {1 The expression block}

    A block's parse is a loop over binding powers, and the system is that
    loop written out. Two numbers say what the loop admits: the threshold it
    entered with, and the ceiling the step before it left behind.

    An expression at threshold [m] takes an atom, or a prefix operator and
    its operand. The tail that follows takes an operator whose left binding
    power is at least [m] and below the ceiling, then the body that operator
    takes, then the next tail.

    The ceiling is what makes the split unique. An infix operator's right
    operand is parsed at [r], and the loop carries [r] as its ceiling
    afterwards, so an operator that operand could have taken is out of reach
    of the loop above it. A prefix operand leaves its own binding power
    behind for the same reason. A postfix operator leaves no ceiling, and the
    loop goes back to admitting everything from [m] up.

    Both numbers come from the binding-power table, so the species count is
    bounded by the table's size. Every other production has passed the LL(1)
    checks and LL(1) implies unambiguous, so a block encoded this way leaves
    the whole system unambiguous. A sampler over it is uniform over the
    strings of a size rather than over derivations.

    {1 Lexemes}

    A pattern token's lexeme is a walk over the grammar's own automaton, and
    it stops at a state that accepts that token. So a lexeme lexes back as
    the token it stands for, and [ident = [a-z]+] cannot spell a keyword.

    Every draw goes through [Bolts.Source.Draw]. The lexeme is then part of
    the trace, so a replay rebuilds it and shrinking can reduce ["xR9qLm"] to
    ["a"]. A generator reaching for the raw PRNG would replay the structure
    and lose the bytes. *)

(** A sampled token: the kind it stands for, and the bytes it spells. *)
type token =
  { kind : Core.Kind.t
  ; text : string
  }

(** The translation's own record: the nonterminal each rule became, and the
    automaton {!decode} reads.

    Build it once per grammar. A sweep that builds it per iteration pays the
    automaton's cost a million times. *)
type map

(** The system, and the map over it.

    [comments] is the chance of a [Preserve] trivia token at a token
    boundary, and it defaults to [0.]. At [0.] no [gen0] is built at all, so
    the system is the meaningful tokens alone and a count over it is a count
    of them. Above [0.] the count is still of meaningful tokens, and the
    bytes a sample carries are more than that.

    Raises [Invalid_argument] where two rules share a name, and where a
    symbol is neither a token nor a rule. A {!Core.Facts.t} has been checked,
    so neither can happen. They raise rather than give nothing back, because
    giving nothing back would build a system quietly not the grammar. *)
val system_of : ?comments:float -> Core.Facts.t -> Bolts.system * map

(** The nonterminal a rule became. [None] for an expression block's roles: a
    role describes the shape of a node the block's parse builds, nothing
    references it, and the block's operator table already says what the parse
    accepts. *)
val nonterminal : map -> Core.Rule.id -> token list Bolts.nonterminal option

(** The tokens as source text.

    The joiner between two of them is nothing, a space, or a newline,
    whichever is the first of the three that leaves the lexer a boundary
    where they meet. That is the rule the formatter's fold takes, over the
    same automaton.

    The left-hand side of each test is every byte written since the last
    joiner, less the prefix of it that later bytes can no longer change. No
    pair of ["7"], ["."] and ["1"] joins into one lexeme; all three do, and
    that is why the test is a run rather than the token before it.

    A blank inside a lexeme is not a joiner and does not end the run. A line
    comment holding a tab still needs a line break after it.

    Raises [Invalid_argument] where the map came from other facts. The map
    holds the automaton, and another grammar's automaton would join these
    tokens by another grammar's rules without saying so.

    Linear in the output. Dropping the settled prefix is what makes it so:
    without it a document needing no joiner keeps all of itself, and 4,000
    tokens of nested json cost 350 ms rather than 5. *)
val decode : Core.Facts.t -> map -> token list -> string
