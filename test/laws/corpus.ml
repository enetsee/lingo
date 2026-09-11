(** The grammars every law is quantified over.

    Six, chosen to cover the shapes the checks tell apart. A law reading
    zero here says nothing about a grammar whose shape is not among them.

    Two things widen it. More grammars — a right-associative operator table,
    a JSON-shaped one with committed productions and two
    delimited-with-separator bodies side by side. And a generator that
    samples grammars, or inputs to them, which reaches cases nobody thought
    of. Until one of those lands, a law's coverage is this list. *)

let all : (string * Core.Grammar.t) list = Lingo_witness.Witnesses.accepted
