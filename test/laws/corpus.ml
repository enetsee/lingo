(** The grammars every law is quantified over.

    Nine, chosen to cover the shapes the checks tell apart. A law reading
    zero here says nothing about a grammar whose shape is not among them.

    Five are built in {!Lingo_witness.Witnesses}, each taking the shape
    nearest to one rejection and stopping short of it. The other four come
    from grammars/ and are the first four rungs of the example ladder: sexp,
    calc, rassoc and json. Those four carry a right-associative operator
    table, a committed production, and two delimited-with-separator bodies
    side by side, which the built ones do not.

    What would widen it further is a generator that samples grammars, or
    inputs to them, and so reaches cases nobody thought of. Until that lands,
    a law's coverage is this list. *)

let all : (string * Core.Grammar.t) list = Lingo_witness.Witnesses.accepted
