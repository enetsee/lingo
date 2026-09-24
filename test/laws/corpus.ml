(** The grammars every law is quantified over.

    Ten, chosen to cover the shapes the checks separate. A law reading
    zero here says nothing about a grammar whose shape is not among them.

    Five are built in {!Lingo_witness.Witnesses}, each taking the shape
    nearest to one rejection and stopping short of it. The other five come
    from grammars/: sexp, calc, rassoc, json and ml. They carry a
    right-associative operator table, a committed production, and two
    delimited-with-separator bodies side by side, which the built ones do
    not. ml carries the settings the rest of grammars/ leaves at their
    defaults: two expression blocks, two roots, a leading operator position,
    a non-default continuation indent and a hand-written Oniguruma form.

    What would widen it further is a generator that samples grammars, or
    inputs to them, and so reaches cases nobody thought of. Until that lands,
    a law's coverage is this list. *)

let all : (string * Core.Grammar.t) list = Lingo_witness.Witnesses.accepted
