(** The phases that reason about a plan.

    A plan is the language-neutral description of a parser. It lives in
    [lingo_runtime.ir], because the generator and the runtime both read it
    and neither owns it.

    {!Check} holds its invariants and {!Text} prints and reads it. The
    lowering from a [Facts.t] joins them here.

    What walks a plan sits above: [lingo.interp] runs one and [lingo.ocaml]
    turns one into source. So nothing here links a parse runtime.

    Each module is a phase. The plan itself is [Ir.Plan], so a reader always
    knows which of the two they are looking at. *)

module Check = Check
module Lower = Lower
module Messages = Messages
module Text = Text
