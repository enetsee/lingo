(** The phases that reason about a layout.

    A layout is the language-neutral description of a formatter. It lives in
    [lingo_runtime.ir], because the generator and the runtime both read it and
    neither owns it.

    {!Check} holds its invariants and {!Text} prints it. The lowering from a
    [Facts.t] joins them here.

    What walks a layout sits above: [lingo_runtime] folds one over a tree. So
    nothing here links a document engine.

    Each module is a phase. The layout itself is [Ir.Layout], so a reader
    always knows which of the two they are looking at. *)

module Check = Check
module Lower = Lower
module Text = Text
