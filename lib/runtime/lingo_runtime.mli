(** What lingo-generated code links.

    A generated parser calls into this library and into nothing else of
    ours. So the code generator, ppxlib and yojson stay out of a consumer's
    build graph.

    {v
      Token.t array
          |
      Cursor.create        the parse state: position, tree, diagnostics
          |
      Cursor . Build . Recover     what a generated parser calls
          |
      Build.finish         the root node and the diagnostics
    v}

    A kind is an integer here. [lingo.core] does the numbering, and generated
    code writes the integers it was given.

    {1 What belongs here}

    This library owns the token stream and the tree. The parse itself is
    emitted, so anything whose shape a grammar decides is written by the
    emitter with the kinds as constants.

    {!Cursor.create} taking the trivia kinds is not an exception to that.
    Trivia is a property of the token stream, and the kinds arrive once per
    parse as an array the cursor indexes. A grammar's delimiter pairs are a
    dispatch, and a dispatch is parse control flow.

    Two things follow. Generated code stays real code rather than a table
    walk. And the interpreter and the emitted parser stay separate
    implementations, so a differential test has two of them to compare. *)

(** Generated code names a kind on almost every line and a message wherever
    it reports something, so both are reachable from here rather than through
    [lingo_runtime.ir]. The plan is not: generated code never names one. *)

module Kind = Ir.Kind
module Message = Ir.Message
module Token = Token
module Diagnostic = Diagnostic
module Cursor = Cursor
module Build = Build
module Recover = Recover
module Ahead = Ahead
