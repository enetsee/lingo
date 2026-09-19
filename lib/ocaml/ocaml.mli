(** The OCaml backend.

    Everything that turns facts and a plan into OCaml source. {!Emit} is the
    syntax, and the emitters above it read the facts and call it.

    It is the only library here that links ppxlib, which is what keeps a code
    generator out of a consumer's build graph. *)

module Emit = Emit
module Formatter = Formatter
module Lexer = Lexer
module Parser = Parser
module Residual = Residual
