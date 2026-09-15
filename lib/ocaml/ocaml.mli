(** The OCaml backend.

    Everything that turns a plan into OCaml source. {!Emit} is the syntax, and
    the emitters above it read a plan and call it.

    It is the only library here that links ppxlib, which is what keeps a code
    generator out of a consumer's build graph. *)

module Emit = Emit
