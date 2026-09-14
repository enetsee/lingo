(** Node and token kinds, as the integers a tree records.

    A kind is an integer here. [lingo.core] does the numbering and hands a
    backend an abstract kind. The code that backend emits writes the integers
    it was given, so there is nothing here to keep abstract.

    Siesta records a kind on every node and token as an [int], and {!t} is
    that integer. *)

type t = int

(** No token. A cursor answers with this past the end of the input, so a
    dispatch over kinds needs no separate test for the end. *)
val none : t
