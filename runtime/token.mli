(** A lexed token: its kind, and the bytes it matched.

    The lexer produces an array of these and the parser walks it. Every byte
    of the input belongs to exactly one token, trivia included, so the tree
    can rebuild the input. *)

type t =
  { kind : Kind.t
  ; text : string
  }
