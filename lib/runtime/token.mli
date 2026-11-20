(** A lexed token: its kind, and the bytes it matched.

    The lexer produces an array of these and the parser walks it. Every byte
    of the input belongs to exactly one token, trivia included, so the tree
    can rebuild the input. *)

type t =
  { kind : Ir.Kind.t
  ; text : string
  }

(** What a lexer says of one byte offset in a string, where the string is the
    start of a longer text.

    A lexer takes the longest token it can, so where one ends can turn on
    bytes far behind it. In a language whose comments are [(* ... *)], the
    bytes [(*] are two tokens until a [*)] follows them. A formatter writes
    its output a token at a time and has to know which boundaries the rest of
    the output can still move. *)
type boundary =
  | Absent (** No token of the string ends at the offset. *)
  | Open
  (** A token ends there. One that starts in front of the offset was still
        being read where the string ran out, so bytes behind the string can
        give it a longer token and take the boundary away. *)
  | Settled (** A token ends there, whatever follows the string. *)
