(** S-expressions, for the plan's text form.

    A plan has to print and read back, and an s-expression is the smallest
    thing that does both without an indentation-sensitive reader. The printer
    puts one field per line where a form does not fit, so a dump still reads
    as a tree and a golden still reads as a diff. *)

type t =
  | Atom of string
  | List of t list

(** A bare atom where the text needs no quoting, and a quoted one where it
    does, so a number prints as a number.

    Inside quotes, a backslash, a quote and the three whitespace characters
    take a named escape, and any other byte below 32 takes [\\ddd]. A byte
    above 127 goes out as it stands, so a name in UTF-8 reads as itself.
    {!of_string} takes exactly those and refuses anything else. *)
val pp : Format.formatter -> t -> unit

val to_string : t -> string

(** The one s-expression in [s], with nothing but whitespace around it. A
    [;] runs to the end of its line and is ignored. *)
val of_string : string -> (t, string) result
