(** The identity of a diagnostic's wording.

    A diagnostic carries an id, and the text comes from a catalogue that is
    read at run time. So the wording changes without a rebuild, and a
    translation is a second catalogue rather than a second parser.

    An id means nothing on its own. It is an index into the catalogue the
    same grammar produced. *)

type id

val of_int : int -> id
val to_int : id -> int
val equal : id -> id -> bool
val compare : id -> id -> int
val pp : Format.formatter -> id -> unit
