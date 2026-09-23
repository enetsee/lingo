(** The example inputs the laws over a parse and over a format read, one entry
    per grammar.

    Four laws read these: test/laws/law_interp.ml, test/laws/law_residual.ml,
    test/parse_emit/law_parse.ml and test/format_emit/law_format.ml. Each lists
    the grammars it covers, so an input added to an entry is an input every law
    listing that grammar sees.

    Every law lists all ten. *)

type t =
  { good : string list (** Input the grammar accepts. A parse of it reports nothing. *)
  ; broken : string list (** Input it does not accept, here to reach recovery. *)
  }

(** [all t] is [good] and then [broken]. A law that runs the same check at
    every position of either reads this. *)
val all : t -> string list

val sexp : t
val json : t
val calc : t
val rassoc : t
val postfix : t
val unicode : t
val recovery : t
val shapes : t
val comments : t
val rust : t
val effekt : t
