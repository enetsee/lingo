(** The example inputs the laws over a parse read, one entry per grammar.

    test/laws/law_interp.ml, test/laws/law_residual.ml and
    test/parse_emit/law_parse.ml all read these, so an input added here is an
    input all three see. *)

type t =
  { good : string list (** Input the grammar accepts. A parse of it reports nothing. *)
  ; broken : string list (** Input it does not accept, here to reach recovery. *)
  }

(** [all t] is [good] and then [broken]. A law that asks the same question at
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
