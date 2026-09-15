(** The catalogue a plan's message ids index.

    A diagnostic carries an id and the text comes from here, so the wording
    changes without a rebuild and a translation is a second catalogue rather
    than a second parser.

    The lowering mints the ids and this at the same time. Nothing else can:
    the plan holds the ids and the facts hold the text, and the lowering is
    the only thing that sees both. *)

type t

(** In id order, so the id is the index. *)
val entries : t -> string array

val text : t -> Ir.Message.id -> string
val count : t -> int

(** A stable dump, one entry per line. *)
val pp : Format.formatter -> t -> unit

(** Minting, for the lowering alone. *)
module Builder : sig
  type catalogue := t
  type t

  val create : unit -> t

  (** The id for that text, minting one where the text is new. Two positions
      wanting the same words share an id, so a translator writes each form
      once. *)
  val intern : t -> string -> Ir.Message.id

  val finish : t -> catalogue
end
