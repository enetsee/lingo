(** What a parse reports about input it could not read.

    A diagnostic is anchored by a byte range over the input, so a consumer
    places one without walking the tree. It carries a {!Ir.Message.id} and not
    the text. {!Message} says why.

    A recovery node resolves to its diagnostic in one step.
    {!Cursor.report_id} gives a 1-based id, the parser stamps that id
    on the node as its payload, and the id indexes the list {!Build.finish}
    returns. *)

(** A child the parser could not fill.

    [expected_kinds] is what would have satisfied the position. A diagnostic
    is read once, so a list serves and a set would cost the order. *)
type missing =
  { at_child : string option
    (** The child's name, where the parser was filling a named position.
          This is a string because the runtime holds no name types. *)
  ; expected : Ir.Message.id
  ; expected_kinds : Ir.Kind.t list
  ; hole_kind : Ir.Kind.t option
    (** The typed hole this gap lowers to, where the grammar declares
          one. *)
  }

type kind =
  | Missing of missing
  | Extra of Ir.Message.id (** Input the parse read and the grammar has no place for. *)
  | Unexpected (** Input a recovery skipped. *)

type t =
  { range : int * int (** Byte offsets into the input, half open. *)
  ; kind : kind
  }

val pp : Format.formatter -> t -> unit
val pp_list : Format.formatter -> t list -> unit
