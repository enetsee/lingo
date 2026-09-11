(** An expression block's operator table. It holds the binding powers, the
    associativities, and the dispatch cascade over atoms. Every name in it is
    resolved to a {!Kind.t}.

    A parser backend reads the table from here. Every operator token resolves
    at the {!Facts} boundary, along with every other name.

    This is the table a parse runs on. The shape of the nodes a parse
    produces lives in the desugared rules instead. See {!Rule.type-origin}. *)

(** An index into {!Facts.blocks}. *)
type id

val id_of_int : int -> id

type op =
  { op_kind : Kind.t
  ; bp : int
  ; assoc : Grammar.assoc
  }

(** {!Grammar.postfix_body} with its symbols resolved.

    An [Enclosed] postfix carries an open token, a close token and a
    separator. So does a production's [Delimited] frame. They are the same
    three things, so both lower through one path. *)
type body =
  | Nothing
  | Then of Kind.t array
  | Enclosed of
      { close : Kind.t
      ; content : content
      }

and content =
  | One of Kind.t array
  | Many of
      { elem : Kind.t array
      ; sep : Rule.sep option
      }

type postfix =
  { p_lead : Kind.t
  ; p_bp : int
  ; p_body : body
  ; p_rule : Rule.id (** The rule describing the node this builds. *)
  }

type def =
  { id : id
  ; rule_id : Rule.id (** The rule other rules reference. *)
  ; kind : Kind.t
  ; hole_kind : Kind.t
  ; name : Grammar.Name.Rule.t
  ; atoms : Kind.t array
    (** In declaration order. Dispatch over them is a cascade, so the order
          decides which atom wins a shared leading token. *)
  ; prefix : op array
  ; infix : op array
  ; postfix : postfix array
  ; infix_recovery : bool
  ; format : Grammar.expr_format
  }

(** The binding-power pair an associativity stands for. [Left] gives
    [(bp, bp + 1)] and [Right] gives [(bp, bp)]. A parser then needs only a
    [left_bp >= min_bp] test to tell them apart. *)
val op_bps : op -> int * int
