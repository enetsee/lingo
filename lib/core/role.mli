(** The shapes an expression block desugars into.

    A block becomes one rule per role. There is one for the atoms, one for
    an infix application, one for a prefix application, and one per postfix
    operator. The staged derivation builds those rules, and
    {!Rule.type-origin} names the role each rule came from.

    So this module fixes how many rules a block turns into, and what each one
    is called.

    Every role is declared, whether or not the block uses it. {!is_active}
    says which ones a parse can produce. An inactive role still has a kind
    constant, and no rule. *)

type t =
  | Base (** The token atoms. *)
  | Bin (** An infix application. *)
  | Prefix (** A prefix application. *)
  | Postfix of int (** An index into {!Grammar.expr_def.postfix}. *)

val equal : t -> t -> bool

(** Every role the block declares, in kind-numbering order. *)
val of_block : Grammar.expr_def -> t list

(** Whether the parser emits a node of this role's kind. *)
val is_active : Grammar.expr_def -> t -> bool

(** {1 The names a role gives}

    Each name has one definition here. A check and a backend therefore look
    at the same string.

    {!kind_name} is the odd one out. It leaves off the [N_] prefix, and
    {!Manifest.Kind_name.of_role} adds it. The other two are used as they
    stand. *)

(** For example ["EXPR_BIN"], ["EXPR_POSTFIX_CALL"]. *)
val kind_name : Grammar.expr_def -> t -> string

(** The PascalCase name, for example ["ExprBin"]. *)
val synthetic_name : Grammar.expr_def -> t -> string

(** The snake-case tail of the formatter function's name. The emitted
    function is ["format_"] followed by this. *)
val format_fn : Grammar.expr_def -> t -> string
