(** OCaml syntax, as values.

    A thin shell over [Ppxlib.Ast_builder] for putting together structures,
    expressions and patterns. It renders through [Pprintast], so nothing here
    indents anything, escapes a newline, or picks between [let] and [and] by
    editing a string.

    {1 What it does not carry}

    Nothing about a grammar, a plan or a kind. An emitter reads those and
    calls this, and the two stay apart so a backend that emits something other
    than OCaml shares everything above this and none of it.

    That is why a kind arrives here as {!eint}. [lingo.plan] numbers kinds and
    an emitter writes the numbers it was given.

    {1 What is here}

    What an emitter has needed. Adding to it when one needs more is a line;
    carrying a constructor nobody calls has cost this project more than that
    twice over. *)

type expr = Ppxlib.expression
type pat = Ppxlib.pattern
type item = Ppxlib.structure_item
type sig_item = Ppxlib.signature_item
type ty = Ppxlib.core_type
type case = Ppxlib.case

(** A lambda's argument. *)
type arg =
  | Plain of pat (** [fun p -> …] *)
  | Named of string (** [fun ~x -> …] *)
  | Named_pat of string * pat (** [fun ~x:p -> …] *)
  | Opt of string * expr option (** [fun ?x -> …], or [fun ?(x = d) -> …] *)

(** A dotted path, as OCaml writes it: ["Lingo_runtime.Cursor.bump"]. *)
val longident : string -> Ppxlib.longident

(** {1 Expressions} *)

val evar : string -> expr
val eint : int -> expr
val estr : string -> expr
val ebool : bool -> expr
val eunit : expr

(** [record.field]. *)
val efield : expr -> string -> expr

val earray : expr list -> expr
val elist : expr list -> expr

(** [{ a = x; b = y }]. *)
val erecord : (string * expr) list -> expr

(** A tuple. One element is that element, because OCaml has no one-tuple, and
    none is [()]. *)
val etuple : expr list -> expr

(** A constructor and its arguments. Several arguments become a tuple. *)
val econstruct : string -> expr list -> expr

val eapply : expr -> expr list -> expr

(** The same, where some arguments carry a label. *)
val eapply_labelled : expr -> (Ppxlib.arg_label * expr) list -> expr

(** [eapply (evar name) args], which is most calls. *)
val ecall : string -> expr list -> expr

(** [(e : t)], where the emitted code needs the type written down. An array
    of [None] forces it: nothing else in the emitted module gives the array a
    type, and a weak type variable does not compile. *)
val econstraint : expr -> ty -> expr

(** {2 Operators}

    [eand a b] is [a && b], [eequal a b] is [a = b], and so on. *)

val eand : left:expr -> right:expr -> expr
val eor : left:expr -> right:expr -> expr
val enot : expr -> expr
val eequal : left:expr -> right:expr -> expr
val enot_equal : left:expr -> right:expr -> expr
val eless : left:expr -> right:expr -> expr
val eless_equal : left:expr -> right:expr -> expr
val egreater : left:expr -> right:expr -> expr
val egreater_equal : left:expr -> right:expr -> expr

(** {2 Control} *)

(** [if c then a else b]. *)
val eif : condition:expr -> then_:expr -> else_:expr -> expr

(** [if c then a], where there is nothing to do otherwise. *)
val ewhen : condition:expr -> then_:expr -> expr

(** [a; b; c]. Right-nested, so the printer writes it without parentheses. *)
val eseq : expr list -> expr

val ewhile : condition:expr -> body:expr -> expr
val ematch : expr -> case list -> expr
val ecase : ?guard:expr -> pat -> expr -> case

(** [let name = body in rest]. *)
val elet : ?rec_:bool -> string -> body:expr -> rest:expr -> expr

(** [let rec f = … and g = … in rest]. An empty list gives the tail
    unchanged. *)
val elet_rec : (string * arg list * expr) list -> expr -> expr

(** {2 Functions} *)

val arg_var : string -> arg
val arg_any : arg

(** [(name : path)], for an argument the emitted code should fix to a type. *)
val arg_typed : arg_name:string -> type_path:string -> arg

val elambda : arg list -> expr -> expr

(** [fun () -> body], for a thunk a runtime helper takes. *)
val ethunk : expr -> expr

(** {1 Patterns} *)

val pvar : string -> pat
val pany : pat
val pint : int -> pat
val pstr : string -> pat
val pconstruct : string -> pat list -> pat
val pchar : char -> pat

(** [lo .. hi]. OCaml has interval patterns for characters and not for
    integers, so a dispatch over bytes matches on the character. The compiler
    turns a match over ranges into a decision tree. *)
val pchar_range : lo:char -> hi:char -> pat

(** A tuple, on the same terms as {!etuple}. *)
val ptuple : pat list -> pat

(** [p | q | r]. The head is separate so there is always one. Folding several
    literals into one arm is what puts the whole match in a jump table. *)
val por : pat -> pat list -> pat

(** {1 Structure items} *)

val ilet : ?rec_:bool -> ?args:arg list -> string -> expr -> item

(** [let rec f = … and g = …] at the top level. *)
val ilet_rec : (string * arg list * expr) list -> item

val iopen : string -> item
val imodule : string -> item list -> item

(** Each constructor is a name and its arguments; [[]] for a nullary one. *)
val itype_variant : string -> (string * ty list) list -> item

val itype_alias : string -> ty -> item
val itype_record : string -> (string * ty) list -> item

(** {1 Types} *)

val tcon : string -> ty list -> ty
val ttuple : ty list -> ty
val tarrow : domain:ty -> codomain:ty -> ty
val tarrow_labelled : string -> domain:ty -> codomain:ty -> ty
val tarrow_optional : string -> domain:ty -> codomain:ty -> ty

(** {1 Signature items} *)

val sval : string -> ty -> sig_item
val stype_variant : string -> (string * ty list) list -> sig_item
val stype_alias : string -> ty -> sig_item
val stype_record : string -> (string * ty) list -> sig_item
val stype_abstract : string -> sig_item
val smodule : string -> sig_item list -> sig_item

(** {1 Rendering}

    Both give valid OCaml source with the generated-file header on the
    front. The header is not a caller's to remember. *)

val render : item list -> string
val render_signature : sig_item list -> string
