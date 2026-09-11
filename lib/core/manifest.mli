(** Every identifier a backend emits for a grammar, with the manglings
    already applied. A collision is two entries that share a {!Scope.t} and
    an emitted string.

    {2 The namespaces}

    There are six, listed at {!Scope.t}:

    - the kind constructors;
    - the parser's [let rec] cluster;
    - the parser's top-level bindings;
    - the formatter's cluster;
    - the view modules;
    - the types and accessors those modules mint.

    A backend emits a handful of names for every grammar: [format_node],
    [format_generic], [parse_tokens] and the four built-in kinds. Each is an
    entry here, with {!builtin} as its {!entry.base}. A user name that
    reaches one collides like any other, and the diagnostic says what it hit.

    {2 The emission this describes}

    This describes the widest emission a backend can produce. Every
    production gets a [__root] variant and an entry point, whether or not a
    caller asks for them. So a grammar that passes here is safe under any
    emission the toolkit makes.

    That is why a grammar with both an [Expr] root and an [Expr_Root]
    production is rejected. *)

(** An identifier a backend emits. The type is abstract, so one is built
    here and read through {!to_string}. *)
type name

val to_string : name -> string

(** Where an emitted name lands. Two entries collide when they share both a
    scope and a string. Two in different scopes have nothing to do with each
    other. *)
module Scope : sig
  type t =
    | Kind_enum (** The [type kind] constructors. *)
    | Parser_cluster (** The parser module's single [let rec] group. *)
    | Parser_toplevel (** Separate top-level [let]s beside that group. *)
    | Format_cluster (** The formatter's [let rec] group. *)
    | View_module (** Module names in the shared module. *)
    | View_type (** Types at the shared module's top level. *)
    | View_accessor of string (** Accessors inside one named view module. *)

  val name : t -> string

  (** Whether a duplicate in this scope stops the emitted code compiling.

      A duplicate in {!Parser_toplevel} shadows instead, so it compiles.
      Those entries are still listed here, so that a cross-check against the
      emitted text can classify every name it finds. *)
  val fails_to_compile : t -> bool
end

(** The {!entry.base} given to a name that a backend emits for every
    grammar, rather than one derived from a declaration. *)
val builtin : string

type entry =
  { emitted : name
  ; scope : Scope.t
  ; base : string (** The user name behind it, or {!builtin}. *)
  ; derivation : string
    (** The name of the derivation that built it, such as ["parse_fn"]. The
          diagnostic prints it. *)
  }

type t

(** Total on any grammar. It applies the manglings and enumerates the
    results. It resolves nothing, so a grammar full of dangling names still
    has a manifest, and the collision checks run alongside the reference
    checks. *)
val of_grammar : Grammar.t -> t

val entries : t -> entry list

(** The raw kind names, [N_FOO], [T_BAR], [N_FOO_HOLE], in the order kind
    integers are assigned. That order is every production, then every token,
    then every block role, then every hole, then the built-ins.

    {!Kind.Table.of_names} takes this list. A name that appears twice is
    [dup-kind-name]. *)
val kind_names : t -> Kind.Name.t list

(** {1 Collisions} *)

type collision =
  { c_scope : Scope.t
  ; c_emitted : string
  ; c_entries : entry list (** Two or more, in declaration order. *)
  }

(** Every scope-and-string pair that more than one entry shares, in a fixed
    order. A check turns each one into a code. It picks the code from the
    scope, and from whether any of the entries is a built-in. *)
val collisions : t -> collision list

(** The kind name each kind of declaration mints. {!Kind.Name} owns the
    spelling. This says which declaration reaches which constructor.

    Both {!kind_names} and the staged derivation's lookups come through here,
    so the two can never disagree. *)
module Kind_name : sig
  val of_production : Grammar.production -> Kind.Name.t
  val of_token : Grammar.token_def -> Kind.Name.t

  (** For example [N_EXPR_BIN], [N_EXPR_POSTFIX_CALL]. *)
  val of_role : Grammar.expr_def -> Role.t -> Kind.Name.t

  (** Only a production with {!Grammar.production.has_hole} mints one. *)
  val hole_of_production : Grammar.production -> Kind.Name.t

  (** Every block mints one. *)
  val hole_of_block : Grammar.expr_def -> Kind.Name.t
end

(** {1 Name derivations}

    One definition each, so the name a check looks at is the name that gets
    emitted. *)

val parse_fn : string -> string
val root_parse_fn : string -> string
val can_start_fn : string -> string
val pratt_lhs_fn : string -> string
val pratt_infix_fn : string -> string
val pratt_infix_bp_fn : string -> string
val pratt_prefix_bp_fn : string -> string
val entry_point_fn : string -> string
val kind_constructor : string -> string
val format_fn : string -> string
val view_module : string -> string
val view_accessor : string -> string
val sum_type : prod:string -> child:string -> string
val block_position_type : string -> string
val block_entry_type : string -> string
