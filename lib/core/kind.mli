(** Node and token kinds.

    {!Facts} numbers every name in a grammar that becomes a kind. That means
    productions, tokens, expression blocks and each of their roles, holes,
    and the built-ins.

    [t] is abstract, so every kind a backend holds came out of a {!Facts.t}. 
    {!to_int} reads it. *)

type t

val to_int : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

(** Prints the integer representation. For a name, see {!Table.name}. *)
val pp : Format.formatter -> t -> unit

(** A set of kinds, as a bitset. Four things in this library are one: FIRST,
    FOLLOW, the recovery set at a position, and the kinds a child slot admits. 
    A backend adds a fifth, the guard on a dispatch arm.

    A set carries no count of the kinds that exist. It holds which bits are
    set and nothing else. {!Set.empty} takes no size argument, and a binary
    operation pads the shorter side. *)
module Set : sig
  (** A kind. Named here because [Set.t] shadows it. *)
  type elt = t

  type t

  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val of_list : elt list -> t
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val mem : t -> elt -> bool
  val union : t -> t -> t
  val unions : t list -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val cardinal : t -> int

  (** In ascending order. Two callers that turn one set into a list get the
      same list, so an emitted dispatch table follows from the set alone. *)
  val elements : t -> elt list

  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val exists : (elt -> bool) -> t -> bool
  val for_all : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t

  (** Prints the integers. For names, see {!Table.pp_set}. *)
  val pp : Format.formatter -> t -> unit
end

(** The name a kind is looked up by: [N_FOO], [T_BAR], [N_FOO_HOLE].

    The type is abstract, and the constructors below are the only way to make 
    one. So the spelling has a single definition.

    Without that, two modules would each write the affixes out, and they would 
    have to agree. If they ever disagreed, every kind would resolve to [N_ERROR] 
    and nothing would be reported.

    A kind name is derived from a name the author wrote. Nobody writes one
    directly. That is why it lives here, beside the kind it indexes, and not
    with the names a grammar carries. *)
module Name : sig
  type t

  val to_string : t -> string

  (** The kind of a production, a block, or a block's role. [node "FooBar"]
      is [N_FOOBAR]. *)
  val node : string -> t

  (** [token "lparen"] is [T_LPAREN]. *)
  val token : string -> t

  (** [hole (node "Foo")] is [N_FOO_HOLE]. *)
  val hole : t -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit

  (** {2 The built-ins}

      Every grammar has all four, so they are always in a table. *)

  val error : t
  val missing : t
  val error_token : t
  val unterminated : t
end

(** The name-to-integer map. {!Facts} builds one from the list of kind names a
    a grammar defines, and keeps the order it is given. So the numbering
    follows from the grammar. *)
module Table : sig
  (** A kind. Named here because [Table.t] shadows it. *)
  type kind = t

  type t

  (** Total on any list, duplicates included.

      A duplicate is still two indices. {!find} gives the first of them, and
      {!val-name} gives a name for each index separately, so the table stays a
      bijection on indices. A check reports the duplicate as
      [dup-kind-name]. *)
  val of_names : Name.t list -> t

  val count : t -> int
  val name : t -> kind -> Name.t
  val find : t -> Name.t -> kind option
  val mem_name : t -> Name.t -> bool
  val kinds : t -> kind list
  val names : t -> Name.t list

  (** {!Set.pp} with names in place of numbers. Use it for a diagnostic, or
      for a golden test. *)
  val pp_set : t -> Format.formatter -> Set.t -> unit
end
