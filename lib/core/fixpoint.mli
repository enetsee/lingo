(** FIRST, FOLLOW, nullability and minimum size, over integers.

    Every backend reads these, so they are computed once, into arrays
    indexed by {!Rule.type-id}.

    - [nullable.(r)] is whether the rule can derive an empty token sequence.
    - [first.(r)] holds the token kinds a non-empty derivation of [r] can
      start with.
    - [follow.(r)] holds the token kinds that can come immediately after a
      complete [r], across every site that references it.
    - [min_size.(r)] is the fewest tokens [r] derives, and [max_int] where it
      derives nothing.
    - [enclosing.(r)] holds the closers and separators of every frame [r] can
      sit inside, across every site that references it.

    FIRST is exact, and so is the minimum size. FIRST holds what the rule's
    parser consumes and nothing else. The minimum is what the sampler's
    system carries for the same rule, derived from the species instead, and
    the two have a law that says they agree.

    The other three may be larger than they need to be. Each owes a
    one-sided law:

    - FOLLOW has to hold everything that can follow. It is the base of the
      recovery set, and a token missing from it is a token recovery skips
      past.
    - [enclosing] has to hold every closer of every frame the rule can sit
      inside, for the same reason.
    - Nullability has to hold every rule that can derive empty.

    A set that is too large costs a stop that was not needed. A set that is
    too small loses a diagnostic, or worse.

    {2 Expression blocks}

    A block's FIRST is its atoms' FIRST plus its prefix operator tokens. Its
    minimum is its smallest atom, because every operator adds its own token
    to an operand. A block is never nullable, because an expression
    takes at least one atom or one prefix operator. That is assumed here rather than derived, and
    {!Check_full} rejects the nullable atom that would break it.

    A block's FOLLOW takes contributions from its own operator table. An
    expression sits immediately left of every infix token and every postfix
    lead. Inside an enclosed postfix it also sits left of that form's
    separator and close. Prefix tokens come before an expression, so they
    belong to FIRST.

    A desugared role rule takes no part in the walk. Its FIRST follows from
    its children. Its FOLLOW and its enclosing frames are copied from the
    block.

    {2 Enclosing frames}

    FOLLOW says what may come after a complete rule. [enclosing] holds the
    delimiters of the frames still open around it. Part-way through a rule
    those are different sets, so one cannot be read off the other.

    [enclosing] keeps a recovery inside a nested rule from eating the closer
    an outer frame is waiting for. It has to hold every such closer. *)

type tables =
  { first : Kind.Set.t array
  ; follow : Kind.Set.t array
  ; nullable : bool array
  ; min_size : int array
  ; enclosing : Kind.Set.t array
  }

val compute
  :  rules:Rule.def array
  -> blocks:Block.def array
  -> kind_rule:int array
       (** Kind to {!Rule.type-id}, or [-1] where the kind is a terminal. A
           terminal's FIRST is itself. *)
  -> tables

(** What the walk reads while building the tables, read once they are built.
    A check over a finished {!tables} needs no copy of its own. *)
module Reader : sig
  type t

  val of_tables
    :  rules:Rule.def array
    -> blocks:Block.def array
    -> kind_rule:int array
    -> tables
    -> t

  (** {!Rule.type-id} of a kind, or [-1] where the kind is a terminal. *)
  val rule_of_kind : t -> Kind.t -> int

  val kind_nullable : t -> Kind.t -> bool
  val child_nullable : t -> Rule.child -> bool
  val kind_first : t -> Kind.t -> Kind.Set.t
  val alts_first : t -> Rule.child -> Kind.Set.t

  (** FIRST of every suffix of a child sequence, and whether each suffix can
      pass without consuming. Index [k] describes the children from [k];
      index [len] is the empty suffix, which passes. *)
  val suffix_first : t -> Rule.child array -> Kind.Set.t array * bool array
end
