(** What may appear next, at a position in a plan.

    A plan says what a parser does. A {!State.t} says where in it a parse has
    got to, and {!at} gives the kinds the grammar admits there. That set is
    the left quotient of the grammar by what has been read, which is where the
    name comes from.

    {!at} gives what a well-formed continuation may start with. A recovering
    parser tolerates more: it reports a token no position admits, and carries
    on. So a kind {!at} leaves out is one a parse may still meet.

    A {!State.t} is built by a parse, because nothing else holds the position.
    The interpreter threads one down its walk and hands it to [Interp.run]'s
    [?at] wherever it reads the cursor, and the constructors below are what
    that walk needs.

    {!Table} is the other route, and the one that ships. It gives the same
    sets, worked out per rule and written out by the emitter, so a tree can
    be read against them with no plan in hand. *)

module State : sig
  (** A stack of frames, innermost first. A frame is a rule and a path into
      that rule's body.

      A path names a position in an expression as well as one in the
      instructions. {!operand} and {!climbing} are the two phases of a Pratt
      parse, and the binding power each carries is the level, so a block's
      states are the path itself. *)
  type t

  (** At that rule's body, with nothing under it. *)
  val enter : int -> t

  (** Into that rule's body, as a frame above [t]. *)
  val call : t -> int -> t

  (** Into the instruction at that index of a [Plan.Seq]. *)
  val item : t -> int -> t

  (** Into the arm at that index of a [Plan.Alt]. *)
  val arm : t -> int -> t

  (** Into a [Plan.Commit]'s body. *)
  val child : t -> t

  (** At a [Plan.Loop], in that state, with no element in hand. *)
  val loop : t -> int -> t

  (** Into what [state] emits. The loop carries on at [goto] once that is
      done. *)
  val emits : t -> state:int -> goto:int -> t

  (** At an expression parsed at that binding power, with the operand still
      to read. *)
  val operand : t -> int -> t

  (** At an expression parsed at that binding power, its left side read, so
      an operator that binds at least as tightly can extend it. *)
  val climbing : t -> int -> t

  (** Into the body of the block's postfix operator at that index. The lead
      token is already taken. *)
  val postfix : t -> int -> t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  (** The innermost frame on its own: the rule the parse is in, and the path
      inside that rule. Two states with the same one are at the same place in
      the same rule, whatever called it.

      Coverage is counted over this rather than over the whole stack. A stack
      grows with the input's nesting, so the number of distinct stacks climbs
      with depth however little of the plan a corpus reaches, and a count over
      them measures the corpus's brackets. The number of sites follows from the
      plan. *)
  val site : t -> t
end

(** A rule's points, as an automaton over the node's own children.

    Green children are what a tree can be stepped over. Every point carries
    its own set, so a walk over these needs no plan: an emitter writes them out
    and the walk reads them.

    Point 0 is where a parse of the rule starts. *)
module Table : sig
  type point =
    { first : Kind.t array (** What may appear next here. *)
    ; may_end : bool (** Whether the frame above carries on from here. *)
    ; on : (Kind.t array * int) array
      (** A child of one of these kinds moves to that point. The first entry
              holding the child's kind is the one to take, and a later entry
              steps over a child the tree does not hold. *)
    }

  (** Every node kind a tree can hold, with the points of a parse inside a
      node of that kind. An expression node comes from its block rather than
      from a rule, because a block rule builds no node of its own. *)
  val of_plan : Plan.t -> (Kind.t * point array) list
end

(** [at plan state] is every kind that may appear next, ascending and with no
    repeats.

    The innermost frame comes first. Where everything left in it can be
    left out, the frame above adds what follows it, and so on out to the
    root.

    Raises [Invalid_argument] where the state does not fit the plan. *)
val at : Plan.t -> State.t -> Kind.t array
