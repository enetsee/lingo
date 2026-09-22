(** Six edits to a drawn input.

    An edit is to the token list, and the bytes come back from
    {!Sample.decode}. Editing the bytes directly would put the corpus outside
    the sampler's own account of the grammar: the joiner between two tokens
    would be the mutator's guess rather than the lexer's answer, and a failing
    oracle would be reporting on a string no draw could have produced.

    A draw is a flat list and a rule is a span of it, so the tree is what says
    which tokens belong to which rule. Every mutator that works at a rule reads
    its spans off the parse of the input it was handed.

    {1 Reach}

    Every call goes through one site that writes the row: the attempt, and then
    either the firing or the reason for declining. So
    [attempts = fired + sum declines] holds on every mutator, and an exit path
    cannot be added that the counters miss.

    The reason is carried rather than only the count, because zero is often the
    right reading. A grammar with no repeated child has nothing for {!repeat}
    to do, and what tells that apart from a mutator that is broken is the
    reason printed beside the zero.

    Firing is not reaching. A mutator that fires on every call can still be
    building one arm of a four-arm slot: the other three are shapes no oracle
    was ever offered, the corpus still parses, and [fired] reads the same
    either way. {!arms} and {!sources} are the second reading, for the one
    mutator that has something to be blind to. *)

(** What a mutator was handed: the tokens, and the tree the parse of their
    decoding built. *)
type subject =
  { tokens : Sample.token list
  ; tree : Siesta.Green.node
  }

type outcome =
  | Fired of Sample.token list
  | Declined of string
  (** Why, in words a reach table groups by. One phrase per exit, so two
          unrelated reasons never share a count. *)

type reach =
  { mutator : string
  ; attempts : int
  ; fired : int
  ; declines : (string * int) list (** Most frequent first. *)
  }

type t

val create : Harness.t -> t

(** The six, in the order {!apply} tries them. *)
val names : string list

val regenerate : t -> Random.State.t -> subject -> outcome
val repeat : t -> Random.State.t -> subject -> outcome
val graft : t -> Random.State.t -> subject -> outcome
val swap_arm : t -> Random.State.t -> subject -> outcome
val corrupt : t -> Random.State.t -> subject -> outcome
val drop : t -> Random.State.t -> subject -> outcome

(** Each in turn from a random start, giving the first that fires and the name
    of the one that did.

    [None] where all six declined. That is the one skip class no reach row can
    be held to, because it is a total over them and cannot be attributed. *)
val apply : t -> Random.State.t -> subject -> (string * Sample.token list) option

(** Keep this subject's spans as donors for {!graft}. Admitting every input
    drawn gives the mutator nothing it did not already have; the caller admits
    the ones whose parse reached somewhere new, and a graft then carries
    forward structure the sampler would not draw on its own. *)
val admit : t -> subject -> unit

(** One row per mutator, in {!names} order, including any the run never
    reached. Those read [0] of [0] rather than being absent: a mutator missing
    from the table is invisible, and a mutator that declines every call costs
    no skip and no failure because the next one absorbs the iteration. *)
val reach : t -> reach list

(** Which arm {!swap_arm} put in, per swap, commonest first, spelled as the
    grammar spells it: ["Token ident"], ["Rule Block"]. An arm of an expression
    block's atom set carries the block's name in front of it, because that
    alternation is written on the block rather than at a child slot and a node
    can sit in both at once. *)
val arms : t -> (string * int) list

(** Every shape {!sources} can report, so a caller can hold the sweep to
    reaching all of them. *)
val shapes : string list

(** Which shape it took out, commonest first: a node or a token, at a child
    slot or at an atom position.

    The arm table is the same swap seen from the other end and says nothing
    about this one. A slot offers the same arms whatever is standing in it, so
    a walk that could see only nodes builds the same arms, declines nothing
    extra, and leaves a slot no node can occupy unreachable with every other
    counter reading as before. *)
val sources : t -> (string * int) list
