(** What may appear next at a point in a tree.

    This is the residual, read off what a parse left behind. A grammar's
    tables come from its plan and the emitter writes them out, so nothing here
    reads a plan and nothing ships one. [Ir.Residual] derives the same sets
    from the plan itself, and is the specification these are checked against.

    A parser records no position as it runs. The work happens here, when
    something calls {!at}. *)

(** One position a parse can be at inside a node, with its set of kinds
    written out.
    Named here as well so generated code reaches it through this library
    alone. *)
type point = Ir.Residual.Table.point =
  { first : Ir.Kind.t array (** What may appear next here. *)
  ; may_end : bool (** Whether the node above carries on from here. *)
  ; on : (Ir.Kind.t array * int) array
    (** A child of one of these kinds moves to that point. *)
  }

type t =
  { points : point array array (** One automaton per node kind that has one. *)
  ; of_kind : int array
    (** Node kind to its automaton in {!t.points}, or [-1] where a node of
          that kind holds no parse position of its own. *)
  ; trivia : Ir.Kind.t array
  ; error_kind : Ir.Kind.t (** What a recovery swept its skipped input into. *)
  }

(** [at t root ~offset] is every kind that may appear at the first meaningful
    token at or after [offset], ascending and with no repeats.

    [\[||\]] where nothing may appear, which is what the end of a complete
    parse gives.

    The position it takes is a byte. A parse passes through several steps at
    one token and they admit different things; the one that counts is where the
    parse stood when it first reached that token, because nothing had yet
    branched on it.

    The walk reads the children of each node on the way down to
    the byte, so a node with many children is read through to reach one of
    them. Over a flat 129 KB document that is 0.1 ms at the start and 1.4 ms
    at the end. Making it the depth alone needs a child index
    from a byte offset, and a green tree carries none. *)
val at : t -> Siesta.Green.node -> offset:int -> Ir.Kind.t array
