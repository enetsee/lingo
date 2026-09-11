(** The names an author writes in a grammar.

    A grammar names three kinds of thing, and each sits in its own
    namespace. A rule and a token may share a name. A child name is scoped to
    the production that declares it.

    Bare strings put all three in one namespace. A lookup can then reach the
    wrong table and still compile. Look a token name up among the rules and
    the answer is "no production named lparen", which blames a name that was
    never missing.

    Each namespace is an abstract type over a string. The {!Grammar}
    constructors take string literals and tag them here. Every name a record
    carries then says which namespace it belongs to. *)

module type S = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

(** A production or an expression block. *)
module Rule : S

(** A token declaration. *)
module Token : S

(** A child slot, scoped to the production that declares it. *)
module Child : S
