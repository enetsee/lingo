(** Where one token takes two scopes in the same place.

    A TextMate engine knows nothing of the grammar's positions. It tries a
    list of patterns at each point of the text and takes the first that
    matches. A per-position scope is therefore a claim on the token wherever
    its list is tried, and a list that is spliced reaches further than the
    rule it came from.

    In json, [Member.key] is a string scoped as a property name. [Member] is
    spliced into the object's region, and so is [Value], whose strings are
    plain strings. Both are strings, and the property pattern comes first,
    so every string in an object comes out as a key.

    The order cuts the other way too. An expression block ends in the
    grammar-wide token list, so a block spliced into a place puts every
    token there, in its own scope, ahead of whatever follows it. In wide,
    [Table.name] comes after the select list's expressions, and the table
    name came out as a plain variable.

    Two positions clash where their tokens can match the same text, their
    scopes differ, and both are tried in the same place. Each clashing
    position with a scope of its own is tied to a literal beside it, and the
    pair goes at the front of every place that tries the position, ahead of
    anything that could take its text first. One with no such literal is
    reported.

    A rule's name in a list is tied down the same way wherever it can be. A
    name that cannot is left in its token's scope and not reported. It is
    what the author gets for a name, and a name is scoped by default. *)

(** A place: a list the engine tries as a whole. *)
type place =
  | Root
  | Body of Core.Rule.id (** The body of a region or a matched pair. *)
  | Enclosed of Core.Rule.id
  (** The body of an enclosed postfix operator, by
        {!Core.Block.postfix.p_rule}. *)

(** A literal that sits beside a position on every occurrence of it. *)
type anchor =
  | Before of
      { lead : Core.Token.def
      ; lead_scope : Scopes.Scope.t option
        (** The literal's scope where it sits, which may be a position of
            the rule that refers to this one. *)
      }
  | After of Core.Token.def

(** A position tied to a literal. A position whose rule is referred to from
    several places, each after a different literal, has a pair for each. *)
type pair =
  { rule : Core.Rule.id
  ; child : int (** An index into {!Core.Rule.def.children}. *)
  ; token : Core.Token.def
  ; scope : Scopes.Scope.t
  ; anchor : anchor
  }

(** A clashing position that no literal can tie down. *)
type unanchored =
  { rule : Core.Rule.id
  ; child : int
  ; token : Core.Token.id
  ; against : (Core.Rule.id * int) option
    (** The position it clashes with. [None] for an operator, an atom of an
        expression block, or the grammar-wide token list. *)
  }

type t

(** Finds every clash in a grammar. [raw] names the rules that carry a
    hand-written pattern, as in {!Shape.forwards_to}. *)
val of_scopes : Scopes.t -> raw:(Core.Rule.id -> bool) -> t

(** Whether a position is carried by pairs. Its own list then emits nothing
    for it. *)
val is_paired : t -> rule:Core.Rule.id -> child:int -> token:Core.Token.id -> bool

(** The pairs a place puts ahead of everything else, in a stable order. *)
val pairs : t -> place -> pair list

(** Every clashing position with no pair, sorted. *)
val unanchored : t -> unanchored list
