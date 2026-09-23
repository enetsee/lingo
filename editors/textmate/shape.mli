(** Which shape a rule emits as, derived from its framing.

    A TextMate grammar has three ways to say something, and a rule takes one
    of them:

    - a [begin]/[end] pair, which opens a region and scopes what is inside
      it;
    - a single [match] with numbered capture groups;
    - a list of patterns, each tried at every position.

    A matched pair and a production that contains its own errors both take
    the first, and their anchors come from different places, so {!t} has
    four arms for the three shapes.

    {2 Where the region comes from}

    The predecessor took one call per production to declare a region, with
    the anchor regexes written out. It had no choice. Its grammar type said
    nothing about which productions contain their own errors.

    lingo's grammar type does. {!Core.Rule.type-frame} is [Delimited] where a
    matched pair surrounds the body, and [Committed] where the production
    contains its own errors, which is the same claim a [begin]/[end] pair
    makes about highlighting. So the region falls out of the framing, and
    the anchors fall out of the first and last children. Nothing is
    configured.

    {2 What that costs}

    A [Committed] region ends on a lookbehind past its last child. The
    engine evaluates that anchor only at positions in the region's own
    context, so a nested region's closer leaves the outer one open. A closer
    the body can reach {e outside} any nested region ends it. A grammar
    whose terminator appears loose inside its own body ends its region
    early. The predecessor documented the same hazard with the anchor
    written by hand, and deriving the anchor carries it over unchanged. *)

(** One group of a single-regex emission. *)
type part =
  { regex : string
  ; scope : Scopes.Scope.t option
  }

type t =
  | Delimited of
      { open_ : Core.Token.def
      ; close : Core.Token.def
      ; sep : Core.Token.def option
      } (** A matched pair around the body, from {!Core.Rule.Delimited}. *)
  | Region of
      { begin_regex : string
      ; begin_captures : (int * Scopes.Scope.t) list
      ; end_regex : string
      ; body_from : int (** Children the [begin] regex already took. *)
      ; use_identity : bool
        (** [false] where the rule names itself by a child the [begin] could
            not fold in. The identity scope then stays out of the body: a
            body pattern is tried at every position, so [entity.name.function]
            on a loose child colours every identifier in the region. *)
      }
  | Capture of part list (** A single [match], one capture group per child. *)
  | Flat (** A list of patterns, one per child. *)

val of_rule : Core.Facts.t -> Scopes.t -> Core.Rule.def -> t

(** The rule this one forwards to, where its body is nothing but a reference
    to that rule.

    A production that wraps one other production and adds nothing emits a
    repository entry holding a single [include]. Dropping the entry and
    rewriting the references to it is one less hop for a reader of the JSON,
    and changes no scope.

    [raw] names the rules that carry a hand-written pattern. One of those is
    never a forwarder, whatever its children look like. *)
val forwards_to
  :  Core.Facts.t
  -> Scopes.t
  -> raw:(Core.Rule.id -> bool)
  -> Core.Rule.def
  -> Core.Rule.id option

(** {1 Pieces the emitter shares}

    Both the shape decision and the emission need these. Deriving them twice
    would let the two disagree. *)

(** The token a child position holds, where it holds exactly one. *)
val token_of_child : Core.Facts.t -> Core.Rule.child -> Core.Token.def option

(** The rule a child position holds, where it holds exactly one. *)
val rule_of_child : Core.Facts.t -> Core.Rule.child -> Core.Rule.def option

(** Where a chain of single-child forwarding rules ends, as the leaf rule,
    the index of its child, and the token there.

    [Field.ty] points at [Type], whose one child is an [ident]. Emitting
    [Field] as a single regex needs that [ident]'s regex, and scoping it
    needs the position it sits in, which is [Type]'s and not [Field]'s. *)
val leaf_token
  :  Core.Facts.t
  -> Core.Rule.def
  -> (Core.Rule.id * int * Core.Token.def) option
