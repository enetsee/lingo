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

    A [Plain] rule is a region too, where it opens on a literal, ends on
    one, and cannot be a single match. Its alternative is a list of
    patterns, and a list is spliced into whatever includes it. Each pattern
    in it is then tried far outside the rule. A declaration's name pattern
    would colour every identifier in the file. A region keeps the name to
    its [begin].

    {2 What that costs}

    A [Committed] region ends on a lookbehind past its last child. The
    engine tries that anchor only at positions in the region's own context,
    so a closer deep inside a nested region leaves the outer one open. A
    closer the body reaches at its own level ends it, and that covers two
    things. A grammar whose terminator appears loose inside its own body ends
    its region early, which the predecessor documented with the anchor
    written by hand. And a child that is itself a matched pair leaves its
    closer just behind the position the engine comes back to, so a region
    whose earlier child closes on the same token as its last ends behind
    the earlier one. [try \{ \} catch (e) \{ \}] ends after the first brace.

    {2 Two children one list cannot tell apart}

    A pattern list has no positions. Two children that hold different matched
    pairs opening on one token are one such case: the first in the list takes
    every occurrence. The early end above is the other.

    A region with either is cut in two. The first part runs from the opening
    token to a child that stands between the two, and what follows is tried in
    an outer region that wraps it:

    {v
      do (head) { body } while (cond)
      \--------- first part ---/
      \--------------- the region ---/
    v}

    The first part of [do] ends by taking [while], so [(head)] and [(cond)]
    are tried in different lists and the region no longer ends behind the
    head's bracket. Where no literal stands between them, as in
    [while (head) (cond) \{ \}], the first part ends behind the first pair's
    closer instead.

    The outer region opens on a lookahead for what the first part opens on,
    and ends on the same lookbehind as before with [(?!\G)] in front of it.
    Without that it could end where it opened, behind a bracket that closed
    just before it.

    Where no cut leaves both sides clean, {!conflicts} names what is left and
    the backend refuses the grammar. *)

(** One group of a single-regex emission. *)
type part =
  { regex : string
  ; scope : Scopes.Scope.t option
  }

(** How the first part of a cut region ends. *)
type close =
  | Literal of Core.Token.def (** By taking this token, which is the child itself. *)
  | Behind of string (** Behind this text, the closer of the pair the child holds. *)

(** A cut. The first part holds the children up to [upto]: up to and including
    it under [Behind], and up to it under [Literal], whose token is the first
    part's own [end]. *)
type split =
  { upto : int
  ; close : close
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
      ; split : split option (** Where the region is cut in two, if it is. *)
      }
  | Capture of part list (** A single [match], one capture group per child. *)
  | Flat (** A list of patterns, one per child. *)

val of_rule : Core.Facts.t -> Scopes.t -> Core.Rule.def -> t

(** Two children of one rule that its pattern list cannot tell apart. Each
    child is an index into {!Core.Rule.def.children}. *)
type conflict =
  | Shares of
      { token : Core.Token.def
      ; children : int list (** One child for each pair. *)
      } (** Children that hold different matched pairs opening on [token]. *)
  | Closes of
      { child : int
      ; text : string
      }
  (** A child of a region that ends on [text], which the region's own end
        looks behind for. *)

(** What is left to tell apart in a rule as {!of_rule} shapes it. A region
    that was cut has nothing left. *)
val conflicts : Core.Facts.t -> Scopes.t -> Core.Rule.def -> conflict list

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

(** {1 What a body list carries} *)

(** The children a body list emits from [from] on, in the order it emits
    them: those that open a matched pair first, then the rest in declaration
    order. *)
val emission_order : Core.Facts.t -> Core.Rule.def -> from:int -> int list

(** The first child a shape emits as a pattern in its list. [None] for a
    single match, which has no list. *)
val body_from : Core.Rule.def -> t -> int option

(** The scope a token takes at one position of a body list. It is
    {!Scopes.resolve}, except for an identity child the shape keeps out of
    its body. That child takes its token's scope. *)
val body_scope
  :  Scopes.t
  -> Core.Rule.def
  -> t
  -> index:int
  -> token:Core.Token.id
  -> Scopes.Scope.t option
