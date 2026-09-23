(** A rule with every name resolved to a {!Kind.t} and every expression
    block desugared. One per production, one per expression block, and one
    per active role of each block.

    {2 The frame and where the body starts}

    A production's [Delimited] framing and an expression block's [Enclosed]
    postfix body are the same thing. Both are a matched pair with an optional
    separator around a body. They become the same value here, so a backend
    can parse both with one loop and lay both out through one frame.

    They differ in where the body starts. A postfix operand sits before the
    frame opens, and a production has nothing before it. {!def.body_from} is
    the index in {!def.children} where the frame's body begins. It is [0] for
    a production and [1] for a desugared [Enclosed] postfix. A fold that
    takes the frame from [body_from] serves both. *)

(** An index into {!Facts.rules}. *)
type id = int

(** Where the rule came from. Read this to separate a desugared role from a
    production the author wrote. Matching on the name would work today and
    break on a grammar that names a production after a role. *)
type origin =
  | User (** A production the author wrote. *)
  | Pratt_block (** The block itself, which other rules reference. *)
  | Pratt_role of
      { block : id
      ; role : Role.t
      }

type sep =
  { sep_tok : Kind.t
  ; trailing : Grammar.trailing_sep
  }

(** Framing with the delimiters resolved. *)
type frame =
  | Plain
  | Committed of { boundary : bool }
  | Delimited of
      { open_ : Kind.t
      ; close : Kind.t
      ; sep : sep option
      ; boundary : bool
      }
  | Separated of
      { sep_tok : Kind.t
      ; trailing : Grammar.trailing_sep
      ; boundary : bool
      }

type child =
  { child_name : Grammar.Name.Child.t
  ; alts : Kind.t array
    (** The child's symbols in declaration order. A single symbol gives a
          one-element array.

          Alternative dispatch is a cascade. It takes the first arm whose
          FIRST set admits the cursor, so where two arms overlap the order
          settles which one takes it. *)
  ; kinds : Kind.Set.t (** {!child.alts} as a set. *)
  ; modifier : Grammar.modifier
  ; greedy : bool
  ; recover_to : Kind.Set.t option
    (** Replaces the recovery set computed at this position. [None] leaves
          that set in place. The closers of enclosing frames are added
          either way. See {!Facts.recovery_set}. *)
  }

type def =
  { id : id
  ; kind : Kind.t
  ; name : Grammar.Name.Rule.t
    (** The author's name, for diagnostics and documentation. *)
  ; children : child array
  ; frame : frame
  ; body_from : int
    (** Where the frame's body starts in {!children}. See the note at the
          top of this module. *)
  ; hole : Kind.t option
  ; origin : origin
  ; recovery : Grammar.recovery_spec
  ; messages : (Grammar.Name.Child.t * string) array
    (** Catalogue entries keyed by child name. *)
  ; resync : Kind.Set.t
    (** Tokens that end this rule's body loop early. The closer and end of
          input still end it; these are extra. Empty is the common case.

          {!Grammar.with_resync_to} sets it, and says what it is for. The
          parser reads it, and nothing in this library does. *)
  ; format : Grammar.production_format
  ; edge_space_before : bool option
    (** Overrides the leading spacing flag the formatter would take from
          the rule's first token. *)
  ; edge_space_after : bool option (** The same on the trailing edge. *)
  ; identity : int option
    (** An index into {!children}, naming the child whose text names the
          rule in a diagnostic. *)
  ; binders : int array
    (** Indices into {!children}, naming the children whose text introduces
          a name. Ascending, and each index appears once.

          {!Grammar.with_binder} sets it, and says what it is for. An editor
          backend reads it, and nothing in this library does. *)
  ; opens_scope : bool
    (** Whether a name introduced inside this rule belongs to it. See
          {!Grammar.with_scope}. *)
  }

(** The children the frame applies to. That is everything from
    {!def.body_from} onwards. *)
val body_children : def -> child list

(** Whether the rule is a desugared role. A role describes the shape of a
    node the block's parser emits. The FIRST and FOLLOW walk runs over
    productions and leaves roles alone. *)
val is_synthetic : def -> bool
