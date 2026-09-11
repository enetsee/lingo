(** What every backend needs from a grammar, derived once.

    More than one backend wants the kinds, the rules, the fixpoint tables and
    the lexer. Deriving them here means deriving them once.

    {2 Building one runs the checks}

    {!t} is a private record. {!of_grammar} is the one function here whose
    result mentions it, so no other module can build one. A backend takes a
    {!t}, and every {!t} has passed the checks.

    {2 What a backend gets}

    Every name has resolved. A {!t} holds {!Kind.t}s, {!Rule.type-id}s and
    {!Token.type-id}s.

    The author's own names are kept beside them, for diagnostics and for
    documentation. Each one is in the namespace it was written in; see
    {!Grammar.Name}.

    {!of_grammar} leaves the grammar with its caller. A documentation backend
    takes the grammar as well as the facts. It shows the grammar as the
    author wrote it, so it needs the form before desugaring. *)

type t = private
  { kinds : Kind.Table.t
  ; tokens : Token.def array (** Indexed by {!Token.type-id}. *)
  ; rules : Rule.def array
    (** Indexed by {!Rule.type-id}. Expression blocks already desugared. *)
  ; blocks : Block.def array (** Indexed by {!Block.type-id}. *)
  ; first : Kind.Set.t array (** Indexed by {!Rule.type-id}. *)
  ; follow : Kind.Set.t array (** Indexed by {!Rule.type-id}. *)
  ; nullable : bool array (** Indexed by {!Rule.type-id}. *)
  ; enclosing : Kind.Set.t array
    (** The closers of every frame the rule can sit inside. Indexed by
          {!Rule.type-id}. See {!recovery_set}. *)
  ; lexer : Redfa.Dfa.t
  ; names : Manifest.t
  ; kind_rule : int array (** Kind to {!Rule.type-id}, or [-1]. *)
  ; kind_token : int array (** Kind to {!Token.type-id}, or [-1]. *)
  ; roots : Rule.id list (** In declaration order. *)
  ; trivia : Kind.Set.t
  ; error_kind : Kind.t
  ; missing_kind : Kind.t
  ; unterminated_kind : Kind.t
  ; error_token_kind : Kind.t
  }

(** Runs the staged derivation and its checks, in order. It stops at the
    first stage that reports anything.

    A stage is total on its input, as long as the stage before it was
    clean. That is why the order matters.

    Take a grammar whose references do not resolve. The second stage would
    compare kinds standing in for names that are not there. The third would
    run a fixpoint over a rule graph with holes in it. The first stage
    reports the dangling reference and neither of them runs.

    Findings come back sorted. *)
val of_grammar : Grammar.t -> (t, Error.t list) result

(** {1 Reading the facts} *)

val kind_name : t -> Kind.t -> Kind.Name.t
val kind_count : t -> int
val find_kind : t -> Kind.Name.t -> Kind.t option
val rule : t -> Rule.id -> Rule.def
val token : t -> Token.id -> Token.def
val rule_of_kind : t -> Kind.t -> Rule.def option
val token_of_kind : t -> Kind.t -> Token.def option
val is_token_kind : t -> Kind.t -> bool
val is_trivia_kind : t -> Kind.t -> bool
val first_of : t -> Rule.id -> Kind.Set.t
val follow_of : t -> Rule.id -> Kind.Set.t
val is_nullable : t -> Rule.id -> bool

(** FIRST of a symbol. If the kind is a rule, this is that rule's FIRST. If
    it is a terminal, this is the kind itself. *)
val first_of_kind : t -> Kind.t -> Kind.Set.t

(** What a child position contributes to a recovery set. Three things go in:

    - what can follow the position inside the rule;
    - the closers of the rule's own frame;
    - the rule's FOLLOW, but only where the position is trailing.

    A child's [recover_to] replaces all three.

    The rule's FOLLOW belongs at a trailing position only. Take it further
    in and a recovery skips past the next item at the parent's level. The
    position has no way to know that it did.

    This is half of the answer, and it is the half a caller can work out on
    its own. The other half is the set of frames open around the rule.
    {!recovery_set} reads those from the table. A parser that already tracks
    its own open frames should union them with this instead. *)
val local_recovery_set : t -> Rule.id -> child:int -> Kind.Set.t

(** The tokens a parser may resume on at a child position. This is
    {!local_recovery_set} together with {!t.enclosing}, for a caller that
    does not know its own call path.

    The enclosing closers keep a nested recovery from eating a delimiter that
    an outer frame is still waiting for. They apply at every position. The
    rule's FOLLOW applies at a trailing position only.

    {2 An over-approximation}

    {!t.enclosing} is held per rule, not per call site. So a rule that is
    referenced from two places with different framing carries the closers of
    both frames, and it carries them at both sites.

    Take [Expr], used inside [( … )] and inside [\[ … \]]:

    {v
      recovery_set(Expr, 0) = { T_RPAREN, T_RBRACK, … }
    v}

    A recovery inside the parenthesised use now stops on [\]] too, even
    though no [\[] is open. On input [( a \] b )] that costs a diagnostic.
    Recovery halts at the [\]] and calls itself resynchronised. [Expr] ends
    short. The parenthesised rule then reports a second failure, looking for
    its [)].

    The error runs in the safe direction. A stop too early costs one
    diagnostic. A stop too late eats a delimiter an outer frame is waiting
    for, and that one mistake costs every frame above it. A table indexed by
    {!Rule.type-id} can do no better, because a rule id carries no record of
    its caller.

    A parser can do better than this. It has to thread the frames it has
    open down its own call stack, and union those with
    {!local_recovery_set}.

    {2 The override}

    A child's [recover_to] replaces {!local_recovery_set}. The enclosing
    closers still go on top. They belong to the frames around the rule. An
    author cannot know which frames those are. The rule is referenced from
    several sites, and the framing differs between them. *)
val recovery_set : t -> Rule.id -> child:int -> Kind.Set.t

(** Every matched delimiter pair in the grammar. The list has no duplicates
    and is in kind order, so it follows from the grammar alone.

    A parser skipping a broken span balances on these. It steps over a nested
    pair instead of resynchronising inside one. The list has to hold every
    pair, because a skip walks straight through one that is missing.

    Both kinds of pair are here. A production's [Delimited] framing and an
    expression block's [Enclosed] postfix become the same
    {!Rule.type-frame}, so one list covers both. *)
val delimiter_pairs : t -> (Kind.t * Kind.t) list

(** {1 Printing} *)

(** A stable dump: the kind numbering, then each rule with its shape, FIRST,
    FOLLOW and nullability. The same grammar gives the same bytes. *)
val pp : Format.formatter -> t -> unit
