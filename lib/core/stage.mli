(** The staged derivation behind {!Facts.of_grammar}.

    Facts come from a checked grammar, and a check reads facts. Staging is how
    both are true at once. Each stage is total on its input. Each check runs at
    the earliest stage that holds what it reads. {!Facts.of_grammar} stops at
    the first stage that reports anything.

    Take a grammar with a dangling rule reference. FIRST over it is not worth
    computing. The first stage reports the dangling reference and FIRST never
    runs.

    There are two stages, and a lexer beside them.

    - {!val-names} holds the kind table, the token definitions, the manifest,
      and one rule slot per production, per block and per role. It is total on
      any grammar, and it holds what a check over declarations reads. Does a
      reference resolve? Is a name an identifier? Do two names mangle onto one?
      Does a token's regex match the empty string?
    - {!val-shape} holds the rules, with every symbol resolved to a {!Kind.t}, 
      blocks desugared and framing normalised. It holds what a check over
      resolved children reads. How many children does a frame wrap? Do two
      child slots admit one node?
    - {!lexer} builds the token automaton. {!Fixpoint.compute} takes a
      {!type-shape} and derives the rest.

    Nothing here escapes {!Facts}. A backend holds a {!Facts.t}. *)

(** Where a rule id came from. Ids are assigned here, in this order: every
    production in declaration order, then every block, then each block's
    active non-base roles.

    A block's base role is the block rule itself. The kind a token atom produces 
    and the kind other rules reference are one kind, so there is one rule for 
    both. *)
type slot =
  | Prod of Grammar.production
  | Block of Grammar.expr_def
  | Role of
      { block_rule : Rule.id
      ; block : Grammar.expr_def
      ; role : Role.t
      }

type names = private
  { grammar : Grammar.t
  ; manifest : Manifest.t
  ; kinds : Kind.Table.t
  ; tokens : Token.def array
  ; slots : slot array
  ; block_base : Rule.id
    (** The id of the first block. Block [i] has id [block_base + i], and every
        id below [block_base] belongs to a production. This is the layout 
        {!type-slot} describes, held as a value. *)
  ; rule_kind : Kind.t array (** {!Rule.type-id} to its node kind. *)
  ; rule_hole : Kind.t option array
  ; rule_by_name : (Grammar.Name.Rule.t, Rule.id) Hashtbl.t
  ; token_by_name : (Grammar.Name.Token.t, Token.id) Hashtbl.t
  ; kind_token : int array (** Kind to {!Token.type-id}, or [-1]. *)
  ; kind_rule : int array (** Kind to {!Rule.type-id}, or [-1]. *)
  ; error_kind : Kind.t
    (** [N_ERROR]. {!val-shape} sends any symbol it cannot resolve to this kind,
        which is what keeps {!val-shape} total.

        Nothing reaches {!val-shape} in that state in practice. A symbol that
        does not resolve has already been reported, and {!Facts.of_grammar} 
        stops at the stage that reported it. *)
  }

(** Total on any grammar. A duplicated name, a dangling reference and an empty 
    production list all still give a [names]. Reporting them is a check's job, 
    not this one's. *)
val names : Grammar.t -> names

val find_rule : names -> Grammar.Name.Rule.t -> Rule.id option
val find_token : names -> Grammar.Name.Token.t -> Token.id option

(** {!Grammar.symbol} carries a bare string. This tags it with the namespace its
    constructor names, then looks it up. *)
val resolve : names -> Grammar.symbol -> Kind.t option

type shape = private
  { names : names
  ; rules : Rule.def array
  ; blocks : Block.def array
  }

(** Total on any [names]. Desugars every block into its role rules and resolves 
    every symbol; an unresolved one becomes [error_kind]. *)
val shape : names -> shape

(** The token automaton, with case ids in declaration order, so declaration
    order is max-munch priority order. *)
val lexer : names -> Redfa.Dfa.t
