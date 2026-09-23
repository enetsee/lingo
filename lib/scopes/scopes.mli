(** What each token and each child position of a grammar is called, in a
    theme's vocabulary.

    TextMate renders this into [.tmLanguage.json], tree-sitter into
    [highlights.scm], and the documentation backend into the class on a
    [<span>]. Each of them reads what is here and derives none of it, so an
    editor and the reference manual colour a keyword alike.

    {1 What is derived and what is written}

    Most of it follows from the facts. A keyword is [keyword.other.<name>],
    punctuation is [punctuation.separator], a comment takes the [comment.*]
    shape its own regex says it has, and whitespace takes nothing. A pattern
    token gets nothing. [ident] and [number] have one shape in a grammar and
    two colours on a page, and nothing here separates them.

    The rest is the author's, through {!type-override}. An override names
    the thing it applies to in the author's own names, and {!of_facts}
    resolves them. An override that names nothing is reported. A silently
    dropped one looks exactly like a theme that does not cover the scope.

    The shapes an expression block desugars into can be named too, by the
    names {!Core.Role} gives them. The author never typed [ExprPostfixCall].
    It is in the kind table and it names a view module, and it is the only
    handle on the span a call occupies.

    {1 The chain}

    A token in a child position can be reached three ways, and the most
    specific wins. {!resolve} is that chain:

    + the override on this (rule, child) position;
    + the override on the rule's identity child, where this is it;
    + the override or default on the token itself.

    A per-position scope exists because a position is more specific than a
    token. The same [ident] is a function name in one slot and a parameter
    in another. *)

module Scope = Scope

(** A scope the author sets, in place of what would be derived.

    [Identity] is separate from [Child] so that a rule whose identity child
    moves does not take its scope with it. A production names itself by one
    of its children ({!Core.Rule.def.identity}). The author is scoping the
    name of the production, and the slot it sits in may change. *)
type override =
  | Token of
      { token : Core.Grammar.Name.Token.t
      ; scope : Scope.t
      }
  | Rule of
      { rule : Core.Grammar.Name.Rule.t
      ; scope : Scope.t
      }
  | Identity of
      { rule : Core.Grammar.Name.Rule.t
      ; scope : Scope.t
      }
  | Child of
      { rule : Core.Grammar.Name.Rule.t
      ; child : Core.Grammar.Name.Child.t
      ; scope : Scope.t
      }

(** Why an override was rejected.

    An override sits outside the grammar, so a bad one is reported here and
    not as a {!Core.Error.t}. That type is the grammar checker's, and every
    code in it has a witness grammar. *)
type finding =
  | Unknown_token of Core.Grammar.Name.Token.t
  | Unknown_rule of Core.Grammar.Name.Rule.t
  | Unknown_child of
      { rule : Core.Grammar.Name.Rule.t
      ; child : Core.Grammar.Name.Child.t
      }
  | No_identity_child of Core.Grammar.Name.Rule.t
  (** {!Identity} on a rule that names itself by nothing. The override
        would have no effect, and the author wrote it for a reason. *)
  | Malformed_scope of
      { override : string (** The override, rendered, so the author finds it. *)
      ; reason : string
      }

val pp_finding : Format.formatter -> finding -> unit
val finding_to_string : finding -> string

type t

(** Derives the defaults, then lays the overrides over them. Findings come
    back sorted, and all of them: an author fixing one override should not
    have to run again to see the next. *)
val of_facts : Core.Facts.t -> overrides:override list -> (t, finding list) result

(** {1 Reading} *)

val facts : t -> Core.Facts.t

(** The scope on the token itself, derived or overridden. It applies
    wherever nothing more specific does. *)
val token : t -> Core.Token.id -> Scope.t option

(** The scope the author put on the rule as a whole, and [None] where they
    put none. Nothing is derived here. What a production's own span is
    called depends on what the backend can hang it from, so each backend
    supplies its own default. *)
val rule : t -> Core.Rule.id -> Scope.t option

(** The scope on the rule's identity child. [None] where the author set
    none, including where the rule has no identity child. *)
val identity : t -> Core.Rule.id -> Scope.t option

(** The scope on one child position, by its index in
    {!Core.Rule.def.children}. *)
val child : t -> Core.Rule.id -> child:int -> Scope.t option

(** The chain in the header: the position, then the identity, then the
    token. [child] is an index into {!Core.Rule.def.children}. *)
val resolve : t -> rule:Core.Rule.id -> child:int -> token:Core.Token.id -> Scope.t option

(** Whether {!resolve} at this position lands on exactly the token's own
    scope. A backend that already emits a rule per token drops the
    per-position one when this holds. Emitting both lets a shorter literal
    claim the first characters of a longer one. *)
val is_token_scope : t -> rule:Core.Rule.id -> child:int -> token:Core.Token.id -> bool

(** {1 Printing} *)

(** A stable dump: every token with its scope, then every rule with its own
    and its children's. The same facts and overrides give the same bytes. *)
val pp : Format.formatter -> t -> unit
