(** What a span of the documentation is, so a fold can style it and link it.

    handsome renders to a stream, and a region of a document can carry an
    annotation. This is that annotation. A fold over the stream turns each
    mark into a class, and wraps it in an anchor where the mark has a link.
    The same document also folds to plain text, laid out the same way. *)

type t =
  | Rule of string (** A production's own name, where the listing declares it. *)
  | Reference of string (** A reference to a production from inside another. *)
  | Token of string (** A token's name, shown where the token matches a pattern. *)
  | Literal of string
  (** A keyword or a punctuation token. The string is its name, and the span
        holds the text it matches. *)
  | Child (** The name of a slot in a production. *)
  | Notation (** Punctuation and labels the listing adds of its own. *)
  | Scoped of Scopes.Scope.t (** A span of a code example, under its scope. *)

(** The classes a span carries, broadest first.

    A scope is a dotted path and a theme matches a prefix of it, so every
    prefix becomes a class of its own: [keyword.other.let] gives three. A
    stylesheet rule written for [keyword] then reaches a span scoped
    [keyword.other.let]. *)
val classes : t -> string list

(** The fragment a span links to, such as [#rule-Struct].

    A reference to a production links to that production's section. A token
    links to its row in the token table. A reader then walks the grammar by
    clicking through it.

    [None] for every other span. *)
val link : t -> string option

(** The anchor a production's section carries. *)
val rule_anchor : string -> string

(** The anchor a token's row carries. *)
val token_anchor : string -> string
