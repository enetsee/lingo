(** The JSON a repository entry is made of.

    Everything here builds a fragment. {!Shape} settles which fragment a
    rule takes, and {!Textmate} assembles them into a document. *)

(** What every fragment needs. Passing it as one value keeps the language
    string and the alias map from being derived twice and drifting apart. *)
type ctx =
  { facts : Core.Facts.t
  ; scopes : Scopes.t
  ; language : string (** The last segment of every scope, and of the scope name. *)
  ; alias : Core.Rule.id -> Core.Rule.id
    (** Forwarding rules resolved to what they forward to. See
        {!Shape.forwards_to}. *)
  ; raw : Core.Rule.id -> Yojson.Basic.t list
    (** Hand-written patterns to splice into a rule's body. *)
  }

(** The repository entry for one rule the author wrote. *)
val entry : ctx -> Core.Rule.def -> Yojson.Basic.t

(** The repository entry for an expression block: its atoms, and a pattern
    per postfix operator that has a shape of its own. The prefix and infix
    operator tokens need none, since they are tokens and the grammar-wide
    token entry covers them. *)
val block_entry : ctx -> Core.Block.def -> Yojson.Basic.t

(** One pattern per trivia token. [None] where the grammar declares none. *)
val trivia_entry : ctx -> Yojson.Basic.t option

(** One pattern per token, for everything a walk over the rules does not
    reach: an operator inside an expression block, and a token in a position
    a body pattern list does not cover.

    A literal is tried before any literal it is a prefix of. A pattern token
    keeps its declaration order. Regex length says nothing about which of
    two patterns is the more specific, and the author's order does. *)
val tokens_entry : ctx -> Yojson.Basic.t

(** Whether the grammar declares any trivia. A body references
    {!trivia_entry} only where it does. *)
val has_trivia : Core.Facts.t -> bool
