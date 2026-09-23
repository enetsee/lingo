(** The grammars the editor backends are recorded against, and the scopes an
    author would set on each.

    This is a library, so the dump and the law share one list. A module
    belongs to one stanza, and two lists of grammars are two corpora that
    drift apart.

    Most of a scope map is derived, and a corpus of derived-only grammars
    would record nothing about the override path. These are the scopes a
    person writing an editor integration would set: which identifier
    position is a type and which is a parameter, and what a doc comment is.
    Nothing in a grammar says any of it. *)

type entry =
  { name : string
  ; grammar : Core.Grammar.t
  ; overrides : Scopes.override list
  }

val all : entry list
val find : string -> entry option

(** Raises [Failure] with the findings rendered. The law checks the corpus,
    so this raises only when the corpus itself is broken. *)
val scopes : entry -> Scopes.t

(** Source to show on a page, drawn from the inputs the parse laws read.

    Two short examples, then the first that spans more than a line. handsome
    rejects a newline welded into a text node, and only a multi-line example
    reaches that path.

    Empty for an entry with no inputs of its own. *)
val examples : entry -> (string * string) list
