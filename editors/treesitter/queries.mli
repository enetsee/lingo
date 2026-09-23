(** The query files: [highlights.scm] and [folds.scm].

    A tree-sitter query names nodes and fields and hangs a capture off one.
    A {!Scopes.t} already has that shape, so both files are a printing
    job.

    {2 Capture names}

    {!Scopes.Scope.t} is TextMate's vocabulary, and tree-sitter themes read a
    different one: [@keyword], [@function], [@type], [@string], [@comment],
    [@punctuation.bracket] and their friends, the set nvim-treesitter settled
    and Helix and Zed read too. {!capture} is the translation, and it loses
    detail in one direction on purpose. [storage.type.struct] becomes
    [@keyword], because no tree-sitter theme has a rule for the first.

    A [meta.*] scope has no translation and is dropped. It names a span for a
    theme to reach, and in tree-sitter the node is the span.

    A {!Scopes.Scope.Custom} scope is dropped. It is the way out of this
    vocabulary and into TextMate's, and it leads nowhere here: emitting the
    path as a capture would give a theme a name no theme defines. The node
    still takes whatever its token is called.

    Specific patterns come first, then the catch-alls. tree-sitter's own
    highlighter takes the first pattern that matches a node, so a
    per-position capture has to come before the one on the token itself. *)

(** The capture a scope translates to, without its [@]. [None] where the
    scope names something tree-sitter has no word for. *)
val capture : Scopes.Scope.t -> string option

val highlights : Scopes.t -> string
val folds : Scopes.t -> string
