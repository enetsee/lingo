(** The query files: [highlights.scm], [folds.scm] and [locals.scm].

    A tree-sitter query names nodes and fields and hangs a capture off one.
    A {!Scopes.t} already has that shape, so all three files are a printing
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

    The catch-alls come first, then the specific patterns. tree-sitter's own
    highlighter takes the last pattern that matches a node, so a per-position
    capture has to come after the one on the token itself. Written the other
    way round, the token's colour wins at every position and the specific
    pattern is dead text. *)

(** The capture a scope translates to, without its [@]. [None] where the
    scope names something tree-sitter has no word for. *)
val capture : Scopes.Scope.t -> string option

val highlights : Scopes.t -> string
val folds : Scopes.t -> string

(** [locals.scm]: where a name is introduced, and where it is in use.

    The grammar states two of the three facts this needs.
    {!Core.Grammar.with_scope} says which productions open a scope, and
    {!Core.Grammar.with_binder} says which child positions introduce a name.
    [word] is the third, the language's identifier, and
    {!Treesitter.word_token} derives it.

    tree-sitter does the resolving. Its runtime walks the tree, keeps a stack
    of the scopes, and matches each reference to the nearest enclosing
    definition of the same text.

    A grammar that declares no binder and no scope gives a file with the
    header alone, plus the blanket reference where [word] is set. Nothing
    here derives where a name is introduced. *)
val locals : Scopes.t -> word:Core.Token.def option -> string
