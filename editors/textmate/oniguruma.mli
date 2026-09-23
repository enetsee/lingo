(** Token regexes, in the dialect a TextMate engine reads.

    TextMate matches with Oniguruma, and a token's own regex is a
    {!Redfa.Regex.t}. Most of them cross over unchanged, and
    {!Redfa.Regex.to_oniguruma} is the crossing. A term with no Oniguruma
    form, such as a complement, has to be written out by hand.
    {!Core.Grammar.pattern_spec.textmate} carries that spelling, and
    {!of_token} takes it where it is set.

    lingo's lexer takes the longest match over the whole token set at once.
    A TextMate engine tries patterns in order and takes the first that
    matches at the cursor. So a regex that is unambiguous inside an
    automaton can be ambiguous on its own: [=] matches the first character
    of [==]. {!of_token_at_child} carries a guard against that, and
    {!of_token} leaves it off. *)

(** Backslashes the Oniguruma metacharacters, so the result matches the
    argument as text. *)
val escape : string -> string

(** The regex for a token, or the reason it has none. *)
val of_token : Core.Token.def -> (string, string) result

(** [of_token], with a guard against the token claiming the first characters
    of a longer literal in the same grammar. Use it wherever the pattern is
    emitted ahead of the grammar-wide token list, which is every per-child
    position. *)
val of_token_at_child : Core.Facts.t -> Core.Token.def -> (string, string) result

(** Rewrites a bare [(] into [(?:], leaving [(?...], an escaped [\\(] and a
    [(] inside a character class alone.

    A single-regex emission wraps each part in its own group and scopes
    capture [n] as part [n]. A capturing group inside a part shifts every
    index after it. A named group stays as it is, since neutralising one
    would break the reference to it. *)
val neutralise : string -> string

(** What may sit between two parts of a single-regex emission: any run of
    trivia, as an atomic group so that a long run of whitespace beside an
    alternation does not backtrack.

    [\\s*+] where the grammar declares no trivia. *)
val trivia_separator : Core.Facts.t -> string

(** Whether a word boundary fires at both ends of the text.

    [\\b] sits between a word character and a non-word one, so [\\bfoo\\b]
    matches and [\\b->\\b] never can. A keyword spelled in punctuation is
    emitted the way punctuation is; see the note in the implementation on
    what counts as a word character above ASCII. *)
val is_word_shaped : string -> bool
