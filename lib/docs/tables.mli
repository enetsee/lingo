(** The tables a language's manual carries, each derived from something the
    toolkit already computed.

    These build HTML directly. A table is a grid and a browser fits it to
    the page, so handsome lays out only what has to fit a width. *)

(** Every token: its name, what kind it is, and what it matches. Each row
    carries an anchor, so a listing or a diagram can link to it.

    A pattern token's regex is printed back to source by redfa, so the page
    shows a term the author could have typed. *)
val tokens : Core.Facts.t -> string

(** Every operator of every expression block, by binding power, tightest
    last. Two operators at one power bind alike and associate alike. The
    checker enforces that, and a reader has to know it. *)
val precedence : Core.Grammar.t -> string

(** What each production can start with, and what can follow it. Each
    production's name links to its own section.

    These are the sets the parser dispatches on. A reader of the manual
    reads them to work out why two forms cannot be told apart. The checker
    reports that as [first-first-conflict]. *)
val first_follow : Core.Facts.t -> string

(** How a kind reads on a page. A token with fixed text reads as that text.
    Anything else reads under the name its author gave it. *)
val kind_text : Core.Facts.t -> Core.Kind.t -> string
