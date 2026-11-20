(** Lay a tree out as a document.

    Every form goes through one fold. A production, a delimited body, a
    separated list and each shape an expression block desugars into are the same
    walk over the children a tree has, with {!Ir.Layout.t} holding what each
    child is. There is no second walk for a fix to miss.

    {1 The document describes the printed bytes}

    So the fold never reads them back. It writes every line break itself, no
    text node holds a newline, and the last token it wrote is still in hand. A
    helper that read the rendered string for whether it ended a line would be a
    second account of something this walk already settled, and the two would
    drift.

    The lexer is the one thing it consults, and about the bytes it is about to
    write rather than the ones it wrote.

    {1 The ruler}

    A width is columns drawn, not bytes stored: the fold measures with
    [Handsome.Utf8]. On ASCII the two are one number, and the difference shows
    on everything else. [«αβγ → αβγ»] is 11 columns and 21 bytes, and it fits a
    ruler of 16.

    Bytes would be sound. Every UTF-8 encoding is at least as long in bytes as
    it is wide, so a byte measure over-reports, and over-reporting costs a line
    break earlier than the ruler asks for rather than a line past it. Sound,
    and wrong about what a reader sees, which is what a ruler is for.

    test/units/width.ml is what says which is in use, because no law here can:
    both measures are sound, so every law reads zero under either.

    {1 The boundary}

    [boundary s i] is what the grammar's lexer says of byte [i] of [s], where
    [s] is the start of a longer text: {!Token.boundary}. The caller supplies
    it, because the automaton belongs to the grammar and this library holds
    none. A generated lexer has one, and test/lex has one over the facts.

    A boundary between two tokens has to survive being read back. The fold has
    three things to put there: nothing, a space and a line break. It asks the
    lexer about each one the gap may turn into, and writes only what the lexer
    allows. Where a space will not do, and where nothing will, come from the
    lexer; no class of characters here could stand in for it.

    The tokens' spacing says whether a space or nothing is wanted, and where
    the lexer refuses that it may allow the other. A gap that may end the line
    is asked about the line break as well, and stays on its line where the
    lexer refuses it. Where neither a space nor nothing will do, a line break
    is all that is left.

    The left-hand side is every byte written since the last blank, rather than
    the token before the boundary. No pair of ["7"], ["."] and ["1"] joins into
    one lexeme; all three do.

    Settled is against the bytes not yet written too. Where comments are
    [(* ... *)], a [(] and a [*] side by side read as two tokens, and as the
    start of a comment once its closer follows. So the fold lets two tokens touch
    where the boundary between them is settled, or where a blank after them
    settles it, and a blank is what ends a run.

    A blank has a boundary on each side, and both have to be settled. Where a
    backslash and a line break are one token, a line that ends behind a
    backslash has made that token of the two. test/units/joins.ml holds all
    of these.

    {1 What reaches the document}

    Every token the tree holds, in the order it holds them. Nothing is dropped
    but trivia whose token is [Reformat] and whose text is nothing but
    whitespace, because the boundaries write that spacing back.

    One thing is added, and only one: the separator a body's policy puts in
    front of its first element or after its last, and only into a frame the
    parse closed. See {!Ir.Layout.type-optional_sep}. *)

(** {1 The table}

    These are {!Ir.Layout}'s types, named here as well so generated code reaches
    them through this library alone. Every field is documented there. *)

type break = Ir.Layout.break =
  | Flat
  | Fit
  | Hard of int

type optional_sep = Ir.Layout.optional_sep =
  | Never
  | On_break
  | Always

type position = Ir.Layout.position =
  | Ends_line
  | Starts_line

type sep = Ir.Layout.sep =
  { sep_kind : Ir.Kind.t
  ; text : string
  ; leading : optional_sep
  ; trailing : optional_sep
  ; position : position
  }

type frame = Ir.Layout.frame =
  | Plain
  | Delimited of
      { open_ : Ir.Kind.t
      ; close : Ir.Kind.t
      ; sep : sep option
      ; open_space : bool
      ; pad : bool
      }
  | Separated of sep

type slot = Ir.Layout.slot =
  { kinds : Ir.Kind.t array
  ; repeats : bool
  ; before : break
  ; between : break
  ; after : break
  ; nests : bool
  ; together : bool
  ; handed : bool
  ; space : bool
  }

type rule = Ir.Layout.rule =
  { name : string
  ; kind : Ir.Kind.t
  ; frame : frame
  ; slots : slot array
  ; body : break
  ; inner : break
  ; indent : int
  }

type trivia = Ir.Layout.trivia =
  | Reformat
  | Preserve

type side = Ir.Layout.side =
  | Hug
  | Free
  | Space

type token = Ir.Layout.token =
  { space_before : side
  ; space_after : side
  ; trivia : trivia option
  }

type t = Ir.Layout.t =
  { rules : rule array
  ; of_kind : int array
  ; tokens : token option array
  }

(** The rule a group decides by, which is [Handsome.Line]: a group measures
    what follows it on its last line as well as its own content.

    It is here rather than inside {!format} because a caller rendering {!doc}
    itself has to use the same one. The fold writes a break wherever the ruler
    could need one and nowhere else, so one of its documents rendered under
    [Handsome.Content] puts lines past the ruler with no break left to take.

    What follows a group counts at its flat width, with one exception. A
    delimited rule yields: what stands before it on its line counts it as far
    as its opener, where it can end the line. So a header with a parameter
    list and a body in braces opens the body, and opens the list only where the
    header alone does not fit. A plain rule yields from its first child that
    does, where no boundary in front of that child may break, so a body behind
    a rule that only holds it is counted the same way.

    A boundary that may break in front of a body is settled with the body
    flat, and opens before the body does. test/units/yields.ml holds each of
    these at the width where it turns.

    Bodies in slots marked {!Ir.Layout.slot.together} are one decision. The
    rule that holds them is a frame, each body is laid out with no group of
    its own, and so they all open or none does. A marked child whose own rule
    has marked slots hands the same decision on, which is what makes the
    blocks of an [if], and of the [if] after its [else], open together.
    test/units/together.ml holds these.

    A list that is all a matched pair holds breaks with the pair. Where the
    separator is the pair's own, the two are one group already. Where the list
    is a rule of its own, the pair's slot is {!Ir.Layout.slot.handed}: the
    list is laid out with no group of its own, and goes one element to a line
    where the pair opens. test/units/enclosed.ml holds these. *)
val fit : Handsome.fit

(** The document, with each token's kind on it. A caller folding the rendered
    stream reads those to colour the output or to check it.

    [trace] is called with each step the fold takes and the kind of the rule it
    took it in: which join the lexer allowed, which break a boundary took, and
    the handful of steps no boundary governs. A law counts them to say which a
    corpus reaches, because a step no fold takes is one no law covers.

    The kind is there for the counting. How many steps there are follows from
    this fold, so a count over steps alone stands still however far a corpus is
    taken. How many steps a rule has follows from the grammar.

    The fold calls [boundary] with the bytes written since the last blank and
    the token about to follow, so the cost is that run and never the
    document. *)
val doc
  :  ?trace:(step:string -> kind:Ir.Kind.t -> unit)
  -> Ir.Layout.t
  -> boundary:(string -> int -> Token.boundary)
  -> Siesta.Green.node
  -> Ir.Kind.t Handsome.Utf8.t

(** [doc] rendered at [width]. *)
val format
  :  ?trace:(step:string -> kind:Ir.Kind.t -> unit)
  -> Ir.Layout.t
  -> boundary:(string -> int -> Token.boundary)
  -> width:int
  -> Siesta.Green.node
  -> string
