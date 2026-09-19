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

    {1 The boundary}

    [boundary s i] is [true] where lexing [s] puts a token boundary at byte [i].
    The caller supplies it, because the automaton belongs to the grammar and
    this library holds none.

    A boundary between two tokens has to survive being read back. The fold tries
    the joins in order, nothing then a space then a line break, and takes the
    first that keeps the boundary. That is the whole of it. Where a space will not do,
    and where nothing will, come from the lexer; no class of characters here
    could stand in for it.

    The left-hand side is every byte written since the last blank, rather than
    the token before the boundary. No pair of ["7"], ["."] and ["1"] joins into
    one lexeme; all three do.

    {1 What reaches the document}

    Every token the tree holds, in the order it holds them. Nothing is dropped
    but trivia whose token is [Reformat] and whose text is nothing but
    whitespace, because the boundaries write that spacing back.

    One thing is added, and only one: the separator a body's policy puts after
    its last element, and only into a frame the parse closed. See
    {!Ir.Layout.type-trailing}. *)

(** {1 The table}

    These are {!Ir.Layout}'s types, named here as well so generated code reaches
    them through this library alone. Every field is documented there. *)

type break = Ir.Layout.break =
  | Flat
  | Fit
  | Hard of int

type trailing = Ir.Layout.trailing =
  | Never
  | On_break
  | Always

type sep = Ir.Layout.sep =
  { sep_kind : Ir.Kind.t
  ; text : string
  ; trailing : trailing
  }

type frame = Ir.Layout.frame =
  | Plain
  | Delimited of
      { open_ : Ir.Kind.t
      ; close : Ir.Kind.t
      ; sep : sep option
      }
  | Separated of sep

type slot = Ir.Layout.slot =
  { kinds : Ir.Kind.t array
  ; repeats : bool
  ; before : break
  ; between : break
  }

type rule = Ir.Layout.rule =
  { name : string
  ; kind : Ir.Kind.t
  ; frame : frame
  ; slots : slot array
  ; body : break
  ; inner : break
  ; indent : int
  ; edge_before : bool option
  ; edge_after : bool option
  }

type trivia = Ir.Layout.trivia =
  | Reformat
  | Preserve

type token = Ir.Layout.token =
  { space_before : bool
  ; space_after : bool
  ; trivia : trivia option
  }

type t = Ir.Layout.t =
  { rules : rule array
  ; of_kind : int array
  ; tokens : token option array
  }

(** [boundary ~lex s i] is [true] where lexing [s] puts a token boundary at
    byte [i]. That is the predicate {!doc} takes, over a grammar's own lexer.

    The fold calls it with a run of bytes since the last blank and the token
    about to follow, so the cost is that run rather than the document. *)
val boundary : lex:(string -> Token.t array) -> string -> int -> bool

(** The document, with each token's kind on it. A caller folding the rendered
    stream reads those to colour the output or to check it.

    [trace] is called with the name of each step the fold takes: which join the
    lexer allowed, which break a boundary took, and the handful of steps no
    boundary governs. A law counts them to say which a corpus reaches, because a
    step no fold takes is one no law covers. *)
val doc
  :  ?trace:(string -> unit)
  -> Ir.Layout.t
  -> boundary:(string -> int -> bool)
  -> Siesta.Green.node
  -> Ir.Kind.t Handsome.Ascii.t

(** [doc] rendered at [width]. *)
val format
  :  ?trace:(string -> unit)
  -> Ir.Layout.t
  -> boundary:(string -> int -> bool)
  -> width:int
  -> Siesta.Green.node
  -> string
