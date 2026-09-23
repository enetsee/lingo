(** [grammar.js]: the structure, as a second parser.

    What comes out is a tree-sitter grammar. It accepts the same language as
    lingo's parser and builds a different tree, by design. Everything
    lingo's parser carries for recovery and for losslessness is dropped
    here, because highlighting needs none of it and tree-sitter has recovery
    of its own.

    {2 The mapping}

    {v
      child, by modifier      field(name, x) under optional / repeat / repeat1
      alternatives            choice(...)
      Plain, Committed        seq(...)
      Delimited               seq(open, sepBy(sep, elem), close)
      Separated               sepBy1(sep, elem)
      keyword, punctuation    a string, so an anonymous node
      pattern token           a rule of its own, so a query can name it
      trivia                  extras
      binding power           prec.left / prec.right, by associativity
    v}

    A production that contains its own errors is a [seq] like any other.
    [Committed] says where recovery resumes, and tree-sitter recovers its
    own way.

    Recovery sets, resync anchors, holes, error messages, the trailing-line
    rules a formatter reads and the flat shape lingo's parser gives an
    expression are all dropped. Every one of those is about something other
    than which strings are in the language.

    tree-sitter's generator may still find a conflict. lingo checks LL(1)
    and tree-sitter builds an LR automaton, so a grammar can pass the first
    and fail the second. The binding powers carry over as precedences, which
    covers most of it, and a greedy child becomes a [prec.right] for the
    dangling [else]. The rest would need the generator in the loop. *)

(** The file's text, or the reason a token has no JavaScript regex. *)
val emit
  :  Core.Facts.t
  -> language:string
  -> word:Core.Token.def option
  -> (string, string) result
