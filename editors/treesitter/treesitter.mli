(** A tree-sitter grammar, from a lingo grammar's facts and its scopes.

    {v
      Grammar.t --> Facts.t --> Scopes.t --> generate --> grammar.js
                                                          highlights.scm
                                                          folds.scm
                                                          locals.scm
    v}

    This is a backend in the same sense the TextMate one is, and it reads the
    same {!Scopes.t}. One vocabulary of scopes drives both, so an editor
    reading either colours a keyword the same way.

    What comes out accepts the same language and builds a different tree.
    Recovery, holes, losslessness and the flat shape lingo gives an
    expression are all dropped, because highlighting needs none of them and
    tree-sitter has recovery of its own.

    Nothing here is configured. The structure comes from the framing and the
    children, the precedences from the binding powers, the fold regions from
    the matched pairs, and the captures from the scopes. The token
    tree-sitter extracts keywords against is worked out too: it is the
    pattern token whose language holds every keyword the grammar declares.

    [locals.scm] is the one output with a declaration on the grammar behind it.
    {!Core.Grammar.with_scope} and {!Core.Grammar.with_binder} say where a
    scope opens and which child positions introduce a name. Neither can be
    derived, and both are claims about the language rather than a colour a
    theme picks.

    {2 What is left out}

    [injections.scm], which needs a region tagged with the language written
    inside it. Nothing derives that. *)

module Js = Js
module Node = Node
module Grammar_js = Grammar_js
module Queries = Queries
module Check = Check

type output =
  { grammar_js : string
  ; highlights : string
  ; folds : string
  ; locals : string
  }

(** The token tree-sitter should do keyword extraction against: the pattern
    token whose language holds the text of every keyword the grammar
    declares.

    [None] where the grammar declares no keyword, or where no single pattern
    token holds them all. Both mean there is nothing for the directive to do.

    The test is exact. A keyword matters to tree-sitter's lexer only where
    the identifier pattern would otherwise swallow it, and that is a
    question about two languages. *)
val word_token : Core.Facts.t -> Core.Token.def option

val generate : Scopes.t -> language:string -> unit -> (output, Check.problem list) result
