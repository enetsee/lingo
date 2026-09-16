(** lingo's core. It holds the grammar a user writes, and the derivation
    every backend starts from.

    {v
         Grammar.t         the grammar: data, no queries
             |
      Facts.of_grammar     checks it, then derives from it
             |
          Facts.t          kinds, tokens, rules, FIRST/FOLLOW/nullable,
             |             the lexer automaton, the emitted-name manifest
       +-----+-----+---- … every backend sits here
    v}

    A {!Facts.t} comes from {!Facts.of_grammar} - it is a private record, and 
    [of_grammar] is the only way to construct it so a backend that takes one is
    guaranteed to have a valid grammar.

    Every name is resolved at that boundary. Downstream code holds {!Kind.t}s
    and {!Rule.type-id}s. Both {!Kind.t} and {!Manifest.name} are abstract,
    so both can only have come from a {!Facts.t}. The author's own names
    travel too, for diagnostics. Each one carries the namespace it was
    written in; see {!Grammar.Name}.

    {1 Writing a grammar}

    Use {!Grammar}, starting at {!Grammar.create}. It is its own library,
    [lingo.grammar], and it depends on redfa alone. A file that holds a
    grammar and nothing else links those two, and leaves the checker out.

    {!Grammar} is re-exported here because {!Rule.def} names its types. A
    backend reading a child's modifier or a production's framing needs those
    types, and a {!Facts.t} brings them along.

    {1 Checking one}

    Call {!Facts.of_grammar}. It answers with a {!Facts.t}, or with a list
    of {!Error.t}. {!Error.codes} lists every rejection it can report.

    {1 Reading the facts}

    {!Facts} holds the record and the queries over it. {!Kind}, {!Token},
    {!Rule}, {!Role} and {!Block} describe what is in it. {!Manifest} holds
    the identifiers a backend emits, and {!Mangle} the manglings behind
    them. {!Lexer} flattens the token automaton into the arrays an emitter
    writes out.

    {1 What lives elsewhere}

    Anything that turns facts into an artefact lives elsewhere: a parser, a
    formatter, a lexer, editor integrations, the sampler.

    Each of those owns its own IR, and the phase that produces it. Each
    depends on this library, and on nothing else of ours. *)

module Error = Error
module Mangle = Mangle
module Grammar = Grammar
module Kind = Kind
module Token = Token
module Rule = Rule
module Role = Role
module Block = Block
module Manifest = Manifest
module Facts = Facts
module Lexer = Lexer

(** The staged derivation and the checks over it.

    These are here for the test suite. It runs the stages one at a time and
    checks that each rejection is reported at the earliest stage that can
    answer it. What they hand out is a partly derived grammar. *)
module Internal : sig
  module Stage = Stage
  module Fixpoint = Fixpoint
  module Check_names = Check_names
  module Check_shape = Check_shape
  module Check_full = Check_full
end
