(** One grammar, wired up once.

    A sweep draws, decodes, parses and formats several hundred thousand times.
    Everything that follows from the grammar alone -- the plan, the layout, the
    lexer's automaton, the species a rule became, the engine that draws from
    them -- is derived here and held, because deriving any of it per iteration
    is the shape of the predecessor's 10x regression (M3).

    The lexer comes from the caller. The automaton belongs to the grammar and
    this library holds none, so a harness takes the one its caller already
    lexes the rest of its corpus with, rather than growing a second. *)

(** Which engine draws a structure of a size.

    Both are in bolts and they answer different questions. The choice is not a
    preference: on grammars/rust_grammar.ml, rejection into a window of 20 to
    60 costs 5 ms a draw and reports no finite spread, and the tables cost
    7 us. *)
type engine =
  | Tables
  (** [Bolts.Exact]: counting tables per nonterminal, then a draw uniform
        over the structures of a size. Reproducible from a seed, which is what
        a law needs -- a count in a falsification record is only reproducible
        while the corpus is. *)
  | Measured
  (** [Bolts.Strategy.auto]: every candidate engine costed by a pilot run of
        it, and the cheapest taken. It measures to choose, so the corpus
        depends on what else the machine was doing, and a law cannot use it. A
        sweep can, and should: it is the whole point of choosing. *)

(** The engines a harness draws through. The root's is settled by {!make}; a
    rule's is built the first time {!draw_at} asks for it. *)
type draws

type t = private
  { name : string
  ; facts : Core.Facts.t
  ; plan : Ir.Plan.t
  ; entry : int (** Where a root parse enters {!Ir.Plan.t.rules}. *)
  ; root : Core.Rule.id
  ; layout : Ir.Layout.t
  ; system : Bolts.system
  ; map : Sample.map
  ; lex : string -> Lingo_runtime.Token.t array
  ; boundary : string -> int -> bool
  ; window : int * int (** The sizes {!draw} keeps an input inside. *)
  ; separators : string list
    (** The separator texts a body's policy may add. These are the only bytes
          a format holds that its tree did not, so every oracle over tokens
          quotients by them. *)
  ; respelt : bool array
    (** Indexed by kind: whether the formatter drops this token and writes the
          spacing again. [Reformat] trivia, and nothing else. A token the
          formatter keeps -- a comment, a doc comment -- reads [false]. *)
  ; draws : draws
  }

(** Raises [Invalid_argument] where the facts have no root with a body, and
    where the root's species cannot be built at these sizes. Both are
    conditions {!Core.Facts.of_grammar} has already ruled out or that bolts
    reports, and a harness that gave nothing back would leave a law reporting
    zero over a grammar it never drew.

    [comments] is passed to {!Sample.system_of}. At [0.] the system holds the
    meaningful tokens alone.

    [mean] is the size an input is drawn around, in tokens, and the window is
    half of it to one and a half times it, clipped to the sizes the species
    admits. *)
val make
  :  lex:(string -> Lingo_runtime.Token.t array)
  -> ?comments:float
  -> ?engine:engine
  -> ?mean:int
  -> name:string
  -> Core.Facts.t
  -> t

(** How the engine was settled, for a run to print beside its counts. *)
val engine_of : t -> string

(** One input, drawn from the root's species inside {!t.window}. *)
val draw : t -> Random.State.t -> Sample.token list

(** The sizes a rule's species has, clipped to what a mutation may splice.
    [None] for a rule with no species -- an expression block's role, which
    nothing references -- and for one whose tables bolts declined to build. *)
val sizes_at : t -> Core.Rule.id -> (int * int) option

(** One structure at a rule, of exactly [size] tokens. [None] where the rule
    has no species, or no structure of that size.

    The engine is built on first use and held. A grammar with sixty rules
    builds sixty sets of tables, and on grammars/effekt_grammar.ml that is
    3.1 s once against 0.16 s for twenty thousand draws from them. Building
    them all up front is what makes that cost fall on a law that never
    mutates. *)
val draw_at : t -> Core.Rule.id -> size:int -> Random.State.t -> Sample.token list option

(** The tokens as source text. *)
val decode : t -> Sample.token list -> string

(** [cover] is given one point per cursor read. It costs a hashtable write per
    read, so a sweep measuring volume leaves it out and a run measuring reach
    passes it. *)
val parse
  :  ?cover:Coverage.t
  -> t
  -> string
  -> Siesta.Green.node * Lingo_runtime.Diagnostic.t list

val doc : t -> Siesta.Green.node -> Ir.Kind.t Handsome.Utf8.t
val format : t -> width:int -> Siesta.Green.node -> string

(** The tokens of a tree that reach the formatter's output: every one whose
    bytes the fold writes rather than re-emits. A comment is here; whitespace
    is not, and neither is a token recovery inserted, which carries no bytes.

    A pair rather than a {!Sample.token}, because {!Core.Kind.t} is abstract
    and nothing outside a draw can build one. The kind is the integer siesta
    records. *)
val written : t -> Siesta.Green.node -> (int * string) list

(** The same reading of a source string: what the lexer gives, less the
    whitespace {!decode} wrote. So [relex (decode ts) = of_tokens ts] wherever
    the joiners held, and that equality is what says a mutated token list and
    the bytes it decoded to are the same thing. *)
val relex : t -> string -> (int * string) list

(** A draw read the way {!written} and {!relex} read a tree and a string. *)
val of_tokens : Sample.token list -> (int * string) list

(** The rule whose species a node of this kind is drawn from. An expression
    block's role gives the block: a role describes the shape of a node the
    block's parse builds, nothing references it, and the system holds no
    species for it. [None] for a token kind, a hole and an error node. *)
val rule_of : t -> Ir.Kind.t -> Core.Rule.id option
