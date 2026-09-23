(** Cutting a witness down to something a person can read.

    A class keeps the shortest input that reached it, and the shortest of
    thousands still runs to ninety bytes of rust: the corpus is drawn around
    forty tokens. The three separator defects test/laws/law_fuzz.ml records as
    M17 to M19 were each reduced by hand, over several runs of a throwaway
    probe, before anybody could say what they were.

    {1 Two reductions}

    Which one runs follows from the claim a finding breaks.

    Only a clean input shows that the corpus is clean, so a witness for that
    claim has to stay a draw. Those are reduced by editing the trace that drew
    them, because bolts replays any edit of a trace to a structure of the same
    species.

    Everywhere else a broken witness is a fine witness, and the token list is
    reduced directly. That reaches further, because a token list can lose a
    node the grammar requires.

    {1 Where a mutant is reduced from}

    A mutator edits the token list after the draw, so a mutant's trace
    describes the draw the mutation was applied to. Replaying an edit of that
    trace gives an input that usually does not reproduce. The bytes are what
    reproduce, and {!Harness.tokens_of} reads them back. Pigeon's
    [fuzz_driver.mli] records the same finding.

    {1 What it costs}

    It runs once per class, over the witness the class already kept. A class
    fires thousands of times and reducing each occurrence costs more than
    drawing the corpus does. A run with no findings pays nothing. *)

(** What the moves edit. *)
type edit =
  | Trace of Harness.trace
  (** The decisions that drew the input. Every candidate is a draw from the
          same species, so the reduced witness still supports a claim about the
          corpus. *)
  | Tokens
  (** The token list the bytes lex back to, through {!Harness.tokens_of}.
          An input no draw produced has only this one. *)

type reduced =
  { tokens : Sample.token list
    (** The reading the moves edited. Empty where the witness gave none, which
            is where {!Harness.tokens_of} met a byte the grammar does not
            declare. *)
  ; src : string
    (** {!Harness.decode} of [tokens], and the witness itself where it gave none. *)
  ; before : int (** Bytes in. *)
  ; after : int (** Bytes out. *)
  ; moves : int (** Candidates that stuck. *)
  ; tried : int (** Candidates [holds] was asked about. *)
  ; capped : bool
    (** Whether [limit] stopped the loop. A reduction that stopped on its
            own has no further move to make. *)
  ; reproduces : bool
    (** [holds] on what came back, asked once more at the end. Every move
            was taken because [holds] admitted it, so this reads [false] only
            where the reduction is broken or the witness never reproduced. *)
  }

(** [reduce t edit ~holds src] applies reducing moves to [src] until none is
    left or [limit] runs out.

    [holds] is given a candidate and the bytes it decodes to, and says whether
    the finding is still there. It is asked once more about the witness that
    comes back, and about a witness no move could touch. Make it the class key
    and the widths the run uses: a weaker predicate reduces off the finding and
    onto a different one.

    [at] is the rule a candidate is parsed at, and it defaults to the root. A
    fragment is read at the rule it was drawn at, and reading it at the root
    builds another tree.

    [limit] is how many candidates [holds] may be asked about, and it defaults
    to 1,200. Each candidate costs a parse and a format at every width, so an
    unbounded loop over a deep sweep's witness costs more than the sweep. *)
val reduce
  :  Harness.t
  -> ?at:Core.Rule.id
  -> ?limit:int
  -> edit
  -> holds:(Sample.token list -> string -> bool)
  -> string
  -> reduced
