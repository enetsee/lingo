(* -- the fuzz harness ---------------------------------------------------------

      (a) The corpus is a corpus of clean draws, and every oracle reads zero
          over it.
      (b) Every oracle reads zero over the corpus once it is mutated.
      (c) An edge is a transition: the points saturate and the edges climb.
      (d) Every mutator is reached, and every shape at an alternation is
          swapped out.
      (e) Every skip class is inside its ceiling.
      (f) Every rule the grammar can reach is one the corpus built, and every
          oracle reads zero over the fragments drawn to reach the rest.
      (g) Every witness a class carries reproduces on its own, and no further
          move reduces it.

      Mechanism. Eleven grammars from grammars/ and the witness grammars
      beside them, less the five that declare no whitespace. For each one a
      harness: the plan, the layout, the species a rule became, and an engine
      over them. A draw is a token list, [Sample.decode] writes the bytes, and
      the oracles run on those bytes at three widths, two fixed and one drawn.
      Then a mutator edits the token list and the same oracles run again. A
      class that fires keeps the shortest input that reached it, and each of
      the ten a report prints is reduced before it is printed.

      There are two corpora here and they are drawn differently. One is a draw
      from a root, which is a whole program. The other is a draw at a rule,
      which is a fragment, and part (f) is where it is built and what it is
      for. Every law runs over both.

      Part (a) is the phase's first claim and the oldest hole in the project.
      Every oracle here is a forbidding law, so the way one fails badly is by
      firing on an input that is fine, and nothing inside an oracle can see
      that. What sees it is a corpus that is known good. Its first half is what
      makes it known: every input lexes back to the tokens drawn and parses
      with no diagnostics. test/laws/law_sample.ml says the same of the
      fourteen grammars it draws; rust and effekt are not among them, and for
      those this is the only place it is said.

      Part (b) is the same oracles over inputs no sampler would draw. 2,583 of
      7,000 reach recovery, against 46% for the corpus test/sweep generates:
      a mutated draw is near the language and a swept input is a mangled one,
      so the two corpora are complements and the sweep does not go away.

      Part (c) is the predecessor's own coverage claim, ported. A point is a
      place in the plan where a parse read the cursor, and an edge is a pair of
      consecutive points. The claim is what separates the two counts: doubling
      the corpus adds 464 points to 1,663 and 1,058 edges to 3,287, so the
      points are a property of the plan and the edges are still finding things.
      The predecessor counted points, saturated at about a thousand iterations,
      and ran a million-sample sweep for ten issues with a flat number beside
      it.

      The threshold in (c) is half the points the first half found. It is a
      threshold and it is worth saying where it sits: the point count here adds
      a quarter of itself, and keying on the whole parse stack rather than on
      {!Ir.Residual.State.site} adds twenty times itself, and an order of
      magnitude sits either side of the line.

      Part (d) has two halves and the second is the one with something to hide.
      A mutator that declines every call is invisible in every other number:
      the driver falls through to the next one, so it costs no skip and no
      failure, and the mutator that did fire absorbs the iteration. That is the
      first half. The second is a mutator that fires on every call and still
      reaches one arm of a four-arm slot -- the corpus parses, the oracles read
      zero, and [fired] reads the same either way. A slot whose every arm is a
      token is one no node can stand in, and a walk that saw only nodes would
      build the same arms and decline nothing extra.

      Part (f) is the one thing a corpus can be silently short of. Every law
      above it is a forbidding one, so a construct the corpus never builds
      leaves every count reading zero: the input that would have failed is not
      there. {!Fuzz.Harness.reachable} walks the grammar and
      {!Fuzz.Harness.built} walks a tree, and the two never consult each other.
      They are compared both ways. A rule the walk reaches and no tree holds is
      what the part is named for; a rule a tree holds and the walk never
      reaches would mean the walk is simply short.

      It read red, and what it read is worth keeping. Twelve of 238 rules at
      depth 8, and the same twelve at depth 32 -- 224,000 mutated inputs reach
      what 56,000 reach, to the rule. Depth took it from 27 at depth 1 to 12
      and then stopped dead, and a count that stops is the mark of something
      the generator cannot produce rather than something it produces rarely.

      The twelve named themselves. effekt's [MatchArm] and everything inside it
      -- [Pattern], [PatternArgs], [AltPattern], [Guard] -- and [Clause] inside
      a handler's body. [Match] and [Try] are built, so what the sampler drew
      was [match (x) {}] and [try {} with H {}] with empty bodies, every time.
      Both bodies are zero-or-more. An empty repetition costs no tokens and a
      [case p => e] arm costs five, so at any size the weight goes where the
      tokens are cheapest.

      Asking for a bigger input made it worse rather than better, which is the
      same fact from the other side: at a mean of 40 the corpus builds 130 of
      140 rules, at 120 it builds 128, and at 300 it builds 119 and rust starts
      losing rules it had. More tokens buy more of the shapes that were already
      cheap. [Bolts.Sampler.Profile] is the other lever and it does not reach
      these either: a single-target profile compiles and reads an expected
      occurrence of exactly 1.0, and the size distribution goes bimodal under
      it, so the median draw is three tokens and no usable draw holds the
      construct.

      What reaches them is to stop asking for a program that happens to hold
      one. A rule has a species of its own -- it is the species a mutator
      splices from -- so the fragment is drawn at the rule and
      {!Fuzz.Harness.parse} reads it back at the same rule. 7,050 fragments
      over 197 of the 238 rules. The 41 left out are all expression rules: 34
      are an expression block's roles, which have no species because nothing
      references them, and 7 are the block rules themselves, which open no node
      of their own and so cannot be entered ({!Fuzz.Harness.enterable}). So no
      fragment here is an expression on its own, and expressions are covered by
      the draws from a root, where every one of them appears.

      Every rule, rather than only the ones the draws from a root missed.
      Keying on the gap was tried and it is backwards: a deeper run's draws
      miss fewer rules, so the fragment corpus shrinks as the work grows and
      the law gets weaker. It cost two of the separator defects their
      falsification at depth 8 while depth 1 still caught both.

      Each fragment is then mutated, and two mutants are dropped rather than
      run. Both are about what a parse entering at a rule does differently from
      one entering at a root, and each was found by a Law B finding that was
      the harness rather than the fold.

      The first is dispatch. A fragment parse forces its entry rule; every rule
      below it is still dispatched on what is under the cursor. So a mutant
      that deletes a nested opener builds no node there, which is ordinary, and
      one that deletes the entry rule's own opener builds a delimited node with
      nothing where its opener goes -- which no parse from a root can build,
      because a root only enters the rule when its opener is under the cursor.
      Nine Law B findings came from that shape.

      That shape was counted rather than argued. Over 16,610 trees from the
      draws at a root, clean and mutated, no delimited node is missing its
      opener; 61 of the 338 mutants this drops hold one. The test is broader
      than the shape, though: the other 277 are mutants of a rule with no
      delimiters at all, refused because the first token is not one the rule
      starts with. That is 5% of the fragment mutants, and no rule loses all of
      its.

      The second is the end of the input. A parse entering at a rule ends where
      the rule does, so a mutant the rule cannot take in full leaves a tail
      outside the tree. That is two inputs rather than one, and every law here
      is about one tree and the bytes it holds. 6,001 of 6,694 mutants are
      taken in full and 1,449 of those recover.

      The fragments are worth what they cost. Two of the separator defects read
      zero over the mutated corpus at every depth now, and 27 and 14 over the
      fragments at depth 1 and 195 and 147 at depth 8, so for those two the
      fragments are the whole of the falsification. The witnesses are shorter
      by an order of magnitude as well: [{//\nnh(h,chi}] against a 180-byte
      rust program.

      Part (g) is about reading the witnesses the other six leave. A class
      keeps the shortest input that reached it, and the shortest of thousands
      is 90 bytes of rust. The three separator defects were each reduced by
      hand before anybody could say what they were, and a throwaway probe was
      written to do the reducing. {!Fuzz.Shrink} does it in the run that finds
      them, and those 90 bytes come out as [fn{a(//\nN{})].

      There are two reductions here, and which one runs follows from the claim
      a finding breaks. Part (a) says the corpus is clean, so a witness for it
      has to stay a clean draw: the reduction edits the trace the draw was
      recorded under, and every replay of an edited trace is a draw. Parts (b)
      and (f) run on mutants and on fragments, where a broken witness is a fine
      witness, so the token list is edited directly. Collapsing the two leaves
      16 of 26 classes with a witness that no longer reproduces: the class
      there is the decoded input failing to lex back to the tokens drawn, and
      reading a witness back through the lexer makes the two agree by
      construction.

      The claim has two halves and a mutation reddens each. Taking a candidate
      whatever the predicate says reduces every witness to [""], and nothing
      reproduces. Admitting a candidate that is no smaller stops two of thirty
      reductions at the cap rather than running out of moves. The cap is 1,200
      candidates a class.

      The corpus. 3,500 clean inputs, 7,000 mutated and 7,050 fragments with
      6,694 mutants of their own, over 14 corpora holding 4,422 comments, in
      13.0 s of processor time on the machine this record was written on. Every
      grammar with [Preserve] trivia is drawn twice, once from a system that
      draws comments and once from one that does not, so Law B is asked of
      comment placement as well as of spacing.

      Five witness grammars are left out and the run names them. They declare
      no whitespace token, so the lexer has nothing to read a joiner as: every
      space the formatter writes comes back an error token and every law below
      it reports on the grammar rather than on the fold. That is by design --
      no sample of theirs ever needs a joiner -- and it is what makes them
      unformattable.

      The engine is [Bolts.Exact], fixed, with the seed fixed beside it, so the
      corpus is the same on every run and a count below is reproducible.
      [Strategy.auto] is the other engine and it measures to choose, which is
      right for a sweep and wrong for a law; [LINGO_FUZZ_ENGINE=measured]
      selects it.

      Every clean draw is recorded as it is made, and part (g) edits the
      recording. It costs 0.3 s of the 54 at depth 8 and nothing measurable at
      depth 1, and the corpus is the same either way. [Strategy.auto] records
      nothing, so under it part (g) reduces part (a)'s witnesses by editing the
      token list. The oracle half still reduces, and the half about a draw
      lexing back to its tokens does not, because reading a witness back
      through the lexer makes the two agree.

      What this says nothing about. Whether the emitted parser and the emitted
      formatter agree with the interpreter and the fold on this corpus, which
      is test/parse_emit and test/format_emit over the corpus test/sweep
      generates. Nor whether the system is the grammar for rust and effekt: the
      structure count, the nullability and the minimum size are law_sample's
      cross-checks and they are asked of the fourteen grammars it draws.

      No defect is open. Three were, all in the separator a body's policy adds,
      and the foot of this record says what they were.

      Three ways the separator a body's policy adds comes back somewhere the
      fold cannot see it: in front of the last element, inside an error node, or
      inside a frame the parse left open at the body's end. Each leaves the fold
      writing another on the next pass, and one of the three grows without
      bound. Only the first is reachable from a root at all, and only through
      the mutated corpus. The other two need a fragment, which is the whole
      argument for part (f): both were open defects that the corpus before it
      reached at depth 8 and stopped reaching when the corpus moved.

      What part (a) is for. An oracle that refuses the separator a body's policy
      adds, comparing for equality where it should compare for a subsequence,
      reads thousands of findings over a corpus that is good, and every other
      part reads exactly what it read before. A forbidding oracle fails by
      firing on input that is fine, and nothing inside an oracle can see that.

      Why both parses enter at the same rule. A fragment read from the root is a
      program that starts with a construct rather than the construct, so the
      parse recovers rather than builds; and a reparse read at the root where
      the first was read at a rule compares two readings of different grammars.
      Part (f) holds both ends to [at].

      Why the shrinker's check is a fresh call to the predicate. The witness
      that comes back is asked again, so the claim does not depend on what the
      reduction loop did on the way there.

      Why coverage keys on the site. A parse stack grows with the input's
      nesting, so distinct stacks track the corpus however little of the plan a
      parse reached: they read twenty times what the site reads and say nothing
      about the plan.

      What only a golden covers. No law here says a body that broke carries its
      separator: Law C allows one and does not require it, and a fold that never
      writes one is stably idempotent. A fold that never writes one moves
      sixteen lines of [comments.format] and nothing else in either law. The
      golden is the whole of what covers that, and it is worth knowing that the
      golden is the only thing there.

      A childless node is what the parse leaves where it wanted a token and
      found none, so only a recovered tree holds one and only a mutated input
      reaches it. Part (a) reads zero for anything done to it.

      Depth. 1 is what the suite runs. [LINGO_SWEEP=8] is 28,000 clean inputs,
      56,000 mutated and 56,509 fragments in 54 s; 16 is 112,000 mutated and 32
      is 224,000. Depth multiplies the draws and not the rules: the same 197
      rules are drawn at whatever the depth, which is what keeps the fragments
      a property of the grammar. Every law reads zero at every depth, on all
      three corpora.

      Depth is what found the first of the three separator defects. It reads 4
      findings at depth 1 and 10 at 8, and the shortest witness is 90 bytes of
      rust at depth 1 and 77 of effekt at 8. Neither is a thing anyone would
      look at twice. Part (g) takes them to 12 and 16 bytes.

      Nothing is open. The Law B findings this law carried until 2026-09-22
      were three defects in the separator a body's policy adds. Each left the
      fold writing a separator the next parse put somewhere the fold could not
      see, so the pass after wrote another.

   -------------------------------------------------------------------------- *)

(* The six blocks below are generated, and they are the evidence. assay derives
   a mutation from the code rather than from a sentence beside it, applies every
   one, and records what went red. Regenerate them with

     assay -config assay.conf -only fuzz

   and take the counts as they come: they move whenever the corpus grows, and
   asserting them exactly would train everyone to ignore a red suite. What they
   assert is that every mutant dies. A survivor is the finding, and the lines it
   names are where to look.

   These are the starkest records in the suite, and the reason is worth saying.
   This is the instrument: the laws above check what it finds, and parts (c),
   (d) and (e) are what check the instrument itself. They do not reach far.
   lib/fuzz/shrink.ml and lib/fuzz/report.ml have nothing watching them at all.

   The fold this law reads is lib/runtime/layout.ml, and its record lives with
   the law that checks it, in test/laws/law_layout.ml. *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/fuzz/harness.ml, 103 mutants, 75 killed, 25 survived, 3 timed out.
        extreme     30  23 killed, law_fuzz (d): 7, law_fuzz (f): 5, law_fuzz calc:: 3, law_fuzz comments: 3, law_parse sexp:: 2, law_fuzz: 1, law_fuzz calc: 1, law_fuzz ml+comments: 1; 7 survived
        sbr         27  21 killed, law_fuzz (f): 14, law_fuzz comments: 3, law_fuzz effekt+comments:: 2, law_fuzz: 1, law_fuzz (d): 1; 4 survived; 2 timed out
        ror         18  9 killed, law_fuzz (f): 2, law_fuzz: 1, law_fuzz (d): 1, law_fuzz calc:: 1, law_fuzz comments: 1, law_fuzz ml+comments: 1, law_parse: 1, law_parse sexp:: 1; 9 survived
        lcr          6  4 killed, law_fuzz: 2, law_fuzz ml+comments: 1, law_fuzz rust: 1; 2 survived
        aor          8  4 killed, law_fuzz: 2, law_parse unicode:: 2; 3 survived; 1 timed out
        uoi         14  all killed, law_fuzz: 3, law_parse sexp:: 3, law_fuzz (d): 2, law_fuzz (f): 2, law_fuzz calc:: 2, law_fuzz comments: 1, law_fuzz rassoc: 1
      survived at lines 51 57 58 66 130 153 189 190 200 209 253 263 290 298 340 342 344 456 468 481 485
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/fuzz/mutate.ml, 131 mutants, 75 killed, 56 survived.
        extreme     23  17 killed, (d) 13 (c) 2 ml+comments 2; 6 survived
        sbr         35  19 killed, law_fuzz (d): 8, law_fuzz: 7, law_fuzz ml+comments: 4; 16 survived
        ror         30  12 killed, law_fuzz (d): 7, law_fuzz ml+comments: 4, law_fuzz: 1; 18 survived
        lcr         11  7 killed, law_fuzz ml+comments: 4, law_fuzz: 2, law_fuzz (d): 1; 4 survived
        aor         13  7 killed, law_fuzz: 3, law_fuzz (d): 3, law_fuzz ml+comments: 1; 6 survived
        uoi         19  13 killed, law_fuzz (d): 7, law_fuzz ml+comments: 4, law_fuzz: 1, law_fuzz (c): 1; 6 survived
      survived at lines 41 42 43 44 86 95 100 124 125 132 136 139 141 151 191 204 221 239 253 280 285 301 309 313 314 321 343 350 361 378 382 388 408 422 423 427 451 454 469 487 493
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/fuzz/oracles.ml, 53 mutants, 11 killed, 42 survived.
        extreme     11  2 killed, calc 2; 9 survived
        sbr         26  0 killed; 26 survived
        ror          4  1 killed, recovery 1; 3 survived
        lcr          1  all killed, recovery 1
        aor          2  0 killed; 2 survived
        uoi          9  7 killed, calc 6 recovery 1; 2 survived
      survived at lines 41 43 57 73 75 76 77 82 106 115 117 133 139 143 145 148 158 168 169 170 176 177 184 188 191 194 201 202
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/fuzz/shrink.ml, 36 mutants, 0 killed, 36 survived.
        extreme      3  0 killed; 3 survived
        sbr         11  0 killed; 11 survived
        ror          8  0 killed; 8 survived
        lcr          2  0 killed; 2 survived
        aor          7  0 killed; 7 survived
        uoi          5  0 killed; 5 survived
      survived at lines 58 64 66 73 76 77 79 84 85 86 89 135 136 137 152 164 173 175 182 183 194 198
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/fuzz/coverage.ml, 12 mutants, 9 killed, 1 survived, 2 timed out.
        extreme      5  4 killed, (c) 3 (d) 1; 1 timed out
        sbr          5  3 killed, (c) 3; 1 survived; 1 timed out
        uoi          2  all killed, (c) 2
      survived at lines 19
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/fuzz/report.ml, 18 mutants, 0 killed, 18 survived.
        extreme      4  0 killed; 4 survived
        sbr          2  0 killed; 2 survived
        ror          4  0 killed; 4 survived
        aor          4  0 killed; 4 survived
        uoi          4  0 killed; 4 survived
      survived at lines 14 19 23 26 28 29 32 43 46 57 60 61
   ---------------------------------------------------------------------- *)

let first_ten l = List.filteri (fun i _ -> i < 10) l

(* -- the run --------------------------------------------------------------- *)

let depth =
  match Sys.getenv_opt "LINGO_SWEEP" with
  | None -> 1
  | Some s ->
    (try int_of_string s with
     | _ -> 1)
;;

let seed = [| 0xF0221 |]

(* Two fixed and one drawn. Law W compares the token runs across them, so three
   gives it two comparisons where two gave it one.

   The fixed pair is the ruler anybody formats at and a narrow one, and it is
   fixed so that a count in the record below means the same thing from one
   depth to the next. Drawing all three instead was tried: it raised the widths
   the corpus reaches and halved the rate at which it finds the three separator
   defects, from 71 per million to 40, because the widths those need came up a
   third as often. Spread is worth having and it is worth having beside a
   baseline rather than instead of one.

   The draw is from the run's own seeded generator, so the corpus is the same
   on every run and a count below is reproducible. *)
let width_pool = [| 1; 2; 3; 4; 6; 8; 12; 16; 28; 40; 60; 120 |]

let widths (rng : Random.State.t) : int list =
  [ 80; 20; width_pool.(Random.State.int rng (Array.length width_pool)) ]
;;

(* Every width the run lays out at. A class is keyed on the law rather than on
   a width, so a reduction tests whether the same law still fires anywhere in
   the set. Redrawing one width per candidate leaves the loop chasing a class
   that comes and goes. *)
let every_width : int list = 80 :: 20 :: Array.to_list width_pool
let iterations = 250 * depth
let comment_density = 0.15

(* [Tables] unless the caller asks otherwise. A law fixes its engine because a
   count in the record below is only reproducible while the corpus is, and
   [Strategy.auto] costs its candidates by running them. *)
let engine =
  match Sys.getenv_opt "LINGO_FUZZ_ENGINE" with
  | Some "measured" -> Fuzz.Harness.Measured
  | Some _ | None -> Fuzz.Harness.Tables
;;

(* -- the grammars ---------------------------------------------------------- *)

let grammars : (string * Core.Grammar.t) list =
  Sample_corpus.all
  @ [ "rust", Lingo_grammars.Rust_grammar.grammar
    ; "effekt", Lingo_grammars.Effekt_grammar.grammar
    ]
;;

type corpus =
  { label : string
  ; h : Fuzz.Harness.t
  ; mut : Fuzz.Mutate.t
  ; cover : Fuzz.Coverage.t
  }

let has_comments (f : Core.Facts.t) : bool =
  Array.exists
    (fun (tok : Core.Token.def) -> tok.trivia = Some Core.Grammar.Preserve)
    f.tokens
;;

(* A formatter writes spacing, so a grammar whose lexer has nothing to lex a
   space as cannot have its output read back: every joiner comes out an error
   token, and every law below it reports on the grammar rather than on the
   fold. Five of the witnesses are that shape by design -- their whole point is
   that no sample of theirs ever needs a joiner -- and they are left out here
   and named where the count is printed. *)
let formattable (f : Core.Facts.t) : bool =
  Array.exists
    (fun (tok : Core.Token.def) -> tok.trivia = Some Core.Grammar.Reformat)
    f.tokens
;;

let unformattable = ref []

let build (label : string) (facts : Core.Facts.t) (comments : float) : corpus option =
  match Fuzz.Harness.make ~lex:(Lex.run facts) ~comments ~engine ~name:label facts with
  | h -> Some { label; h; mut = Fuzz.Mutate.create h; cover = Fuzz.Coverage.create () }
  | exception e ->
    Law.fail "%s: no harness (%s)" label (Printexc.to_string e);
    None
;;

let corpora : corpus list =
  List.concat_map
    (fun (name, grammar) ->
       match Core.Facts.of_grammar grammar with
       | Error _ ->
         Law.fail "%s: the grammar was rejected" name;
         []
       | Ok facts when not (formattable facts) ->
         unformattable := name :: !unformattable;
         []
       | Ok facts ->
         List.filter_map
           Fun.id
           [ build name facts 0.
           ; (if has_comments facts
              then build (name ^ "+comments") facts comment_density
              else None)
           ])
    grammars
;;

(* -- what a finding is reported as ----------------------------------------- *)

(* Which corpus a finding came from, and the trace of the draw behind it where
   the draw was traced. A report holds a class key and a witness, and neither
   of them carries the harness that reads it back. Written per finding and read
   once per class, so a run with no findings writes nothing. *)
let context : (string * string, corpus * Fuzz.Harness.trace option) Hashtbl.t =
  Hashtbl.create 8
;;

let keep
      (report : Fuzz.Report.t)
      (c : corpus)
      ?(at : Core.Rule.id option)
      ?(trace : Fuzz.Harness.trace option)
      (key : string)
      (src : string)
  : unit
  =
  Hashtbl.replace context (key, src) (c, trace);
  Fuzz.Report.note report ?at key ~witness:src
;;

let note
      (report : Fuzz.Report.t)
      (c : corpus)
      ?(at : Core.Rule.id option)
      ?(trace : Fuzz.Harness.trace option)
      (src : string)
      (f : Fuzz.Oracles.finding)
  : unit
  =
  keep report c ?at ?trace (Printf.sprintf "%s %s" c.label (Fuzz.Oracles.describe f)) src
;;

let tally (counts : (string, int) Hashtbl.t) (f : Fuzz.Oracles.finding) : unit =
  let law = Fuzz.Oracles.law f in
  Hashtbl.replace counts law (1 + Option.value (Hashtbl.find_opt counts law) ~default:0)
;;

let per_law (counts : (string, int) Hashtbl.t) : string =
  String.concat
    " "
    (List.map
       (fun law ->
          Printf.sprintf
            "%s %d"
            law
            (Option.value (Hashtbl.find_opt counts law) ~default:0))
       Fuzz.Oracles.laws)
;;

(* Part (g) reads this at the end, which is the one place the claim over all
   three reports can be made. *)
let reductions : (string * Fuzz.Shrink.reduced) list ref = ref []
let unreduced = ref 0

(* A class reduces where a context was kept for it. [None] is a class noted by
   a path that kept none, and part (g) fails on it: a witness nobody can re-run
   is a witness nobody can reduce either. *)
let cut_down
      ~(draw : bool)
      ~(holds : corpus -> Fuzz.Report.klass -> Sample.token list -> string -> bool)
      (k : Fuzz.Report.klass)
  : Fuzz.Shrink.reduced option
  =
  match Hashtbl.find_opt context (k.key, k.witness) with
  | None -> None
  | Some (c, trace) ->
    let edit =
      match draw, trace with
      | true, Some trace -> Fuzz.Shrink.Trace trace
      | true, None | false, _ -> Fuzz.Shrink.Tokens
    in
    Some (Fuzz.Shrink.reduce c.h ?at:k.at edit ~holds:(holds c k) k.witness)
;;

let rule_at (c : corpus) (at : Core.Rule.id) : string =
  Core.Grammar.Name.Rule.to_string c.h.facts.rules.(at).name
;;

let show
      (report : Fuzz.Report.t)
      ~(reduce : Fuzz.Report.klass -> Fuzz.Shrink.reduced option)
  : unit
  =
  List.iter
    (fun (k : Fuzz.Report.klass) ->
       let where =
         match k.at, Hashtbl.find_opt context (k.key, k.witness) with
         | Some at, Some (c, _) -> Printf.sprintf " read at %s" (rule_at c at)
         | Some _, None | None, _ -> ""
       in
       match reduce k with
       | None ->
         incr unreduced;
         Law.fail "%s, %d times, shortest %S%s" k.key k.count k.witness where
       | Some cut ->
         reductions := (k.key, cut) :: !reductions;
         Law.fail
           "%s, %d times, shortest %S%s, %d bytes from %d in %d moves of %d candidates%s"
           k.key
           k.count
           cut.src
           where
           cut.after
           cut.before
           cut.moves
           cut.tried
           (if cut.capped then ", capped" else ""))
    (first_ten (Fuzz.Report.classes report))
;;

(* The class key a candidate has to keep. *)
let fires (c : corpus) (k : Fuzz.Report.klass) (_ : Sample.token list) (src : string)
  : bool
  =
  List.exists
    (fun (f : Fuzz.Oracles.finding) ->
       String.equal (Printf.sprintf "%s %s" c.label (Fuzz.Oracles.describe f)) k.key)
    (Fuzz.Oracles.run c.h ?at:k.at ~widths:every_width src)
;;

(* -- (a) the corpus is good, and every oracle reads zero over it ------------ *)

(* Which rules each corpus built, over both halves. Part (f) reads it at the
   end, so every draw and every mutant counts toward it. *)
let raised : (string, (Core.Rule.id, unit) Hashtbl.t) Hashtbl.t = Hashtbl.create 32

(* A rule a tree holds that the walk over the grammar never reached. Part (f)
   compares the two walks one way; this is the other way, and it is what says
   the walk is not simply short. *)
let unreached : (string, (Core.Rule.id, unit) Hashtbl.t) Hashtbl.t = Hashtbl.create 8

let note_built (c : corpus) (tree : Siesta.Green.node) : unit =
  let seen =
    match Hashtbl.find_opt raised c.label with
    | Some t -> t
    | None ->
      let t = Hashtbl.create 64 in
      Hashtbl.replace raised c.label t;
      t
  in
  let walked = Fuzz.Harness.reachable c.h in
  List.iter
    (fun id ->
       Hashtbl.replace seen id ();
       if not (List.mem id walked)
       then (
         let missed =
           match Hashtbl.find_opt unreached c.label with
           | Some t -> t
           | None ->
             let t = Hashtbl.create 4 in
             Hashtbl.replace unreached c.label t;
             t
         in
         Hashtbl.replace missed id ()))
    (Fuzz.Harness.built c.h tree)
;;

let good = Fuzz.Report.create ()
let sound = Fuzz.Report.create ()
let sound_counts : (string, int) Hashtbl.t = Hashtbl.create 8
let clean_inputs = ref 0
let clean_comments = ref 0

(* The corpus is only known good while this gives nothing back. law_sample
   states the same two over its own grammars; rust and effekt are not among
   them, so for those this is the only place it is said.

   It gives the keys rather than writing the reports, because part (g) reduces
   a witness under the same two checks and has to arrive at the same class. *)
let uncleanness
      (c : corpus)
      (tokens : Sample.token list)
      (src : string)
      (diags : Lingo_runtime.Diagnostic.t list)
  : string list
  =
  (match diags with
   | [] -> []
   | first :: _ ->
     [ Printf.sprintf
         "%s: %s"
         c.label
         (Format.asprintf "%a" Lingo_runtime.Diagnostic.pp first)
     ])
  @
  if Fuzz.Harness.relex c.h src <> Fuzz.Harness.of_tokens tokens
  then
    [ Printf.sprintf "%s: the decoded input does not lex back to the tokens drawn" c.label
    ]
  else []
;;

let still_unclean
      (c : corpus)
      (k : Fuzz.Report.klass)
      (tokens : Sample.token list)
      (src : string)
  : bool
  =
  let _, diags = Fuzz.Harness.parse c.h src in
  List.mem k.key (uncleanness c tokens src diags)
;;

(* Part (a) is a claim over a corpus that is good, so a witness for it has to
   stay a clean draw. Editing the trace keeps every candidate a draw; editing
   the token list instead does not. *)
let fires_on_clean
      (c : corpus)
      (k : Fuzz.Report.klass)
      (tokens : Sample.token list)
      (src : string)
  : bool
  =
  let _, diags = Fuzz.Harness.parse c.h src in
  uncleanness c tokens src diags = [] && fires c k tokens src
;;

(* The draw is recorded, so that a finding over the clean corpus reduces by
   editing the decisions that drew it and every candidate stays a draw.
   [Strategy.auto] records nothing, so under it the trace is [None] and part
   (g) falls back to editing the token list. *)
let draw_one (c : corpus) (rng : Random.State.t)
  : Fuzz.Mutate.subject * string * Fuzz.Harness.trace option
  =
  let tokens, trace =
    match Fuzz.Harness.draw_traced c.h rng with
    | Some (tokens, trace) -> tokens, Some trace
    | None -> Fuzz.Harness.draw c.h rng, None
  in
  let src = Fuzz.Harness.decode c.h tokens in
  let tree, diags = Fuzz.Harness.parse c.h src in
  List.iter
    (fun (key : string) -> keep good c ?trace key src)
    (uncleanness c tokens src diags);
  List.iter
    (fun (tok : Sample.token) ->
       if Core.Facts.is_trivia_kind c.h.facts tok.kind then incr clean_comments)
    tokens;
  incr clean_inputs;
  note_built c tree;
  { Fuzz.Mutate.tokens; tree }, src, trace
;;

let () =
  List.iter
    (fun c ->
       let rng = Random.State.make seed in
       for _ = 1 to iterations do
         let _, src, trace = draw_one c rng in
         List.iter
           (fun f ->
              tally sound_counts f;
              note sound c ?trace src f)
           (Fuzz.Oracles.run c.h ~widths:(widths rng) src)
       done)
    corpora
;;

let () =
  (match Fuzz.Report.classes good with
   | [] ->
     Law.pass
       "(a) the corpus is good, over %d inputs in %d corpora holding %d comments (%s \
        declare no whitespace and are left out)"
       !clean_inputs
       (List.length corpora)
       !clean_comments
       (String.concat " " (List.rev !unformattable))
   | _ ->
     show good ~reduce:(cut_down ~draw:true ~holds:still_unclean);
     Law.fail "(a) %d inputs of the corpus are not clean draws" (Fuzz.Report.total good));
  Printf.printf
    "DRAWN by %s; findings over the good corpus %s\n"
    (match corpora with
     | c :: _ -> Fuzz.Harness.engine_of c.h
     | [] -> "nothing")
    (per_law sound_counts);
  match Fuzz.Report.classes sound with
  | [] -> Law.pass "(a) every oracle reads zero over it"
  | _ ->
    show sound ~reduce:(cut_down ~draw:true ~holds:fires_on_clean);
    Law.fail
      "(a) %d findings over a corpus that is good, so an oracle over-fires"
      (Fuzz.Report.total sound)
;;

(* What (a) had read by the time it reported. The sweep below draws fresh
   inputs of its own and they go through the same two checks, so a corpus that
   stops being good partway down would otherwise land in a report nobody reads
   again. *)
let good_when_read = Fuzz.Report.total good

(* -- (b) every oracle reads zero over the mutated corpus ------------------- *)

let broken = Fuzz.Report.create ()
let broken_counts : (string, int) Hashtbl.t = Hashtbl.create 8
let skips = Fuzz.Report.create ()
let mutated = ref 0
let recovered = ref 0
let shallow = ref (0, 0)
let deep = ref (0, 0)

let sweep
      (c : corpus)
      ~(rounds : int)
      (rng : Random.State.t)
      (held : Fuzz.Mutate.subject option ref)
  : unit
  =
  for _ = 1 to rounds do
    let subject =
      match !held with
      | Some s -> s
      | None ->
        let subject, _, _ = draw_one c rng in
        subject
    in
    match Fuzz.Mutate.apply c.mut rng subject with
    | None ->
      Fuzz.Report.note skips "every mutator declined" ~witness:c.label;
      held := None
    | Some (_, tokens) ->
      let src = Fuzz.Harness.decode c.h tokens in
      let tree, diags = Fuzz.Harness.parse ~cover:c.cover c.h src in
      incr mutated;
      note_built c tree;
      if diags <> [] then incr recovered;
      List.iter
        (fun f ->
           tally broken_counts f;
           note broken c src f)
        (Fuzz.Oracles.run c.h ~widths:(widths rng) src);
      (* Coverage-guided: an input whose parse reached an edge no earlier one
         did is kept, both as the next iteration's base and as a donor. One
         that reached nowhere new is dropped, so the next iteration starts from
         a fresh draw. *)
      let next = { Fuzz.Mutate.tokens; tree } in
      if Fuzz.Coverage.fresh c.cover
      then (
        Fuzz.Mutate.admit c.mut next;
        held := Some next)
      else held := None
  done
;;

let () =
  let totals () =
    List.fold_left
      (fun (p, e) c -> p + Fuzz.Coverage.points c.cover, e + Fuzz.Coverage.edges c.cover)
      (0, 0)
      corpora
  in
  List.iter
    (fun c ->
       let rng = Random.State.make seed in
       let held = ref None in
       sweep c ~rounds:iterations rng held;
       (* The second half runs on the same tables, so the counts below are the
          corpus at depth 1 against the same corpus at depth 2. *)
       let shallow_p, shallow_e =
         Fuzz.Coverage.points c.cover, Fuzz.Coverage.edges c.cover
       in
       shallow := fst !shallow + shallow_p, snd !shallow + shallow_e;
       sweep c ~rounds:iterations rng held)
    corpora;
  deep := totals ()
;;

let () =
  Printf.printf
    "SWEPT %d mutated inputs at depth %d, %d of which recovered; findings %s\n"
    !mutated
    depth
    !recovered
    (per_law broken_counts);
  if Fuzz.Report.total good > good_when_read
  then (
    show good ~reduce:(cut_down ~draw:true ~holds:still_unclean);
    Law.fail
      "(b) %d draws the sweep took are not clean, so the corpus stopped being good under \
       it"
      (Fuzz.Report.total good - good_when_read));
  match Fuzz.Report.classes broken with
  | [] -> Law.pass "(b) every oracle reads zero over the mutated corpus"
  | _ ->
    show broken ~reduce:(cut_down ~draw:false ~holds:fires);
    Law.fail "(b) %d findings over the mutated corpus" (Fuzz.Report.total broken)
;;

(* -- (c) an edge is a transition, and the count climbs --------------------- *)

let () =
  let shallow_p, shallow_e = !shallow in
  let deep_p, deep_e = !deep in
  Printf.printf
    "COVER depth %d: %d points, %d edges; depth %d: %d points, %d edges\n"
    depth
    shallow_p
    shallow_e
    (depth * 2)
    deep_p
    deep_e;
  if deep_e <= shallow_e
  then
    Law.fail
      "(c) twice the corpus reached no edge the first half had not, over %d"
      shallow_e
  else if deep_p - shallow_p >= shallow_p / 2
  then
    Law.fail
      "(c) doubling the corpus added %d points to %d, so the points are tracking the \
       corpus rather than the plan and an edge count over them measures nothing"
      (deep_p - shallow_p)
      shallow_p
  else
    Law.pass
      "(c) the points saturate, %d and %d new, where the edges climb %d to %d"
      shallow_p
      (deep_p - shallow_p)
      shallow_e
      deep_e
;;

(* -- (d) every mutator is reached, and reached fully ----------------------- *)

let () =
  let rows =
    List.concat_map
      (fun c -> List.map (fun r -> c.label, r) (Fuzz.Mutate.reach c.mut))
      corpora
  in
  let failures_before = Law.failures () in
  (* [attempts = fired + sum declines] on every row. The table is written from
     one site, so a breach here is the counting and not the mutator. *)
  List.iter
    (fun (label, (r : Fuzz.Mutate.reach)) ->
       let declined = List.fold_left (fun sum (_, n) -> sum + n) 0 r.declines in
       if r.attempts <> r.fired + declined
       then
         Law.fail
           "(d) %s %s: %d attempts against %d fired and %d declined"
           label
           r.mutator
           r.attempts
           r.fired
           declined)
    rows;
  List.iter
    (fun name ->
       let mine =
         List.filter (fun (_, (r : Fuzz.Mutate.reach)) -> r.mutator = name) rows
       in
       let attempts =
         List.fold_left (fun s (_, (r : Fuzz.Mutate.reach)) -> s + r.attempts) 0 mine
       in
       let fired =
         List.fold_left (fun s (_, (r : Fuzz.Mutate.reach)) -> s + r.fired) 0 mine
       in
       if attempts = 0
       then Law.fail "(d) %s was never called, so nothing here covers it" name
       else if fired = 0
       then
         Law.fail
           "(d) %s fired on no grammar; it declined %s"
           name
           (String.concat
              ", "
              (first_ten
                 (List.concat_map
                    (fun (_, (r : Fuzz.Mutate.reach)) -> List.map fst r.declines)
                    mine))))
    Fuzz.Mutate.names;
  if Law.failures () = failures_before
  then
    Law.pass
      "(d) every mutator is reached (%s)"
      (String.concat
         " "
         (List.map
            (fun name ->
               let fired =
                 List.fold_left
                   (fun s (_, (r : Fuzz.Mutate.reach)) ->
                      if r.mutator = name then s + r.fired else s)
                   0
                   rows
               in
               Printf.sprintf "%s %d" name fired)
            Fuzz.Mutate.names))
;;

(* The arms and the shapes, over the union of the grammars. Both are per-grammar
   zeros wherever a grammar has no alternation of that kind, so the claim is the
   union and never the row. *)
let () =
  let arms = Hashtbl.create 64 in
  let sources = Hashtbl.create 8 in
  List.iter
    (fun c ->
       List.iter
         (fun (k, n) -> Hashtbl.replace arms (c.label, k) n)
         (Fuzz.Mutate.arms c.mut);
       List.iter
         (fun (k, n) ->
            Hashtbl.replace
              sources
              k
              (n + Option.value (Hashtbl.find_opt sources k) ~default:0))
         (Fuzz.Mutate.sources c.mut))
    corpora;
  match List.filter (fun s -> not (Hashtbl.mem sources s)) Fuzz.Mutate.shapes with
  | [] ->
    Law.pass
      "(d) every shape at an alternation is swapped out, and %d arms are swapped in (%s)"
      (Hashtbl.length arms)
      (String.concat
         ", "
         (List.map
            (fun s -> Printf.sprintf "%s %d" s (Hashtbl.find sources s))
            Fuzz.Mutate.shapes))
  | missing ->
    Law.fail
      "(d) no swap took a %s, so this law says nothing about that end of an alternation"
      (String.concat " or a " missing)
;;

(* -- (e) every skip class is inside its ceiling ---------------------------- *)

let () =
  let rounds = iterations * 2 * List.length corpora in
  match Fuzz.Report.check_skips skips ~iterations:rounds with
  | [] ->
    Law.pass
      "(e) every skip class is inside its ceiling, over %d iterations (%s)"
      rounds
      (String.concat
         ", "
         (List.map
            (fun (k : Fuzz.Report.klass) -> Printf.sprintf "%s %d" k.key k.count)
            (Fuzz.Report.classes skips)))
  | bad -> List.iter (fun m -> Law.fail "(e) %s" m) bad
;;

(* -- (f) every rule the grammar can reach is one the corpus built ---------- *)

let names_of (c : corpus) (ids : Core.Rule.id list) : string list =
  List.sort
    compare
    (List.map (fun id -> Core.Grammar.Name.Rule.to_string c.h.facts.rules.(id).name) ids)
;;

(* What the draws so far never built. *)
let missing_from (c : corpus) : Core.Rule.id list =
  let seen = Option.value (Hashtbl.find_opt raised c.label) ~default:(Hashtbl.create 1) in
  List.filter (fun id -> not (Hashtbl.mem seen id)) (Fuzz.Harness.reachable c.h)
;;

(* How many rules the draws from a root reached on their own, before the
   fragments below add the rest. *)
let reached_from_roots =
  List.fold_left
    (fun n c ->
       n + List.length (Fuzz.Harness.reachable c.h) - List.length (missing_from c))
    0
    corpora
;;

(* A draw at the rule itself, for the rules a draw from the root never reaches.

   A root draw spends its size where the tokens are cheapest, so a construct
   that costs five tokens inside a zero-or-more body does not come up at any
   size: asking for a bigger input buys more of the shapes that were already
   cheap. The rule's own species sidesteps that. It is the species a mutator
   already splices from, and {!Fuzz.Harness.parse} reads the bytes back at the
   same rule, so the laws are asked of the construct rather than of a program
   that happens to contain one.

   Every rule, rather than the ones the draws above missed. Keying on the gap
   was tried and it is backwards: a deeper run's draws miss fewer rules, so the
   fragment corpus shrinks as the work grows and the law gets weaker. It cost
   two of the separator defects their falsification at depth 8 while depth 1
   still caught both. Drawing at every rule makes this half of the corpus a
   property of the grammar, so depth only ever adds.

   The species costs tables to build -- 3.1 s for all sixty-two of effekt's --
   and that is the whole of what this adds to a short run. *)
let fragment_draws = 40 * depth
let fragments = Fuzz.Report.create ()
let fragment_counts : (string, int) Hashtbl.t = Hashtbl.create 8
let fragment_inputs = ref 0
let fragment_mutants = ref 0
let fragment_recovered = ref 0
let fragment_partial = ref 0
let fragment_refused = ref 0
let fragment_rules = ref 0

let check (c : corpus) ~(at : Core.Rule.id) (rng : Random.State.t) (src : string) : unit =
  List.iter
    (fun f ->
       tally fragment_counts f;
       note fragments c ~at src f)
    (Fuzz.Oracles.run c.h ~at ~widths:(widths rng) src)
;;

(* The same construct off the language. A clean fragment reaches what part (a)
   reaches and nothing more: a defect that needs an error node or a frame the
   parse never closed is one only a mutant builds, and until this went in no
   mutant here held the construct at all.

   Two mutants are dropped rather than run, and both are about what a parse
   entering at a rule does differently from one entering at a root.

   The first is dispatch. A fragment parse forces its entry rule; every rule
   below it is still dispatched on what is under the cursor. So a mutant that
   deletes a nested opener builds no node there, which is ordinary, but one
   that deletes the entry rule's own opener builds a node with nothing where
   its opener goes, and no parse from a root can build that. It read nine Law B
   findings before {!Fuzz.Harness.dispatches} went in, all of them the closer
   of a frame with no opener.

   The second is the end of the input. A parse entering at a rule ends where
   the rule does, so a mutant the rule cannot take in full leaves a tail
   outside the tree. That is two inputs rather than one, and every law here is
   about one tree and the bytes it holds. *)
let mutate
      (c : corpus)
      ~(at : Core.Rule.id)
      (rng : Random.State.t)
      (subject : Fuzz.Mutate.subject)
  : unit
  =
  match Fuzz.Mutate.apply c.mut rng subject with
  | None -> ()
  | Some (_, edited) ->
    let src = Fuzz.Harness.decode c.h edited in
    if not (Fuzz.Harness.dispatches c.h at src)
    then incr fragment_refused
    else (
      let tree, diags = Fuzz.Harness.parse c.h ~at src in
      incr fragment_mutants;
      note_built c tree;
      if not (String.equal (Siesta.Green.to_source tree) src)
      then incr fragment_partial
      else (
        if diags <> [] then incr fragment_recovered;
        check c ~at rng src))
;;

let fragment (c : corpus) ~(at : Core.Rule.id) ~(size : int) (rng : Random.State.t) : unit
  =
  match Fuzz.Harness.draw_at c.h at ~size rng with
  | None -> ()
  | Some tokens ->
    let src = Fuzz.Harness.decode c.h tokens in
    let tree, _ = Fuzz.Harness.parse c.h ~at src in
    incr fragment_inputs;
    note_built c tree;
    check c ~at rng src;
    mutate c ~at rng { Fuzz.Mutate.tokens; tree }
;;

let () =
  List.iter
    (fun c ->
       let rng = Random.State.make seed in
       List.iter
         (fun (at : Core.Rule.id) ->
            match Fuzz.Harness.sizes_at c.h at with
            | Some (lo, hi) when Fuzz.Harness.enterable c.h at ->
              incr fragment_rules;
              for _ = 1 to fragment_draws do
                fragment c ~at ~size:(lo + Random.State.int rng (hi - lo + 1)) rng
              done
            | Some _ | None -> ())
         (Fuzz.Harness.reachable c.h))
    corpora
;;

let () =
  let short = ref [] in
  let total = ref 0 in
  let reached = ref 0 in
  List.iter
    (fun c ->
       let missing = missing_from c in
       let all = List.length (Fuzz.Harness.reachable c.h) in
       total := !total + all;
       reached := !reached + all - List.length missing;
       if missing <> [] then short := (c.label, names_of c missing) :: !short)
    corpora;
  Printf.printf
    "FRAGMENTS %d drawn at %d of the %d rules, %d of them beyond what a root reached; %d \
     mutated, %d of which the rule took in full and %d of those recovered; %d mutants no \
     dispatch would have entered; findings %s\n"
    !fragment_inputs
    !fragment_rules
    !total
    (!reached - reached_from_roots)
    !fragment_mutants
    (!fragment_mutants - !fragment_partial)
    !fragment_recovered
    !fragment_refused
    (per_law fragment_counts);
  (match
     List.sort
       compare
       (Hashtbl.fold
          (fun label t acc -> Hashtbl.fold (fun id () acc -> (label, id) :: acc) t acc)
          unreached
          [])
   with
   | [] -> ()
   | bad ->
     List.iter
       (fun (label, id) ->
          let c = List.find (fun c -> String.equal c.label label) corpora in
          Law.fail
            "(f) %s builds %s, which the walk over the grammar never reaches"
            label
            (Core.Grammar.Name.Rule.to_string c.h.facts.rules.(id).name))
       (first_ten bad));
  (match List.rev !short with
   | [] -> Law.pass "(f) every rule the grammar reaches is built, over %d rules" !total
   | bad ->
     List.iter
       (fun (label, names) ->
          Law.fail "(f) %s never builds %s" label (String.concat " " names))
       (first_ten bad);
     Law.fail
       "(f) %d of %d rules the grammars reach are in no tree the corpus holds"
       (!total - !reached)
       !total);
  match Fuzz.Report.classes fragments with
  | [] -> Law.pass "(f) every oracle reads zero over the fragments"
  | _ ->
    show fragments ~reduce:(cut_down ~draw:false ~holds:fires);
    Law.fail "(f) %d findings over the fragments" (Fuzz.Report.total fragments)
;;

(* -- (g) every witness reproduces on its own ------------------------------- *)

let () =
  let reduced = List.rev !reductions in
  let sum (f : Fuzz.Shrink.reduced -> int) : int =
    List.fold_left (fun total (_, cut) -> total + f cut) 0 reduced
  in
  let settled =
    List.filter (fun (_, (cut : Fuzz.Shrink.reduced)) -> not cut.capped) reduced
  in
  let lost =
    List.filter (fun (_, (cut : Fuzz.Shrink.reduced)) -> not cut.reproduces) reduced
  in
  let failures_before = Law.failures () in
  if reduced <> []
  then
    Printf.printf
      "REDUCED %d witnesses, %d bytes to %d, over %d candidates\n"
      (List.length reduced)
      (sum (fun cut -> cut.before))
      (sum (fun cut -> cut.after))
      (sum (fun cut -> cut.tried));
  List.iter
    (fun (key, (cut : Fuzz.Shrink.reduced)) ->
       Law.fail "(g) %s reduced to %S, which does not reproduce it" key cut.src)
    (first_ten lost);
  if lost <> []
  then
    Law.fail
      "(g) %d of %d witnesses do not reproduce"
      (List.length lost)
      (List.length reduced);
  if !unreduced > 0
  then
    Law.fail "(g) %d classes carry a witness with nothing to reproduce it from" !unreduced;
  if List.length settled < List.length reduced
  then
    Law.fail
      "(g) %d of %d reductions stopped at the candidate cap, which leaves the moves past \
       it untried"
      (List.length reduced - List.length settled)
      (List.length reduced);
  if Law.failures () = failures_before
  then
    Law.pass
      "(g) every witness reproduces on its own and no further move reduces it, over %d \
       classes"
      (List.length reduced)
;;

let () =
  Printf.printf "law_fuzz: %.1f s of processor time\n" (Sys.time ());
  Law.summarise "law_fuzz"
;;
