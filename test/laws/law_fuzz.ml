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

      Mechanism. Eleven grammars from grammars/ and the witness grammars
      beside them, less the five that declare no whitespace. For each one a
      harness: the plan, the layout, the species a rule became, and an engine
      over them. A draw is a token list, [Sample.decode] writes the bytes, and
      the oracles run on those bytes at three widths, two fixed and one drawn.
      Then a mutator edits the token list and the same oracles run again.

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

      Part (c) is M1, ported. A point is a place in the plan where a parse read
      the cursor, and an edge is a pair of consecutive points. The claim is
      what separates the two counts: doubling the corpus adds 464 points to
      1,663 and 1,058 edges to 3,287, so the points are a property of the plan
      and the edges are still finding things. The predecessor counted points,
      saturated at about a thousand iterations, and ran a million-sample sweep
      for ten issues with a flat number beside it.

      The threshold in (c) is half the points the first half found. It is a
      threshold and it is worth saying where it sits: the point count here adds
      a quarter of itself, and keying on the whole parse stack rather than on
      {!Ir.Residual.State.site} adds twenty times itself. M2 is that mutation,
      and an order of magnitude sits either side of the line.

      Part (d) has two halves and the second is the one with something to hide.
      A mutator that declines every call is invisible in every other number:
      the driver falls through to the next one, so it costs no skip and no
      failure, and the mutator that did fire absorbs the iteration. That is the
      first half. The second is a mutator that fires on every call and still
      reaches one arm of a four-arm slot -- the corpus parses, the oracles read
      zero, and [fired] reads the same either way. A slot whose every arm is a
      token is one no node can stand in, and a walk that saw only nodes would
      build the same arms and decline nothing extra. M4 is that walk.

      Part (f) is the one thing a corpus can be silently short of. Every law
      above it is a forbidding one, so a construct the corpus never builds
      leaves every count reading zero: the input that would have failed is not
      there. {!Fuzz.Harness.reachable} walks the grammar and
      {!Fuzz.Harness.built} walks a tree, and the two never consult each other.
      They are compared both ways. A rule the walk reaches and no tree holds is
      what the part is named for; a rule a tree holds and the walk never
      reaches would mean the walk is simply short, and M23 is that mutation.

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
      the law gets weaker. It cost M18 and M19 their falsification at depth 8
      while depth 1 still caught both.

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
      Nine Law B findings came from that shape and M22 is the mutation.

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

      The fragments are worth what they cost. M18 and M19 read zero over the
      mutated corpus at every depth now, and 27 and 14 over the fragments at
      depth 1 and 195 and 147 at depth 8, so for those two the fragments are
      the whole of the falsification. The witnesses are shorter by an order of magnitude as
      well: [{//\nnh(h,chi}] against a 180-byte rust program.

      The corpus. 3,500 clean inputs, 7,000 mutated and 7,050 fragments with
      6,694 mutants of their own, over 14 corpora holding 4,422 comments, in
      12.4 s of processor time on the machine this record was written on. Every
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

      What this says nothing about. Whether the emitted parser and the emitted
      formatter agree with the interpreter and the fold on this corpus, which
      is test/parse_emit and test/format_emit over the corpus test/sweep
      generates. Nor whether the system is the grammar for rust and effekt: the
      structure count, the nullability and the minimum size are law_sample's
      cross-checks and they are asked of the fourteen grammars it draws.

      No defect is open. Three were, all in the separator a body's policy adds,
      and the foot of this record says what they were.

      Falsification. Every mutation was applied, built, run and reverted, and
      the result recorded is the one observed. A count is the findings that
      law reads, at depth 1 unless a depth is named, and where law_layout moves
      as well it is named beside.

        M1  In [Fuzz.Coverage.at], pair nothing: read the previous point as
            [None] always, so an edge is a point.
            -> (c), 0 edges at both depths. (d), graft fires on no grammar: an
               input is admitted as a donor when its parse found a new edge,
               and there are none.
        M2  In [Fuzz.Coverage.at], key on the whole parse stack rather than on
            [State.site].
            -> (c). The points read 32,900 and 67,592 where the site reads
               1,663 and 2,127. A stack grows with the input's nesting, so
               distinct stacks track the corpus however little of the plan a
               parse reached.
        M3  In [Fuzz.Mutate.regenerate], decline every call.
            -> (d), naming it. Nothing else moves: the other five absorb the
               iterations.
        M4  In [Fuzz.Mutate.swap_arm], walk only the node children at an
            alternation.
            -> (d), naming both token shapes, with every other counter reading
               as before. (c) moves too, because the corpus after it is a
               different corpus.
        M5  In [Sample.decode], write no joiner at all.
            -> (a) first half, 3,956 inputs; (b) first half, 7,747 draws the
               sweep took are not clean. (d), the token arm at an atom is then
               never reached, because the corpus holds far fewer expressions to
               swap one into. (f), 678 Law A findings: a fragment whose tokens
               fused is one the rule stops partway through.
        M6  In [Lingo_runtime.Layout.flat_end], let a child that writes nothing
            end the run.
            -> (b), 5 findings, and (f), 2. All Law F, and (a) reads zero.
               law_layout does not move.
        M7  In [Lingo_runtime.Layout.node], start the body segment at the
            opener rather than after its leading run.
            -> (a), 330 findings; (b), 807; (f), 59. All Law F. law_layout does
               not move.
        M8  In [Lingo_runtime.Layout.node], read [flat_through] as [false].
            -> (a), 22 findings; (b), 119; (f), 2. All Law F. law_layout (e)
               reads lines past the ruler, which it did not before the
               separator was folded once: this is the one of the three its own
               corpus reaches.
        M9  In [Lingo_runtime.Layout.walk], hand every child an empty tail.
            -> (a), 35,935 findings; (b), 72,115 -- B 13,819, C and D 20,241
               each, E 17,814; (f), 116,110. law_layout as well, and its (g)
               then reads five steps the fold never takes. This is D8 with
               nothing left of it.
       M10  In [Lingo_runtime.Layout.join], answer [Touching] at every
            boundary.
            -> (a), 2,130 findings; (b), 4,209 -- B 657, C and D 1,314 each,
               E 578, W 346; (f), 3,261. comments' ["//"] comes back ["//,"],
               which is Law C and Law D reading one fusion from the two ends.
       M11  In [Lingo_runtime.Layout.node], write a byte for a childless node.
            -> (b), 14,143 findings -- B 3,840, C and D 3,840 each, E 2,622,
               F 1; (f), 8,833 -- and (a) reads zero. A childless node is what
               the parse leaves where it wanted a token and found none, so only
               a recovered tree holds one and only a mutated input reaches it.
       M12  In [Fuzz.Mutate.apply], decline every iteration.
            -> (c), (d) six times and its arm half, and (e): the skip class
               reads 1,000,000 per million against a ceiling of 20,000. The
               fragments still draw, because a draw at a rule does not go
               through a mutator, and they still read zero.
       M13  In [Fuzz.Oracles.keeps], refuse the separator a body's policy adds,
            so the comparison is an equality rather than a subsequence.
            -> (a), 4,754 findings over Laws C and D on a corpus that is good;
               (b), 9,592; (f), 9,790. This is the mutation part (a) exists
               for: the oracle is wrong and every other part reads exactly what
               it read before.
       M14  In [Lingo_runtime.Layout.body], split the walk at the last element
            under [`Plain] as well.
            -> (b), 60 findings, all Law B; law_layout (c), 11 formats. A
               policy that adds nothing has nothing to insert there, and
               cutting the walk truncates every run that crosses the cut.
       M15  In [Lingo_runtime.Layout.body], carry the state the folding *with*
            the separator left rather than the one without.
            -> (a), 231 findings; (b), 543 -- B 104, C and D 110 each, E 110,
               W 109; (f), 185; law_layout loses a token. The flat branch
               writes no separator, so its state is the one the closer glues
               against.
       M16  In [Lingo_runtime.Layout.node], take a plain group and a
            conditional that always answers flat, so an [On_break] separator is
            never written.
            -> nothing, in either law, and 16 lines of comments.format move.
               No law here says a body that broke carries its separator: Law C
               allows one and does not require it, and a fold that never writes
               one is stably idempotent. The golden is the whole of what covers
               it, which is mechanism L and is where it belongs -- but it is
               worth knowing that it is the only thing there.

       M17  In [Layout.Lower.expansion], leave a block's rule atoms out of the
            kinds a slot admits.
            -> (b), 4 findings at depth 1 and 10 at depth 8; (f), 1 and 11.
               All Law B, and law_layout does not move.
       M18  In [Lingo_runtime.Layout.node], write the policy separator after
            bytes an error node swept up.
            -> (f), 27 findings at depth 1 and 195 at depth 8. (b) reads zero
               at both.
       M19  In [Lingo_runtime.Layout.node], read an inner frame the parse never
            closed as closed.
            -> (f), 14 findings at depth 1 and 147 at depth 8. (b) reads zero
               at both. The two share their witnesses, and the shortest at
               depth 8 is [enum r{Ge(l}].

               Three ways for the separator a body's policy adds to come back
               somewhere the fold cannot see it: in front of the last element,
               inside an error node, or inside a frame still open at the body's
               end. Each leaves the fold writing another on the next pass, and
               M18's grows without bound.

               Only M17 is reachable from a root at all, and only through the
               mutated corpus. M18 and M19 need a fragment, which is the whole
               argument for part (f): both were open defects that the corpus
               before it reached at depth 8 and stopped reaching when the
               corpus moved.

       M20  In [Fuzz.Harness.parse], ignore [at] and enter at the root always.
            -> (f) first half, 13 of 238 rules. A fragment read from the root
               is a program that starts with a construct rather than the
               construct, and the parse recovers rather than builds.
       M21  In [Fuzz.Oracles.run], read the reparse at the root rather than at
            [at].
            -> (f) second half, 54,673 findings -- B 18,583, E 36,090. Both
               parses have to enter at the same rule or the comparison is
               between two different readings of the bytes.
       M22  In [Fuzz.Harness.dispatches], answer [true] always.
            -> (f) second half, 9 findings, all Law B, and the shortest is 15
               bytes. Every one is a delimited node whose opener a mutant
               deleted, formatting its closer one way and the reparse of that
               the other. The tree is not one a parser can build.
       M23  In [Fuzz.Harness.reachable], drop the first rule it found.
            -> (f), naming [File] on every corpus that builds one. The walk
               over the grammar is checked against the walk over a tree in both
               directions, and this is the direction that says the first walk
               is not short.

      Depth. 1 is what the suite runs. [LINGO_SWEEP=8] is 28,000 clean inputs,
      56,000 mutated and 56,509 fragments in 52 s; 16 is 112,000 mutated and 32
      is 224,000. Depth multiplies the draws and not the rules: the same 197
      rules are drawn at whatever the depth, which is what keeps the fragments
      a property of the grammar. Every law reads zero at every depth, on all
      three corpora.

      Depth is what found M17. It reads 4 findings at depth 1 and 10 at 8, and
      the shortest witness each time was a rust input of about 180 bytes that
      no one would have looked at twice. Reducing one took it to 13.

      Nothing is open. The Law B findings this record carried until 2026-09-22
      were three defects in the separator a body's policy adds, and M17 to M19
      are them. Each left the fold writing a separator the next parse put
      somewhere the fold could not see, so the pass after wrote another.

   -------------------------------------------------------------------------- *)

let failures = ref 0

let fail : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt ->
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt -> Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt
;;

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
   the corpus reaches and halved the rate at which it finds the defects M17 to
   M19 name, from 71 per million to 40, because the widths those need came up
   a third as often. Spread is worth having and it is worth having beside a
   baseline rather than instead of one.

   The draw is from the run's own seeded generator, so the corpus is the same
   on every run and a count below is reproducible. *)
let width_pool = [| 1; 2; 3; 4; 6; 8; 12; 16; 28; 40; 60; 120 |]

let widths (rng : Random.State.t) : int list =
  [ 80; 20; width_pool.(Random.State.int rng (Array.length width_pool)) ]
;;

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
    fail "%s: no harness (%s)" label (Printexc.to_string e);
    None
;;

let corpora : corpus list =
  List.concat_map
    (fun (name, grammar) ->
       match Core.Facts.of_grammar grammar with
       | Error _ ->
         fail "%s: the grammar was rejected" name;
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

let note (report : Fuzz.Report.t) (c : corpus) (src : string) (f : Fuzz.Oracles.finding)
  : unit
  =
  Fuzz.Report.note
    report
    (Printf.sprintf "%s %s" c.label (Fuzz.Oracles.describe f))
    ~witness:src
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

let show (report : Fuzz.Report.t) : unit =
  List.iter
    (fun (k : Fuzz.Report.klass) ->
       fail "%s, %d times, shortest %S" k.key k.count k.witness)
    (first_ten (Fuzz.Report.classes report))
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

let draw_one (c : corpus) (rng : Random.State.t) : Fuzz.Mutate.subject * string =
  let tokens = Fuzz.Harness.draw c.h rng in
  let src = Fuzz.Harness.decode c.h tokens in
  let tree, diags = Fuzz.Harness.parse c.h src in
  (* The corpus is only known good while these hold. law_sample states the same
     two over its own grammars; rust and effekt are not among them, so for
     those this is the only place it is said. *)
  if diags <> []
  then
    Fuzz.Report.note
      good
      (Printf.sprintf
         "%s: %s"
         c.label
         (Format.asprintf "%a" Lingo_runtime.Diagnostic.pp (List.hd diags)))
      ~witness:src;
  if Fuzz.Harness.relex c.h src <> Fuzz.Harness.of_tokens tokens
  then
    Fuzz.Report.note
      good
      (Printf.sprintf
         "%s: the decoded input does not lex back to the tokens drawn"
         c.label)
      ~witness:src;
  List.iter
    (fun (tok : Sample.token) ->
       if Core.Facts.is_trivia_kind c.h.facts tok.kind then incr clean_comments)
    tokens;
  incr clean_inputs;
  note_built c tree;
  { Fuzz.Mutate.tokens; tree }, src
;;

let () =
  List.iter
    (fun c ->
       let rng = Random.State.make seed in
       for _ = 1 to iterations do
         let _, src = draw_one c rng in
         List.iter
           (fun f ->
              tally sound_counts f;
              note sound c src f)
           (Fuzz.Oracles.run c.h ~widths:(widths rng) src)
       done)
    corpora
;;

let () =
  (match Fuzz.Report.classes good with
   | [] ->
     pass
       "(a) the corpus is good, over %d inputs in %d corpora holding %d comments (%s \
        declare no whitespace and are left out)"
       !clean_inputs
       (List.length corpora)
       !clean_comments
       (String.concat " " (List.rev !unformattable))
   | _ ->
     show good;
     fail "(a) %d inputs of the corpus are not clean draws" (Fuzz.Report.total good));
  Printf.printf
    "DRAWN by %s; findings over the good corpus %s\n"
    (match corpora with
     | c :: _ -> Fuzz.Harness.engine_of c.h
     | [] -> "nothing")
    (per_law sound_counts);
  match Fuzz.Report.classes sound with
  | [] -> pass "(a) every oracle reads zero over it"
  | _ ->
    show sound;
    fail
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
      | None -> fst (draw_one c rng)
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
    show good;
    fail
      "(b) %d draws the sweep took are not clean, so the corpus stopped being good under \
       it"
      (Fuzz.Report.total good - good_when_read));
  match Fuzz.Report.classes broken with
  | [] -> pass "(b) every oracle reads zero over the mutated corpus"
  | _ ->
    show broken;
    fail "(b) %d findings over the mutated corpus" (Fuzz.Report.total broken)
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
    fail "(c) twice the corpus reached no edge the first half had not, over %d" shallow_e
  else if deep_p - shallow_p >= shallow_p / 2
  then
    fail
      "(c) doubling the corpus added %d points to %d, so the points are tracking the \
       corpus rather than the plan and an edge count over them measures nothing"
      (deep_p - shallow_p)
      shallow_p
  else
    pass
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
  let failures_before = !failures in
  (* [attempts = fired + sum declines] on every row. The table is written from
     one site, so a breach here is the counting and not the mutator. *)
  List.iter
    (fun (label, (r : Fuzz.Mutate.reach)) ->
       let declined = List.fold_left (fun sum (_, n) -> sum + n) 0 r.declines in
       if r.attempts <> r.fired + declined
       then
         fail
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
       then fail "(d) %s was never called, so nothing here covers it" name
       else if fired = 0
       then
         fail
           "(d) %s fired on no grammar; it declined %s"
           name
           (String.concat
              ", "
              (first_ten
                 (List.concat_map
                    (fun (_, (r : Fuzz.Mutate.reach)) -> List.map fst r.declines)
                    mine))))
    Fuzz.Mutate.names;
  if !failures = failures_before
  then
    pass
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
    pass
      "(d) every shape at an alternation is swapped out, and %d arms are swapped in (%s)"
      (Hashtbl.length arms)
      (String.concat
         ", "
         (List.map
            (fun s -> Printf.sprintf "%s %d" s (Hashtbl.find sources s))
            Fuzz.Mutate.shapes))
  | missing ->
    fail
      "(d) no swap took a %s, so this law says nothing about that end of an alternation"
      (String.concat " or a " missing)
;;

(* -- (e) every skip class is inside its ceiling ---------------------------- *)

let () =
  let rounds = iterations * 2 * List.length corpora in
  match Fuzz.Report.check_skips skips ~iterations:rounds with
  | [] ->
    pass
      "(e) every skip class is inside its ceiling, over %d iterations (%s)"
      rounds
      (String.concat
         ", "
         (List.map
            (fun (k : Fuzz.Report.klass) -> Printf.sprintf "%s %d" k.key k.count)
            (Fuzz.Report.classes skips)))
  | bad -> List.iter (fun m -> fail "(e) %s" m) bad
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
   M18 and M19 their falsification at depth 8 while depth 1 still caught both.
   Drawing at every rule makes this half of the corpus a property of the
   grammar, so depth only ever adds.

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
       note fragments c src f)
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
          fail
            "(f) %s builds %s, which the walk over the grammar never reaches"
            label
            (Core.Grammar.Name.Rule.to_string c.h.facts.rules.(id).name))
       (first_ten bad));
  (match List.rev !short with
   | [] -> pass "(f) every rule the grammar reaches is built, over %d rules" !total
   | bad ->
     List.iter
       (fun (label, names) ->
          fail "(f) %s never builds %s" label (String.concat " " names))
       (first_ten bad);
     fail
       "(f) %d of %d rules the grammars reach are in no tree the corpus holds"
       (!total - !reached)
       !total);
  match Fuzz.Report.classes fragments with
  | [] -> pass "(f) every oracle reads zero over the fragments"
  | _ ->
    show fragments;
    fail "(f) %d findings over the fragments" (Fuzz.Report.total fragments)
;;

let () =
  Printf.printf "law_fuzz: %.1f s of processor time\n" (Sys.time ());
  if !failures = 0
  then print_endline "law_fuzz: 0 failures"
  else (
    Printf.printf "law_fuzz: %d failures\n" !failures;
    exit 1)
;;
