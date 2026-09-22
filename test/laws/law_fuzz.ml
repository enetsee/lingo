(* -- the fuzz harness ---------------------------------------------------------

      (a) The corpus is a corpus of clean draws, and every oracle reads zero
          over it.
      (b) Every oracle reads zero over the corpus once it is mutated.
      (c) An edge is a transition: the points saturate and the edges climb.
      (d) Every mutator is reached, and every shape at an alternation is
          swapped out.
      (e) Every skip class is inside its ceiling.

      Mechanism. Eleven grammars from grammars/ and the witness grammars
      beside them, less the five that declare no whitespace. For each one a
      harness: the plan, the layout, the species a rule became, and an engine
      over them. A draw is a token list, [Sample.decode] writes the bytes, and
      the oracles run on those bytes at three widths, two fixed and one drawn.
      Then a mutator edits the
      token list and the same oracles run again.

      Part (a) is the phase's first claim and the oldest hole in the project.
      Every oracle here is a forbidding law, so the way one fails badly is by
      firing on an input that is fine, and nothing inside an oracle can see
      that. What sees it is a corpus that is known good. Its first half is what
      makes it known: every input lexes back to the tokens drawn and parses
      with no diagnostics. test/laws/law_sample.ml says the same of the nine
      grammars it draws; rust and effekt are not among them, and for those this
      is the only place it is said.

      Part (b) is the same oracles over inputs no sampler would draw. 1,903 of
      7,000 reach recovery, against 46% for the corpus test/sweep generates:
      a mutated draw is near the language and a swept input is a mangled one,
      so the two corpora are complements and the sweep does not go away.

      Part (c) is M1, ported. A point is a place in the plan where a parse read
      the cursor, and an edge is a pair of consecutive points. The claim is
      what separates the two counts: doubling the corpus adds 440 points to
      1,696 and 998 edges to 3,298, so the points are a property of the plan
      and the edges are still finding things. The predecessor counted points,
      saturated at about a thousand iterations, and ran a million-sample sweep
      for ten issues with a flat number beside it.

      The threshold in (c) is half the points the first half found. It is a
      threshold and it is worth saying where it sits: the point count here adds
      a quarter of itself, and keying on the whole parse stack rather than on
      {!Ir.Residual.State.site} adds 1.7 times itself. M2 is that mutation, and
      an order of magnitude sits either side of the line.

      Part (d) has two halves and the second is the one with something to hide.
      A mutator that declines every call is invisible in every other number:
      the driver falls through to the next one, so it costs no skip and no
      failure, and the mutator that did fire absorbs the iteration. That is the
      first half. The second is a mutator that fires on every call and still
      reaches one arm of a four-arm slot -- the corpus parses, the oracles read
      zero, and [fired] reads the same either way. A slot whose every arm is a
      token is one no node can stand in, and a walk that saw only nodes would
      build the same arms and decline nothing extra. M4 is that walk.

      The corpus. 3,500 clean inputs and 7,000 mutated, over 14 corpora
      holding 4,385 comments, in 8.6 s of processor time on the machine this
      record was written on. Every grammar with [Preserve] trivia is drawn
      twice, once from a system that draws comments and once from one that does
      not, so Law B is asked of comment placement as well as of spacing.

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
      cross-checks and they are asked of the nine grammars it draws.

      No defect is open. Three were, all in the separator a body's policy adds,
      and the foot of this record says what they were.

      Falsification. Every mutation was applied, built, run and reverted, and
      the result recorded is the one observed. A count is the findings that
      law reads, and where law_layout moves as well it is named beside.

        M1  In [Fuzz.Coverage.at], pair nothing: read the previous point as
            [None] always, so an edge is a point.
            -> (c), 0 edges at both depths. (d), graft fires on no grammar: an
               input is admitted as a donor when its parse found a new edge,
               and there are none.
        M2  In [Fuzz.Coverage.at], key on the whole parse stack rather than on
            [State.site].
            -> (c). The points read 37,494 and 70,107 where the site reads
               1,696 and 2,136. A stack grows with the input's nesting, so
               distinct stacks track the corpus however little of the plan a
               parse reached.
        M3  In [Fuzz.Mutate.regenerate], decline every call.
            -> (d), naming it. Nothing else moves: the other four absorb the
               iterations.
        M4  In [Fuzz.Mutate.swap_arm], walk only the node children at an
            alternation.
            -> (d), naming both token shapes, with every other counter reading
               as before. (c) moves too, because the corpus after it is a
               different corpus.
        M5  In [Sample.decode], write no joiner at all.
            -> (a) first half, 3,953 inputs; (b) first half, 7,714 draws the
               sweep took are not clean. (d), the token arm at an atom is then
               never reached, because the corpus holds far fewer expressions to
               swap one into.
        M6  In [Lingo_runtime.Layout.flat_end], let a child that writes nothing
            end the run.
            -> (b), 8 findings, all Law F. law_layout does not move.
        M7  In [Lingo_runtime.Layout.node], start the body segment at the
            opener rather than after its leading run.
            -> (a), 270 findings; (b), 476. All Law F. law_layout does not
               move.
        M8  In [Lingo_runtime.Layout.node], read [flat_through] as [false].
            -> (a), 14 findings; (b), 45. All Law F. law_layout (e) reads 70
               lines past the ruler, which it did not before the separator was
               folded once: this is the one of the three its own corpus
               reaches.
        M9  In [Lingo_runtime.Layout.walk], hand every child an empty tail.
            -> (a), 23,787 findings -- B 4,573, C and D 6,780 each, E 5,654;
               (b), 48,201. law_layout as well, and its (g) then reads five
               steps the fold never takes. This is D8 with nothing left of it.
       M10  In [Lingo_runtime.Layout.join], answer [Touching] at every
            boundary.
            -> (a), 1,438 findings -- B 231, C and D 479 each, E 184, W 65;
               (b), 2,970. comments' ["//"] comes back ["//,"], which is Law C
               and Law D reading one fusion from the two ends.
       M11  In [Lingo_runtime.Layout.node], write a byte for a childless node.
            -> (b), 6,999 findings -- B 1,928, C and D 1,932 each, E 1,204, F
               3 -- and (a) reads zero. A childless node is what the parse
               leaves where it wanted a token and found none, so only a
               recovered tree holds one and only the mutated corpus reaches it.
       M12  In [Fuzz.Mutate.apply], decline every iteration.
            -> (c), (d) six times, and (e): the skip class reads 1,000,000 per
               million against a ceiling of 20,000.
       M13  In [Fuzz.Oracles.keeps], refuse the separator a body's policy adds,
            so the comparison is an equality rather than a subsequence.
            -> (a), 3,144 findings over Laws C and D on a corpus that is good.
               This is the mutation part (a) exists for: the oracle is wrong
               and every other part reads exactly what it read before.
       M14  In [Lingo_runtime.Layout.body], split the walk at the last element
            under [`Plain] as well.
            -> (b), 21 findings, all Law B; law_layout (c), 13 formats. A
               policy that adds nothing has nothing to insert there, and
               cutting the walk truncates every run that crosses the cut.
       M15  In [Lingo_runtime.Layout.body], carry the state the folding *with*
            the separator left rather than the one without.
            -> (a), 160 findings -- B 36, C and D 37 each, E 37, W 13; (b),
               421; law_layout loses a token. The flat branch writes no
               separator, so its state is the one the closer glues against.
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
       M18  In [Lingo_runtime.Layout.node], write the policy separator after
            bytes an error node swept up.
       M19  In [Lingo_runtime.Layout.node], read an inner frame the parse never
            closed as closed.
            -> (b) at depth 8, 4 findings each, all Law B, and law_layout does
               not move under any of them. M17 has its own witness and M18 and
               M19 share one.

               Three ways for the separator a body's policy adds to come back
               somewhere the fold cannot see it: in front of the last element,
               inside an error node, or inside a frame still open at the body's
               end. Each leaves the fold writing another on the next pass, and
               M18's grows without bound.

               All three read zero at depth 1. This is what a depth is for, and
               the only entry here that needs one.

      Depth. 1 is what the suite runs. [LINGO_SWEEP=8] is 28,000 clean inputs
      and 56,000 mutated in 29 s, 16 is 112,000 mutated and 32 is 224,000.
      Every law reads zero at every one of them, on both corpora.

      Depth is what found M17 to M19. All three read zero at depth 1 and four
      findings at 8, and the shortest witness each time was a rust input of
      about 180 bytes that no one would have looked at twice. Reducing one took
      it to 13.

      Nothing is open. The Law B findings this record carried until 2026-09-22
      were three defects in the separator a body's policy adds, and M17 to M19
      are them. Each left the fold writing a separator the next parse put
      somewhere the fold could not see, so the pass after wrote another.

      M1 and M4 each read one Law B finding before those went in. Both move the
      corpus, and a corpus that moves reached the defect at depth 1 where the
      corpus this record measures did not. Both read zero now.

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

let () =
  Printf.printf "law_fuzz: %.1f s of processor time\n" (Sys.time ());
  if !failures = 0
  then print_endline "law_fuzz: 0 failures"
  else (
    Printf.printf "law_fuzz: %d failures\n" !failures;
    exit 1)
;;
