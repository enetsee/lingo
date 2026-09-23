(* -- the emitted parser -------------------------------------------------------

      (a) The emitted parser and the interpreter build the same tree: the same
          kinds, the same payloads, the same tokens, in the same places.
      (b) They report the same diagnostics, in the same order, with the same
          ranges and the same fields.
      (c) The tree rebuilds the input, byte for byte, whatever the input.
      (d) Every instruction form the corpus plans hold is one a compared parse
          runs.

      Mechanism. Twelve grammars, and two corpora over each.

      One is generated from seed inputs written here: a seed's tokens dropped,
      duplicated, swapped, replaced and truncated, and runs drawn at random
      from the token texts the seeds hold. It reaches the shapes an author
      thought of, and it is where every recovery path comes from.

      The other is drawn from the grammar itself, through the same sampler
      test/laws/law_fuzz.ml uses. It reaches the shapes nobody wrote a seed
      for. Every draw is clean, so it adds structure and no recovery: with it
      the corpus runs 2.4 times the operator instructions it did without.

      Both sides parse every input, entering at the grammar's first root.

      The interpreter is the oracle. It shares no parse code with the
      emitter: each brings its own dispatch, its own loops and its own
      balanced skip, so anything lost in writing the plan out as control flow
      shows up as a disagreement. The emitter also settles shapes the plan
      does not: which arms a match holds, where a [let] goes, and which of
      the two recovery sets is in scope at a position.

      The tree is compared as a dump holding every field a green node carries
      that is not derived: the kind, the payload, and each token's kind and
      text. The diagnostics are compared as values, so a field that no
      printer shows is still read.

      The generator is a linear congruential one seeded here, so the corpus
      is the same on every run and a failure names an input that can be
      pasted back.

      Coverage. Twelve grammars, 202,250 inputs, 6,000 of them drawn, and the
      counts print beside the result. Part (d) is the coverage claim: a form no parse runs is a
      form this law says nothing about. The count of inputs carrying a
      diagnostic is beside it, because a corpus that never recovers says
      nothing about recovery, which is where this project's hard bugs have
      lived.

      grammars/recovery_grammar.ml is in the corpus for the recovery set
      alone, and M7 is what says so: a boundary rule's inbound set reddens on
      that grammar and on no other. M6 and M8 did too until rust and effekt
      went in. Both now move on more than recovery, which is the honest
      reading of what a big grammar buys: a resume set and a rule's adds were
      covered by a set the position already held on every small grammar, and
      are not on a grammar with sixty productions.

      What this says nothing about. Whether the interpreter is right, which
      test/laws/law_interp.ml reads, and what recovery builds, which
      test/expect/*.parse reads. Nor any entry but a root: the emitted module
      has an entry point per root and the interpreter takes any rule.

      Part (d) says nothing about the recovery set either. It counts what the
      interpreter traces, and the interpreter traces no event for a boundary
      rule, for a rule's adds, or for a resume that declined a skip. Dropping
      the recovery grammar from the corpus here reddens nothing in part (d),
      and M6 to M8 in everything else.

      Falsification. Re-run on 2026-09-23, after comments and wide joined the
      corpus. Every mutation was applied, built, run and reverted, and the
      result recorded is the one observed. A count of inputs counts distinct
      inputs, and the grammars beside it are where they came from.

      Where a mutation's wording could be read two ways, the edit is named
      exactly, because three of these could not be re-created from their own
      prose the last time round.

        M1  In [Ocaml.Parser.loop], build [stops] from the ends-on kinds alone:
            [union [ ends; continues ]] becomes [ends].
            -> parts (a) and (b), 24,562 inputs, none of them drawn: sexp 298, json 1,309,
               postfix 169, unicode 464, recovery 5,897, shapes 5,206,
               comments 784, rust 3,215, effekt 7,213, wide 7. A body that
               meets junk runs to the closer instead of picking up at its next
               element.
        M2  In [Ocaml.Parser.loop], have [stuck] give [swept] whatever
            [when_missing] holds.
            -> part (a), 2,460 inputs: json 466, postfix 69, unicode 307,
               recovery 35, shapes 101, comments 668, rust 231, effekt 66,
               wide 517. Part (b), 3,430: json 774, postfix 147, unicode 417,
               recovery 35, shapes 123, comments 1,013, rust 277, effekt 127,
               wide 517. The separator that is not there goes unreported and
               the element after it is swept away.
        M3  In [Ocaml.Parser.loop], bind [from] to [Cursor.offset] rather than
            to the first byte of [Cursor.range].
            -> part (b), 239 inputs: json 10, postfix 10, unicode 89,
               recovery 112, shapes 8, rust 1, wide 9. A trailing separator is
               then reported over the trivia in front of it as well. comments
               and effekt do not move: no body of theirs reports on exit.
        M4  In [Ocaml.Parser.loop], stop the progress guard ending the body:
            both [assign "going" false] inside it become [Emit.eunit].
            -> the suite does not finish inside 120 s. A body whose step takes
               no token runs forever, and the guard is what stops it. Reverted
               without a count.

               This is the only place that guard is read. The runtime held it as
               [Cursor.while_progress] and no longer does, so no law but this
               one and test/laws/law_interp.ml part (b) says a parse stops.
        M5  In [Ocaml.Parser.commit], skip on [recover] alone, leaving out what
            [extend] adds from the position's own [local].
            -> part (a), 5,734 inputs: calc 565, postfix 785, recovery 1,534,
               shapes 76, comments 72, rust 695, effekt 1,712, wide 295.
               Part (b), 5,603: the same but shapes 9 and effekt 1,648.
        M6  In [Ocaml.Parser.commit], build the skip's set from the resume set
            rather than from [local].
            -> part (a), 3,838 inputs: calc 565, postfix 468, recovery 840,
               rust 537, effekt 313, wide 1,115. Part (b), 3,837: the same but
               effekt 312.

               The record used to read 579, recovery alone, from an edit its own
               prose never named exactly. This one is named above and measured. A
               commit's resume set holds the FIRST set of every later child and
               its recovery set stops at the first later child that is not
               nullable, so the two differ wherever two children follow the
               commit.
        M7  In [Ocaml.Parser.rule_binding], bind [inbound] to the caller's set
            whether or not the rule is a boundary.
            -> parts (a) and (b), 1,612 inputs, recovery alone, rust and
               effekt included. [Group] is committed and a boundary, and its
               body names [rparen] and nothing else, so dropping the inbound set
               is the difference between stopping at the close and stopping at
               the caller's next item.

               shapes holds the other boundary rule and does not move: its
               body's stopping set already holds the [let] and [{] a caller
               contributes.
        M8  In [Ocaml.Parser.rule_binding], extend [passed_down] with the empty
            list rather than with [rule.adds].
            -> parts (a) and (b), 232 inputs: recovery 231, effekt 1.
               [Fields] is a separated list of a rule. A separated body has no
               closer, so its loop adds nothing to what it passes its elements,
               and the separator reaches them through the adds alone. effekt's
               one is the same shape and the only one outside recovery in
               202,250 inputs, which is how narrow this position is.
        M9  In [Ocaml.Parser.rule_binding], bind [inbound] to the empty set for
            every rule.
            -> parts (a) and (b), 5,233 inputs: json 377, postfix 5,
               recovery 2,195, shapes 9, comments 5, rust 628, effekt 2,013,
               wide 1.
       M10  In [Ocaml.Parser.skip_item], make [unmatched_closer] bump
            unconditionally before halting.
            -> parts (a) and (b), 2,447 inputs: sexp 56, json 389, postfix 68,
               unicode 88, shapes 461, comments 39, rust 658, effekt 688. The
               skip still halts, so the frame above is handed a closer that is
               already eaten.
       M11  In [Ocaml.Parser.skip_item], make [unmatched_closer] just [bump], so
            the skip runs on.
            -> parts (a) and (b), 25,402 inputs: sexp 2,454, json 2,997,
               calc 1,786, postfix 4,315, unicode 2,854, shapes 2,009,
               comments 970, rust 4,338, effekt 3,679. The skip swallows the
               rest of the production it was recovering inside.
       M12  In [Ocaml.Parser.infix_body], guard an infix arm with
            [Emit.egreater] rather than [Emit.egreater_equal].
            -> part (a), 1,245 inputs: rassoc 1,040, rust 205. Right
               associativity is the [>=] and nothing else, so [1^2^3] groups
               the other way. rust's [=] is the second right-associative
               operator in the corpus. calc and effekt do not move: their
               operators are all left-associative, and [(bp, bp + 1)] groups
               the same under either test. rassoc exists for this.

               It read 702 over the swept corpus alone. The drawn half nests
               operators deeper than a hand-written seed does, and this is one
               of the two mutations that gained from it.
       M13  In [Ocaml.Parser.wrap], call [Build.start_node] rather than
            [Build.start_node_at].
            -> part (a), 33,936 inputs: calc 8,820, rassoc 8,366,
               postfix 6,383, rust 2,924, effekt 5,260, wide 2,183. An operator
               that has already read its left side stops wrapping it, and the
               tree flattens.

               Every grammar with an operator table gained exactly 500, which
               is every drawn input it has. A draw from a grammar with an
               expression in it is an expression, so all of them catch this and
               only some swept inputs do.

               Part (b) read 113 on postfix before the guard changed and reads
               zero now. The trees still differ; the diagnostics no longer do.
       M14  In [Ocaml.Parser.drain], drop the trailing [Cursor.skip_trivia].
            -> parts (a) and (c), 8,614 inputs: sexp 1,934, json 341,
               calc 1,034, postfix 549, unicode 778, comments 3,978. shapes,
               recovery, rust, effekt and wide do not move: every one of those
               roots ends its body with a [Trivia], which has taken the
               trailing trivia before the drain runs.
       M15  In [Ocaml.Parser.hole_node], add one to the id [Cursor.report_id]
            gives.
            -> part (a), 100,927 inputs, every grammar. Nothing but the
               payload in the dump reads this, which is why the dump carries
               it.
       M16  In [Ocaml.Parser.instr], pass [None] for an [Expect]'s placeholder.
            -> part (a), 25,359 inputs: sexp 2,119, json 3,666, calc 2,009,
               postfix 1,647, unicode 1,646, shapes 4,664, comments 3,909,
               rust 2,982, effekt 2,717. Part (b), 6,455: sexp 704, json 512,
               calc 1,305, postfix 759, shapes 665, comments 780, rust 1,043,
               effekt 687.
       M17  Drop the postfix case from [corpus] here.
            -> nothing, and it used to redden part (d) with ["postfix"]
               reading zero, before the drawn corpus and before rust and
               effekt. rust and effekt reach the same instruction, so
               dropping the one grammar named after it no longer empties the
               count. The same thing happened to law_residual's M16, and both
               are recorded rather than replaced: a form reached three ways is
               the result.

               Emptying [Inputs.postfix] instead does not work either. The
               generator draws from the pool its seeds make, and an empty pool
               raises.
   -------------------------------------------------------------------------- *)

(* -- the corpus ------------------------------------------------------------- *)

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; parse :
      Lingo_runtime.Token.t array -> Siesta.Green.node * Lingo_runtime.Diagnostic.t list
  ; seeds : string list
  }

let emitted
      (parse_tokens :
        ?cache:Siesta.Cache.t
        -> Lingo_runtime.Token.t array
        -> Siesta.Green.node * Lingo_runtime.Diagnostic.t list)
  =
  fun (tokens : Lingo_runtime.Token.t array) ->
  parse_tokens ~cache:(Siesta.Cache.create_plain ()) tokens
;;

(* The seeds are test/inputs, where the forms a grammar reaches only once were
   chosen. The generator grows them; it does not replace them. *)
let corpus : case list =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; parse = emitted Emitted_parsers.Sexp_parser.parse_tokens
    ; seeds = Inputs.all Inputs.sexp
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; parse = emitted Emitted_parsers.Json_parser.parse_tokens
    ; seeds = Inputs.all Inputs.json
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; parse = emitted Emitted_parsers.Calc_parser.parse_tokens
    ; seeds = Inputs.all Inputs.calc
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; parse = emitted Emitted_parsers.Rassoc_parser.parse_tokens
    ; seeds = Inputs.all Inputs.rassoc
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; parse = emitted Emitted_parsers.Postfix_parser.parse_tokens
    ; seeds = Inputs.all Inputs.postfix
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; parse = emitted Emitted_parsers.Unicode_parser.parse_tokens
    ; seeds = Inputs.all Inputs.unicode
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; parse = emitted Emitted_parsers.Recovery_parser.parse_tokens
    ; seeds = Inputs.all Inputs.recovery
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; parse = emitted Emitted_parsers.Shapes_parser.parse_tokens
    ; seeds = Inputs.all Inputs.shapes
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; parse = emitted Emitted_parsers.Comments_parser.parse_tokens
    ; seeds = Inputs.all Inputs.comments
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; parse = emitted Emitted_parsers.Rust_parser.parse_tokens
    ; seeds = Inputs.all Inputs.rust
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; parse = emitted Emitted_parsers.Effekt_parser.parse_tokens
    ; seeds = Inputs.all Inputs.effekt
    }
  ; { name = "wide"
    ; grammar = Lingo_grammars.Wide_grammar.grammar
    ; parse = emitted Emitted_parsers.Wide_parser.parse_tokens
    ; seeds = Inputs.all Inputs.wide
    }
  ]
;;

(* -- comparing -------------------------------------------------------------- *)

(* Every field a green node carries that is not derived from its children:
   the kind, the payload that resolves a hole to its diagnostic, and each
   token's kind and text. *)
let dump (root : Siesta.Green.node) : string =
  let buffer = Buffer.create 256 in
  let rec go (n : Siesta.Green.node) =
    Buffer.add_string
      buffer
      (Printf.sprintf "(%d#%d" (Siesta.Green.kind n) (Siesta.Green.payload n));
    Array.iter
      (function
        | Siesta.Green.Node m ->
          Buffer.add_char buffer ' ';
          go m
        | Siesta.Green.Token t ->
          Buffer.add_string
            buffer
            (Printf.sprintf
               " %d:%S"
               (Siesta.Green.Token.kind t)
               (Siesta.Green.Token.text t)))
      (Siesta.Green.children_array n);
    Buffer.add_char buffer ')'
  in
  go root;
  Buffer.contents buffer
;;

let show_diagnostics (ds : Lingo_runtime.Diagnostic.t list) : string =
  String.concat
    "; "
    (List.map
       (fun (d : Lingo_runtime.Diagnostic.t) ->
          let lo, hi = d.range in
          let body =
            match d.kind with
            | Missing m ->
              Printf.sprintf
                "missing %d at %s expecting [%s] hole %s"
                (Ir.Message.to_int m.expected)
                (Option.value m.at_child ~default:"-")
                (String.concat "," (List.map string_of_int m.expected_kinds))
                (match m.hole_kind with
                 | None -> "-"
                 | Some k -> string_of_int k)
            | Extra id -> Printf.sprintf "extra %d" (Ir.Message.to_int id)
            | Unexpected -> "unexpected"
          in
          Printf.sprintf "%d-%d %s" lo hi body)
       ds)
;;

let show (src : string) : string =
  if String.length src > 40 then String.sub src 0 40 ^ "\xe2\x80\xa6" else src
;;

(* -- what the corpus reaches ------------------------------------------------ *)

let reach : (string, int) Hashtbl.t = Hashtbl.create 32

let ran (name : string) : unit =
  Hashtbl.replace reach name (1 + Option.value (Hashtbl.find_opt reach name) ~default:0)
;;

let forms =
  [ "seq"
  ; "open"
  ; "close"
  ; "trivia"
  ; "bump"
  ; "drain"
  ; "expect"
  ; "call"
  ; "pratt"
  ; "alt"
  ; "commit"
  ; "loop"
  ; "exit-reporting"
  ; "loop-recover"
  ; "loop-missing"
  ; "postfix"
  ; "prefix"
  ; "infix"
  ; "atom-token"
  ; "atom-rule"
  ]
;;

(* -- running ---------------------------------------------------------------- *)

let inputs = ref 0
let tokens_seen = ref 0
let with_diagnostics = ref 0
let trees_differ = ref 0
let diagnostics_differ = ref 0
let not_lossless = ref 0

(* Which grammars a disagreement came from, so a failure says where to look
   and the falsification record can be written from what the run printed. *)
let blamed : (string * string, int) Hashtbl.t = Hashtbl.create 16

let blame (part : string) (grammar : string) : unit =
  let key = part, grammar in
  Hashtbl.replace blamed key (1 + Option.value (Hashtbl.find_opt blamed key) ~default:0)
;;

let by_grammar (part : string) : string =
  String.concat
    ", "
    (List.filter_map
       (fun (case : case) ->
          Option.map
            (fun (n : int) -> Printf.sprintf "%s %d" case.name n)
            (Hashtbl.find_opt blamed (part, case.name)))
       corpus)
;;

let source_of (tokens : Lingo_runtime.Token.t array) : string =
  String.concat
    ""
    (Array.to_list (Array.map (fun (t : Lingo_runtime.Token.t) -> t.text) tokens))
;;

let run_one (case : case) (facts : Core.Facts.t) (plan : Ir.Plan.t) (src : string) : unit =
  let tokens = Lex.run facts src in
  incr inputs;
  tokens_seen := !tokens_seen + Array.length tokens;
  let walked, walked_diagnostics = Interp.run ~trace:ran plan plan.roots.(0) tokens in
  let built, built_diagnostics = case.parse tokens in
  if built_diagnostics <> [] then incr with_diagnostics;
  let here = dump built
  and there = dump walked in
  if not (String.equal here there)
  then (
    incr trees_differ;
    blame "a" case.name;
    if !trees_differ <= 3
    then
      Law.fail
        "(a) %s: %S builds\n      %s\n    and the interpreter builds\n      %s"
        case.name
        (show src)
        here
        there);
  if built_diagnostics <> walked_diagnostics
  then (
    incr diagnostics_differ;
    blame "b" case.name;
    if !diagnostics_differ <= 3
    then
      Law.fail
        "(b) %s: %S reports\n      %s\n    and the interpreter reports\n      %s"
        case.name
        (show src)
        (show_diagnostics built_diagnostics)
        (show_diagnostics walked_diagnostics));
  if not (String.equal (Siesta.Green.to_source built) (source_of tokens))
  then (
    incr not_lossless;
    blame "c" case.name;
    if !not_lossless <= 3
    then Law.fail "(c) %s: %S does not come back from its tree" case.name (show src))
;;

(* -- the drawn corpus ------------------------------------------------------- *)

(* [Sweep] damages inputs an author wrote, so it reaches the shapes an author
   thought of. The sampler draws from the grammar, so it reaches the ones
   nobody wrote a seed for. What the two sides are compared on is the same
   either way; only where the input came from differs.

   [Bolts.Exact] and a fixed seed, so the drawn half is the same corpus on
   every run and a count in the record below is reproducible.

   test/laws/law_fuzz.ml draws the same way and runs the forbidding oracles
   over it. Its header says what it leaves out: whether the emitted parser
   agrees with the interpreter on a drawn input. That is this.

   500 a grammar is where every instruction form is reached and the run still
   costs under half a second. Raising it scales the counts and finds no new
   shape, so a deeper draw is a run by hand rather than a number to raise
   here. That is the same thing [Sweep]'s [depth] says about its own. *)
let drawn_per_grammar = 500
let drawn_seed = [| 0x1EA5; 0x100 |]
let drawn_inputs = ref 0

let drawn (name : string) (facts : Core.Facts.t) : string list =
  match Fuzz.Harness.make ~lex:(Lex.run facts) ~name facts with
  | exception e ->
    Law.fail "%s: no harness to draw from (%s)" name (Printexc.to_string e);
    []
  | harness ->
    let rng = Random.State.make drawn_seed in
    List.init drawn_per_grammar (fun _ ->
      Fuzz.Harness.decode harness (Fuzz.Harness.draw harness rng))
;;

let () =
  List.iter
    (fun (case : case) ->
       match Core.Facts.of_grammar case.grammar with
       | Error _ -> Law.fail "%s: the grammar does not check" case.name
       | Ok facts ->
         let plan, _ = Plan.Lower.of_facts facts in
         let drawn = drawn case.name facts in
         drawn_inputs := !drawn_inputs + List.length drawn;
         List.iter
           (run_one case facts plan)
           (case.seeds @ Sweep.inputs facts case.seeds @ drawn))
    corpus
;;

let () =
  if !trees_differ = 0
  then
    Law.pass
      "the emitted parser and the interpreter build the same tree, over %d inputs"
      !inputs
  else
    Law.fail
      "(a) %d of %d inputs build different trees (%s)"
      !trees_differ
      !inputs
      (by_grammar "a");
  if !diagnostics_differ = 0
  then
    Law.pass
      "the emitted parser and the interpreter report the same diagnostics, over %d inputs"
      !inputs
  else
    Law.fail
      "(b) %d of %d inputs report different diagnostics (%s)"
      !diagnostics_differ
      !inputs
      (by_grammar "b");
  if !not_lossless = 0
  then
    Law.pass
      "a parse rebuilds its input, over %d inputs and %d tokens"
      !inputs
      !tokens_seen
  else
    Law.fail
      "(c) %d of %d parses did not rebuild their input (%s)"
      !not_lossless
      !inputs
      (by_grammar "c");
  let unreached = List.filter (fun name -> not (Hashtbl.mem reach name)) forms in
  (match unreached with
   | [] ->
     Law.pass
       "every instruction form ran (%s)"
       (String.concat
          " "
          (List.map
             (fun name -> Printf.sprintf "%s %d" name (Hashtbl.find reach name))
             forms))
   | missed -> List.iter (fun name -> Law.fail "(d) no parse ran %s" name) missed);
  Law.pass "%d of %d inputs reported at least one diagnostic" !with_diagnostics !inputs;
  if !with_diagnostics = 0 then Law.fail "no input reached a recovery path";
  Law.exit_on_failure ()
;;
