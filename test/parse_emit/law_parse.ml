(* -- the emitted parser -------------------------------------------------------

      (a) The emitted parser and the interpreter build the same tree: the same
          kinds, the same payloads, the same tokens, in the same places.
      (b) They report the same diagnostics, in the same order, with the same
          ranges and the same fields.
      (c) The tree rebuilds the input, byte for byte, whatever the input.
      (d) Every instruction form the corpus plans hold is one a compared parse
          runs.
      (e) Every module the emitter wrote is what it writes now, byte for byte.

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

      Coverage. Thirteen grammars, 223,582 inputs, 6,500 of them drawn, and
      the counts print beside the result. Part (d) is the coverage claim: a form no parse runs is a
      form this law says nothing about. The count of inputs carrying a
      diagnostic is beside it, because a corpus that never recovers says
      nothing about recovery, which is where this project's hard bugs have
      lived.

      grammars/recovery_grammar.ml is in the corpus for the recovery set
      alone. A boundary rule's inbound set reddens on that grammar and on no
      other. A rule's adds and a commit's resume set did too until rust and
      effekt went in; both now move on more than recovery, which is the honest
      reading of what a big grammar buys: each was covered by a set the
      position already held on every small grammar, and is not on a grammar
      with sixty productions.

      What this says nothing about. Whether the interpreter is right, which
      test/laws/law_interp.ml reads, and what recovery builds, which
      test/expect/*.parse reads. Nor any entry but a root: the emitted module
      has an entry point per root and the interpreter takes any rule.

      Part (d) says nothing about the recovery set either. It counts what the
      interpreter traces, and the interpreter traces no event for a boundary
      rule, for a rule's adds, or for a resume that declined a skip. Dropping
      the recovery grammar from the corpus reddens nothing in part (d), and
      three of the recovery mutations in everything else.

      What the progress guard is for. A body whose step takes no token runs
      forever, and the guard inside the body loop is the whole of what stops
      it: dropping it hangs the law rather than reddening it, which is what a
      timeout in the record below means. This is the only place that guard is
      read. The runtime held it as [Cursor.while_progress] and no longer does,
      so no law but this one and test/laws/law_interp.ml part (b) says a parse
      stops at all.

      What each grammar is in the corpus for. rassoc is here for right
      associativity: it is the [>=] in the infix guard and nothing else, so
      [1^2^3] groups the other way under a [>]. calc and effekt do not move on
      that one, because their operators are all left-associative and
      [(bp, bp + 1)] groups the same under either test. recovery holds one
      separated list of a rule, where the body has no closer and the separator
      reaches the elements through the rule's adds alone; effekt's one is the
      same shape and the only other in 202,250 inputs, which is how narrow
      that position is. shapes holds the other boundary rule and its body's
      stopping set already holds the [let] and [{] a caller contributes.

      What the drawn half of the corpus buys. Every grammar with an operator
      table gains exactly 500 findings on a mutation to the wrapping of an
      operator's left side -- every drawn input it has. A draw from a grammar
      with an expression in it is an expression, so all of them catch that and
      only some hand-written seeds do.

      Six roots end their body with a [Trivia], which takes the trailing
      trivia before the drain runs. shapes, recovery, rust, effekt, wide and ml
      read zero for anything done to the drain's own trailing skip, and the
      other six grammars are the whole of what covers it.

      Nothing but the payload in a dump reads a hole's report id, which is why
      the dump carries it.

      One form in part (d) is worth knowing about. The postfix instruction read
      its whole count from the one grammar named after it, before the drawn
      corpus and before rust and effekt. All three reach it now, so no form in
      the list rests on a single grammar.

   -------------------------------------------------------------------------- *)

(* The record below is generated. assay derives a mutation from the emitter's
   code rather than from a sentence beside it, applies every one, and records
   what went red. Regenerate it with

     assay -config assay.conf -only ocaml

   Part (e) is what makes those mutants reachable at all: the module parts (a)
   to (d) run is compiled before the law starts, so without (e) every point in
   [Ocaml.Parser] reads 0 killed whatever the tests do. That is what this
   record read on 2026-09-26, and the number said nothing about the tests.

   Five survivors. Four are the width of a recovery set: [words] takes the
   whole plan's highest kind, and no grammar in the corpus is wide enough for a
   looser bound to cost it a word. The fifth is a function nothing calls. All
   five are in assay.findings. *)

(* -- mutation testing, generated by assay on 2026-10-01 ---------------------
      lib/ocaml/parser.ml, 239 mutants, 230 killed, 5 survived, 4 timed out.
        extreme     11  10 killed, law_parse sexp_parser.ml:: 6, law_parse calc_parser.ml:: 2, law_parse: 1, law_parse sexp_parser.mli:: 1; 1 survived
        sbr        211  204 killed, sexp_parser.ml: 144 calc_parser.ml: 31 json_parser.ml: 22 sexp_parser.mli: 5 postfix_parser.ml: 2; 3 survived; 4 timed out
        ror          1  0 killed; 1 survived
        lcr          2  all killed, sexp_parser.ml: 2
        aor          4  all killed, law_parse: 3, law_parse sexp_parser.ml:: 1
        uoi         10  all killed, sexp_parser.ml: 10
      survived at lines 51 69 128 165
   ---------------------------------------------------------------------- *)

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
  ; { name = "ml"
    ; grammar = Lingo_grammars.Ml_grammar.grammar
    ; parse = emitted Emitted_parsers.Ml_parser.parse_tokens
    ; seeds = Inputs.all Inputs.ml
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

(* Part (e) reads the module dune compiled and asks the emitter for it again.
   Everything above runs the parser that module holds, and the module is
   compiled before the law starts, so nothing above can see a change in
   {!Ocaml.Parser} itself. This can: the emitter runs here. *)
let modules = ref 0

(* Which grammars a disagreement came from, so a failure says where to look. *)
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
           (case.seeds @ Sweep.inputs facts case.seeds @ drawn);
         modules := !modules + 2;
         Law.generated
           ~file:(case.name ^ "_parser.ml")
           (Ocaml.Emit.render (Ocaml.Parser.generate plan));
         Law.generated
           ~file:(case.name ^ "_parser.mli")
           (Ocaml.Emit.render_signature (Ocaml.Parser.signature plan)))
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
  Law.pass "%d emitted modules are what the emitter writes now" !modules;
  Law.exit_on_failure ()
;;
