(* -- the interpreter ----------------------------------------------------------

      (a) A parse rebuilds its input, byte for byte, whatever the input.
      (b) A parse stops.
      (c) Input the grammar accepts parses with no diagnostics.
      (d) Every instruction the corpus plans hold is one a corpus parse runs.
      (e) No node but the root begins with trivia.
      (f) [run] refuses an entry that names no rule, and one that names a rule
          opening no node of its own, and takes every entry [may_enter] admits.

      Mechanism. Thirteen grammars, and for each of them a list of inputs
      written here. Every input goes through part (a) and part (b). The ones marked
      good go through part (c) as well, and the ones marked broken are there
      to reach the recovery paths that part (d) counts.

      Part (a)'s oracle is not a second parse. It joins the token texts the
      lexer produced and compares that to the tree's source, so the two agree
      only where every byte the cursor read reached the tree in order.

      Part (d) is the coverage claim, and it is the reason the corpus grew.
      Before postfix and shapes were written, seven instruction forms read
      zero and nothing said so.

      Coverage. Thirteen grammars, 154 inputs the grammar accepts and 128 it
      does not, from test/inputs. A count per instruction prints beside the
      result, and the law fails where one reads zero.

      Part (e) is the one claim here about the tree's shape, and it is the one
      that can be made without a second implementation to compare against.
      Leading trivia belongs to the frame that is already open, so a space
      before a child sits between the caller's children rather than inside the
      child. The root is the exception and has to be: at the entry nothing is
      open, so leading trivia has nowhere else to go.

      Part (f) is about the one index a caller supplies. Every other index in
      a plan is checked by [Plan.Check] before a parse starts; this one arrives
      with the call. {!Interp.may_enter} is the predicate and two rules fail
      it. An expression block's role has an empty body. A block's rule has a
      body and opens no node anyway, because its parse wraps what the frame
      above holds and an entry has no frame above. Both raise on their own, and
      the message then names the builder rather than the entry.

      What this says nothing about. Whether the rest of the tree is the right
      shape. That
      needs a second implementation to compare against, and the emitted parser
      is it. Part (c) is the nearest thing available: a grammar that accepts
      an input has to parse it without complaint.

      What the parts see, and what only the goldens do. Parts (a) to (f) say
      what holds for every input. None of them reads the shape of what recovery
      built, so a change that leaves the same bytes in the tree under a
      different frame passes every one of them. test/expect/*.parse is what
      reads that shape, and it is the only reader of it.

      Where the trivia sweeps are, and are not, load-bearing. Dropping the
      sweep after a repetition moves where trivia lands and not whether it
      lands: a delimited body's close takes the trivia in front of it either
      way, because taking a token takes its leading trivia too. Dropping the
      sweep at the start of a node is the one change that puts leading trivia
      inside the node it precedes, which is what part (e) exists to catch.

      What the guards on [run]'s entry buy. A parse with an entry out of range,
      or one that may not be entered, raises whichever way the guards go. What
      the guards buy is a message naming the entry rather than [Builder.finish:
      nothing built] from somewhere further in.

      Why the loop recovers rather than ending. A body that meets a token it
      cannot use sweeps it into an error node and carries on. Ending instead and
      leaving the rest to the caller loses everything after the stray token:
      json's ["\[1 : 2\]"] lost the [2] altogether before this arm existed.
      The same argument settles a position that is missing something -- it is
      reported rather than swept -- and no part here can see the difference,
      because recovery reports through a sweep as well. That is what the
      coverage count is for, and test/laws/law_residual.ml is what reads the
      two apart: a sweep and a report leave the parse at different positions.

      What the anchors and [ends_on] buy the tables. A declared resync anchor
      stops a body sweeping past it; without one, shapes' ["{ let a end }"]
      carries the [end] up to the closer. And a loop that recovers carries an
      edge on the error kind back to its entry where a loop that ends carries
      none, which is the difference the *.residual dumps show.

      Why the unicode grammar is in the corpus. Stepping the scan one byte
      rather than one codepoint moves every one of its inputs and nothing else
      at all: the other grammars are ASCII, where a byte and a codepoint are the
      same thing.

   -------------------------------------------------------------------------- *)

(* The block below is generated, and it is the evidence for the interpreter
   itself. assay derives a mutation from the code rather than from a sentence
   beside it, applies every one, and records what went red. Regenerate it with

     assay -config assay.conf -only interp

   and take the counts as they come: they move whenever the corpus grows, and
   asserting them exactly would train everyone to ignore a red suite. What it
   asserts is that every mutant dies. A survivor is the finding, and the lines
   it names are where to look.

   The lowering and the builder this law also leans on are recorded under their
   own laws: lib/plan in test/laws/law_plan.ml and lib/runtime in
   test/laws/law_runtime.ml. *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/interp/interp.ml, 124 mutants, 113 killed, 11 survived.
        extreme      9  8 killed, law_interp (c): 3, dump_format: 2, law_interp: 1, law_interp (a): 1, law_residual (d): 1; 1 survived
        sbr         66  62 killed, law_interp (d): 21, law_interp: 8, law_interp (c): 8, dump_format: 6, law_residual (a): 5, law_interp (b): 4, law_interp (a): 2, law_interp (f): 2, law_parse (a): 2, dump_parse: 1, law_residual (e): 1, law_residual (g): 1, law_residual (h): 1; 4 survived
        ror         17  13 killed, law_interp (b): 5, dump_format: 3, law_interp: 2, law_interp (f): 2, law_parse (a): 1; 4 survived
        lcr          9  7 killed, dump_format: 2, law_interp (b): 2, law_interp (f): 2, dump_parse: 1; 2 survived
        aor          1  all killed, law_interp: 1
        uoi         22  all killed, dump_format: 10, law_interp (b): 4, law_interp: 3, law_interp (c): 3, dump_parse: 1, law_interp (a): 1
      survived in exec (sbr 2), loop (sbr 2, ror), pratt (ror),
        may_enter (extreme, ror 2, lcr 2)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/core/token.ml, 2 mutants, 2 killed.
        extreme      1  all killed, (a) 1
        ror          1  all killed, (c) 1
   ---------------------------------------------------------------------- *)

(* -- the corpus ------------------------------------------------------------ *)

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; inputs : Inputs.t
  }

let corpus =
  [ { name = "sexp"; grammar = Lingo_grammars.Sexp_grammar.grammar; inputs = Inputs.sexp }
  ; { name = "json"; grammar = Lingo_grammars.Json_grammar.grammar; inputs = Inputs.json }
  ; { name = "calc"; grammar = Lingo_grammars.Calc_grammar.grammar; inputs = Inputs.calc }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; inputs = Inputs.rassoc
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; inputs = Inputs.postfix
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; inputs = Inputs.unicode
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; inputs = Inputs.recovery
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; inputs = Inputs.shapes
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; inputs = Inputs.comments
    }
  ; { name = "rust"; grammar = Lingo_grammars.Rust_grammar.grammar; inputs = Inputs.rust }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    }
  ; { name = "wide"; grammar = Lingo_grammars.Wide_grammar.grammar; inputs = Inputs.wide }
  ; { name = "ml"; grammar = Lingo_grammars.Ml_grammar.grammar; inputs = Inputs.ml }
  ]
;;

(* -- what the corpus reaches ----------------------------------------------- *)

(* One counter per instruction form. A form no parse runs is a form this law
   says nothing about, and the corpus is what has to grow. *)
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

(* -- the runs -------------------------------------------------------------- *)

(* Answers every node under [n], the root excluded, that begins with a trivia
   token. *)
let leading_trivia (f : Core.Facts.t) (root : Siesta.Green.node) =
  let is_trivia k = Core.Kind.Set.exists (fun t -> Core.Kind.to_int t = k) f.trivia in
  let bad = ref [] in
  let rec go ~is_root (n : Siesta.Green.node) =
    (match Siesta.Green.nth_child n 0 with
     | Some (Siesta.Green.Token t)
       when (not is_root) && is_trivia (Siesta.Green.Token.kind t) ->
       bad := Siesta.Green.kind n :: !bad
     | Some _ | None -> ());
    Array.iter
      (function
        | Siesta.Green.Node m -> go ~is_root:false m
        | Siesta.Green.Token _ -> ())
      (Siesta.Green.children_array n)
  in
  go ~is_root:true root;
  !bad
;;

let source_of (tokens : Lingo_runtime.Token.t array) =
  String.concat
    ""
    (Array.to_list (Array.map (fun (t : Lingo_runtime.Token.t) -> t.text) tokens))
;;

(* A parse runs one instruction at a time, so a loop that stopped making
   progress shows up as instructions without end. The ceiling reports that
   here rather than hanging the suite. *)
exception Runaway

let ceiling = 100_000

let () =
  let parses = ref 0 in
  let not_lossless = ref 0 in
  let trivia_first = ref [] in
  let runaway = ref [] in
  let noisy = ref [] in
  List.iter
    (fun c ->
       match Core.Facts.of_grammar c.grammar with
       | Error _ -> Law.fail "%s: the grammar does not check" c.name
       | Ok f ->
         let plan, _ = Plan.Lower.of_facts f in
         let entry = plan.Ir.Plan.roots.(0) in
         let run ~expect_clean src =
           let tokens = Lex.run f src in
           let steps = ref 0 in
           let trace name =
             incr steps;
             if !steps > ceiling then raise Runaway;
             ran name
           in
           incr parses;
           match Interp.run ~trace plan entry tokens with
           | exception Runaway -> runaway := (c.name, src) :: !runaway
           | root, diags ->
             if not (String.equal (Siesta.Green.to_source root) (source_of tokens))
             then incr not_lossless;
             List.iter
               (fun k -> trivia_first := (c.name, src, k) :: !trivia_first)
               (leading_trivia f root);
             if expect_clean && diags <> []
             then noisy := (c.name, src, List.length diags) :: !noisy
         in
         List.iter (run ~expect_clean:true) c.inputs.good;
         List.iter (run ~expect_clean:false) c.inputs.broken)
    corpus;
  (match !runaway with
   | [] -> Law.pass "every parse stopped, over %d parses" !parses
   | rs ->
     List.iter
       (fun (g, src) -> Law.fail "(b) %s ran past %d instructions on %S" g ceiling src)
       rs);
  if !not_lossless > 0
  then Law.fail "(a) %d of %d parses did not rebuild their input" !not_lossless !parses
  else
    Law.pass "a parse rebuilds its input, over %d parses" (!parses - List.length !runaway);
  (match !noisy with
   | [] ->
     Law.pass
       "input the grammar accepts parses clean, over %d inputs"
       (List.length (List.concat_map (fun c -> c.inputs.good) corpus))
   | bad ->
     List.iter
       (fun (g, src, n) ->
          Law.fail "(c) %s accepts %S, and the parse reported %d" g src n)
       bad);
  match !trivia_first with
  | [] -> Law.pass "no node but the root begins with trivia, over %d parses" !parses
  | bad ->
    List.iter
      (fun (g, src, k) ->
         Law.fail "(e) %s on %S: a node of kind %d begins with trivia" g src k)
      bad
;;

(* -- (f) the entry --------------------------------------------------------- *)

let () =
  match Core.Facts.of_grammar Lingo_grammars.Calc_grammar.grammar with
  | Error _ -> Law.fail "(f) calc does not check"
  | Ok f ->
    let plan, _ = Plan.Lower.of_facts f in
    let tokens = Lex.run f "1" in
    (* The message has to name the call. An array access raises
       [Invalid_argument] on its own, so catching the exception alone cannot
       separate a refusal from a bounds error that happened to surface. *)
    let names_the_call m =
      let prefix = "Interp.run" in
      String.length m >= String.length prefix
      && String.equal (String.sub m 0 (String.length prefix)) prefix
    in
    let refuses what entry =
      match Interp.run plan entry tokens with
      | exception Invalid_argument m when names_the_call m -> ()
      | exception Invalid_argument m ->
        Law.fail "(f) %s raised %S, which does not say what was wrong with it" what m
      | exception e ->
        Law.fail
          "(f) %s raised %s, which does not name the entry"
          what
          (Printexc.to_string e)
      | _ -> Law.fail "(f) %s was accepted" what
    in
    refuses "an entry below zero" (-1);
    refuses "an entry past the last rule" (Array.length plan.rules);
    (* An expression block's roles are the rules with nothing to run. *)
    (match
       Array.to_list plan.rules
       |> List.mapi (fun i (r : Ir.Plan.rule) -> i, r)
       |> List.find_opt (fun (_, (r : Ir.Plan.rule)) -> r.body = Ir.Plan.Seq [||])
     with
     | None -> Law.fail "(f) calc holds no rule with an empty body, so this reads nothing"
     | Some (i, _) -> refuses "an entry naming a rule with nothing to run" i);
    (* A block's rule is the other shape [Interp.may_enter] turns down. It has
       a body, and it still opens no node: its parse takes a checkpoint in the
       frame above and wraps what that frame holds, and at an entry there is no
       frame. *)
    (match
       Array.to_list plan.rules
       |> List.mapi (fun i (r : Ir.Plan.rule) -> i, r)
       |> List.find_opt (fun (_, (r : Ir.Plan.rule)) ->
         match r.body with
         | Ir.Plan.Seq [| Ir.Plan.Pratt _ |] -> true
         | _ -> false)
     with
     | None -> Law.fail "(f) calc holds no block rule, so this reads nothing"
     | Some (i, _) -> refuses "an entry naming a rule that opens no node" i);
    (* The other side: every rule [Interp.may_enter] admits is one [run]
       takes. Rule 0 is calc's file and opens a node, so the bottom of the
       range is read; the top is read by the refusal one past the end above. *)
    Array.iteri
      (fun i (_ : Ir.Plan.rule) ->
         if Interp.may_enter plan i
         then (
           match Interp.run plan i tokens with
           | exception e ->
             Law.fail "(f) rule %d is admitted and run raised %s" i (Printexc.to_string e)
           | _ -> ()))
      plan.rules;
    if not (Interp.may_enter plan 0)
    then
      Law.fail
        "(f) calc's rule 0 is not admitted, so the bottom of the range reads nothing";
    if Law.failures () = 0 then Law.pass "run refuses an entry it cannot parse with"
;;

(* -- (b) a loop no lowering builds --------------------------------------- *)

(* A loop stops even where its states hand each other a missing token for
   ever. No plan the lowering builds has two steps in a row that take no
   token, so no input reaches the interpreter's guard against it, and this
   builds the loop by hand: two states that each want a [(] and send the other
   on when it is missing, and a third that takes the [1] under the cursor, which
   is what makes the missing token a gap rather than junk. *)
let () =
  match Core.Facts.of_grammar Lingo_grammars.Calc_grammar.grammar with
  | Error _ -> Law.fail "(b) calc does not check"
  | Ok f ->
    let plan, _ = Plan.Lower.of_facts f in
    let tokens = Lex.run f "1" in
    let one = tokens.(0).Lingo_runtime.Token.kind
    and lparen = (Lex.run f "(").(0).Lingo_runtime.Token.kind in
    let state accepts when_missing =
      { Ir.Plan.accepts; exit = Ir.Plan.May_exit; when_missing; emits = Ir.Plan.Bump }
    in
    let missing goto =
      Some { Ir.Plan.tok = lparen; message = Ir.Message.of_int 0; goto }
    in
    let loop =
      Ir.Plan.Loop
        { entry = 0
        ; ends_on = Some [||]
        ; states =
            [| state [| [| lparen |], 0 |] (missing 1)
             ; state [| [| lparen |], 1 |] (missing 0)
             ; state [| [| one |], 2 |] None
            |]
        }
    in
    let entry = plan.Ir.Plan.roots.(0) in
    let rule = plan.rules.(entry) in
    let body = Ir.Plan.Seq [| Ir.Plan.Open rule.kind; loop; Ir.Plan.Close |] in
    let plan =
      { plan with
        rules =
          Array.mapi
            (fun i r -> if i = entry then { r with Ir.Plan.body } else r)
            plan.rules
      }
    in
    let steps = ref 0 in
    let trace (_ : string) =
      incr steps;
      if !steps > ceiling then raise Runaway
    in
    (match Interp.run ~trace plan entry tokens with
     | exception Runaway ->
       Law.fail "(b) a loop of steps that take no token ran past %d instructions" ceiling
     | _ -> Law.pass "(b) a loop of steps that take no token stops")
;;

let () =
  let missing = List.filter (fun n -> not (Hashtbl.mem reach n)) forms in
  if missing <> []
  then
    Law.fail
      "(d) no parse ran %s, so this law says nothing about it"
      (String.concat ", " missing)
  else
    Law.pass
      "every instruction form ran (%s)"
      (String.concat
         " "
         (List.map (fun n -> Printf.sprintf "%s %d" n (Hashtbl.find reach n)) forms))
;;

let () = Law.summarise "law_interp"
