(* -- the emitted residual tables ----------------------------------------------

      (a) The tables a grammar's module holds are the tables its plan gives.
      (b) The entry point over them gives the kinds a root starts with.

      Mechanism. Eight grammars. For each, [Ir.Residual.Table.of_plan] is read
      off the plan here and compared with the literal the emitter wrote into
      the generated module, point for point.

      That is the whole of what generation has to get right. Whether those
      tables are the right tables is test/laws/law_residual part (h), which
      checks them against the interpreter at every byte of its corpus. Between
      the two, what a generated module gives is what the parse would take.

      The generated modules link [lingo_runtime] and nothing else, which the
      library stanza beside this enforces: one reaching for a plan would not
      build.

      Falsification. Re-run on 2026-09-19, after the body loop's progress guard
      changed. Every mutation was applied, built, run and reverted, and the
      result recorded is the one observed.

        M1  In [Ocaml.Residual.point], write [may_end] as [true] always.
            -> part (a), all eight grammars.
        M2  In [Ocaml.Residual.generate], leave the last table out of
            [of_kind].
            -> part (a), all eight grammars, and part (b) on one of them. The
               record read part (a) alone before the guard changed: the kind
               whose table goes missing is now reached by a walk from a root as
               well as by the comparison.
        M3  In [Ocaml.Residual.generate], write the trivia kinds as empty.
            -> part (a), all eight grammars.
        M4  In [Ocaml.Residual.point], write the transition targets one too
            high.
            -> part (a), all eight grammars, and part (b) on none of them: the
               root's own first point is reached before any transition runs.
   -------------------------------------------------------------------------- *)

open StdLabels

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

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; tables : Lingo_runtime.Ahead.t
  ; source : string
  }

let corpus =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; tables = Emitted_parsers.Sexp_residual.tables
    ; source = "(a b)"
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; tables = Emitted_parsers.Json_residual.tables
    ; source = "[1, 2]"
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; tables = Emitted_parsers.Calc_residual.tables
    ; source = "1+2*3"
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; tables = Emitted_parsers.Rassoc_residual.tables
    ; source = "1^2^3"
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; tables = Emitted_parsers.Postfix_residual.tables
    ; source = "a(1, 2)"
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; tables = Emitted_parsers.Shapes_residual.tables
    ; source = "let a = b"
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; tables = Emitted_parsers.Unicode_residual.tables
    ; source = "\xc2\xabhello\xc2\xbb"
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; tables = Emitted_parsers.Recovery_residual.tables
    ; source = "let a in end"
    }
  ]
;;

let points = ref 0

let () =
  List.iter corpus ~f:(fun c ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> fail "%s: the grammar does not check" c.name
    | Ok facts ->
      let plan, _ = Plan.Lower.of_facts facts in
      let entries = Ir.Residual.Table.of_plan plan in
      let from_plan = Array.of_list (List.map entries ~f:snd) in
      Array.iter from_plan ~f:(fun table -> points := !points + Array.length table);
      if c.tables.points <> from_plan
      then
        fail
          "(a) %s emitted %d automata and its plan gives %d, or their points differ"
          c.name
          (Array.length c.tables.points)
          (Array.length from_plan);
      List.iteri entries ~f:(fun index (kind, _) ->
        if kind >= Array.length c.tables.of_kind || c.tables.of_kind.(kind) <> index
        then fail "(a) %s sends kind %d to the wrong automaton" c.name kind);
      if c.tables.trivia <> plan.Ir.Plan.trivia
      then fail "(a) %s emitted the wrong trivia kinds" c.name;
      if c.tables.error_kind <> plan.Ir.Plan.error_kind
      then fail "(a) %s emitted the wrong error kind" c.name)
;;

let () =
  if !failures = 0
  then pass "every emitted table is the one its plan gives, over %d points" !points
;;

(* -- (b) the entry point ---------------------------------------------------- *)

(* At the first byte nothing has been read, so what may appear is what the root
   starts with. The plan already carries that set, worked out by a different
   route. *)
let () =
  let before = !failures in
  List.iter corpus ~f:(fun c ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> ()
    | Ok facts ->
      let plan, _ = Plan.Lower.of_facts facts in
      let root, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run facts c.source) in
      let given = Lingo_runtime.Ahead.at c.tables root ~offset:0 in
      let first = plan.Ir.Plan.rules.(plan.Ir.Plan.roots.(0)).first in
      if given <> first
      then
        fail
          "(b) %s gives %s at the first byte, where its root starts with %s"
          c.name
          (String.concat ~sep:" " (List.map (Array.to_list given) ~f:string_of_int))
          (String.concat ~sep:" " (List.map (Array.to_list first) ~f:string_of_int)));
  if !failures = before
  then pass "the entry point gives what the root starts with, over 8 grammars"
;;

let () =
  if !failures = 0
  then print_endline "law_ahead: 0 failures"
  else (
    Printf.printf "law_ahead: %d failures\n" !failures;
    exit 1)
;;
