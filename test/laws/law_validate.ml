(* -- every rejection is deserved ----------------------------------------------

      (a) For every code in [Error.codes] there is a witness grammar that
          provokes that code and no other.
      (b) [Error.codes] lists every code the checker emits: no witness
          produces one outside it.
      (c) Every check runs at the stage its code is filed under in
          [Error.codes_by_stage].
      (d) Every grammar in the accepted corpus yields a [Facts.t].
      (e) Findings come back sorted and free of duplicates.
      (f) One left-recursive cycle is one finding, whatever its length.

      Mechanism: an oracle over the witness corpus, with no baseline. Part (c)
      is a cross-check between the stage a code is filed under and the stage
      that emits it.

      Falsification. Every mutation below was applied, run and reverted, and
      the result recorded is the one observed.

        M1  Delete [|> nullable_tokens n] from [Check_names.run].
            -> this law. nullable-token's witness is accepted, so (a) reports
               the code as having no witness.
        M2  In [Check_names.pratt], replace the mixed-role test
            [List.mem t postfix_toks] with [false].
            -> this law. mixed-pratt-role's witness is accepted.
        M3  Move the nullable-token check from [Check_names.run] to
            [Check_full.run].
            -> this law, part (c). The code is filed under "names" and its
               witness is now rejected at "full". Part (a) still passes, which
               is why (c) is a separate check.
        M4  In [Check_shape.arity], widen the delimited guard from [n = 1] to
            [n <= 2].
            -> this law. delimited-arity's witness is accepted.
        M5  Add an entry to [Error.full_stage_codes] that nothing emits.
            -> this law, part (a). The code has no witness.
        M6  In [Check_full.first_follow], drop the [ch.c_greedy] guard.
            -> this law. Four witnesses that use the flag to isolate their
               code report two codes each, and two accepted grammars are
               rejected.
        M7  In [Facts.of_grammar], replace [sorted] with the identity.
            -> this law, part (e). Findings come back in fold order.
        M8  In [Check_names.collisions], skip the [View_accessor] scope.
            -> this law. dup-child-name's witness is accepted. Recorded here
               rather than under law_manifest: that law asks the manifest
               whether it sees the collision and it does.
        M9  In [Check_full.left_recursion], report one finding per member of
            a cyclic component rather than one per component.
            -> this law, part (f), at every length above one, and nothing
               else. Part (a) compares sets of codes, so k copies of one code
               collapse, and part (e) rejects identical findings, where the
               members differ in their site and message. The check had this
               shape before the component walk, and it is why (f) asks for
               the count.
        M10 In [Check_full.left_recursion], drop the self-edge test on a
            one-rule component, so every component reports.
            -> this law and four others. Every rule is a component of its
               own, so every grammar is rejected: parts (a), (d) and (f) here
               (15 failures), and law_facts, law_first_follow, law_manifest,
               sexp_facts and pratt_desugar with it, since each wants a
               [Facts.t] the checker now refuses to build. The blast radius
               is the observation. That one test is what separates a rule
               from a rule that reaches itself.
        M11 In [Check_names.invalid_names], drop the postfix [kind_suffix]
            entries from the list of names checked.
            -> this law, parts (a) and (c). invalid-name's second witness is
               accepted, and (c) reads the same fact from the other side: a
               code filed under "names" whose grammar reaches "accepted".

      Coverage. The 44 witness grammars and the 6 accepted ones: at least one
      grammar per rejection and a handful of near misses, together with the
      six cycles part (f) builds for itself. It says nothing about whether a
      check's reason is right, only that it fires on one shape and stays
      quiet on six others. Six accepted grammars is a statement about six
      grammars; a generated corpus is what would make it a statement about
      the checker.
   -------------------------------------------------------------------------- *)

open Core

let failures = ref 0

let fail fmt =
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt

let codes_of = function
  | Ok _ -> []
  | Error es ->
    List.sort_uniq String.compare (List.map (fun (e : Error.t) -> Error.code e) es)
;;

let stage_of_code code =
  List.find_map
    (fun (stage, cs) -> if List.mem code cs then Some stage else None)
    Error.codes_by_stage
;;

(* (a) and (b) *)
let () =
  List.iter
    (fun (code, g) ->
       match codes_of (Facts.of_grammar g) with
       | [] -> fail "%s: the witness grammar was accepted" code
       | [ c ] when c = code -> pass "%s: witness provokes exactly it" code
       | cs ->
         fail
           "%s: witness provokes {%s} instead of exactly it"
           code
           (String.concat ", " cs))
    Lingo_witness.Witnesses.all
;;

let () =
  let witnessed =
    List.sort_uniq String.compare (List.map fst Lingo_witness.Witnesses.all)
  in
  let declared = List.sort_uniq String.compare Error.codes in
  List.iter
    (fun c ->
       if not (List.mem c witnessed)
       then
         fail
           "%s is in Error.codes but has no witness: it is untested or unreachable, and \
            both are findings"
           c)
    declared;
  List.iter
    (fun c ->
       if not (List.mem c declared) then fail "%s is emitted but not in Error.codes" c)
    witnessed;
  if List.length Error.codes <> List.length declared
  then fail "Error.codes contains a duplicate";
  List.iter
    (fun c ->
       if stage_of_code c = None then fail "%s is in Error.codes but in no stage list" c)
    declared;
  if !failures = 0 then pass "Error.codes and the witness table are the same set"
;;

(* (c). The one caller of [Internal]. A code filed under "names" and caught
   at the third stage passes the completeness law while reporting two
   derivations later than it could, and running the stages one at a time is
   how that shows. *)
open Core.Internal

let () =
  List.iter
    (fun (code, g) ->
       let declared = Option.get (stage_of_code code) in
       let n = Stage.names g in
       let s1 = Check_names.run n in
       let observed =
         if s1 <> []
         then "names"
         else (
           let shape = Stage.shape n in
           if Check_shape.run shape <> []
           then "shape"
           else if
             Check_full.run
               shape
               (Fixpoint.compute
                  ~rules:shape.rules
                  ~blocks:shape.blocks
                  ~kind_rule:shape.names.kind_rule)
               (Stage.lexer n)
             <> []
           then "full"
           else "accepted")
       in
       if observed = declared
       then pass "%s: rejected at stage %s, as filed" code declared
       else fail "%s: filed under stage %s but rejected at %s" code declared observed)
    Lingo_witness.Witnesses.all
;;

(* (e). Several checks build their reports by folding a hash table, so the
   sort is what fixes the order a caller sees. *)
let () =
  List.iter
    (fun (code, g) ->
       match Facts.of_grammar g with
       | Ok _ -> ()
       | Error es ->
         if List.sort Error.compare es <> es
         then fail "%s: findings are not sorted" code
         else if List.sort_uniq Error.compare es <> es
         then fail "%s: findings contain a duplicate" code)
    Lingo_witness.Witnesses.all;
  if !failures = 0 then pass "findings are sorted and duplicate-free"
;;

(* (d) *)
let () =
  List.iter
    (fun (name, g) ->
       match Facts.of_grammar g with
       | Ok _ -> pass "accepted: %s" name
       | Error es ->
         fail "accepted corpus grammar %s was rejected:@\n%a" name Error.pp_list es)
    Lingo_witness.Witnesses.accepted
;;

(* (f). The property is about the length of the cycle and the witness table
   fixes two lengths, so these are built here and quantified over. A cycle of
   one is the length at which reporting rotations and reporting components
   agree, which is why the corpus on its own could not separate them. *)
let () =
  let name i = Printf.sprintf "P%d" i in
  let cycle n =
    Grammar.create
      ~tokens:[ Grammar.punct_tight ~name:"ta" "a" ]
      ~roots:[ name 0 ]
      (List.init n (fun i ->
         Grammar.prod
           (name i)
           [ Grammar.child_opt ~greedy:true "l" (Grammar.Rule (name ((i + 1) mod n)))
           ; Grammar.child_req "t" (Grammar.Token "ta")
           ]))
  in
  List.iter
    (fun n ->
       match Facts.of_grammar (cycle n) with
       | Ok _ -> fail "a cycle of %d rules was accepted" n
       | Error es ->
         let mine =
           List.filter (fun (e : Error.t) -> Error.code e = "left-recursion") es
         in
         if List.length es <> 1 || List.length mine <> 1
         then
           fail
             "a cycle of %d rules reports %d findings, %d of them left-recursion: one \
              cycle is one left recursion"
             n
             (List.length es)
             (List.length mine))
    [ 1; 2; 3; 4; 5; 8 ];
  if !failures = 0 then pass "one left recursion is one finding, at every cycle length"
;;

let () =
  if !failures = 0
  then print_endline "law_validate: 0 failures"
  else (
    Printf.printf "law_validate: %d failures\n" !failures;
    exit 1)
;;
