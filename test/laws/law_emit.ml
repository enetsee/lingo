(* -- the OCaml syntax builder -------------------------------------------------

      (a) A structure [Emit] builds renders to source that parses back to the
          same structure.
      (b) The same for a signature.
      (c) Rendering is stable: the source that comes back renders to itself.

      Mechanism. One structure and one signature, in test/emit_fixture, which
      reach every value [emit.mli] exports. Part (a) renders, parses the
      source with ppxlib, and compares the two trees with locations set
      aside. A built tree carries [Location.none] everywhere and a parsed one
      carries real positions, and nothing here has an opinion about positions.

      What a round trip cannot see, and why the fixture is shared. A builder
      that builds a different well-formed thing round-trips just as well: fold
      [eseq] the other way and [(b; c); a] renders, parses and compares equal
      to itself. test/expect/emit.expected holds the source instead, and the
      two read the same fixture so neither drifts from what the other checks.

      Part (a)'s oracle is not a second printer. It is ppxlib's parser, which
      knows nothing about how the tree was built, so the two agree only where
      the source says what the tree said.

      Part (c) is the weaker claim that catches what part (a) cannot: a
      printer that renders two structures the same way still round-trips each
      of them.

      Coverage. Every value in [emit.mli] appears below. There is no mechanism
      that says so, and the two files move together by hand.

      What this says nothing about. Whether the emitted code compiles, or
      means anything. It parses, which is all a syntax builder owes. The
      emitters above it owe the rest.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        M1  In [Emit.eseq], fold left rather than right.
            -> nothing here, and test/expect/emit.expected moves. [(b; c); a]
               is a different thing that round-trips as well as [a; b; c]
               does, which is the shape of everything this law cannot see.
        M2  In [Emit.elist], end the cells in [()] rather than [[]].
            -> nothing here, and test/expect/emit.expected moves. [1 :: []]
               and [1 :: ()] are both well formed, so both survive a parse.
        M3  In [Emit.render], leave the header off.
            -> nothing here, and test/expect/emit.expected moves. A comment is
               not in the tree either time, so only the source shows it.

      All three reddened nothing when this law was the only reader, which is
      what put the source in test/expect/emit.expected beside it.
   -------------------------------------------------------------------------- *)

open Ocaml.Emit

let failures = ref 0

let fail fmt =
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt

(* A built tree carries no positions and a parsed one carries real ones, so
   the comparison sets both kinds aside.

   The stack is the one that is easy to miss. A parser records there where it
   took a pair of parentheses off, so [(1, 2)] and a tuple built here differ
   in a field neither the source nor [location] shows. Four of this law's
   constructs are parenthesised, and all four read as different until the
   stack goes too. *)
let strip =
  object
    inherit Ppxlib.Ast_traverse.map
    method! location _ = Ppxlib.Location.none
    method! location_stack _ = []
  end
;;

(* -- the runs -------------------------------------------------------------- *)

let () =
  let src = render Fixture.structure in
  match Ppxlib.Parse.implementation (Lexing.from_string src) with
  | exception e -> fail "(a) the source did not parse: %s" (Printexc.to_string e)
  | back ->
    if strip#structure back <> strip#structure Fixture.structure
    then fail "(a) the structure that came back is not the one rendered"
    else if not (String.equal (render back) src)
    then fail "(c) the structure rendered a second time gave different bytes"
    else
      pass
        "a structure renders, parses back and renders the same, over %d bytes"
        (String.length src)
;;

let () =
  let src = render_signature Fixture.signature in
  match Ppxlib.Parse.interface (Lexing.from_string src) with
  | exception e -> fail "(b) the source did not parse: %s" (Printexc.to_string e)
  | back ->
    if strip#signature back <> strip#signature Fixture.signature
    then fail "(b) the signature that came back is not the one rendered"
    else if not (String.equal (render_signature back) src)
    then fail "(c) the signature rendered a second time gave different bytes"
    else
      pass
        "a signature renders, parses back and renders the same, over %d bytes"
        (String.length src)
;;

(* [longident] is the one value the structure above does not reach, because
   every builder that takes a path goes through it. *)
let () =
  match longident "A.B.c" with
  | Ppxlib.Longident.Ldot (Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "A", "B"), "c")
    -> pass "a dotted path reads left to right"
  | _ -> fail "longident did not read \"A.B.c\" as A then B then c"
;;

let () =
  if !failures = 0
  then print_endline "law_emit: 0 failures"
  else (
    Printf.printf "law_emit: %d failures\n" !failures;
    exit 1)
;;
