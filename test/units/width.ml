(* -- the ruler counts columns ---------------------------------------------------

      The fold measures with {!Handsome.Utf8}, so a width is columns drawn and
      not bytes stored. On ASCII the two are the same number and nothing here
      would say which was being used; on anything else they are not.

      [«αβγ → αβγ»] is 11 columns and 21 bytes. At a ruler of 16 it fits, and
      under a byte measure it would break. That is the whole test, and it is a
      literal expected string rather than a law because no law states it: both
      measures are sound, and the difference is legibility.

      A golden would be the natural home, and test/expect/unicode.format is
      where one would go. Every input it holds is short enough to fit under
      either measure, so it pins nothing, and widening test/inputs to fix that
      moves every count in test/laws/law_layout.ml's record. This is the same
      claim for the cost of one file.
   -------------------------------------------------------------------------- *)

let check (what : string) (expected : string) (got : string) : unit =
  if String.equal expected got
  then Law.pass "%s" what
  else Law.fail "%s\n  expected %S\n  got      %S" what expected got
;;

let () =
  let f =
    match Core.Facts.of_grammar Lingo_grammars.Unicode_grammar.grammar with
    | Ok f -> f
    | Error _ -> failwith "the unicode grammar does not check"
  in
  let plan, _ = Plan.Lower.of_facts f in
  let layout = Layout.Lower.of_facts f in
  let boundary = Lex.boundary f in
  let format (width : int) (src : string) : string =
    let tree, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
    Lingo_runtime.Layout.format layout ~boundary ~width tree
  in
  let greek =
    "\xc2\xab\xce\xb1\xce\xb2\xce\xb3 \xe2\x86\x92 \xce\xb1\xce\xb2\xce\xb3\xc2\xbb"
  in
  check
    "11 columns of Greek fit a ruler of 16, where 21 bytes would not"
    greek
    (format 16 greek);
  (* The same shape in ASCII, where columns and bytes agree. 11 of either fits
     16 and 21 of either does not, so the pair brackets the measure. *)
  let latin = "\xc2\xababc \xe2\x86\x92 abc\xc2\xbb" in
  check "the same shape in ASCII letters fits too" latin (format 16 latin);
  let long =
    "\xc2\xab\xce\xb1\xce\xb2\xce\xb3 \xe2\x86\x92 \xce\xb1\xce\xb2\xce\xb3 \xe2\x86\x92 \
     \xce\xb1\xce\xb2\xce\xb3 \xe2\x86\x92 \xce\xb1\xce\xb2\xce\xb3\xc2\xbb"
  in
  check
    "23 columns do not"
    "\xc2\xab\n\
    \  \xce\xb1\xce\xb2\xce\xb3 \xe2\x86\x92\n\
    \  \xce\xb1\xce\xb2\xce\xb3 \xe2\x86\x92\n\
    \  \xce\xb1\xce\xb2\xce\xb3 \xe2\x86\x92\n\
    \  \xce\xb1\xce\xb2\xce\xb3\n\
     \xc2\xbb"
    (format 16 long);
  Law.summarise "width"
;;
