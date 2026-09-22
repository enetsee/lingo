(* -- the grammars the sampler's law reads -------------------------------------

      The example ladder, and the witness grammars beside it. A witness takes
      the shape nearest to one rejection and stops short of it, so between them
      they reach framings and operator tables the ladder does not: a delimited
      body with a separator, a separated body, two operators at one binding
      power, and a block with three postfix shapes.

      test/laws/corpus.ml holds the witness list alone and law_layout reads it.
      This one is the union, and it is here rather than there because widening
      that list would move four falsification records.

      rust and effekt are not here, and the reason is the engine rather than
      the grammars. law_sample fixes a one-pointing Boltzmann sampler so its
      counts are reproducible, and rust rejects into the window at 5 ms a draw
      under it, which is 10 s for one corpus against 0.07 s for every grammar
      above. Worse, the sampler reports no finite spread for rust, so part (g)
      would hold it to a tolerance of infinity and say nothing at all. Both are
      drawn in test/laws/law_fuzz.ml, which fixes [Bolts.Exact] instead and
      pays neither cost.
   -------------------------------------------------------------------------- *)

let ladder : (string * Core.Grammar.t) list =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar
  ; "json", Lingo_grammars.Json_grammar.grammar
  ; "calc", Lingo_grammars.Calc_grammar.grammar
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar
  ; "postfix", Lingo_grammars.Postfix_grammar.grammar
  ; "unicode", Lingo_grammars.Unicode_grammar.grammar
  ; "recovery", Lingo_grammars.Recovery_grammar.grammar
  ; "shapes", Lingo_grammars.Shapes_grammar.grammar
  ; "comments", Lingo_grammars.Comments_grammar.grammar
  ]
;;

(* The four the witnesses share with the ladder are dropped rather than drawn
   twice. *)
let all : (string * Core.Grammar.t) list =
  let already = List.map fst ladder in
  ladder
  @ List.filter
      (fun (name, _) -> not (List.mem name already))
      Lingo_witness.Witnesses.accepted
;;
