(* -- a generated corpus -------------------------------------------------------

      Seeds edited into inputs near the language they came from, so a law runs
      on far more than anyone wrote by hand.

      test/parse_emit/law_parse.ml and test/laws/law_layout.ml both read this.
      A copy in each would be two corpora that drift, and the numbers in a
      falsification record are only reproducible while the corpus is.
   -------------------------------------------------------------------------- *)

(* A linear congruential generator. The corpus is then the same on every run,
   so a count in the record above is reproducible and a failure names an
   input that can be pasted back. *)
let state = ref 0x2545F491

let roll (bound : int) : int =
  state := ((!state * 1103515245) + 12345) land 0x3FFFFFFF;
  if bound <= 0 then 0 else !state mod bound
;;

let pick (xs : 'a array) : 'a = xs.(roll (Array.length xs))

let texts (facts : Core.Facts.t) (src : string) : string array =
  Array.map (fun (t : Lingo_runtime.Token.t) -> t.text) (Lex.run facts src)
;;

(* One edit to a token sequence. Every shape here keeps the pieces the
   grammar's own lexer produced, so the result stays close enough to the
   language to reach a parser's recovery rather than its first refusal. *)
let edit (pool : string array) (tokens : string array) : string array =
  let n = Array.length tokens in
  let at = roll (max n 1) in
  let drop (i : int) =
    Array.of_list (List.filteri (fun j _ -> j <> i) (Array.to_list tokens))
  in
  let insert (i : int) (text : string) =
    Array.concat [ Array.sub tokens 0 i; [| text |]; Array.sub tokens i (n - i) ]
  in
  match roll 6 with
  | _ when n = 0 -> [| pick pool |]
  | 0 -> drop at
  | 1 -> insert at tokens.(at)
  | 2 ->
    let copy = Array.copy tokens in
    let other = roll n in
    copy.(at) <- tokens.(other);
    copy.(other) <- tokens.(at);
    copy
  | 3 ->
    let copy = Array.copy tokens in
    copy.(at) <- pick pool;
    copy
  | 4 -> insert at (pick pool)
  | _ -> Array.sub tokens 0 at
;;

(* Between one and eight tokens drawn from the pool. A seed's shape survives
   every edit above, so a corpus of edits alone never reaches a shape no seed
   had. *)
let drawn (pool : string array) : string =
  let separator = if roll 2 = 0 then " " else "" in
  String.concat separator (List.init (1 + roll 8) (fun _ -> pick pool))
;;

let rounds_per_seed = 400
let draws_per_grammar = 8000

(* Each round starts again from the seed and applies up to five edits, so the
   corpus stays near inputs the grammar nearly accepts. A single walk drifts
   away from them, and an input the grammar cannot begin to read exercises
   one refusal rather than a recovery. *)
let inputs ?(depth = 1) (facts : Core.Facts.t) (seeds : string list) : string list =
  let rounds_per_seed = rounds_per_seed * depth in
  let draws_per_grammar = draws_per_grammar * depth in
  let pool =
    Array.of_list
      (List.sort_uniq
         String.compare
         (List.concat_map (fun src -> Array.to_list (texts facts src)) seeds))
  in
  let from_seeds =
    List.concat_map
      (fun (src : string) ->
         let seed = texts facts src in
         List.init rounds_per_seed (fun _ ->
           let tokens = ref seed in
           for _ = 0 to roll 5 do
             tokens := edit pool !tokens
           done;
           String.concat "" (Array.to_list !tokens)))
      seeds
  in
  from_seeds @ List.init draws_per_grammar (fun _ -> drawn pool)
;;
