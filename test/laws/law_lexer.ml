(* -- the lexer table ----------------------------------------------------------

      (a) The table's shape holds. The segments ascend from zero, the two
          segment arrays are the same length, the state and class counts are
          the automaton's, and the cell count is their product.
      (b) [class_of] gives the class the automaton was built on. Every
          codepoint of a class gives that class, and one in no class gives
          [-1].
      (c) [step] gives the automaton's transition, for every state and
          every codepoint probed.
      (d) [accept] gives the token a state accepts, which is the one with
          the lowest case id.

      Mechanism. Seven grammars. Every state of each is probed on every
      codepoint from 0 to 0x2FF, on both sides of every segment boundary, on
      the least and greatest codepoint of every class, and at the edges of
      the codespace and the surrogate block.

      The oracle is the automaton itself, read a different way. Part (c)
      searches the interval list [Dfa.transitions] gives, and part (b)
      reads the class sets from [Dfa.table]. The table under test is those
      two flattened, so anything the flattening lost shows up as a
      disagreement.

      Coverage. 93 states over seven grammars, 72,321 state-codepoint probes.
      The codespace is 0x110000 wide. 0x300 of it is read exhaustively and
      the rest at the boundaries the table names, because that is where a
      flattening goes wrong.

      What this says nothing about. Whether the automaton is the right one.
      test/units/sexp_facts.ml checks one grammar's by hand, and
      test/lex_emit/law_lex.ml reads the token regexes directly.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        T1  In [Core.Lexer.step], index the row major table column major.
            -> part (c), 49,481 of 72,321 probes. Both indices stay inside
               the array, so the table reads as a permutation of the
               automaton.

               It reads zero in test/lex_emit/law_lex.ml. The emitted lexer
               indexes the table inline and never calls [step]; M7 there
               covers the emitter's own indexing.
        T2  In [Core.Lexer.flatten], drop the gap before a run that does not
            start where the last one ended.
            -> part (b), 14 probes, and part (c), 6. Every segment past the
               gap shifts down one.

               It reads zero in test/lex_emit/law_lex.ml, and can do nothing
               else. The only gap the partition leaves is the surrogate
               block, and no decoded scalar is a surrogate, so no input
               reaches one.
        T3  In [Core.Lexer.of_facts], take the last accepting case id rather
            than the first.
            -> part (d), 3 states across sexp, json and shapes. A token's
               case id is its position in the grammar, so the head of
               [accepts] is the token declared first.
        T4  In [Core.Lexer.class_of], search with [<] where it searches with
            [<=].
            -> part (b), 182 probes, and part (c), 356. A codepoint that
               starts a segment reads as the segment before it. The probes
               on both sides of every boundary are there for this.
        T5  In [Core.Lexer.flatten], leave the runs in the order the classes
            gave them rather than sorting by least codepoint.
            -> parts (a), (b) and (c): 10, 3,726 and 2,646. Part (a) reads it
               first. The segments stop ascending, and a binary search over
               them then lands arbitrarily.
   -------------------------------------------------------------------------- *)

let grammars : (string * Core.Grammar.t) list =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar
  ; "json", Lingo_grammars.Json_grammar.grammar
  ; "calc", Lingo_grammars.Calc_grammar.grammar
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar
  ; "postfix", Lingo_grammars.Postfix_grammar.grammar
  ; "shapes", Lingo_grammars.Shapes_grammar.grammar
  ; "unicode", Lingo_grammars.Unicode_grammar.grammar
  ]
;;

let probes = ref 0
let states = ref 0

(* Where the automaton sends [state] on a codepoint, read off the interval
   lists. *)
let transition (dfa : Redfa.Dfa.t) (state : int) (codepoint : int) : int =
  match
    List.find_opt
      (fun ((set, _) : Ucharset.t * int) -> Ucharset.mem set codepoint)
      (Redfa.Dfa.transitions dfa state)
  with
  | Some (_, dest) -> dest
  | None -> -1
;;

(* The class holding a codepoint, read off the class sets. *)
let klass (classes : Ucharset.t array) (codepoint : int) : int =
  let found = ref (-1) in
  Array.iteri
    (fun (klass : int) (set : Ucharset.t) ->
       if Ucharset.mem set codepoint then found := klass)
    classes;
  !found
;;

(* A dense sweep low down, then the edges of every segment, every class, the
   surrogate block and the codespace. A flattening goes wrong at a boundary,
   so every boundary is probed. *)
let codepoints (table : Core.Lexer.t) (classes : Ucharset.t array) : int list =
  let out = ref [] in
  let add (codepoint : int) : unit = out := codepoint :: !out in
  for codepoint = 0 to 0x2FF do
    add codepoint
  done;
  Array.iter
    (fun (lo : int) ->
       add (lo - 1);
       add lo;
       add (lo + 1))
    table.segments;
  Array.iter
    (fun (set : Ucharset.t) ->
       Option.iter add (Ucharset.min_elt_opt set);
       Option.iter add (Ucharset.max_elt_opt set))
    classes;
  List.iter add [ -1; 0xD7FF; 0xD800; 0xDFFF; 0xE000; 0x10FFFF; 0x110000 ];
  List.sort_uniq Int.compare !out
;;

let check (name : string) (facts : Core.Facts.t) : unit =
  let table = Core.Lexer.of_facts facts in
  let dfa = facts.lexer in
  let classes = (Redfa.Dfa.table dfa).classes in
  (* (a) *)
  if Array.length table.segments <> Array.length table.segment_class
  then
    Law.fail
      "%s: %d segments against %d classes"
      name
      (Array.length table.segments)
      (Array.length table.segment_class);
  if table.segments.(0) <> 0
  then Law.fail "%s: the first segment starts at %d" name table.segments.(0);
  Array.iteri
    (fun (index : int) (lo : int) ->
       if index > 0 && lo <= table.segments.(index - 1)
       then
         Law.fail
           "%s: segment %d starts at %d, after %d"
           name
           index
           lo
           table.segments.(index - 1))
    table.segments;
  if table.num_classes <> Array.length classes
  then
    Law.fail
      "%s: %d classes against the automaton's %d"
      name
      table.num_classes
      (Array.length classes);
  if table.num_states <> Redfa.Dfa.num_states dfa
  then
    Law.fail
      "%s: %d states against the automaton's %d"
      name
      table.num_states
      (Redfa.Dfa.num_states dfa);
  if Array.length table.next <> table.num_states * table.num_classes
  then
    Law.fail
      "%s: %d cells for %d states and %d classes"
      name
      (Array.length table.next)
      table.num_states
      table.num_classes;
  (* (b) and (c) *)
  List.iter
    (fun (codepoint : int) ->
       let c = Core.Lexer.class_of table codepoint in
       let expected =
         if codepoint < 0 || codepoint > Ucharset.max_codepoint
         then -1
         else klass classes codepoint
       in
       if c <> expected
       then
         Law.fail
           "%s: U+%X is in class %d, and the automaton puts it in %d"
           name
           codepoint
           c
           expected;
       for state = 0 to table.num_states - 1 do
         incr probes;
         let dest = Core.Lexer.step table ~state ~klass:c in
         let oracle =
           if codepoint < 0 || codepoint > Ucharset.max_codepoint
           then -1
           else transition dfa state codepoint
         in
         if dest <> oracle
         then
           Law.fail
             "%s: state %d on U+%X goes to %d, and the automaton goes to %d"
             name
             state
             codepoint
             dest
             oracle
       done)
    (codepoints table classes);
  (* (d) *)
  Array.iteri
    (fun (state : int) (accept : Core.Kind.t option) ->
       incr states;
       let expected =
         match Redfa.Dfa.accepts dfa state with
         | [] -> None
         | token_id :: _ -> Some (Core.Facts.token facts token_id).kind
       in
       if not (Option.equal Core.Kind.equal accept expected)
       then
         Law.fail
           "%s: state %d accepts %s, and the automaton accepts %s"
           name
           state
           (match accept with
            | None -> "nothing"
            | Some kind -> Core.Kind.Name.to_string (Core.Facts.kind_name facts kind))
           (match expected with
            | None -> "nothing"
            | Some kind -> Core.Kind.Name.to_string (Core.Facts.kind_name facts kind)))
    table.accept
;;

let () =
  List.iter
    (fun ((name, grammar) : string * Core.Grammar.t) ->
       match Core.Facts.of_grammar grammar with
       | Error errors ->
         List.iter
           (fun (error : Core.Error.t) -> Format.printf "%a@." Core.Error.pp error)
           errors;
         Law.fail "%s: the grammar does not check" name
       | Ok facts -> check name facts)
    grammars;
  Law.pass
    "%d states and %d state-codepoint probes over %d grammars"
    !states
    !probes
    (List.length grammars);
  Law.exit_on_failure ()
;;
