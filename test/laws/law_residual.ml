(* -- the residual ------------------------------------------------------------

      (a) The residual at a position names nothing the parse will not take
          there.
      (b) It names everything the parse will take there.
      (c) A residual is ascending with no repeats.
      (d) The corpus reaches holes.
      (e) The corpus reaches every grammar, every kind of position, and lexes
          every token kind at least once.
      (f) [at] refuses a state the plan does not fit, and says so.
      (g) At a byte, the residual is exactly the set of kinds the parse takes
          there.
      (h) The tables a grammar's generated module holds give that same set,
          read off the tree with no plan in hand.

      Mechanism. The interpreter hands out its position wherever it reads the
      cursor, and [Ir.Residual.at] turns one of those into the kinds the
      grammar admits there. The oracle is a second parse: take what the first
      parse had read by that point, splice one token of the kind under test
      on the end, and run it again. The kind is admitted where that
      parse reaches the same position and reads past the token without
      reporting on the way.

      The oracle shares nothing with the residual but the plan. A second walk
      of the same tables would show only that two walks agree, which is not the
      claim.

      The probe takes a position rather than a byte offset. Several positions sit at
      one token and they admit different things: the parse tests a production's
      FIRST set, chooses an arm, and then expects that arm's opener, all
      without moving.

      What the probe cannot say, and why (g) is here. A position the parse
      reaches only once it has declined what is under the cursor cannot be
      reached with that kind there, whatever the input. The outer climb of
      [1+2*3] is the case: it runs only after the inner climbs have refused,
      and a [*] is what an inner climb takes. Parts (a) and (b) count those
      pairs and print them rather than settling them either way.

      Part (g) takes a byte where (a) and (b) take a step, and it has no such
      gap: the parse either reads past the token or it does not.
      The step a byte is taken from is the one the parse was at when it first
      reached that token, because nothing has branched on it yet. It is also
      the half of the claim that matters for constrained decoding: a kind the
      residual names and the parse refuses is text a generator would emit and
      the grammar would reject.

      Part (h) is what keeps the generated module honest. It reads the tables
      out of [Emitted_parsers], walks them over the tree the interpreter built,
      and compares. So the thing under test is what ships, rather than tables
      this file could build for itself. law_ahead checks the same tables
      against the plan they came from, structure for structure; between the
      two, a fault in the emitter shows as a wrong set here and as a wrong
      literal there.

      It found one, on the day rust and effekt joined this corpus. An operand
      that is a token gets a base role wrapped round it, and that role's last
      point carries the operators that may extend it. An operand that is a rule
      -- rust's [{}] and [match x {}], effekt's [do], [try] and [{}] -- builds
      its own node and no role wraps it, so the tree held the atom where every
      other operand shape holds a role, and the walk popped straight out to the
      parent. After [{}] the tables offered [;] alone where the grammar takes
      any of twelve operators. 13 bytes read it.

      The fix is [Table.rule_atoms]: the edge that takes a rule atom carries
      the climb. The edge rather than the point, because a hole reaches the
      same point and nothing may follow one; and gated on the edge leaving an
      expression behind, because the same [Block] is a function's body
      elsewhere and nothing may follow it there. M17 is each half of that.

      calc had the same shape and never reached it: [(1)] alone is not among
      its inputs, and every other use of [Parens] there sits under a prefix or
      an infix. The walk over the tree is the only reader of this, so the
      defect was invisible to every part but (h).

      A byte is the meaningful token rather than the raw index. The cursor looks past
      trivia, so the two positions either side of a space are looking at the
      same token and settling the same thing. Keying on the raw index instead
      read 73 failures in sexp, json, postfix, recovery and shapes, all of
      them the trivia between a body's elements.

      Coverage. The 221 inputs in test/inputs, over ten grammars: 3,823
      positions, 74,394 pairs of a kind and a position settled and 55,734 the
      parse cannot be put at. 111 of the positions are holes, where the kind
      really under the cursor is one the residual leaves out. Part (g) covers
      1,574 bytes and 52,010 pairs of a kind and a byte, with none left
      undecided.

      Part (e) is what keeps the rest honest. It counts positions per grammar
      and per kind of position, and fails where either reads zero, so a corpus
      that stops reaching something says so rather than passing quietly. The
      kind of a position is the last step [Ir.Residual.State.pp] prints.
      Which instruction forms these same parses run is law_interp part (d)'s
      count, over the same inputs.

      Falsification. Re-run on 2026-09-22, after rust and effekt joined the
      corpus. Every mutation was applied, built, run and reverted, and the
      result recorded is the one observed. A count counts pairs of a kind and a
      position, and the grammars beside it are where they came from. Each edit is
      named exactly, because a mutation nobody can re-create is a mutation
      nobody can check.

        M1  In [Residual.entered], give [whole plan null body] where the steps
            run out and the position is inclusive, rather than the gate.
            -> part (b), 85 pairs: sexp 18, json 26, postfix 2, unicode 10,
               recovery 3, shapes 6, rust 10, effekt 10. A commit's body is
               often a bare [Bump],
               which names no kind of its own, so the position loses the set that
               admitted it.

               The other half of what [entered] buys reads nothing. Answering
               [gate, true] adds what follows the commit, and those are kinds
               that cannot be under the cursor at a body a dispatch chose, so no
               input puts the parse there to disagree.
        M2  In [Residual.ends], give [true] for [May_exit_reporting] as well.
            -> part (a), 15: json 5, postfix 2, unicode 2, recovery 3,
               shapes 2, rust 1. Part (g), the same 15. A body that forbids a
               trailing separator
               can still be ended at one, by taking the separator and reporting
               it, and the residual then offers the closer straight after a
               separator. This is the mutation that found the rule.

               It moves five of the eight *.residual dumps: json, postfix,
               recovery, shapes and unicode.
        M3  In [Residual.at]'s [walk], drop the recursion into the frame above.
            -> part (b), 532: calc 8, postfix 20, recovery 5, shapes 94,
               rust 66, effekt 339. Part (g), 374. Part (h), 207. What follows a
               rule goes missing at every position the rule's own body can
               complete from.
        M4  In [Residual.whole], give [false] for an [Alt]'s nullability.
            -> part (b), 1,503: shapes 110, rust 10, effekt 1,383. Part (g),
               882. effekt is the grammar of optional children -- a parameter's
               annotation, a definition's return type, a match arm's guard --
               and what follows one is what goes missing. shapes.residual moves.
        M5  In [Residual.remains], resume a loop at [e.state] rather than at
            [goto].
            -> part (a), 185: postfix 16, shapes 40, rust 28, effekt 101.
               Part (b), 75. Part (g), 149 and 51. Part (h), 49. After an
               element a separator or the closer comes next, and the mutation
               offers another element.
        M6  In [Residual.climb_set], take every operator whatever its binding
            power.
            -> nothing. Every climb sits under a chain of climbs reaching the
               [Pratt] instruction's own [min_bp], which the lowering always
               writes as 0, and a climb may end wherever it is, so the walk
               already unions [climb_set 0] in. Which frame takes an operator
               follows from the binding power, and a set of kinds does not record
               that.
        M7  In [Residual.at], call [walk] on the innermost frame with
            [~inclusive:false].
            -> part (a), 3,680: json 58, calc 52, rassoc 24, postfix 41,
               recovery 98, shapes 94, rust 742, effekt 2,571. Part (b), 5,917:
               sexp 136, json 806, calc 121, rassoc 54, postfix 120, unicode 38,
               recovery 108, shapes 152, rust 720, effekt 3,662. Part (g), 1,720
               and 2,549. Part (h), 820. M12 is the same claim at the other end
               of the stack.
        M8  In [Residual.expression], give [first, false] at an [Operand]
            rather than letting a nullable operand's climb through.
            -> part (b), 66: calc 8, rassoc 2, rust 8, effekt 48. Part (g),
               45. Part (h), 15.

               This read nothing at all until the corpus reached an atom rule
               with a position it could complete from. The claim was about the
               corpus rather than the code, and the corpus has moved twice
               since.
        M9  In [Residual.head_set], leave out the prefix operators.
            -> part (b), 239: calc 31, rassoc 19, rust 33, effekt 156.
               Part (g), 44. calc.residual and rassoc.residual move.
       M10  In [Interp.loop], record [!state] as the state to resume at rather
            than [dest].
            -> part (a), 60: postfix 7, shapes 18, rust 6, effekt 29.
               Part (b), 162. Part (g), 40 and 132. Part (h), 39. The
               interpreter's half of M5. The claim fails whether the plan walk
               or the parse has the state wrong, which is what makes the
               threading worth testing rather than trusting.

               It also reddens law_interp part (c) on four inputs rust and
               effekt accept, which it did not on the eight small grammars: a
               body that resumes in the wrong state reports on a clean parse
               once the body has enough elements.
       M11  In [Residual.whole], give [true] for a [Commit]'s nullability.
            -> part (a), 1,219: json 12, calc 5, postfix 29, recovery 27,
               shapes 6, rust 346, effekt 794. Part (g), 1,194. All eight
               *.residual dumps move; rust and effekt have none.
       M12  In [Residual.at]'s [walk], call the frame above with
            [~inclusive:true].
            -> part (a), 2,072: calc 78, rassoc 48, postfix 90, recovery 5,
               shapes 23, rust 232, effekt 1,596. Part (b), 494. Part (g), 1,627
               and 354. Part (h), 271. The call is counted twice: as the frame
               above's pending instruction and as the frame below.
       M13  In [Residual.taken], ignore [goto] and give an element every kind its
            state takes.
            -> nothing. The lowering writes one transition per loop state, so the
               kinds that led to a destination and the kinds the state takes are
               the same array. A plan with two transitions out of one state would
               separate them, and no lowering writes one.
       M14  In [Residual.nullable_rules], start [settled] at [true] so the
            fixpoint never runs.
            -> nothing. No rule in the ten grammars is nullable, so the table
               reads false either way. A rule whose every child is optional would
               read it, and none of the ten has one.
       M15  Empty calc's input list in test/inputs.
            -> part (e), twice: "no position came from these grammars: calc" and
               "no input lexes these token kinds: calc T_INT". Parts (a) to (d)
               stay green, which is the whole reason (e) is here.
       M16  Drop postfix's dotted inputs ["a.b"], ["a."], ["a.?"] and
            ["a.b\[2\]?+1"] from test/inputs.
            -> part (e), "no position was one of these kinds: postfix". The
               dotted forms are the only ones that reach a [Postfix] step with a
               body, so the coverage claim rests on four inputs.

       M17  In [Residual.Table.of_body], drop the [base_kind] gate on the
            rule-atom edge, so every rule atom carries a climb wherever it is a
            child.
            -> part (h), 14, and calc.residual moves. rust's [Block] is an
               expression and it is also a function's body, and the mutation
               offers the operators after the second. Turning the split off
               altogether reads the same 14 the other way round, which is the
               defect this closed: see the note under part (h).

            Part (h) reads nothing for a mutation of the walk the *.residual
            dumps come from, because both sides move together. It reads
            something for a mutation of the tables alone, which M17 is, and
            that is the half of (h) that found the defect below. Parts (a), (b)
            and (g) are what check the walk itself.
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

(* -- the corpus ------------------------------------------------------------ *)

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; tables : Lingo_runtime.Ahead.t
    (** The tables the emitter wrote, so part (h) reads what ships rather than
        what this file could build for itself. *)
  ; inputs : string list
  }

(* Accepted and broken input together. This law puts the same question at
   every position of either, so the split between them makes no difference
   here. *)
let corpus =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; tables = Emitted_parsers.Sexp_residual.tables
    ; inputs = Inputs.all Inputs.sexp
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; tables = Emitted_parsers.Json_residual.tables
    ; inputs = Inputs.all Inputs.json
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; tables = Emitted_parsers.Calc_residual.tables
    ; inputs = Inputs.all Inputs.calc
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; tables = Emitted_parsers.Rassoc_residual.tables
    ; inputs = Inputs.all Inputs.rassoc
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; tables = Emitted_parsers.Postfix_residual.tables
    ; inputs = Inputs.all Inputs.postfix
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; tables = Emitted_parsers.Unicode_residual.tables
    ; inputs = Inputs.all Inputs.unicode
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; tables = Emitted_parsers.Recovery_residual.tables
    ; inputs = Inputs.all Inputs.recovery
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; tables = Emitted_parsers.Shapes_residual.tables
    ; inputs = Inputs.all Inputs.shapes
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; tables = Emitted_parsers.Rust_residual.tables
    ; inputs = Inputs.all Inputs.rust
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; tables = Emitted_parsers.Effekt_residual.tables
    ; inputs = Inputs.all Inputs.effekt
    }
  ]
;;

(* -- the probe ------------------------------------------------------------- *)

(* A token of each kind, to splice, and the kinds with none. A keyword or a
   punctuation token carries its own text. A pattern token's text is whatever
   it matched, so the corpus has to supply one. A kind with no example is one
   this law cannot test, which is why it comes back rather than being
   dropped. *)
let samples (facts : Core.Facts.t) (inputs : string list)
  : (Ir.Kind.t * string) list * Ir.Kind.t list
  =
  let lexed = List.concat_map inputs ~f:(fun src -> Array.to_list (Lex.run facts src)) in
  let text_of (def : Core.Token.def) : string option =
    match Core.Token.text def with
    | Some text -> Some text
    | None ->
      List.find_map lexed ~f:(fun (t : Lingo_runtime.Token.t) ->
        if Int.equal t.kind (Core.Kind.to_int def.kind) then Some t.text else None)
  in
  let have, missing =
    Array.to_list facts.tokens
    |> List.filter ~f:(fun (def : Core.Token.def) -> not (Core.Token.is_trivia def))
    |> List.fold_left ~init:([], []) ~f:(fun (have, missing) (def : Core.Token.def) ->
      let kind = Core.Kind.to_int def.kind in
      match text_of def with
      | Some text -> (kind, text) :: have, missing
      | None -> have, kind :: missing)
  in
  List.rev have, List.rev missing
;;

(* Runs the parse again over what it had read by [at_index], with one token of
   [kind] spliced on the end, and records every position it reached: the
   position, the index it had read to, and how many times it had reported.

   A position is what makes this the right question. Several positions sit at
   one token index and they admit different things: the parse tests a
   production's FIRST set, chooses an arm, and expects that arm's opener, all
   without moving. So the probe looks at what happened at the position the
   residual was taken from, rather than at what happened to the input.

   This is the oracle, and it shares nothing with [Ir.Residual.at] but the
   plan. A second walk of the same tables would show only that the two walks
   agree, which is not the claim. *)
let probe
      (plan : Ir.Plan.t)
      (entry : int)
      (tokens : Lingo_runtime.Token.t array)
      ~(at_index : int)
      ~(kind : Ir.Kind.t)
      ~(text : string)
  : (Ir.Residual.State.t * int * int) list
  =
  let prefix = Array.sub tokens ~pos:0 ~len:at_index in
  let steps = ref [] in
  let at (state : Ir.Residual.State.t) ~(index : int) ~(reported : int) =
    steps := (state, index, reported) :: !steps
  in
  let _ =
    Interp.run
      ~at
      plan
      entry
      (Array.append prefix [| { Lingo_runtime.Token.kind; text } |])
  in
  List.rev !steps
;;

(* What the probe made of one position.

   [Unknown] is where the parse never reached the position with that token
   under the cursor. Something earlier at the same index went another way, so
   no input puts the parse there and the probe has nothing to say. An
   expression is where this shows: the outer climb of [1+2*3] is reached only
   once the inner climbs have declined what is there, and a [*] is what an
   inner climb takes. *)
type verdict =
  | Took
  | Refused
  | Unknown

(* The parse took the token where it read past it and reported nothing on the
   way. The step after says it read past it, and a parse that never reads past
   it never took it: a drain sweeps the token up and the root closes behind
   it, reaching no further position. *)
let outcome
      (steps : (Ir.Residual.State.t * int * int) list)
      (state : Ir.Residual.State.t)
      ~(at_index : int)
  : verdict
  =
  let rec arrivals acc (steps : (Ir.Residual.State.t * int * int) list) =
    match steps with
    | [] -> List.rev acc
    | (reached, index, reported) :: rest ->
      let acc =
        if Int.equal index at_index && Ir.Residual.State.equal reached state
        then (reported, rest) :: acc
        else acc
      in
      arrivals acc rest
  in
  let took ((reported, rest) : int * (Ir.Residual.State.t * int * int) list) =
    match List.find_opt rest ~f:(fun (_, index, _) -> index > at_index) with
    | Some (_, _, beyond) -> Int.equal beyond reported
    | None -> false
  in
  match arrivals [] steps with
  | [] -> Unknown
  | reached -> if List.exists reached ~f:took then Took else Refused
;;

(* Whether the probe parse read past the spliced token without reporting on
   the way, wherever it managed it.

   Put at a byte rather than at a step, this always settles: the parse either
   got past the token or it did not. The step
   the parse was at when it arrived is the one the residual is taken from,
   because nothing has branched on the new token yet. *)
let took_here (steps : (Ir.Residual.State.t * int * int) list) ~(at_index : int) : bool =
  let rec arrive (steps : (Ir.Residual.State.t * int * int) list) : bool =
    match steps with
    | [] -> false
    | (_, index, reported) :: rest ->
      if Int.equal index at_index
      then (
        match List.find_opt rest ~f:(fun (_, index, _) -> index > at_index) with
        | Some (_, _, beyond) -> Int.equal beyond reported
        | None -> false)
      else arrive rest
  in
  arrive steps
;;

(* -- the runs -------------------------------------------------------------- *)

let inputs_seen = ref 0
let positions = ref 0
let probes = ref 0
let holes = ref 0
let no_example = ref []
let bytes = ref 0
let byte_pairs = ref 0
let from_tree = ref 0
let tree_apart = ref []
let over_byte = ref []
let under_byte = ref []
let decided = ref 0
let unknown = ref 0
let over = ref []
let under = ref []
let unordered = ref []

(* The first meaningful token at or after [index], as the parse reads it: the
   cursor looks past trivia, so two positions either side of a space are
   looking at the same token and settling the same thing. *)
let meaningful (plan : Ir.Plan.t) (tokens : Lingo_runtime.Token.t array) (index : int)
  : int
  =
  let rec go (i : int) : int =
    if i >= Array.length tokens
    then i
    else if
      Array.exists plan.Ir.Plan.trivia ~f:(Int.equal tokens.(i).Lingo_runtime.Token.kind)
    then go (i + 1)
    else i
  in
  go index
;;

let kind_under (plan : Ir.Plan.t) (tokens : Lingo_runtime.Token.t array) (index : int)
  : Ir.Kind.t option
  =
  let at = meaningful plan tokens index in
  if at >= Array.length tokens then None else Some tokens.(at).Lingo_runtime.Token.kind
;;

let kind_names (facts : Core.Facts.t) : (Ir.Kind.t * string) list =
  List.map (Core.Kind.Table.kinds facts.kinds) ~f:(fun (kind : Core.Kind.t) ->
    ( Core.Kind.to_int kind
    , Core.Kind.Name.to_string (Core.Kind.Table.name facts.kinds kind) ))
;;

(* What the corpus reached, so a run that stopped reaching something says so
   rather than passing quietly. Two counts: one per grammar, and one per kind
   of position.

   The kind of a position is the last step [Ir.Residual.State.pp] prints for
   it. Reading it back off the printed form keeps the test from carrying a
   second walk of the plan, which would be the thing under test written
   twice. *)
let by_grammar : (string, int) Hashtbl.t = Hashtbl.create 16
let by_form : (string, int) Hashtbl.t = Hashtbl.create 16

let count (table : (string, int) Hashtbl.t) (key : string) : unit =
  Hashtbl.replace table key (1 + Option.value (Hashtbl.find_opt table key) ~default:0)
;;

let forms = [ "item"; "arm"; "child"; "loop"; "emits"; "operand"; "climbing"; "postfix" ]

let form_of (state : Ir.Residual.State.t) : string =
  let printed = Format.asprintf "%a" Ir.Residual.State.pp state in
  let last =
    match String.rindex_opt printed '/' with
    | None -> printed
    | Some at -> String.sub printed ~pos:(at + 1) ~len:(String.length printed - at - 1)
  in
  match String.index_opt last ' ' with
  | None -> last
  | Some at -> String.sub last ~pos:0 ~len:at
;;

let byte_of (tokens : Lingo_runtime.Token.t array) (index : int) : int =
  let total = ref 0 in
  for i = 0 to min index (Array.length tokens) - 1 do
    total := !total + String.length tokens.(i).Lingo_runtime.Token.text
  done;
  !total
;;

let ascending (kinds : Ir.Kind.t array) : bool =
  let ok = ref true in
  Array.iteri kinds ~f:(fun i k -> if i > 0 && kinds.(i - 1) >= k then ok := false);
  !ok
;;

let () =
  List.iter corpus ~f:(fun c ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> fail "%s: the grammar does not check" c.name
    | Ok facts ->
      let plan, _ = Plan.Lower.of_facts facts in
      let entry = plan.Ir.Plan.roots.(0) in
      let names = kind_names facts in
      let name_of (kind : Ir.Kind.t) : string =
        Option.value (List.assoc_opt kind names) ~default:(string_of_int kind)
      in
      let probe_kinds, unsampled = samples facts c.inputs in
      List.iter unsampled ~f:(fun kind ->
        no_example := Printf.sprintf "%s %s" c.name (name_of kind) :: !no_example);
      List.iter c.inputs ~f:(fun src ->
        incr inputs_seen;
        let tokens = Lex.run facts src in
        let seen = ref [] in
        let at (state : Ir.Residual.State.t) ~(index : int) ~reported:(_ : int) =
          seen := (state, index) :: !seen
        in
        let root, _ = Interp.run ~at plan entry tokens in
        (* One probe serves every position at an index, so it is run once per
           index and read back per position. *)
        let probed = Hashtbl.create 64 in
        let probe_at (index : int) (kind : Ir.Kind.t) (text : string) =
          match Hashtbl.find_opt probed (index, kind) with
          | Some result -> result
          | None ->
            incr probes;
            let result = probe plan entry tokens ~at_index:index ~kind ~text in
            Hashtbl.add probed (index, kind) result;
            result
        in
        (* The step the parse was at when it first reached a byte. Its
           residual is what may appear at that byte, because nothing has yet
           branched on what is there. *)
        let firsts =
          List.fold_left (List.rev !seen) ~init:[] ~f:(fun acc (state, index) ->
            let token = meaningful plan tokens index in
            if List.mem_assoc token ~map:acc then acc else (token, (index, state)) :: acc)
          |> List.rev
        in
        List.iter firsts ~f:(fun (token, (index, state)) ->
          incr from_tree;
          let offset = byte_of tokens token in
          let known = Ir.Residual.at plan state in
          let walked =
            (* A table the emitter got wrong can send the walk off the end of
               itself. That is a failure to report rather than one to die of. *)
            match Lingo_runtime.Ahead.at c.tables root ~offset with
            | walked -> walked
            | exception Invalid_argument message ->
              fail "(h) %s on %S at %d: %s" c.name src offset message;
              known
          in
          if walked <> known
          then
            tree_apart
            := ( c.name
               , src
               , offset
               , String.concat ~sep:" " (List.map (Array.to_list walked) ~f:name_of)
               , String.concat ~sep:" " (List.map (Array.to_list known) ~f:name_of) )
               :: !tree_apart);
        List.iter firsts ~f:(fun (_, (index, state)) ->
          incr bytes;
          let residual = Ir.Residual.at plan state in
          List.iter probe_kinds ~f:(fun (kind, text) ->
            incr byte_pairs;
            let steps = probe_at index kind text in
            let named = Array.exists residual ~f:(Int.equal kind) in
            let name = name_of kind in
            match took_here steps ~at_index:index, named with
            | false, true -> over_byte := (c.name, src, state, name) :: !over_byte
            | true, false -> under_byte := (c.name, src, state, name) :: !under_byte
            | true, true | false, false -> ()));
        List.iter (List.rev !seen) ~f:(fun (state, index) ->
          incr positions;
          count by_grammar c.name;
          count by_form (form_of state);
          let residual = Ir.Residual.at plan state in
          if not (ascending residual) then unordered := (c.name, src, state) :: !unordered;
          (match kind_under plan tokens index with
           | Some kind when not (Array.exists residual ~f:(Int.equal kind)) -> incr holes
           | Some _ | None -> ());
          List.iter probe_kinds ~f:(fun (kind, text) ->
            let steps = probe_at index kind text in
            let named = Array.exists residual ~f:(Int.equal kind) in
            let name = name_of kind in
            match outcome steps state ~at_index:index, named with
            | Took, false ->
              incr decided;
              under := (c.name, src, state, name) :: !under
            | Refused, true ->
              incr decided;
              over := (c.name, src, state, name) :: !over
            | Unknown, _ -> incr unknown
            | Took, true | Refused, false -> incr decided))))
;;

let report (what : string) (bad : (string * string * Ir.Residual.State.t * string) list)
  : unit
  =
  match bad with
  | [] -> ()
  | bad ->
    let per_grammar =
      List.map corpus ~f:(fun c ->
        ( c.name
        , List.length (List.filter bad ~f:(fun (grammar, _, _, _) -> grammar = c.name)) ))
      |> List.filter ~f:(fun (_, count) -> count > 0)
      |> List.map ~f:(fun (grammar, count) -> Printf.sprintf "%s %d" grammar count)
    in
    fail "%s, %d times: %s" what (List.length bad) (String.concat ~sep:", " per_grammar);
    List.iteri (List.rev bad) ~f:(fun index (grammar, src, state, kind) ->
      if index < 3
      then
        Format.printf
          "     %s on %S at %a: %s@."
          grammar
          src
          Ir.Residual.State.pp
          state
          kind)
;;

let () =
  report "(a) the residual names a kind the parse will not take" !over;
  report "(b) the parse takes a kind the residual leaves out" !under;
  report "(g) at a byte, the residual names a kind the parse will not take" !over_byte;
  report "(g) at a byte, the parse takes a kind the residual leaves out" !under_byte;
  if !over_byte = [] && !under_byte = []
  then
    pass
      "at a byte the residual is exactly what the parse takes: %d bytes, %d kinds, none \
       left undecided"
      !bytes
      !byte_pairs;
  if !over = [] && !under = []
  then
    pass
      "the residual is what the parse admits: %d positions over %d inputs, %d parses, %d \
       kinds settled at a position and %d the parse cannot be put there to try"
      !positions
      !inputs_seen
      !probes
      !decided
      !unknown;
  (match !unordered with
   | [] -> pass "every residual is ascending with no repeats"
   | bad ->
     fail "(c) %d residuals are not ascending with no repeats" (List.length bad);
     List.iter bad ~f:(fun (grammar, src, state) ->
       Format.printf "     %s on %S at %a@." grammar src Ir.Residual.State.pp state));
  (match !tree_apart with
   | [] ->
     pass
       "the emitted tables read off the tree give what the plan gives, over %d bytes"
       !from_tree
   | bad ->
     fail "(h) the emitted tables and the plan disagree, %d times" (List.length bad);
     List.iteri (List.rev bad) ~f:(fun index (grammar, src, offset, walked, known) ->
       if index < 6
       then
         Printf.printf
           "     %s on %S at %d: tree [%s] plan [%s]\n"
           grammar
           src
           offset
           walked
           known));
  if !holes = 0
  then fail "(d) no position met a token the residual leaves out, so no hole was read"
  else pass "%d of %d positions are holes" !holes !positions;
  let tally
        (table : (string, int) Hashtbl.t)
        ~(each : string)
        ~(none : string)
        (keys : string list)
    : unit
    =
    match List.filter keys ~f:(fun key -> not (Hashtbl.mem table key)) with
    | [] ->
      pass
        "every %s was read (%s)"
        each
        (String.concat
           ~sep:" "
           (List.map keys ~f:(fun key ->
              Printf.sprintf "%s %d" key (Hashtbl.find table key))))
    | missing -> fail "(e) %s: %s" none (String.concat ~sep:", " missing)
  in
  tally
    by_grammar
    ~each:"grammar"
    ~none:"no position came from these grammars"
    (List.map corpus ~f:(fun c -> c.name));
  tally by_form ~each:"kind of position" ~none:"no position was one of these kinds" forms;
  match !no_example with
  | [] -> pass "every token kind has an example to splice"
  | bad ->
    fail
      "(e) no input lexes these token kinds, so nothing can be spliced for them: %s"
      (String.concat ~sep:", " (List.rev bad))
;;

(* -- (f) a state the plan does not fit ------------------------------------- *)

(* The indices in a state are the caller's, and the only caller that builds one
   correctly is the interpreter. A state carried to another grammar's plan
   reads an array out of range, and the message then names the array rather
   than the call. *)
let () =
  match Core.Facts.of_grammar Lingo_grammars.Json_grammar.grammar with
  | Error _ -> fail "(f) json does not check"
  | Ok facts ->
    let plan, _ = Plan.Lower.of_facts facts in
    let names_the_call (message : string) : bool =
      let prefix = "Residual.at" in
      String.length message >= String.length prefix
      && String.equal (String.sub message ~pos:0 ~len:(String.length prefix)) prefix
    in
    let refuses (what : string) (state : Ir.Residual.State.t) : unit =
      match Ir.Residual.at plan state with
      | exception Invalid_argument message when names_the_call message -> ()
      | exception Invalid_argument message ->
        fail "(f) %s raised %S, which does not say what was wrong with it" what message
      | exception e ->
        fail "(f) %s raised %s, which does not name the call" what (Printexc.to_string e)
      | _ -> fail "(f) %s was accepted" what
    in
    let before = !failures in
    let enter = Ir.Residual.State.enter in
    refuses "a rule the plan does not hold" (enter (Array.length plan.rules));
    refuses
      "an instruction past the end of a rule's body"
      (Ir.Residual.State.item (enter 0) 99);
    (* Rule 4 is the array, and its third instruction is the body loop. *)
    refuses
      "a path carrying on past a loop state"
      (Ir.Residual.State.child
         (Ir.Residual.State.loop (Ir.Residual.State.item (enter 4) 2) 0));
    if !failures = before then pass "at refuses a state the plan does not fit"
;;

let () =
  if !failures = 0
  then print_endline "law_residual: 0 failures"
  else (
    Printf.printf "law_residual: %d failures\n" !failures;
    exit 1)
;;
