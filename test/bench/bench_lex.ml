(* -- how fast each shape lexes ------------------------------------------------

      Min-of-N over three inputs per grammar, for both emitted shapes.

      - "mixed" is source made of the tokens the grammar is about. It shows
        what lexing costs, and most of that cost is building tokens. The byte
        count beside it is how much one token allocates.
      - "interned" is source made only of the grammar's fixed-text tokens.
        Every token comes from the intern table, so nothing is allocated and
        what is left is the scan, walking states the way real source does.
        The shapes are compared on this row.
      - "one token" is a single lexeme filling the same bytes. It is the scan
        too, but of one state on one character. The row and the class never
        change, so both loads hit the same address and the core forwards them
        without reaching the cache. It bounds scan speed from above.

      Min filters warm-up and GC noise, and the median tracks the tail.
      Allocation is measured outside the timed loop, because a timed loop with
      the GC in it measures the GC. The byte count is reliable on the mixed
      rows. A block over 256 words is allocated straight into the major heap
      and the runtime's counter for that only moves at a collection, so the
      one-token rows undercount by the size of the token; those rows are here
      for the time.

      The clock is the wall clock, and [Sys.time] agrees with it here to
      within a few percent. The calibration line walks a dependent load chain
      whose cost is known. Around four nanoseconds a step is an L2 hit, which
      is what that chain should cost. Around one says the walk is sitting on a
      single address, which is what a chase array with a fixed point gives,
      and the readings below are then worthless.

      The method is otherwise pigeon's test/bench_lex.ml, so the two sets of
      numbers can be read beside each other. The input differs: pigeon's [lex]
      takes a streaming buffer and refills it, and both of these take a
      string.

      Why it exists. The table and the match shape each looked faster from one
      angle, and nobody had measured either.

      Both shapes come out of one [Ocaml.Lexer], sharing max munch, the intern
      table, the two synthesised tokens and the output, so what is timed is
      the scan. Both are checked against test/lex before either is timed,
      because a benchmark of a wrong lexer is worse than no benchmark.

      Not a test. Nothing here passes or fails, and [dune test] does not run
      it.
   -------------------------------------------------------------------------- *)

(* Every row does about the same total work. A scan row reads two megabytes a
   run and the others sixty-four kilobytes, so a fixed count would leave the
   scan rows running thirty times as long for no more confidence. *)
let budget : int = 200 * 65536
let runs_for (src : string) : int = max 20 (budget / String.length src)

let time_ns (f : unit -> 'a) : float =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  ignore (Sys.opaque_identity r);
  (t1 -. t0) *. 1e9
;;

(* A dependent load chain over half a megabyte, which costs an L2 hit a step.
   It prints before anything else, because a reading far below it means the
   loop is not measuring what it looks like. *)
let calibrate () : unit =
  let n = 1 lsl 16 in
  let chase = Array.init n (fun (index : int) -> ((index * 7919) + 1) mod n) in
  let walk () : int =
    let at = ref 0 in
    for _ = 1 to n do
      at := Array.unsafe_get chase !at
    done;
    !at
  in
  let best = ref infinity in
  for _ = 1 to 50 do
    best := Float.min !best (time_ns walk)
  done;
  Printf.printf
    "calibration: a dependent load chain over %d words costs %.2f ns a step\n\n%!"
    n
    (!best /. float_of_int n)
;;

type shape =
  { label : string
  ; lex : string -> Lingo_runtime.Token.t array
  }

let measure (kind : string) (src : string) (shape : shape) : float =
  Gc.full_major ();
  let tokens = Array.length (shape.lex src) in
  let bytes = String.length src in
  let runs = runs_for src in
  let times = Array.make runs 0.0 in
  for i = 0 to runs - 1 do
    times.(i) <- time_ns (fun () -> shape.lex src)
  done;
  Array.sort compare times;
  let min_ns = times.(0) in
  let med_ns = times.(runs / 2) in
  (* [Gc.allocated_bytes] counts the minor heap, the major heap and
     promotions together. [quick_stat]'s major counter only moves at a
     collection, so a token whose text is one large block does not show up
     there at all. *)
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let _ = shape.lex src in
  let after = Gc.allocated_bytes () in
  Printf.printf
    "    %-9s %-7s %6d toks  min %7.1f us  %5.2f ns/byte  %6.1f MB/s  med %7.1f us  \
     %5.1f B/tok\n\
     %!"
    kind
    shape.label
    tokens
    (min_ns /. 1e3)
    (min_ns /. float_of_int bytes)
    (float_of_int bytes /. min_ns *. 1e3)
    (med_ns /. 1e3)
    ((after -. before) /. float_of_int tokens);
  min_ns
;;

(* Both shapes have to give what test/lex gives. Otherwise the numbers
   below describe a lexer that does the wrong thing. *)
let agree (name : string) (facts : Core.Facts.t) (src : string) (shapes : shape list)
  : unit
  =
  let oracle = Lex.run facts src in
  List.iter
    (fun (shape : shape) ->
       let got = shape.lex src in
       let same =
         Array.length got = Array.length oracle
         && Array.for_all2
              (fun (a : Lingo_runtime.Token.t) (b : Lingo_runtime.Token.t) ->
                 a.kind = b.kind && String.equal a.text b.text)
              got
              oracle
       in
       if not same then Printf.printf "  %s/%s DISAGREES with test/lex\n" name shape.label)
    shapes
;;

let target : int = 65536

(* One token costs one scan and nothing else, so that input needs more bytes
   in it before the clock can read the difference. *)
let scan_target : int = 1 lsl 21

let repeat_to (size : int) (unit_ : string) : string =
  let b = Buffer.create (size + String.length unit_) in
  while Buffer.length b < size do
    Buffer.add_string b unit_
  done;
  Buffer.contents b
;;

(* Enough of the grammar's own tokens to run long enough to time. *)
let repeat (unit_ : string) : string = repeat_to target unit_

let run
      (name : string)
      (grammar : Core.Grammar.t)
      ~(mixed : string)
      ~(interned : string)
      ~(single : string)
      (shapes : shape list)
  =
  match Core.Facts.of_grammar grammar with
  | Error _ -> Printf.printf "  %s: the grammar does not check\n" name
  | Ok facts ->
    Printf.printf "  %s\n%!" name;
    agree name facts mixed shapes;
    agree name facts interned shapes;
    agree name facts single shapes;
    List.iter
      (fun ((kind, src) : string * string) ->
         match List.map (measure kind src) shapes with
         | first :: rest ->
           (* Everything is quoted against the table, which is what ships. *)
           List.iter2
             (fun (shape : shape) (elapsed : float) ->
                Printf.printf
                  "    %-9s %-7s %+.1f%% against the table\n%!"
                  kind
                  shape.label
                  ((elapsed -. first) /. first *. 100.0))
             (List.tl shapes)
             rest
         | [] -> ())
      [ "mixed", mixed; "interned", interned; "one token", single ]
;;

let () =
  calibrate ();
  Printf.printf
    "lex, min of enough runs to read %d MB a row. table, function per state, and match\n\n"
    (budget / (1 lsl 20));
  run
    "sexp"
    Lingo_grammars.Sexp_grammar.grammar
    ~mixed:(repeat "(define (square x) (* x x))\n")
    ~interned:(repeat "(())")
    ~single:(String.make scan_target 'a')
    [ { label = "table"; lex = Emitted.Sexp_lexer.lex }
    ; { label = "match"; lex = Emitted.Sexp_match.lex }
    ];
  run
    "json"
    Lingo_grammars.Json_grammar.grammar
    ~mixed:(repeat "{\"name\": \"value\", \"n\": -12.5e3, \"ok\": true}, ")
    ~interned:(repeat "{}[],:")
    ~single:("\"" ^ String.make (scan_target - 2) 'a' ^ "\"")
    [ { label = "table"; lex = Emitted.Json_lexer.lex }
    ; { label = "match"; lex = Emitted.Json_match.lex }
    ];
  run
    "calc"
    Lingo_grammars.Calc_grammar.grammar
    ~mixed:(repeat "12 + 34 * (56 - 78) / 90  ")
    ~interned:(repeat "+-*/()")
    ~single:(String.make scan_target '7')
    [ { label = "table"; lex = Emitted.Calc_lexer.lex }
    ; { label = "match"; lex = Emitted.Calc_match.lex }
    ];
  run
    "rassoc"
    Lingo_grammars.Rassoc_grammar.grammar
    ~mixed:(repeat "12 ^ 34 + 56 ^ 78  ")
    ~interned:(repeat "+^-")
    ~single:(String.make scan_target '7')
    [ { label = "table"; lex = Emitted.Rassoc_lexer.lex }
    ; { label = "match"; lex = Emitted.Rassoc_match.lex }
    ];
  run
    "shapes"
    Lingo_grammars.Shapes_grammar.grammar
    ~mixed:(repeat "let alpha = beta, gamma; ")
    ~interned:(repeat "{};,=@")
    ~single:(String.make scan_target 'a')
    [ { label = "table"; lex = Emitted.Shapes_lexer.lex }
    ; { label = "match"; lex = Emitted.Shapes_match.lex }
    ];
  run
    "postfix"
    Lingo_grammars.Postfix_grammar.grammar
    ~mixed:(repeat "alpha.beta[12]?+gamma(34, 56)  ")
    ~interned:(repeat ".,?+()[]{}")
    ~single:(String.make scan_target 'a')
    [ { label = "table"; lex = Emitted.Postfix_lexer.lex }
    ; { label = "match"; lex = Emitted.Postfix_match.lex }
    ];
  (* Every character here is more than one byte, so this pair is the only one
     that times the arm below the ASCII fast path. *)
  run
    "unicode"
    Lingo_grammars.Unicode_grammar.grammar
    ~mixed:
      (repeat "\xc2\xab\xc3\xa9t\xc3\xa9 \xe2\x86\x92 \xce\xb1\xce\xb2\xce\xb3\xc2\xbb ")
    ~interned:(repeat "\xc2\xab\xe2\x86\x92\xc2\xbb")
    ~single:(repeat_to scan_target "\xce\xb1")
    [ { label = "table"; lex = Emitted.Unicode_lexer.lex }
    ; { label = "match"; lex = Emitted.Unicode_match.lex }
    ]
;;
