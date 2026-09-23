(* -- what a parse costs -------------------------------------------------------

      Min-of-N over four input classes per grammar, for the emitted parser and
      for the interpreter.

      What it is for: changing [Ocaml.Parser], rebuilding, running this again,
      and reading the emitted column against the run before. The lexer bench
      compares two shapes that both ship. A parser has one shape, so the
      comparison worth anything here is across builds rather than
      across columns.

      The interpreter column is the control. It is unchanged by any edit to
      the emitter, so a move in it between two runs is the machine rather than
      the change. Measured on one machine, one run against another of the same
      binary drifts 1.7% on average and 4.0% at worst, so a row that moves by
      less than that has not moved. Take the row-wise minimum of three runs on
      each side to resolve anything smaller.

      The gap between the two columns settles something else, once. Keeping
      both an interpreter and an emitter was decided before either existed,
      and the interpreter was priced at "one indirect dispatch per
      instruction" with that figure recorded as unmeasured. It is measured
      now, and it is not the reason this file exists.

      The rows, and what each one is for:

      - "clean" is source the grammar accepts, with one space between tokens.
        It is the baseline every other row is read against, and its diagnostic
        count has to print zero.
      - "spaced" holds the same meaningful tokens as "clean" and more
        whitespace between them. Only trivia separates the two, so the
        difference in total time is what trivia costs.

        A run of whitespace lexes as one token however long it is, so a
        grammar whose clean row already has a space at every gap gets the same
        token count in both rows and the pair prices a longer run rather than
        an extra one. sexp and recovery read that way. calc's clean row has no
        whitespace at all, so its spaced row adds trivia tokens where there
        were none, and the pair prices those instead. Both are worth having
        and they are not the same question.

        Read the pair by [min us], not by [ns/tok]. Where the spaced row does
        add tokens they are all trivia, which is the cheapest thing a parse
        does, so the per-token average falls even as the total rises.
      - "broken" carries junk the grammar cannot use. It is the balanced skip,
        the recovery sets, and the holes. The diagnostic count beside it says
        the row reached them. Its tokens are not the clean row's, so it is
        read across engines rather than against the row above it.
      - "nested" is one deep tree rather than one wide one. A frame is a call,
        and a call unions the rule's adds into the set it passes down, so this
        row is where threading a recovery set as a list shows up if it shows
        up anywhere.

      What a root admits settles how a row is built. sexp, json and calc take
      one form and drain whatever follows, so their rows are one wide form
      rather than a sequence of forms; a sequence would measure the drain.
      shapes and recovery take a sequence of items, so theirs are sequences.

      Min filters warm-up and GC noise, and the median tracks the tail.
      Allocation is measured outside the timed loop, because a timed loop with
      the GC in it measures the GC. A parse allocates a tree, so the byte
      count is large by construction; what it is for is the difference between
      two rows rather than its own size.

      Lexing happens once, before the clock starts. Both engines take the same
      [Token.t array], so nothing here times the lexer; test/bench/bench_lex.ml
      does that.

      Both engines are given [Siesta.Cache.create_plain ()]. The interpreter
      takes no cache argument and uses that one, and the emitted entry point
      defaults to the hash-consing cache, so the default would have compared a
      parse that interns subtrees against one that does not.

      The calibration line is the lexer bench's, so a reading from one file
      can be held against a reading from the other on the same machine. A
      parse row is thousands of times slower than a scan row, so the clock is
      not in doubt here; the line is there to say which machine.

      What this says nothing about. Whether a recovery set should be a list or a
      packed bitset. Only the list exists, so the "nested" and "broken" rows
      say what the list costs and nothing says what the alternative would.
      Writing the second one is what would settle it, and these rows are the
      baseline it would have to beat.

      Nor what entering a loop costs. A loop binds two refs and hands
      [Cursor.while_progress] two closures, and that happens once a body
      rather than once an element, so no row here separates it from the
      elements the body then takes. A grammar of many short bodies would, and
      none of these is one.

      Both engines are checked to agree on every input before any of them is
      timed, because a benchmark of a parser that builds the wrong tree is
      worse than no benchmark. The trees are compared as a dump of the fields
      a green node carries, because [Siesta.Green.equal] compares hash-cons
      tags and each engine holds its own cache, where equal trees take
      different tags. test/parse_emit/law_parse.ml is the real comparison;
      this repeats it on these inputs alone.

      Not a test. Nothing here passes or fails, and [dune test] does not run
      it.
   -------------------------------------------------------------------------- *)

(* A parse is far slower per byte than a scan, so the budget is a fraction of
   the lexer bench's. Every row still runs long enough for the minimum to
   settle. *)
let budget : int = 40 * 65536
let runs_for (bytes : int) : int = max 10 (budget / max bytes 1)

let time_ns (f : unit -> 'a) : float =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  ignore (Sys.opaque_identity r);
  (t1 -. t0) *. 1e9
;;

(* The lexer bench's chain, unchanged, so the two files' numbers can be read
   beside each other. Around four nanoseconds a step is an L2 hit. *)
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

type engine =
  { label : string
  ; parse :
      Lingo_runtime.Token.t array -> Siesta.Green.node * Lingo_runtime.Diagnostic.t list
  }

let measure (kind : string) (tokens : Lingo_runtime.Token.t array) (engine : engine)
  : float
  =
  let bytes =
    Array.fold_left
      (fun (acc : int) (token : Lingo_runtime.Token.t) -> acc + String.length token.text)
      0
      tokens
  in
  let count = Array.length tokens in
  Gc.full_major ();
  let _, diagnostics = engine.parse tokens in
  let runs = runs_for bytes in
  let times = Array.make runs 0.0 in
  for i = 0 to runs - 1 do
    times.(i) <- time_ns (fun () -> engine.parse tokens)
  done;
  Array.sort compare times;
  let min_ns = times.(0) in
  let med_ns = times.(runs / 2) in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let _ = engine.parse tokens in
  let after = Gc.allocated_bytes () in
  Printf.printf
    "    %-7s %-8s %6d toks  min %8.1f us  %6.1f ns/tok  %6.1f MB/s  med %8.1f us  %6.1f \
     B/tok  %5d diag\n\
     %!"
    kind
    engine.label
    count
    (min_ns /. 1e3)
    (min_ns /. float_of_int count)
    (float_of_int bytes /. min_ns *. 1e3)
    (med_ns /. 1e3)
    ((after -. before) /. float_of_int count)
    (List.length diagnostics);
  min_ns
;;

(* Every field a green node carries that is not derived from its children.
   [Siesta.Green.equal] compares hash-cons tags, and each engine holds its own
   cache, so equal trees there take different tags. *)
let dump (root : Siesta.Green.node) : string =
  let buffer = Buffer.create 4096 in
  let rec go (node : Siesta.Green.node) : unit =
    Buffer.add_string
      buffer
      (Printf.sprintf "(%d#%d" (Siesta.Green.kind node) (Siesta.Green.payload node));
    Array.iter
      (function
        | Siesta.Green.Node child ->
          Buffer.add_char buffer ' ';
          go child
        | Siesta.Green.Token token ->
          Buffer.add_string
            buffer
            (Printf.sprintf
               " %d:%S"
               (Siesta.Green.Token.kind token)
               (Siesta.Green.Token.text token)))
      (Siesta.Green.children_array node);
    Buffer.add_char buffer ')'
  in
  go root;
  Buffer.contents buffer
;;

(* The emitted parser has to agree with the interpreter. Otherwise the numbers
   below describe a parser that builds the wrong tree. *)
let agree
      (name : string)
      (kind : string)
      (tokens : Lingo_runtime.Token.t array)
      (engines : engine list)
  : unit
  =
  match engines with
  | [] | [ _ ] -> ()
  | first :: rest ->
    let root, diagnostics = first.parse tokens in
    let expected = dump root in
    List.iter
      (fun (engine : engine) ->
         let other, other_diagnostics = engine.parse tokens in
         if not (String.equal expected (dump other))
         then Printf.printf "  %s/%s/%s builds a different tree\n" name kind engine.label;
         if diagnostics <> other_diagnostics
         then
           Printf.printf
             "  %s/%s/%s reports different diagnostics\n"
             name
             kind
             engine.label)
      rest
;;

(* -- inputs ---------------------------------------------------------------- *)

let target : int = 65536

let repeat_n (count : int) (item : string) : string =
  let buffer = Buffer.create ((count * String.length item) + 16) in
  for _ = 1 to count do
    Buffer.add_string buffer item
  done;
  Buffer.contents buffer
;;

(* How many copies of an item fill the target. The "spaced" row reuses the
   count its "clean" row was built from, so the two hold the same meaningful
   tokens and differ only in the trivia between them. Filling both to one byte
   count instead would put fewer items in the spaced one, and the two rows
   would be two different documents. *)
let count_for (item : string) : int = max 1 (target / String.length item)

(* A sequence of items, for a root that takes one. *)
let sequence ~(count : int) ~(item : string) : string = repeat_n count item

(* One form holding many items, for a root that takes one form and drains
   whatever follows. A sequence there would measure the drain. *)
let wide ~(count : int) ~(open_ : string) ~(close : string) ~(item : string) : string =
  open_ ^ repeat_n count item ^ close
;;

(* The same, where the items need a separator between them and no trailing
   one. *)
let wide_sep
      ~(count : int)
      ~(open_ : string)
      ~(close : string)
      ~(item : string)
      ~(sep : string)
  : string
  =
  open_ ^ repeat_n (max 0 (count - 1)) (item ^ sep) ^ item ^ close
;;

(* [open_] nested [depth] deep around [leaf]. A frame is a call, so this is
   the row that prices a deep call stack. Both engines recurse on the OCaml
   stack, so the depth is kept well inside it. *)
let nest ~(depth : int) ~(open_ : string) ~(close : string) ~(leaf : string) : string =
  let buffer = Buffer.create 4096 in
  for _ = 1 to depth do
    Buffer.add_string buffer open_
  done;
  Buffer.add_string buffer leaf;
  for _ = 1 to depth do
    Buffer.add_string buffer close
  done;
  Buffer.contents buffer
;;

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; emitted :
      ?cache:Siesta.Cache.t
      -> Lingo_runtime.Token.t array
      -> Siesta.Green.node * Lingo_runtime.Diagnostic.t list
  ; rows : (string * string) list
  }

let corpus : case list =
  [ (* One [Sexp] and a drain, so every row is one form. *)
    (let count = count_for "a b 12 " in
     { name = "sexp"
     ; grammar = Lingo_grammars.Sexp_grammar.grammar
     ; emitted = Emitted_parsers.Sexp_parser.parse_tokens
     ; rows =
         [ "clean", wide ~count ~open_:"(" ~close:")" ~item:"a b 12 "
         ; "spaced", wide ~count ~open_:"(" ~close:")" ~item:"a   b   12   "
         ; "broken", wide ~count ~open_:"(" ~close:")" ~item:"a b ] 12 "
         ; "nested", nest ~depth:2000 ~open_:"(" ~close:")" ~leaf:"a b"
         ]
     })
  ; (* One [Value] and a drain. *)
    (let count = count_for "{\"a\": 1}, " in
     { name = "json"
     ; grammar = Lingo_grammars.Json_grammar.grammar
     ; emitted = Emitted_parsers.Json_parser.parse_tokens
     ; rows =
         [ "clean", wide_sep ~count ~open_:"[" ~close:"]" ~item:"{\"a\": 1}" ~sep:", "
         ; ( "spaced"
           , wide_sep ~count ~open_:"[" ~close:"]" ~item:"{ \"a\" :  1 }" ~sep:" ,  " )
         ; "broken", wide_sep ~count ~open_:"[" ~close:"]" ~item:"{\"a\" 1}" ~sep:", "
         ; "nested", nest ~depth:2000 ~open_:"[" ~close:"]" ~leaf:"1"
         ]
     })
  ; (* One [Expr] and a drain. A left-associative chain is the infix loop. *)
    (let count = count_for "1+2*3-4/5+" in
     { name = "calc"
     ; grammar = Lingo_grammars.Calc_grammar.grammar
     ; emitted = Emitted_parsers.Calc_parser.parse_tokens
     ; rows =
         [ "clean", wide_sep ~count ~open_:"" ~close:"" ~item:"1+2*3-4/5" ~sep:"+"
         ; ( "spaced"
           , wide_sep
               ~count
               ~open_:""
               ~close:""
               ~item:"1 + 2  *  3  -  4  /  5"
               ~sep:"  +  " )
         ; "broken", wide_sep ~count ~open_:"" ~close:"" ~item:"1+*2" ~sep:"+"
         ; "nested", nest ~depth:2000 ~open_:"(" ~close:")" ~leaf:"1+2"
         ]
     })
  ; (* [Decl*] and a drain, so the rows are sequences. The separated body
       inside [Names] forbids a trailing separator, so a state reports on exit
       and every transition reads [Cursor.range]. *)
    (let count = count_for "let a = b, c " in
     { name = "shapes"
     ; grammar = Lingo_grammars.Shapes_grammar.grammar
     ; emitted = Emitted_parsers.Shapes_parser.parse_tokens
     ; rows =
         [ "clean", sequence ~count ~item:"let a = b, c "
         ; "spaced", sequence ~count ~item:"let  a  =  b ,  c   "
         ; "broken", sequence ~count ~item:"let a = b, c @ let "
         ; ( "nested"
           , sequence
               ~count:(max 1 (count / 20))
               ~item:(nest ~depth:20 ~open_:"{ " ~close:" }" ~leaf:"let a" ^ " ") )
         ]
     })
  ; (* [Item*] and a drain. Every part of a recovery set is load-bearing
       here, so the broken row reaches all four rather than whichever one the
       grammar happens to have. [Group] holds a [Triple] and not another
       [Group], so there is no nesting to time; the fourth row is one long
       separated body of rules instead, which is what carries a rule's adds
       down to an element. *)
    (let count = count_for "let a in end ( let b in end ) sig : a ; in " in
     { name = "recovery"
     ; grammar = Lingo_grammars.Recovery_grammar.grammar
     ; emitted = Emitted_parsers.Recovery_parser.parse_tokens
     ; rows =
         [ "clean", sequence ~count ~item:"let a in end ( let b in end ) sig : a ; in "
         ; ( "spaced"
           , sequence
               ~count
               ~item:"let  a  in  end   (  let  b  in  end  )   sig  :  a  ;   in   " )
         ; "broken", sequence ~count ~item:"let end sig end in ( sig ) sig : , : a ; in "
         ; ( "fields"
           , "sig "
             ^ wide_sep ~count:(count * 4) ~open_:"" ~close:"" ~item:": a ;" ~sep:", "
             ^ " in " )
         ]
     })
  ]
;;

(* -- running --------------------------------------------------------------- *)

let run (case : case) : unit =
  match Core.Facts.of_grammar case.grammar with
  | Error _ -> Printf.printf "  %s: the grammar does not check\n" case.name
  | Ok facts ->
    let plan, _ = Plan.Lower.of_facts facts in
    let entry = plan.Ir.Plan.roots.(0) in
    let engines =
      [ { label = "emitted"
        ; parse =
            (fun (tokens : Lingo_runtime.Token.t array) ->
              case.emitted ~cache:(Siesta.Cache.create_plain ()) tokens)
        }
      ; { label = "interp"
        ; parse =
            (fun (tokens : Lingo_runtime.Token.t array) -> Interp.run plan entry tokens)
        }
      ]
    in
    Printf.printf "  %s\n%!" case.name;
    List.iter
      (fun ((kind, src) : string * string) ->
         let tokens = Lex.run facts src in
         agree case.name kind tokens engines;
         match List.map (measure kind tokens) engines with
         | emitted :: rest ->
           List.iter2
             (fun (engine : engine) (elapsed : float) ->
                Printf.printf
                  "    %-7s %-8s %+.1f%% against the emitted parser\n%!"
                  kind
                  engine.label
                  ((elapsed -. emitted) /. emitted *. 100.0))
             (List.tl engines)
             rest
         | [] -> ())
      case.rows
;;

let () =
  calibrate ();
  Printf.printf
    "parse, min of enough runs to read %d KB a row. the emitted parser and the \
     interpreter\n\n\
     %!"
    (budget / 1024);
  List.iter run corpus
;;
