(* -- the emitted layout table -------------------------------------------------

      (a) The table a grammar's module holds is the table its facts lower to.
      (b) Formatting through that module writes the bytes the fold writes.

      Mechanism. Eleven grammars. [Layout.Lower.of_facts] is read off the facts
      here and compared with the literal the emitter wrote, field for field.
      Then both format the same trees and the bytes are compared, over the hand
      corpus at four widths and a generated one at two.

      (a) is the whole of what generation has to get right. Whether the table
      itself is right is test/laws/law_layout, which holds every law over the
      fold. (b) covers the one thing a comparison of tables does not reach:
      the wiring, where the emitted module builds its boundary from the lexer it
      is handed.

      The generated modules link [lingo_runtime] and nothing else, which the
      library stanza beside this enforces: one reaching for lingo.layout would
      not build.

      [LINGO_SWEEP] sets the generated corpus's depth, as in law_layout. The
      suite runs it at 1, which is 370,568 formats over 2,804 fields, and a
      deep run is green: depth 32 is 11,828,168 formats.

      Falsification. Every mutation was applied, built, run and reverted, and
      the result recorded is the one observed.

        M1  In [Ocaml.Formatter.break], write [Fit] where the layout says
            [Hard].
            -> (a) 34 fields, (b) 17,004 formats.
        M2  In [Ocaml.Formatter.generate], write every [of_kind] entry as -1.
            -> (a) 11 fields, (b) 41,836 formats. Every node is then unruled, and
               an unruled node is laid out by the fold's own account of it.
        M3  In [Ocaml.Formatter.trailing], write [On_break] as [Never].
            -> (a) 10 fields, (b) 3,403 formats.
        M4  In [Ocaml.Formatter.slot], write [repeats] as [false].
            -> (a) 39 fields, (b) 11,032 formats. A slot that stops after one
               child sends the next one to the slot after it.
        M5  In [Ocaml.Formatter.token], write every [trivia] as [None].
            -> (a) 16 fields, (b) 286,039 formats. The source's whitespace is
               then written out as it stood and indented again.
        M6  In [Ocaml.Formatter.rule], write [indent] as zero.
            -> (a) 138 fields, (b) 26,962 formats.
        M7  In [Ocaml.Formatter.rule], write [edge_before] and [edge_after] as
            [None].
            -> (a) 2 fields, (b) 1,985 formats. Unmoved by rust and effekt,
               which set neither override, so sexp's [Group] is still the only
               production in the corpus that does. This read nothing at all
               until 2026-09-20, when that override went in: every grammar had
               [None] there before, so the mutation moved no byte of the
               emitted source and the emitter could have left both fields out.
        M8  In [Ocaml.Formatter.rule], write [name] as the empty string.
            -> (a) 140 fields, and (b) nothing. The name is for a dump and a
               diagnostic. The fold reads none of it, so (a) is the only part
               the mutation reaches.
        M9  In [Ocaml.Formatter.generate], give [format] a boundary that is
            [true] at every byte, rather than one built from [lex].
            -> (b) 2,185 formats, and (a) nothing. This is the wiring, which a
               comparison of tables cannot reach. Part (b) is here for it.
       M10  In [Ocaml.Formatter.sep], write every [sep_kind] as zero.
            -> (a) 21 fields, (b) 5,244 formats.

      What (b) does not cover. Both sides build their boundary from the same
      helper, so a defect in [Lingo_runtime.Layout.boundary] moves the two
      together. Answering [true] at every byte there reads zero here and reddens
      test/laws/law_layout by 34.
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

let first_ten l = List.filteri l ~f:(fun i _ -> i < 10)

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; emitted : Lingo_runtime.Layout.t
  ; format :
      lex:(string -> Lingo_runtime.Token.t array)
      -> width:int
      -> Siesta.Green.node
      -> string
  ; inputs : Inputs.t
  }

let corpus =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; emitted = Emitted_formatters.Sexp_layout.layout
    ; format =
        (fun ~lex ~width root -> Emitted_formatters.Sexp_layout.format ~lex ~width root)
    ; inputs = Inputs.sexp
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; emitted = Emitted_formatters.Json_layout.layout
    ; format =
        (fun ~lex ~width root -> Emitted_formatters.Json_layout.format ~lex ~width root)
    ; inputs = Inputs.json
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; emitted = Emitted_formatters.Calc_layout.layout
    ; format =
        (fun ~lex ~width root -> Emitted_formatters.Calc_layout.format ~lex ~width root)
    ; inputs = Inputs.calc
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; emitted = Emitted_formatters.Rassoc_layout.layout
    ; format =
        (fun ~lex ~width root -> Emitted_formatters.Rassoc_layout.format ~lex ~width root)
    ; inputs = Inputs.rassoc
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; emitted = Emitted_formatters.Postfix_layout.layout
    ; format =
        (fun ~lex ~width root ->
          Emitted_formatters.Postfix_layout.format ~lex ~width root)
    ; inputs = Inputs.postfix
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; emitted = Emitted_formatters.Shapes_layout.layout
    ; format =
        (fun ~lex ~width root -> Emitted_formatters.Shapes_layout.format ~lex ~width root)
    ; inputs = Inputs.shapes
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; emitted = Emitted_formatters.Unicode_layout.layout
    ; format =
        (fun ~lex ~width root ->
          Emitted_formatters.Unicode_layout.format ~lex ~width root)
    ; inputs = Inputs.unicode
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; emitted = Emitted_formatters.Recovery_layout.layout
    ; format =
        (fun ~lex ~width root ->
          Emitted_formatters.Recovery_layout.format ~lex ~width root)
    ; inputs = Inputs.recovery
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; emitted = Emitted_formatters.Comments_layout.layout
    ; format =
        (fun ~lex ~width root ->
          Emitted_formatters.Comments_layout.format ~lex ~width root)
    ; inputs = Inputs.comments
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; emitted = Emitted_formatters.Rust_layout.layout
    ; format =
        (fun ~lex ~width root -> Emitted_formatters.Rust_layout.format ~lex ~width root)
    ; inputs = Inputs.rust
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; emitted = Emitted_formatters.Effekt_layout.layout
    ; format =
        (fun ~lex ~width root -> Emitted_formatters.Effekt_layout.format ~lex ~width root)
    ; inputs = Inputs.effekt
    }
  ]
;;

let facts_of (c : case) : Core.Facts.t option =
  match Core.Facts.of_grammar c.grammar with
  | Ok f -> Some f
  | Error _ ->
    fail "%s: the grammar does not check" c.name;
    None
;;

(* -- (a) the table ---------------------------------------------------------- *)

(* Field for field rather than one comparison of the whole, so a failure names
   the place. *)

let fields = ref 0
let wrong_field : (string * string) list ref = ref []

let check_table (c : case) (lowered : Ir.Layout.t) : unit =
  let emitted = c.emitted in
  let say fmt =
    Format.kasprintf (fun s -> wrong_field := (c.name, s) :: !wrong_field) fmt
  in
  let differs : 'a. string -> 'a -> 'a -> unit =
    fun at mine theirs ->
    incr fields;
    if mine <> theirs then say "%s" at
  in
  if Array.length emitted.rules <> Array.length lowered.rules
  then
    say
      "%d rules where the facts give %d"
      (Array.length emitted.rules)
      (Array.length lowered.rules)
  else
    Array.iteri lowered.rules ~f:(fun i (l : Ir.Layout.rule) ->
      let e = emitted.rules.(i) in
      let at (what : string) = Printf.sprintf "rule %s %s" l.name what in
      differs (at "name") e.name l.name;
      differs (at "kind") e.kind l.kind;
      differs (at "frame") e.frame l.frame;
      differs (at "body") e.body l.body;
      differs (at "inner") e.inner l.inner;
      differs (at "indent") e.indent l.indent;
      differs (at "edge_before") e.edge_before l.edge_before;
      differs (at "edge_after") e.edge_after l.edge_after;
      if Array.length e.slots <> Array.length l.slots
      then
        say
          "rule %s has %d slots where the facts give %d"
          l.name
          (Array.length e.slots)
          (Array.length l.slots)
      else
        Array.iteri l.slots ~f:(fun j (s : Ir.Layout.slot) ->
          let es = e.slots.(j) in
          let at (what : string) = Printf.sprintf "rule %s slot %d %s" l.name j what in
          differs (at "kinds") es.kinds s.kinds;
          differs (at "repeats") es.repeats s.repeats;
          differs (at "before") es.before s.before;
          differs (at "between") es.between s.between));
  differs "of_kind" emitted.of_kind lowered.of_kind;
  if Array.length emitted.tokens <> Array.length lowered.tokens
  then
    say
      "%d token entries where the facts give %d"
      (Array.length emitted.tokens)
      (Array.length lowered.tokens)
  else
    Array.iteri lowered.tokens ~f:(fun k t ->
      differs (Printf.sprintf "token %d" k) emitted.tokens.(k) t)
;;

(* -- (b) the bytes ---------------------------------------------------------- *)

let depth =
  match Sys.getenv_opt "LINGO_SWEEP" with
  | None -> 1
  | Some s ->
    (try int_of_string s with
     | _ -> 1)
;;

let hand_widths = [ 80; 40; 20; 8 ]
let swept_widths = [ 80; 20 ]
let formats = ref 0
let wrong_bytes : (string * string * int * string * string) list ref = ref []

let check_bytes (c : case) (facts : Core.Facts.t) (lowered : Ir.Layout.t) : unit =
  let plan, _ = Plan.Lower.of_facts facts in
  let entry = plan.Ir.Plan.roots.(0) in
  let lex (src : string) = Lex.run facts src in
  let boundary = Lex.boundary facts in
  let over ~(widths : int list) (srcs : string list) =
    List.iter srcs ~f:(fun src ->
      let tree, _ = Interp.run plan entry (lex src) in
      List.iter widths ~f:(fun width ->
        incr formats;
        let folded = Lingo_runtime.Layout.format lowered ~boundary ~width tree in
        let generated = c.format ~lex ~width tree in
        if not (String.equal folded generated)
        then wrong_bytes := (c.name, src, width, generated, folded) :: !wrong_bytes))
  in
  let hand = Inputs.all c.inputs in
  over ~widths:hand_widths hand;
  over ~widths:swept_widths (Sweep.inputs ~depth facts hand)
;;

(* -- the runs --------------------------------------------------------------- *)

let () =
  List.iter corpus ~f:(fun c ->
    match facts_of c with
    | None -> ()
    | Some facts ->
      let lowered = Layout.Lower.of_facts facts in
      check_table c lowered;
      check_bytes c facts lowered)
;;

let () =
  (match !wrong_field with
   | [] ->
     pass "(a) every emitted table is the one its facts give, over %d fields" !fields
   | bad ->
     List.iter (first_ten bad) ~f:(fun (g, at) -> fail "(a) %s: %s" g at);
     fail "(a) %d fields differ from what the facts give" (List.length bad));
  match !wrong_bytes with
  | [] ->
    pass
      "(b) the emitted formatter writes the fold's bytes, over %d formats at depth %d"
      !formats
      depth
  | bad ->
    List.iter (first_ten bad) ~f:(fun (g, src, w, generated, folded) ->
      fail "(b) %s on %S at %d:\n  module %S\n  fold   %S" g src w generated folded);
    fail "(b) %d formats differ from the fold's" (List.length bad)
;;

let () =
  if !failures = 0
  then print_endline "law_format: 0 failures"
  else (
    Printf.printf "law_format: %d failures\n" !failures;
    exit 1)
;;
