(* -- the interpreter ----------------------------------------------------------

      (a) A parse rebuilds its input, byte for byte, whatever the input.
      (b) A parse stops.
      (c) Input the grammar accepts parses with no diagnostics.
      (d) Every instruction the corpus plans hold is one a corpus parse runs.
      (e) No node but the root begins with trivia.
      (f) [run] refuses an entry that names no rule, and one that names a rule
          with nothing to run.

      Mechanism. Eight grammars, and for each of them a list of inputs written
      here. Every input goes through part (a) and part (b). The ones marked
      good go through part (c) as well, and the ones marked broken are there
      to reach the recovery paths that part (d) counts.

      Part (a)'s oracle is not a second parse. It joins the token texts the
      lexer produced and compares that to the tree's source, so the two agree
      only where every byte the cursor read reached the tree in order.

      Part (d) is the coverage claim, and it is the reason the corpus grew.
      Before postfix and shapes were written, seven instruction forms read
      zero and nothing said so.

      Coverage. Eight grammars, 54 inputs the grammar accepts and 58 it does
      not. A count per instruction prints beside the result, and the law fails
      where one reads zero.

      Part (e) is the one claim here about the tree's shape, and it is the one
      that can be made without a second implementation to compare against.
      Leading trivia belongs to the frame that is already open, so a space
      before a child sits between the caller's children rather than inside the
      child. The root is the exception and has to be: at the entry nothing is
      open, so leading trivia has nowhere else to go.

      Part (f) is about the one index a caller supplies. Every other index in
      a plan is checked by [Plan.Check] before a parse starts; this one
      arrives with the call. A rule with an empty body is the awkward case,
      because it raises on its own and the message then names the builder
      rather than the entry.

      What this says nothing about. Whether the rest of the tree is the right
      shape. That
      needs a second implementation to compare against, and the emitted parser
      is it. Part (c) is the nearest thing available: a grammar that accepts
      an input has to parse it without complaint.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        M1  In [Interp.drain], drop the trailing [Cursor.skip_trivia].
            -> part (a), 3 of 112 parses. [Cursor.eof] looks past trivia, so
               the sweep stops with the trailing trivia unread and it never
               reaches the tree.

               The first run of this mutation reddened nothing, because of a
               gap in this law rather than in the code: no input ended in
               whitespace, so no parse had trailing trivia to lose. Five
               inputs carry it now.
        M2  In [Interp.exec], let an [Alt] take its first arm whatever the
            kind under the cursor is.
            -> part (c), 24 inputs: json 6, sexp 6, shapes 8, recovery 4. An
               alt over rules picks the wrong one, and the parse then reports
               what the arm it took could not find. Part (d) as well:
               ["loop-missing"] reads zero, because the body never gets far
               enough to want a separator.
        M3  In [Lower.repetition], drop the [Trivia] after the loop.
            -> nothing here, and three hunks in test/expect/*.plan: sexp,
               shapes and recovery. The sweep matters for where trivia lands
               and not for whether it lands: a delimited body's close takes
               the trivia before it either way, because taking a token takes
               the trivia in front of it too. So the bytes still reach the
               tree, in a different frame.

               It moved four hunks in test/expect/*.parse once and now moves
               none. No input in that dump ends a one-state repetition on
               trivia any more, which is a gap in the dump rather than in
               this law.
        M4  In [Build.start_node], drop the [Cursor.skip_trivia].
            -> part (e), 60 nodes: json 6, sexp 11, shapes 20, recovery 23.
               It is the one change that makes leading trivia land inside the
               node it precedes, which is what part (e) exists to catch.
        M5  In [Lex.uchar_at], step one byte at a time rather than one
            codepoint.
            -> part (c), all six inputs of the unicode grammar. Every token
               there is more than one byte in UTF-8, so a byte-stepping scan
               matches none of them and the parse reports on input the
               grammar accepts. No other grammar moves: they are all ASCII,
               where a byte and a codepoint are the same thing.
        M6  In [Interp.run], drop the range test on the entry.
            -> part (f), both out-of-range entries: "index out of bounds",
               which does not say what was wrong with it.

               The first run of this reddened nothing. The test caught
               [Invalid_argument] and asked no more, and an array access
               raises that on its own, so a bounds error read as a refusal.
               The test reads the message now.
        M7  In [Interp.run], drop the test for a rule with an empty body.
            -> part (f), one case: [Failure "Builder.finish: nothing built"].
               The parse raises either way; what the check buys is a message
               about the entry rather than about the builder.
        M8  In [Interp.loop], end the body where no state accepts, instead of
            recovering.
            -> part (d), ["loop-recover"] reads zero. This is the arm pigeon
               has and this loop did not: a body that meets a token it cannot
               use sweeps it into an error node and carries on, rather than
               ending and leaving the rest to the caller. On json's
               ["\[1 : 2\]"] the old shape lost the [2] altogether.
        M9  In [Interp.loop], sweep where a position is missing something,
            instead of reporting it.
            -> part (d), ["loop-missing"] reads zero, and json's ["\[1 2\]"]
               parses with no diagnostic at all: the separator that is not
               there goes unreported and the second element is swept away.
               Silent acceptance of input the grammar rejects is the worst
               shape a parse can take, which is why the two cases are told
               apart rather than both recovered.
       M10  In [Lower.ends_on_of], leave the resync anchors out of what ends a
            body.
            -> nothing here, and two hunks in test/expect: shapes.parse and
               shapes.plan. shapes' ["{ let a end }"] sweeps the [end] up and
               carries on to the closer, which is what declaring an anchor is
               meant to stop.
       M11  In [Lower.repeat_ends_on_of], give a root's repeated body [None]
            so it ends where no element can start.
            -> nothing here, and five hunks in test/expect: shapes.parse 2,
               and one each in shapes.plan, recovery.parse and
               recovery.plan. A stray token between two declarations loses
               every declaration after it.
       M12  In [Interp.loop], parse an element without the body's stopping
            points.
            -> nothing here, and one hunk in test/expect/json.parse. A
               failure nested inside an element escapes past the start of the
               next one.

      Parts (a) to (f) say what holds for every input; none of them reads the
      shape of what recovery built. test/expect/*.parse is that reader, and
      M3, M10, M11 and M12 are the mutations that show it.
   -------------------------------------------------------------------------- *)

let failures = ref 0

let fail fmt =
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt

(* -- the corpus ------------------------------------------------------------ *)

(* [good] is input the grammar accepts, so a parse of it reports nothing.
   [broken] is input it does not, and it is here to reach recovery. *)
type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; good : string list
  ; broken : string list
  }

let corpus =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; good =
        [ "(a b)"; "(a (b 12) c)"; "()"; "( a  b )"; "(a\n b)"; "(a b)  " ]
        (* The last three carry leading trivia in front of the token that fails,
         which is the case that put part (e) here: the skip used to open its
         error node before taking that trivia, so the trivia landed inside the
         node rather than in the frame around it. *)
    ; broken = [ "("; "(a"; ")"; "(a ) b"; "(()"; "  )"; "( a  )  )"; "(  ]" ]
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; good =
        [ "1"; "[1, 2]"; "{\"a\": 1}"; "[]"; "{}"; "[{\"a\": [1]}]"; "  [1]  " ]
        (* The last one ends inside a string. The lexer leaves those bytes as
         one unterminated token, so the parse has something to report. *)
    ; broken = [ "[1, 2,]"; "[1 2]"; "{\"a\" 1}"; "[1"; "{"; "[1] junk"; "{\"a\": \"b" ]
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; good = [ "1"; "1+2*3"; "-1*2"; "(1+2)*3"; "1-2-3"; " 1 + 2 " ]
    ; broken = [ "1+"; "1+*2"; "("; "(1"; "1 2" ]
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; good = [ "1"; "1^2^3"; "1+2+3"; "-1^2" ]
    ; broken = [ "1^"; "^1"; "1++" ]
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; good =
        [ "a"; "a?"; "a.b"; "a[1]"; "a{1}"; "a(1, 2)"; "a()"; "a.b[2]?+1"; "a(1)(2)" ]
    ; broken = [ "a."; "a(1"; "a[1"; "a(1,)"; "a["; "a.?" ]
    }
  ; { name = "unicode"
    ; grammar =
        Lingo_grammars.Unicode_grammar.grammar
        (* Every token here is more than one byte in UTF-8, so a lexer that
         stepped a byte at a time would read none of them and part (c) would
         report on input the grammar accepts. *)
    ; good =
        [ "\xc2\xabhello\xc2\xbb"
        ; "\xc2\xab\xc3\xa9t\xc3\xa9\xc2\xbb"
        ; "\xc2\xab\xce\xb1\xce\xb2\xce\xb3\xc2\xbb"
        ; "\xc2\xaba \xe2\x86\x92 b\xc2\xbb"
        ; "\xc2\xab\xc2\xbb"
        ; "\xc2\xab \xc3\xa9t\xc3\xa9 \xe2\x86\x92 \xce\xb1 \xc2\xbb"
        ]
    ; broken =
        [ "\xc2\xabhello"
        ; "hello\xc2\xbb"
        ; "\xc2\xab$\xc2\xbb"
        ; "\xc2\xab\xe2\x86\x92\xc2\xbb"
        ]
    }
    (* The parts of a recovery set the other grammars leave unexercised. The
       broken inputs here are one per part, each at the position that reads
       it; grammars/recovery_grammar.ml says which is which. *)
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; good =
        [ ""
        ; "let a in end"
        ; "( let a in end )"
        ; "sig : a ; in"
        ; "sig : a ; , : b ; in"
        ; "let a in end ( let b in end )"
        ; "let a in end  "
        ]
    ; broken =
        [ "let end"
        ; "let in"
        ; "let"
        ; "( sig )"
        ; "( in let a in end )"
        ; "( let a in end"
        ; "sig end in"
        ; "sig in"
        ; "sig"
        ; "sig : , : a ; in"
        ; "sig : a ; , in"
        ; ")"
        ; ","
        ; "end"
        ]
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; good =
        [ "let a"
        ; "let a = b"
        ; "let a = b, c"
        ; "{ let a }"
        ; "{ let a; let b }"
        ; "{ let a; }"
        ; "let a { let b }"
        ; "let a  "
        ; ""
        ]
        (* The last two turn on the resync anchor. [end] starts nothing, so a
         body without an anchor would sweep it up; [Block] declares it, so the
         body stops there instead. *)
    ; broken =
        [ "let"
        ; "{"
        ; "{ let }"
        ; "let a ="
        ; "{ let a; ; }"
        ; "}"
        ; "{ let a end"
        ; "{ let a end }"
          (* A root of repeated items recovers to the end of the input, so a
             stray token between two declarations costs a diagnostic rather
             than every declaration after it. *)
        ; "let a ; let b"
        ; "@ let a"
        ; "let a ; ; let b"
        ]
    }
  ]
;;

(* -- what the corpus reaches ----------------------------------------------- *)

(* One counter per instruction form. A form no parse runs is a form this law
   says nothing about, and the corpus is what has to grow. *)
let reach : (string, int) Hashtbl.t = Hashtbl.create 32

let ran name =
  Hashtbl.replace reach name (1 + Option.value (Hashtbl.find_opt reach name) ~default:0)
;;

let forms =
  [ "seq"
  ; "open"
  ; "close"
  ; "trivia"
  ; "bump"
  ; "drain"
  ; "expect"
  ; "call"
  ; "pratt"
  ; "alt"
  ; "commit"
  ; "loop"
  ; "exit-reporting"
  ; "loop-recover"
  ; "loop-missing"
  ; "postfix"
  ; "prefix"
  ; "infix"
  ; "atom-token"
  ; "atom-rule"
  ]
;;

(* -- the runs -------------------------------------------------------------- *)

(* Answers every node under [n], the root excluded, that begins with a trivia
   token. *)
let leading_trivia (f : Core.Facts.t) (root : Siesta.Green.node) =
  let is_trivia k = Core.Kind.Set.exists (fun t -> Core.Kind.to_int t = k) f.trivia in
  let bad = ref [] in
  let rec go ~is_root (n : Siesta.Green.node) =
    (match Siesta.Green.nth_child n 0 with
     | Some (Siesta.Green.Token t)
       when (not is_root) && is_trivia (Siesta.Green.Token.kind t) ->
       bad := Siesta.Green.kind n :: !bad
     | Some _ | None -> ());
    Array.iter
      (function
        | Siesta.Green.Node m -> go ~is_root:false m
        | Siesta.Green.Token _ -> ())
      (Siesta.Green.children_array n)
  in
  go ~is_root:true root;
  !bad
;;

let source_of (tokens : Lingo_runtime.Token.t array) =
  String.concat
    ""
    (Array.to_list (Array.map (fun (t : Lingo_runtime.Token.t) -> t.text) tokens))
;;

(* A parse runs one instruction at a time, so a loop that stopped making
   progress shows up as instructions without end. The ceiling reports that
   here rather than hanging the suite. *)
exception Runaway

let ceiling = 100_000

let () =
  let parses = ref 0 in
  let not_lossless = ref 0 in
  let trivia_first = ref [] in
  let runaway = ref [] in
  let noisy = ref [] in
  List.iter
    (fun c ->
       match Core.Facts.of_grammar c.grammar with
       | Error _ -> fail "%s: the grammar does not check" c.name
       | Ok f ->
         let plan, _ = Plan.Lower.of_facts f in
         let entry = plan.Ir.Plan.roots.(0) in
         let run ~expect_clean src =
           let tokens = Lex.run f src in
           let steps = ref 0 in
           let trace name =
             incr steps;
             if !steps > ceiling then raise Runaway;
             ran name
           in
           incr parses;
           match Interp.run ~trace plan entry tokens with
           | exception Runaway -> runaway := (c.name, src) :: !runaway
           | root, diags ->
             if not (String.equal (Siesta.Green.to_source root) (source_of tokens))
             then incr not_lossless;
             List.iter
               (fun k -> trivia_first := (c.name, src, k) :: !trivia_first)
               (leading_trivia f root);
             if expect_clean && diags <> []
             then noisy := (c.name, src, List.length diags) :: !noisy
         in
         List.iter (run ~expect_clean:true) c.good;
         List.iter (run ~expect_clean:false) c.broken)
    corpus;
  (match !runaway with
   | [] -> pass "every parse stopped, over %d parses" !parses
   | rs ->
     List.iter
       (fun (g, src) -> fail "(b) %s ran past %d instructions on %S" g ceiling src)
       rs);
  if !not_lossless > 0
  then fail "(a) %d of %d parses did not rebuild their input" !not_lossless !parses
  else pass "a parse rebuilds its input, over %d parses" (!parses - List.length !runaway);
  (match !noisy with
   | [] ->
     pass
       "input the grammar accepts parses clean, over %d inputs"
       (List.length (List.concat_map (fun c -> c.good) corpus))
   | bad ->
     List.iter
       (fun (g, src, n) -> fail "(c) %s accepts %S, and the parse reported %d" g src n)
       bad);
  match !trivia_first with
  | [] -> pass "no node but the root begins with trivia, over %d parses" !parses
  | bad ->
    List.iter
      (fun (g, src, k) ->
         fail "(e) %s on %S: a node of kind %d begins with trivia" g src k)
      bad
;;

(* -- (f) the entry --------------------------------------------------------- *)

let () =
  match Core.Facts.of_grammar Lingo_grammars.Calc_grammar.grammar with
  | Error _ -> fail "(f) calc does not check"
  | Ok f ->
    let plan, _ = Plan.Lower.of_facts f in
    let tokens = Lex.run f "1" in
    (* The message has to name the call. An array access raises
       [Invalid_argument] on its own, so catching the exception alone cannot
       tell a refusal from a bounds error that happened to surface. *)
    let names_the_call m =
      let prefix = "Interp.run" in
      String.length m >= String.length prefix
      && String.equal (String.sub m 0 (String.length prefix)) prefix
    in
    let refuses what entry =
      match Interp.run plan entry tokens with
      | exception Invalid_argument m when names_the_call m -> ()
      | exception Invalid_argument m ->
        fail "(f) %s raised %S, which does not say what was wrong with it" what m
      | exception e ->
        fail "(f) %s raised %s, which does not name the entry" what (Printexc.to_string e)
      | _ -> fail "(f) %s was accepted" what
    in
    refuses "an entry below zero" (-1);
    refuses "an entry past the last rule" (Array.length plan.rules);
    (* An expression block's roles are the rules with nothing to run. *)
    (match
       Array.to_list plan.rules
       |> List.mapi (fun i (r : Ir.Plan.rule) -> i, r)
       |> List.find_opt (fun (_, (r : Ir.Plan.rule)) -> r.body = Ir.Plan.Seq [||])
     with
     | None -> fail "(f) calc holds no rule with an empty body, so this reads nothing"
     | Some (i, _) -> refuses "an entry naming a rule with nothing to run" i);
    if !failures = 0 then pass "run refuses an entry it cannot parse with"
;;

let () =
  let missing = List.filter (fun n -> not (Hashtbl.mem reach n)) forms in
  if missing <> []
  then
    fail
      "(d) no parse ran %s, so this law says nothing about it"
      (String.concat ", " missing)
  else
    pass
      "every instruction form ran (%s)"
      (String.concat
         " "
         (List.map (fun n -> Printf.sprintf "%s %d" n (Hashtbl.find reach n)) forms))
;;

let () =
  if !failures = 0
  then print_endline "law_interp: 0 failures"
  else (
    Printf.printf "law_interp: %d failures\n" !failures;
    exit 1)
;;
