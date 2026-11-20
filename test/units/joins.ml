(* -- what goes between two tokens ---------------------------------------------

      The fold writes a token at a time and asks the lexer what may stand
      between each and the last: nothing, a blank, or a line break. A wrong
      answer is not a matter of taste. The output reads back as other tokens,
      and what was code is gone.

      The lexer takes the longest token it can, so the answer for two tokens
      can turn on bytes written long after them. Here a comment is
      [(* ... *)] on one line, and [(] and [*] are tokens too. Side by side
      the two read as two tokens. With a comment further along the line they
      are the opening of one that its closer ends. The source is

      {v
        ( * x (* c *)
      v}

      and until 2026-11-20 the fold wrote the bracket and the star side by
      side. The line then read as one comment from the bracket to the
      closer, and the star and the name were gone. The fold had asked the
      lexer about the two tokens alone, and the lexer answered for the two
      alone.

      {!Lingo_runtime.Token.boundary} is the answer it takes now. A boundary
      is settled where nothing behind the string can move it, and open where
      a token in front of it was still being read as the string ran out. Two
      tokens touch where the boundary between them is settled, or where a
      blank after them settles it.

      The second half matters as much as the first. [1] and [.] are open
      too: [1.] could go on to be [1.5]. A blank ends that, so [1.x] stays
      as it is written.

      The automaton has to be honest about "still being read" for any of
      this. A comment whose body is a complement leaves a state behind its
      closer that reads on and can accept nothing. lib/core/stage.ml
      minimises the automaton, which drops such states. With one left in,
      every comment looks open and the token after it goes to a new line.

      No law in test/laws/law_layout.ml lacks this in principle: part (b)
      says no format loses a token. It found the fault by drawing the input,
      and had not drawn it before. So the last check here is exhaustive
      where that one samples: every sequence of four tokens, from the nine
      that can make trouble for one another.

      The second grammar is for what the fold writes into a gap. It has
      three things to choose from, and it has to ask about the one it
      writes. Until 2026-11-20 it asked about nothing and about a space, and
      then wrote whatever the layout wanted:

      - a line break, where the gap may end the line or the grammar says it
        does. Here [\] is a token and so is [\] with a line break behind it,
        so a line that ends behind a [\] has made the other token of it.
      - a space, where the tokens ask for one. Here [<] and [>] ask for one,
        and [< >] with a space in it is a token of its own.

      A blank also has two sides. The boundary behind a line break can
      stand while the one in front of it is gone: [\] and the line break
      are one token, and that token ends where the next begins. And the one
      in front can stand while the blank has begun a token with what is
      behind it: here a space, [-] and [>] are one token, so a space in
      front of [-] is settled in front and open behind. Or the one in front
      can be left open: [>], a space, a name and a [!] are one token, so a
      space between [>] and a name holds only until a [!] follows.
   -------------------------------------------------------------------------- *)

let grammar : Grammar.t =
  let open Grammar in
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let letter = Redfa.Regex.range_char ~lo:'a' ~hi:'z' in
  (* On one line, with a body that holds no closer: what
     grammars/ml_grammar.ml writes. *)
  let comment =
    Redfa.Regex.(
      seqs
        [ str "(*"
        ; inter
            (complement (seqs [ star any; str "*)"; star any ]))
            (star (not_singleton_char '\n'))
        ; str "*)"
        ])
  in
  let tokens =
    [ punct ~space_after:Hug ~name:"lparen" "("
    ; punct ~space_before:Hug ~name:"rparen" ")"
    ; punct_tight ~name:"star" "*"
    ; punct_tight ~name:"dot" "."
    ; punct_tight ~name:"less" "<"
      (* A block of text that opens on [<<] and a line break. So two [<] side
         by side are open, a space settles them, and a line break does not. *)
    ; pat "block" Redfa.Regex.(seqs [ str "<<\n"; star letter; str ">>" ])
    ; pat "float" Redfa.Regex.(seqs [ plus digit; singleton_char '.'; plus digit ])
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat "name" (Redfa.Regex.plus letter)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ; pat ~trivia:Preserve "comment" comment
    ]
  in
  (* Every token in any order, and no boundary that may break. What stands
     between two of them is then the join's to say and nothing else's. *)
  let file =
    prod
      ~indent_width:0
      "File"
      [ child_rep ~break:Never ~between:Never "item" (Rule "Item") ]
  in
  let item =
    prod
      "Item"
      [ child_alt
          ~modifier:Exactly_one
          "token"
          [ Token "lparen"
          ; Token "rparen"
          ; Token "star"
          ; Token "dot"
          ; Token "float"
          ; Token "int"
          ; Token "name"
          ; Token "less"
          ; Token "block"
          ]
      ]
  in
  create ~tokens ~roots:[ "File" ] [ file; item ]
;;

let check (what : string) ~(expected : string) (got : string) : unit =
  if String.equal expected got
  then Law.pass "%s" what
  else Law.fail "%s\n  expected %S\n  got      %S" what expected got
;;

(* Tokens in any order again, with [between] at every boundary. *)
let breakable (between : Grammar.break_style) : Grammar.t =
  let open Grammar in
  let tokens =
    [ punct_tight ~name:"bslash" "\\"
    ; pat "join" (Redfa.Regex.str "\\\n")
    ; punct ~name:"langle" "<"
    ; punct ~name:"rangle" ">"
    ; pat "pair" (Redfa.Regex.str "< >")
      (* A token that starts with a blank, so a blank in front of [-] has
         begun one. And [x-] is a token, so the two cannot touch either. *)
    ; punct ~space_after:Hug ~name:"minus" "-"
    ; pat "arrow" (Redfa.Regex.str " ->")
    ; pat "xdash" (Redfa.Regex.str "x-")
      (* A token that is [>], a space, a name and a [!]. So a space behind
         [>] leaves the boundary in front of it open until the [!] comes or
         does not. *)
    ; punct_tight ~name:"bang" "!"
    ; pat
        "note"
        Redfa.Regex.(seqs [ str "> "; star (range_char ~lo:'a' ~hi:'z'); str "!" ])
    ; pat "name" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file =
    prod ~indent_width:0 "File" [ child_rep ~break:between ~between "item" (Rule "Item") ]
  in
  let item =
    prod
      "Item"
      [ child_alt
          ~modifier:Exactly_one
          "token"
          [ Token "bslash"
          ; Token "join"
          ; Token "langle"
          ; Token "rangle"
          ; Token "pair"
          ; Token "minus"
          ; Token "arrow"
          ; Token "xdash"
          ; Token "bang"
          ; Token "note"
          ; Token "name"
          ]
      ]
  in
  create ~tokens ~roots:[ "File" ] [ file; item ]
;;

let gaps () : unit =
  let over (between : Grammar.break_style) =
    let f =
      match Core.Facts.of_grammar (breakable between) with
      | Ok f -> f
      | Error errors ->
        List.iter (fun e -> Format.eprintf "%a@." Core.Error.pp e) errors;
        failwith "the second grammar of test/units/joins.ml does not check"
    in
    let plan, _ = Plan.Lower.of_facts f in
    let layout = Layout.Lower.of_facts f in
    let boundary = Lex.boundary f in
    let format (width : int) (src : string) : string =
      let tree, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
      Lingo_runtime.Layout.format layout ~boundary ~width tree
    in
    let read (src : string) : string list =
      Array.to_list (Lex.run f src)
      |> List.filter_map (fun (t : Lingo_runtime.Token.t) ->
        if String.trim t.text = "" then None else Some t.text)
    in
    let _, map = Sample.system_of f in
    let sampled (src : string) : string =
      Array.to_list (Lex.run f src)
      |> List.filter_map (fun (t : Lingo_runtime.Token.t) ->
        if String.trim t.text = ""
        then None
        else
          Array.find_opt
            (fun (def : Core.Token.def) -> Core.Kind.to_int def.kind = t.kind)
            f.tokens
          |> Option.map (fun (def : Core.Token.def) ->
            { Sample.kind = def.kind; text = t.text }))
      |> Sample.decode f map
    in
    format, read, sampled
  in
  let may_break, read, sampled = over Grammar.Fit in
  let breaks, _, _ = over (Grammar.always 1) in
  (* Two blanks between tokens in a source, so that [<  >] is two tokens. *)
  check
    "a backslash touches what is on either side of it"
    ~expected:"aaaa\\bbbb"
    (may_break 80 "aaaa  \\  bbbb");
  check
    "too wide, the line ends in front of the backslash and never behind it"
    ~expected:"aaaa\n\\bbbb"
    (may_break 6 "aaaa  \\  bbbb");
  check
    "two that ask for a space touch, where the space would make one token of them"
    ~expected:"<>"
    (may_break 80 "<  >");
  check
    "a break the grammar asks for stands between two names"
    ~expected:"aaaa\nbbbb"
    (breaks 80 "aaaa  bbbb");
  check "and is refused behind a backslash" ~expected:"\\bbbb" (breaks 80 "\\  bbbb");
  (* A space in front of [-] would be the start of [ ->], and [x-] is a token.
     A line break is what is left, and [-] and [>] touch behind it. *)
  check
    "where a space would begin a token with what follows, and nothing would too"
    ~expected:"x\n->"
    (may_break 80 "x  -  >");
  (* A space behind [>] would leave [>] open to the [!] two tokens on. *)
  check
    "where a space would leave the token in front of it open, the two touch"
    ~expected:">x!"
    (may_break 80 ">  x  !");
  (* Every sequence of four again, under both grammars, wide and as narrow
     as a ruler goes. At one column every gap that may end the line does. *)
  let alphabet = [ "\\"; "<"; ">"; "x"; "-"; "!" ] in
  let rec sequences (n : int) : string list list =
    if n = 0
    then [ [] ]
    else
      List.concat_map
        (fun (rest : string list) -> List.map (fun (t : string) -> t :: rest) alphabet)
        (sequences (n - 1))
  in
  let all = List.concat_map sequences [ 1; 2; 3; 4 ] in
  let unread = ref 0 in
  let lost = ref [] in
  let moved = ref [] in
  let drawn = ref [] in
  List.iter
    (fun (tokens : string list) ->
       let src = String.concat "  " tokens in
       if read src <> tokens
       then incr unread
       else (
         let decoded = sampled src in
         if read decoded <> tokens then drawn := (src, decoded) :: !drawn;
         List.iter
           (fun ((what, format) : string * (int -> string -> string)) ->
              List.iter
                (fun (width : int) ->
                   let once = format width src in
                   if read once <> tokens then lost := (what, width, src, once) :: !lost;
                   let twice = format width once in
                   if not (String.equal once twice)
                   then moved := (what, width, src, once) :: !moved)
                [ 80; 1 ])
           [ "may end the line", may_break; "ends the line", breaks ]))
    all;
  if !unread > 0
  then Law.fail "%d sequences of the second grammar do not read as their tokens" !unread;
  (match List.rev !lost with
   | [] ->
     Law.pass
       "all %d sequences format to the same tokens, where a gap may end the line and \
        where it does"
       (List.length all)
   | (what, width, src, once) :: _ ->
     Law.fail
       "%d formats read as other tokens, the first %S where a gap %s, at %d: %S"
       (List.length !lost)
       src
       what
       width
       once);
  (match List.rev !moved with
   | [] -> Law.pass "and stand when formatted again"
   | (what, width, src, once) :: _ ->
     Law.fail
       "%d formats move when formatted again, the first %S where a gap %s, at %d: %S"
       (List.length !moved)
       src
       what
       width
       once);
  match List.rev !drawn with
  | [] -> Law.pass "and the sampler writes every one of these as text that reads as it"
  | (src, decoded) :: _ ->
    Law.fail
      "the sampler writes %d of these as other tokens, the first %S, written %S"
      (List.length !drawn)
      src
      decoded
;;

let () =
  let f =
    match Core.Facts.of_grammar grammar with
    | Ok f -> f
    | Error errors ->
      List.iter (fun e -> Format.eprintf "%a@." Core.Error.pp e) errors;
      failwith "the grammar of test/units/joins.ml does not check"
  in
  let plan, _ = Plan.Lower.of_facts f in
  let layout = Layout.Lower.of_facts f in
  let boundary = Lex.boundary f in
  let format (src : string) : string =
    let tree, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
    Lingo_runtime.Layout.format layout ~boundary ~width:80 tree
  in
  (* The texts of the tokens a string reads as, less the whitespace. *)
  let read (src : string) : string list =
    Array.to_list (Lex.run f src)
    |> List.filter_map (fun (t : Lingo_runtime.Token.t) ->
      if String.trim t.text = "" then None else Some t.text)
  in
  (* -- what the lexer says --

     The first three are one boundary under three futures. *)
  let says (s : string) (at : int) (expected : Lingo_runtime.Token.boundary) : unit =
    let name (b : Lingo_runtime.Token.boundary) : string =
      match b with
      | Absent -> "absent"
      | Open -> "open"
      | Settled -> "settled"
    in
    check
      (Printf.sprintf "byte %d of %S is %s" at s (name expected))
      ~expected:(name expected)
      (name (boundary s at))
  in
  says "(*" 1 Open;
  says "(* " 1 Open;
  says "(*\n" 1 Settled;
  says "( *" 2 Settled;
  says "(x" 1 Settled;
  says "1." 1 Open;
  says "1. " 1 Settled;
  says "1.5" 1 Absent;
  says "1.x" 1 Settled;
  says "ab" 1 Absent;
  says "a b" 2 Settled;
  says "a" 0 Settled;
  (* Behind a comment's closer nothing is being read any more. *)
  says "(* c *) x" 7 Settled;
  says "(* c *)x" 7 Settled;
  (* A token that runs to the end of the string leaves what is in front of
     it settled, and itself open to what follows. *)
  says "x (* c" 2 Settled;
  says "(* c" 3 Open;
  (* Open, and a blank of one kind settles it where the other does not. *)
  says "<<" 1 Open;
  says "<< " 1 Settled;
  says "<<\n" 1 Open;
  (* -- what the fold writes --

     The source has a blank between every two tokens, so it reads as the
     tokens it names. *)
  check "a bracket and a name touch" ~expected:"(x)" (format "( x )");
  check
    "a bracket and a star stand apart, with nothing behind them to close a comment"
    ~expected:"( *x"
    (format "( * x");
  check
    "and with a comment behind them, which is where it matters"
    ~expected:"( *x (* c *)"
    (format "( * x (* c *)");
  check
    "a number and a dot touch, because a blank ends what the two could become"
    ~expected:"1.x"
    (format "1 . x");
  check
    "a dot and a number behind a number do not: the three are one number"
    ~expected:"1. 5"
    (format "1 . 5");
  check
    "a token behind a comment stays on its line"
    ~expected:"(* c *) x"
    (format "(* c *) x");
  (* The fold may end the run with a line break as well as with a space, so
     either has to settle what two touching tokens left open. *)
  check
    "two that a space would settle and a line break would not stand apart"
    ~expected:"< <x"
    (format "< < x");
  (* -- every sequence of four --

     Written with a blank between each two, a sequence reads as itself. What
     the fold writes has to read as the same tokens, and stand when it is
     formatted again.

     {!Sample.decode} writes tokens out by the same rule, for the laws that
     draw their inputs from a grammar. It is held to the same thing here. *)
  let alphabet = [ "("; ")"; "*"; "."; "1"; "1.5"; "x"; "<"; "(* c *)" ] in
  let _, map = Sample.system_of f in
  let sampled (src : string) : string =
    Array.to_list (Lex.run f src)
    |> List.filter_map (fun (t : Lingo_runtime.Token.t) ->
      if String.trim t.text = ""
      then None
      else
        Array.find_opt
          (fun (def : Core.Token.def) -> Core.Kind.to_int def.kind = t.kind)
          f.tokens
        |> Option.map (fun (def : Core.Token.def) ->
          { Sample.kind = def.kind; text = t.text }))
    |> Sample.decode f map
  in
  (* The fold's three answers above, from the sampler. Reading back as the
     same tokens would not tell a needless blank from none. *)
  check "the sampler keeps a bracket and a star apart" ~expected:"( *x" (sampled "( * x");
  check "it lets a number and a dot touch" ~expected:"1.x" (sampled "1 . x");
  check
    "and it keeps apart two that a line break would not settle"
    ~expected:"< <x"
    (sampled "< < x");
  let rec sequences (n : int) : string list list =
    if n = 0
    then [ [] ]
    else
      List.concat_map
        (fun (rest : string list) -> List.map (fun (t : string) -> t :: rest) alphabet)
        (sequences (n - 1))
  in
  let all = List.concat_map sequences [ 1; 2; 3; 4 ] in
  let unread = ref [] in
  let lost = ref [] in
  let moved = ref [] in
  let drawn = ref [] in
  List.iter
    (fun (tokens : string list) ->
       let src = String.concat " " tokens in
       if read src <> tokens
       then unread := src :: !unread
       else (
         let once = format src in
         if read once <> tokens then lost := (src, once) :: !lost;
         let twice = format once in
         if not (String.equal once twice) then moved := (src, once, twice) :: !moved;
         let decoded = sampled src in
         if read decoded <> tokens then drawn := (src, decoded) :: !drawn))
    all;
  (match !unread with
   | [] -> Law.pass "all %d sequences read as the tokens they name" (List.length all)
   | src :: _ ->
     Law.fail
       "%d sequences do not read as their own tokens, the first %S"
       (List.length !unread)
       src);
  (match List.rev !lost with
   | [] -> Law.pass "every one of them formats to the same tokens"
   | (src, once) :: _ ->
     Law.fail
       "%d sequences format to other tokens, the first %S, written %S"
       (List.length !lost)
       src
       once);
  (match List.rev !moved with
   | [] -> Law.pass "and stands when formatted again"
   | (src, once, twice) :: _ ->
     Law.fail
       "%d sequences move when formatted again, the first %S: %S, then %S"
       (List.length !moved)
       src
       once
       twice);
  (match List.rev !drawn with
   | [] -> Law.pass "and the sampler writes every one as text that reads as it"
   | (src, decoded) :: _ ->
     Law.fail
       "the sampler writes %d sequences as other tokens, the first %S, written %S"
       (List.length !drawn)
       src
       decoded);
  gaps ();
  Law.summarise "joins"
;;
