(* -- where a call that does not fit opens -----------------------------------------

      An operator that encloses its operand is a matched pair behind something:
      the arguments of a call, the index of a subscript, a trailing block. A
      pair that does not fit opens just inside its brackets, with what it holds
      one step in and its closer back where the line began. That is what a
      matched pair does anywhere, and the gap just inside one is where it
      starts.

      The operand stands in front of the opener, inside the same node. The
      fold once took it for the first thing in the body, so the argument
      behind the bracket was the second and had no break in front of it. A
      call that did not fit kept its first argument on the bracket's line and
      sent the closer to a line of its own:

      {v
        outer(first,
          inner(aa, bb),
          tail,
        );
      v}

      No law states where a pair opens. Every law in test/laws/law_layout.ml
      holds for that layout too: the same tokens, inside the ruler, and a
      fixed point. So these are literal expected strings, each at the width
      where the layout turns.

      The second half is what the pair holds. A list that is all a matched
      pair holds breaks with the pair: one element to a line where the pair
      opens, and flat where it does not. A call whose comma is its own does
      that already, since the pair and the commas are one group. Where the
      list is a rule of its own between the brackets it was a second
      decision, and a call that opened kept its arguments on one line:

      {v
        run<
          aa, bb | cc, dd
        >;
      v}

      The layout says which pairs hand their decision to what they hold, in
      {!Ir.Layout.slot.handed}: a pair with no separator of its own that may
      hold nothing but tokens and lists. The first check below reads that
      off the layout, and the rest are what the fold does with it.

      The grammar is this file's own: names, a call, a subscript and a
      trailing block, then the pairs that hold a rule each.

      - [Tuple] holds a list.
      - [<] holds [Args], which is two lists and a bar.
      - [#(] holds [Exprs], and [@(] holds the same expressions between
        commas of its own.
      - [\[|] holds [Table], a list of rows, and a row is a list.
      - [<<] holds rows between bars of its own.
      - [(|] holds [Mixed], a list and a tuple.
      - [Either] holds a list or a tuple.
      - [Tagged] holds [Tag], which is tokens and no list.
      - [Loose] holds [Args] between a [!] and a [.], which are no pair.
   -------------------------------------------------------------------------- *)

let grammar : Grammar.t =
  let open Grammar in
  let name = Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z')) in
  let tokens =
    [ punct ~space_after:Hug ~name:"lparen" "("
    ; punct ~space_before:Hug ~name:"rparen" ")"
    ; punct ~space_after:Hug ~name:"lbracket" "["
    ; punct ~space_before:Hug ~name:"rbracket" "]"
    ; punct ~space_before:Space ~name:"lbrace" "{"
    ; punct ~name:"rbrace" "}"
    ; punct ~space_before:Hug ~name:"comma" ","
    ; punct ~space_before:Hug ~name:"semi" ";"
    ; punct ~space_after:Hug ~name:"lrow" "<<"
    ; punct ~space_before:Hug ~name:"rrow" ">>"
    ; punct ~space_after:Hug ~name:"lmix" "(|"
    ; punct ~space_before:Hug ~name:"rmix" "|)"
    ; punct ~space_after:Hug ~name:"langle" "<"
    ; punct ~space_before:Hug ~name:"rangle" ">"
    ; punct ~space_after:Hug ~name:"lown" "@("
    ; punct ~space_before:Hug ~name:"rown" ")@"
    ; punct ~space_after:Hug ~name:"lheld" "#("
    ; punct ~space_before:Hug ~name:"rheld" ")#"
    ; punct ~space_after:Hug ~name:"lgrid" "[|"
    ; punct ~space_before:Hug ~name:"rgrid" "|]"
    ; punct ~space_after:Hug ~name:"leither" "[<"
    ; punct ~space_before:Hug ~name:"reither" ">]"
    ; punct ~space_after:Hug ~name:"ltag" "<|"
    ; punct ~space_before:Hug ~name:"rtag" "|>"
    ; punct ~space_after:Hug ~name:"bang" "!"
    ; punct ~space_before:Hug ~name:"dot" "."
    ; punct ~name:"bar" "|"
    ; punct_tight ~name:"colon" ":"
    ; pat "name" name
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file =
    prod
      ~indent_width:0
      "File"
      [ child_rep ~break:(always 1) ~between:(always 1) "stmt" (Rule "Stmt") ]
  in
  let stmt =
    prod
      "Stmt"
      [ child_req "value" (Rule "Expr"); child_req ~break:Never "semi" (Token "semi") ]
  in
  let list (rule : string) ~(sep : string) : production =
    prod rule [ child_rep1 "name" (Token "name") ] |> with_separator ~sep
  in
  (* A list that writes a separator in front of its first element only where
     it breaks. It is all [Tuple] holds, so whether it breaks is the tuple's
     to say. *)
  let items =
    prod "Items" [ child_rep1 "name" (Token "name") ]
    |> with_separator ~sep:"bar" ~leading_sep:On_break ~sep_position:Op_before
  in
  let tuple =
    prod "Tuple" [ child_req "items" (Rule "Items") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  let args =
    prod
      "Args"
      [ child_opt ~break:Never "front" (Rule "Names")
      ; child_opt ~break:Never "back" (Rule "Back")
      ]
  in
  let back =
    prod
      "Back"
      [ child_req "bar" (Token "bar"); child_req ~break:Never "more" (Rule "More") ]
  in
  let mixed =
    prod
      ~indent_width:0
      "Mixed"
      [ child_req "names" (Rule "Names"); child_req ~break:Never "tuple" (Rule "Tuple") ]
  in
  let either =
    prod "Either" [ child_alt_rules ~modifier:Exactly_one "kind" [ "Names"; "Tuple" ] ]
    |> with_delimited ~open_tok:"leither" ~close_tok:"reither"
  in
  let tag =
    prod
      ~indent_width:0
      "Tag"
      [ child_req "key" (Token "name")
      ; child_req ~break:Never "colon" (Token "colon")
      ; child_req ~break:Never "value" (Token "name")
      ]
  in
  let tagged =
    prod "Tagged" [ child_req "tag" (Rule "Tag") ]
    |> with_delimited ~open_tok:"ltag" ~close_tok:"rtag"
  in
  let loose =
    prod
      "Loose"
      [ child_req "bang" (Token "bang")
      ; child_req ~break:Never "args" (Rule "Args")
      ; child_req ~break:Never "dot" (Token "dot")
      ]
  in
  let exprs =
    prod "Exprs" [ child_rep1 "expr" (Rule "Expr") ] |> with_separator ~sep:"comma"
  in
  let table =
    prod "Table" [ child_rep1 "row" (Rule "Row") ] |> with_separator ~sep:"bar"
  in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "name"; Rule "Tuple"; Rule "Either"; Rule "Tagged"; Rule "Loose" ]
      ~postfix:
        [ postfix_call
            ~kind_suffix:"own"
            ~open_tok:"lown"
            ~close_tok:"rown"
            ~elem:(Rule "Expr")
            ~sep_policy:(with_sep "comma")
            ~bp:100
            ()
        ; postfix_index
            ~kind_suffix:"held"
            ~open_tok:"lheld"
            ~close_tok:"rheld"
            ~index:(Rule "Exprs")
            ~bp:100
            ()
        ; postfix_index
            ~kind_suffix:"grid"
            ~open_tok:"lgrid"
            ~close_tok:"rgrid"
            ~index:(Rule "Table")
            ~bp:100
            ()
        ; postfix_index
            ~kind_suffix:"apply"
            ~open_tok:"langle"
            ~close_tok:"rangle"
            ~index:(Rule "Args")
            ~bp:100
            ()
        ; postfix_call
            ~kind_suffix:"rows"
            ~open_tok:"lrow"
            ~close_tok:"rrow"
            ~elem:(Rule "Row")
            ~sep_policy:(with_sep "bar")
            ~bp:100
            ()
        ; postfix_index
            ~kind_suffix:"mix"
            ~open_tok:"lmix"
            ~close_tok:"rmix"
            ~index:(Rule "Mixed")
            ~bp:100
            ()
        ; postfix_index
            ~kind_suffix:"index"
            ~open_tok:"lbracket"
            ~close_tok:"rbracket"
            ~index:(Rule "Expr")
            ~bp:100
            ()
        ; postfix_call
            ~kind_suffix:"call"
            ~open_tok:"lparen"
            ~close_tok:"rparen"
            ~elem:(Rule "Expr")
            ~sep_policy:(with_sep ~trailing:On_break "comma")
            ~bp:100
            ()
        ; postfix_brace
            ~kind_suffix:"block"
            ~pad:true
            ~open_tok:"lbrace"
            ~close_tok:"rbrace"
            ~body:(Rule "Stmt")
            ~bp:100
            ()
        ]
      ~continuation_indent:2
      ()
  in
  create
    ~expr:[ expr ]
    ~tokens
    ~roots:[ "File" ]
    [ file
    ; stmt
    ; tuple
    ; items
    ; args
    ; back
    ; mixed
    ; either
    ; tag
    ; tagged
    ; loose
    ; exprs
    ; table
    ; list "Names" ~sep:"comma"
    ; list "More" ~sep:"comma"
    ; list "Row" ~sep:"colon"
    ]
;;

let check (what : string) ~(expected : string) (got : string) : unit =
  if String.equal expected got
  then Law.pass "%s" what
  else Law.fail "%s\n  expected %S\n  got      %S" what expected got
;;

let () =
  let f =
    match Core.Facts.of_grammar grammar with
    | Ok f -> f
    | Error errors ->
      List.iter (fun e -> Format.eprintf "%a@." Core.Error.pp e) errors;
      failwith "the grammar of test/units/enclosed.ml does not check"
  in
  let plan, _ = Plan.Lower.of_facts f in
  let layout = Layout.Lower.of_facts f in
  let boundary = Lex.boundary f in
  let format (width : int) (src : string) : string =
    let tree, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
    Lingo_runtime.Layout.format layout ~boundary ~width tree
  in
  let settles (what : string) (width : int) (src : string) : unit =
    let once = format width src in
    check (what ^ ", and stands when formatted again") ~expected:once (format width once)
  in
  (* -- which slots hand a decision on --

     A pair with no separator of its own, where all it may hold is tokens and
     lists, and the plain rules those pairs may hold. The subscript is not
     one: an expression is no list, and neither is the rule that wraps a
     name. [Tag] is tokens with no list among them. [Mixed] and [Either] may
     hold a tuple. The rows of [<<] and the arguments of a call are elements,
     between separators the pair owns. [Loose] has no brackets to open. *)
  let handed =
    Array.to_list layout.rules
    |> List.concat_map (fun (r : Ir.Layout.rule) ->
      Array.to_list r.slots
      |> List.mapi (fun i (s : Ir.Layout.slot) -> i, s)
      |> List.filter_map (fun ((i, s) : int * Ir.Layout.slot) ->
        if s.handed then Some (Printf.sprintf "%s.%d" r.name i) else None))
    |> List.sort String.compare
  in
  check
    "the slots that hand a decision on"
    ~expected:
      "Args.0 Args.1 Back.1 ExprPostfixApply.1 ExprPostfixGrid.1 ExprPostfixHeld.1 \
       Tuple.0"
    (String.concat " " handed);
  (* -- a call --

     34 columns in all. *)
  let call = "outer(first, inner(aa, bb), tail);" in
  check "a call that fits lies on one line" ~expected:call (format 34 call);
  check
    "one column short, it opens behind its bracket and the inner call stays whole"
    ~expected:"outer(\n  first,\n  inner(aa, bb),\n  tail,\n);"
    (format 33 call);
  check
    "the inner call stays whole down to the width of its own line"
    ~expected:"outer(\n  first,\n  inner(aa, bb),\n  tail,\n);"
    (format 16 call);
  check
    "below that the inner call opens the same way, one step further in"
    ~expected:"outer(\n  first,\n  inner(\n    aa,\n    bb,\n  ),\n  tail,\n);"
    (format 15 call);
  settles "a call that opened" 33 call;
  settles "a call inside a call that opened" 15 call;
  (* -- a subscript --

     19 columns. It holds one thing and no separator. *)
  let index = "table[longestindex];" in
  check "a subscript that fits lies on one line" ~expected:index (format 20 index);
  check
    "one column short, it opens behind its bracket"
    ~expected:"table[\n  longestindex\n];"
    (format 19 index);
  (* -- a trailing block --

     16 columns. The block holds one statement, which starts the line after
     its brace, and the call in front of it stays whole. *)
  let block = "run(aa) { bb; };" in
  check "a trailing block that fits lies on one line" ~expected:block (format 16 block);
  check
    "one column short, the block opens and the call stays whole"
    ~expected:"run(aa) {\n  bb;\n};"
    (format 15 block);
  settles "a trailing block that opened" 15 block;
  (* -- a pair that holds a list --

     15 columns. The list is a rule of its own, and the pair is a production.
     The bar in front of the first name is written where the list breaks,
     which is where the tuple opens. *)
  let tuple = "(aa | bb | cc);" in
  check "a tuple that fits lies on one line" ~expected:tuple (format 15 tuple);
  check
    "one column short, it opens and its names go one to a line, each behind a bar"
    ~expected:"(\n  | aa\n  | bb\n  | cc\n);"
    (format 14 tuple);
  settles "a tuple that opened" 14 tuple;
  (* -- a pair that holds two lists and a token --

     21 columns. [Args] is no list itself. It holds nothing but lists and a
     bar, so the pair's decision reaches both. The bar's gaps cannot break. *)
  let apply = "run<aa, bb | cc, dd>;" in
  check "two lists in one pair fit on one line" ~expected:apply (format 21 apply);
  check
    "one column short, the pair opens and both lists break with it"
    ~expected:"run<\n  aa,\n  bb | cc,\n  dd\n>;"
    (format 20 apply);
  settles "a pair of two lists that opened" 20 apply;
  (* -- a list rule against a separator the pair owns --

     The same expressions between [#(] and [)#], where the commas are a
     rule's, and between [@(] and [)@], where they are the pair's. The two
     lay out alike at every width. *)
  let swapped (s : string) : string =
    let b = Buffer.create (String.length s) in
    let n = String.length s in
    let rec go (i : int) : unit =
      if i < n
      then
        if i + 1 < n && s.[i] = '@' && s.[i + 1] = '('
        then (
          Buffer.add_string b "#(";
          go (i + 2))
        else if i + 1 < n && s.[i] = ')' && s.[i + 1] = '@'
        then (
          Buffer.add_string b ")#";
          go (i + 2))
        else (
          Buffer.add_char b s.[i];
          go (i + 1))
    in
    go 0;
    Buffer.contents b
  in
  let apart = ref [] in
  let widths = ref 0 in
  List.iter
    (fun (src : string) ->
       for width = 1 to String.length src + 1 do
         incr widths;
         let own = swapped (format width src) in
         let held = format width (swapped src) in
         if not (String.equal own held) then apart := (src, width, own, held) :: !apart
       done)
    [ "f@(aa)@;"
    ; "f@(aa, bb, cc)@;"
    ; "outer@(first, inner@(aa, bb)@, tail)@;"
    ; "f@(g@(h@(aa, bb)@, cc)@, dd)@@(ee, ff)@;"
    ; "run@(aa, bb)@ { cc@(dd, ee)@; };"
    ; "f@(table[aa], (bb | cc), g(dd, ee))@;"
    ];
  (match List.rev !apart with
   | [] ->
     Law.pass
       "a list rule in a pair lays out as a pair that owns the separator, at %d widths"
       !widths
   | (src, width, own, held) :: _ ->
     Law.fail
       "%S at %d differs\n  the pair owns the commas %S\n  a rule owns them        %S"
       src
       width
       own
       held);
  (* -- a list of lists --

     The pair holds [Table] and nothing else, so the table breaks with it.
     A row is an element of the table, and an element decides for itself. *)
  let grid = "t[|aa:bb | cc:dd | ee|];" in
  check "a table that fits lies on one line" ~expected:grid (format 24 grid);
  check
    "one column short, the rows go one to a line and each stays whole"
    ~expected:"t[|\n  aa:bb |\n  cc:dd |\n  ee\n|];"
    (format 23 grid);
  (* -- a pair that may hold a list or a tuple --

     14 columns. What the pair may hold is not only lists, so the layout marks
     nothing, and the list in it keeps its own decision. *)
  let either = "[<aa, bb, cc>];" in
  check "a list in such a pair fits on one line" ~expected:either (format 15 either);
  check
    "one column short, the pair opens and the list lies as it fits"
    ~expected:"[<\n  aa, bb, cc\n>];"
    (format 14 either);
  (* -- a list inside a list --

     22 columns. The bars are the pair's own, and a row between two of them
     is an element. It is not all the pair holds, so it keeps its own
     decision: a row stays whole where the pair opens. *)
  let rows = "t<<aa:bb | cc:dd | ee>>;" in
  check "rows that fit lie on one line" ~expected:rows (format 24 rows);
  check
    "one column short, the rows a pair owns go one to a line and each stays whole"
    ~expected:"t<<\n  aa:bb |\n  cc:dd |\n  ee\n>>;"
    (format 23 rows);
  (* -- a rule that holds a list and a pair --

     22 columns. [Mixed] holds a tuple, so it is no list with no brackets of
     its own, and the pair hands it nothing. Its names fit the line they are
     on and stay there. *)
  let mix = "m(|aa, bb (cc | dd)|);" in
  check "a list and a tuple in one pair fit on one line" ~expected:mix (format 22 mix);
  check
    "one column short, the pair opens and what it holds lies as it fits"
    ~expected:"m(|\n  aa, bb (cc | dd)\n|);"
    (format 21 mix);
  (* -- the same two lists with no brackets --

     26 columns. [Args] passes on a decision it was handed. Behind a [!]
     nothing hands it one, so each of its lists decides for itself, and is
     indented by its own rule. Between brackets the pair's indent stands in
     for that, which is why the lists above sit one step in and no more. *)
  let loose = "!aaaa, bbbb | cccc, dddd.;" in
  check "two lists with no brackets fit on one line" ~expected:loose (format 26 loose);
  check
    "one column short, the first breaks and the second stays whole"
    ~expected:"!aaaa,\n  bbbb | cccc, dddd.;"
    (format 25 loose);
  check
    "the second breaks where its own line does not fit"
    ~expected:"!aaaa,\n  bbbb | cccc,\n    dddd.;"
    (format 16 loose);
  Law.summarise "enclosed"
;;
