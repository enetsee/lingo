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

      The grammar is this file's own: names, a call, a subscript and a
      trailing block.
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
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "name" ]
      ~postfix:
        [ postfix_index
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
  create ~expr:[ expr ] ~tokens ~roots:[ "File" ] [ file; stmt ]
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
  Law.summarise "enclosed"
;;
