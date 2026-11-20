(* -- bodies that open together -------------------------------------------------

      The blocks of one [if ... else] are marked [~together], so where the
      chain does not fit on a line every one of them opens, a short one
      included. Unmarked, each body would decide for itself, and a chain one
      column too long would open its last block alone:

      {v
        if (c) { aaaa; bbbb } else {      if (c) {
          x;                                aaaa;
        }                                   bbbb;
                                          } else {
                                            x;
                                          }
      v}

      No law states it, for the reason test/units/yields.ml gives: both
      layouts hold the same tokens inside the same ruler. So these are literal
      expected strings, each at the width where the layout turns, and each is
      formatted a second time to see that it stands.

      The grammar is this file's own. [If] marks its block and its
      [ElseBody], and [ElseBody] marks the block or the [If] it holds, which
      is what makes [if ... else if ... else] one decision. [Both] holds two
      blocks and marks neither. [When] has a gap that may break in front of
      its first block, and [Tail] has one behind a block it starts with. [Mix]
      marks a block, a separated list and a production of two names, and only
      the block is a body.

      The last part is a sweep. The corpus the laws read has no block that is
      marked and may lie on one line, so nothing there formats through the
      conditional a marked block's breaks follow. A handful of inputs, the
      broken ones among them, go through every width here instead: each has
      to come out as a fixed point, with every conditional inside the frame
      that handed it out.
   -------------------------------------------------------------------------- *)

let grammar : Grammar.t =
  let open Grammar in
  let name = Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z')) in
  let tokens =
    [ kw "if"
    ; kw "else"
    ; kw "both"
    ; kw "and"
    ; kw "when"
    ; kw "pre"
    ; kw "then"
    ; kw "mix"
    ; kw "with"
    ; kw "case"
    ; kw "val"
    ; punct ~name:"lbrace" "{"
    ; punct ~name:"rbrace" "}"
    ; punct ~space_after:Hug ~name:"lparen" "("
    ; punct ~space_before:Hug ~name:"rparen" ")"
    ; punct ~space_before:Hug ~name:"comma" ","
    ; punct ~space_before:Hug ~name:"semi" ";"
    ; punct ~name:"eq" "="
    ; punct ~name:"arrow" "=>"
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
      [ child_alt
          ~modifier:Exactly_one
          "kind"
          [ Rule "If"
          ; Rule "Both"
          ; Rule "Pre"
          ; Rule "Mix"
          ; Rule "When"
          ; Rule "Arm"
          ; Rule "Val"
          ; Rule "Block"
          ; Token "name"
          ]
      ]
  in
  let params =
    prod "Params" [ child_rep "param" (Token "name") ]
    |> with_delimited_sep ~open_tok:"lparen" ~close_tok:"rparen" ~sep:"comma"
  in
  let block =
    prod "Block" [ child_rep "stmt" (Rule "Stmt") ]
    |> with_delimited_sep
         ~pad:true
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"semi"
         ~trailing_sep:On_break
  in
  let if_ =
    prod
      "If"
      [ child_req "kw" (Token "if")
      ; child_req ~break:Never "cond" (Rule "Params")
      ; child_req ~break:Never ~together:true "then_" (Rule "Block")
      ; child_req ~break:Never "else_kw" (Token "else")
      ; child_req ~break:Never ~together:true "else_" (Rule "ElseBody")
      ]
  in
  let else_body =
    prod
      "ElseBody"
      [ child_alt_rules ~together:true ~modifier:Exactly_one "kind" [ "If"; "Block" ] ]
  in
  let both =
    prod
      "Both"
      [ child_req "kw" (Token "both")
      ; child_req ~break:Never "one" (Rule "Block")
      ; child_req ~break:Never "and_kw" (Token "and")
      ; child_req ~break:Never "two" (Rule "Block")
      ]
  in
  let when_ =
    prod
      "When"
      [ child_req "kw" (Token "when")
      ; child_req ~break:Never "cond" (Token "name")
      ; child_req ~together:true "then_" (Rule "Block")
      ; child_req ~break:Never "else_kw" (Token "else")
      ; child_req ~break:Never ~together:true "else_" (Rule "Block")
      ]
  in
  let tail =
    prod
      "Tail"
      [ child_req ~together:true "block" (Rule "Block")
      ; child_req "then_kw" (Token "then")
      ; child_req ~break:Never "name" (Token "name")
      ]
  in
  let pre =
    prod
      "Pre"
      [ child_req "kw" (Token "pre"); child_req ~break:Never "tail" (Rule "Tail") ]
  in
  let names =
    prod "Names" [ child_rep1 "name" (Token "name") ] |> with_separator ~sep:"comma"
  in
  let pair =
    prod "Pair" [ child_req "one" (Token "name"); child_req "two" (Token "name") ]
  in
  let mix =
    prod
      "Mix"
      [ child_req "kw" (Token "mix")
      ; child_req ~break:Never ~together:true "block" (Rule "Block")
      ; child_req ~break:Never "with_kw" (Token "with")
      ; child_req ~break:Never ~together:true "names" (Rule "Names")
      ; child_req ~break:Never "and_kw" (Token "and")
      ; child_req ~break:Never ~together:true "pair" (Rule "Pair")
      ]
  in
  let body =
    prod
      "Body"
      [ child_alt ~modifier:Exactly_one "kind" [ Rule "If"; Rule "Block"; Token "name" ] ]
  in
  let arm =
    prod
      "Arm"
      [ child_req "kw" (Token "case")
      ; child_req ~break:Never "ctor" (Token "name")
      ; child_req ~break:Never ~space:false "params" (Rule "Params")
      ; child_req ~break:Never "arrow" (Token "arrow")
      ; child_req ~break:Never "body" (Rule "Body")
      ]
  in
  let val_ =
    prod
      "Val"
      [ child_req "kw" (Token "val")
      ; child_req ~break:Never "name" (Token "name")
      ; child_req ~break:Never "eq" (Token "eq")
      ; child_req "value" (Rule "Body")
      ]
  in
  create
    ~tokens
    ~roots:[ "File" ]
    [ file
    ; stmt
    ; params
    ; block
    ; if_
    ; else_body
    ; both
    ; when_
    ; tail
    ; pre
    ; names
    ; pair
    ; mix
    ; body
    ; arm
    ; val_
    ]
;;

let () =
  let f =
    match Core.Facts.of_grammar grammar with
    | Ok f -> f
    | Error errors ->
      List.iter (fun e -> Format.eprintf "%a@." Core.Error.pp e) errors;
      failwith "the grammar of test/units/together.ml does not check"
  in
  let plan, _ = Plan.Lower.of_facts f in
  let layout = Layout.Lower.of_facts f in
  let boundary = Lex.boundary f in
  let format (width : int) (src : string) : string =
    let tree, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
    Lingo_runtime.Layout.format layout ~boundary ~width tree
  in
  (* What a source comes out as at a width, and that the result stands. *)
  let check (what : string) ~(width : int) ~(expected : string) (src : string) : unit =
    let got = format width src in
    if not (String.equal expected got)
    then Law.fail "%s\n  expected %S\n  got      %S" what expected got
    else if not (String.equal expected (format width got))
    then Law.fail "%s\n  %S does not stand: it formats to %S" what got (format width got)
    else Law.pass "%s" what
  in
  (* -- one chain --

     23 columns. *)
  let chain = "if (a) { x } else { y }" in
  check "a chain that fits lies on one line" ~width:23 ~expected:chain chain;
  check
    "one column short, both of its blocks open"
    ~width:22
    ~expected:"if (a) {\n  x;\n} else {\n  y;\n}"
    chain;
  (* Two blocks unmarked: 23 columns, and 16 up to the second brace, so the
     first fits and the second opens alone. *)
  let unmarked = "both { x } and { y; z }" in
  check
    "two blocks that are not marked decide apart"
    ~width:22
    ~expected:"both { x } and {\n  y;\n  z;\n}"
    unmarked;
  (* -- a chain through [else if] --

     41 columns. *)
  let longer = "if (a) { x } else if (b) { y } else { z }" in
  check "a longer chain that fits lies on one line" ~width:41 ~expected:longer longer;
  check
    "one column short, every block of it opens"
    ~width:40
    ~expected:"if (a) {\n  x;\n} else if (b) {\n  y;\n} else {\n  z;\n}"
    longer;
  (* -- what is not one of the blocks --

     The condition is a matched pair too, and it is not marked, so it decides
     for itself: it stays whole while its own line fits. 32 columns in all, and
     17 up to the first brace. *)
  let wide_cond = "if (aaaa, bbbb) { x } else { y }" in
  check
    "the condition stays whole where the chain opens"
    ~width:17
    ~expected:"if (aaaa, bbbb) {\n  x;\n} else {\n  y;\n}"
    wide_cond;
  check
    "and opens where its own line does not fit"
    ~width:16
    ~expected:"if (\n  aaaa,\n  bbbb\n) {\n  x;\n} else {\n  y;\n}"
    wide_cond;
  (* A chain inside a block of another is a decision of its own. 45 columns in
     all, and the inner chain is 23. *)
  let nested = "if (a) { if (b) { x } else { y } } else { z }" in
  check
    "a chain inside a block of another decides for itself"
    ~width:40
    ~expected:"if (a) {\n  if (b) { x } else { y };\n} else {\n  z;\n}"
    nested;
  (* -- what stands in front of a chain --

     A list in front counts what follows it as far as the next bracket that
     can end the line, as test/units/yields.ml has it. Here that is the
     condition's, so the list is the last of the three to open. 40 columns in
     all, 24 up to the first brace and 20 up to the condition's bracket. *)
  let arm = "case p(a, b) => if (c) { xx } else { y }" in
  check
    "a list in front of a chain stays whole and the chain opens"
    ~width:39
    ~expected:"case p(a, b) => if (c) {\n  xx;\n} else {\n  y;\n}"
    arm;
  check
    "down to the width of the line that ends at the first brace"
    ~width:24
    ~expected:"case p(a, b) => if (c) {\n  xx;\n} else {\n  y;\n}"
    arm;
  check
    "one column short of it the condition opens, and the list stays whole"
    ~width:23
    ~expected:"case p(a, b) => if (\n  c\n) {\n  xx;\n} else {\n  y;\n}"
    arm;
  check
    "and the list opens where the line to the condition's bracket does not fit"
    ~width:19
    ~expected:"case p(\n  a,\n  b\n) => if (c) {\n  xx;\n} else {\n  y;\n}"
    arm;
  (* A gap that may break in front of a chain opens before the chain does, as
     in front of any body. 31 columns, and the chain is 23. *)
  let value = "val r = if (a) { x } else { y }" in
  check
    "a gap that may break in front of a chain opens first"
    ~width:30
    ~expected:"val r =\n  if (a) { x } else { y }"
    value;
  (* -- a gap inside the chain --

     [When]'s first block has a gap in front of it that may break. The block
     stays out of the group that settles the gap, so the gap sees it as far as
     its brace and stays closed. 28 columns. *)
  let gapped = "when a { xx; yy } else { z }" in
  check
    "a chain with a gap in it lies on one line where it fits"
    ~width:28
    ~expected:gapped
    gapped;
  check
    "one column short its blocks open, and the gap in front of the first stays closed"
    ~width:27
    ~expected:"when a {\n  xx;\n  yy;\n} else {\n  z;\n}"
    gapped;
  (* [Tail] starts with its block, and the gap behind the block may break.
     The block stays out of that gap's group too, so the gap is settled by
     what follows it. 21 columns. *)
  let tailed = "pre { xx; yy } then z" in
  check
    "a gap behind a block its rule starts with stays closed where the block opens"
    ~width:20
    ~expected:"pre {\n  xx;\n  yy;\n} then z"
    tailed;
  (* -- what a mark does not reach --

     A marked child that holds a separated list, or a production with no
     marked child of its own, keeps the group it had. So where [Mix] opens,
     its block opens and the other two lie as they fit. 31 columns. *)
  let mixed = "mix { x } with aa, bb and cc dd" in
  check
    "a list with no brackets and a plain production keep their own groups"
    ~width:30
    ~expected:"mix {\n  x;\n} with aa, bb and cc dd"
    mixed;
  (* -- the table --

     Nine slots are marked: two in [If], one in [ElseBody], two in [When], one
     in [Tail] and three in [Mix]. *)
  let dumped = Format.asprintf "%a" Layout.Text.pp layout in
  let marked =
    List.length
      (List.filter
         (fun line ->
            let mark = " together" in
            let width = String.length mark in
            let rec from (at : int) : bool =
              at + width <= String.length line
              && (String.equal (String.sub line at width) mark || from (at + 1))
            in
            from 0)
         (String.split_on_char '\n' dumped))
  in
  if marked = 9
  then Law.pass "the dump shows each marked slot"
  else Law.fail "the dump shows %d marked slots, and the grammar marks 9" marked;
  (* -- the sweep -- *)
  let inputs =
    [ chain
    ; longer
    ; wide_cond
    ; nested
    ; arm
    ; value
    ; gapped
    ; tailed
    ; unmarked
    ; mixed
    ; "if (a) { } else { }"
    ; "if (a) { x; } else { y; z; }"
    ; "if (a) { if (b) { if (c) { x } else { y } } else { z } } else { w }"
    ; "case p(a, b) => if (c) { x } else if (d) { y } else { z }"
    ; "if (a) { x } else {"
    ; "if (a) { x"
    ; "if (a) { x } else"
    ; "if (a) { x; ; y } else { z }"
    ; "if (a { x } else { y }"
    ; "if { x } else { y } }"
    ; "when a { x } else"
    ; "pre { x } then"
    ]
  in
  let before = Law.failures () in
  let formats = ref 0 in
  List.iter
    (fun src ->
       let tree, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
       (match Handsome.Utf8.check (Lingo_runtime.Layout.doc layout ~boundary tree) with
        | Ok () -> ()
        | Error _ -> Law.fail "%S: a conditional is outside its frame" src);
       for width = 1 to 60 do
         incr formats;
         let once = format width src in
         let twice = format width once in
         if not (String.equal once twice)
         then Law.fail "%S at %d: %S formats to %S" src width once twice
       done)
    inputs;
  if Law.failures () = before
  then Law.pass "every input stands after one format, over %d formats" !formats;
  Law.summarise "together"
;;
