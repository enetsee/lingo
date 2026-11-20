(* -- which bracket opens first ---------------------------------------------------

      A line that does not fit has to open somewhere, and where it opens is
      what a reader sees. A header with a parameter list and a body in braces
      opens the body, and the list only where the header alone does not fit.

      No law states it. Every law in test/laws/law_layout.ml holds whichever
      bracket opens, because both layouts hold the same tokens inside the same
      ruler. So these are literal expected strings, each at the width where the
      layout turns.

      The grammar is this file's own. Its bodies lie on one line where they
      fit, and no grammar in grammars/ has a list followed by one of those: a
      body there always breaks, and a body that always breaks is measured as
      far as its opener already.

      {v
        def f(a, b): t { x; y }
        case p(a, b) => { x; y }
        case p(a, b) => if c { x } else { y }
        case p(a, b) => do { x } while c
        case p(a, b) => try { x }
        catch { y }
        case p(a, b) => (x, y)
        case p(a, b) => x, y
        val r = { x; y }
      v}

      [Stmt] and [Body] hold one child each. Every body here is reached
      through one or both, which is how a grammar reaches a body nearly
      everywhere, and a mark on the delimited rule alone stops at the first of
      them.

      [Block] writes its trailing separator only where it breaks, so its group
      is a frame. [Params] has no such separator and its group is a plain one.
      An arm's body may be either, so a body takes both paths through the
      fold.
   -------------------------------------------------------------------------- *)

let grammar : Grammar.t =
  let open Grammar in
  let name = Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z')) in
  let tokens =
    [ kw "def"
    ; kw "val"
    ; kw "case"
    ; kw "if"
    ; kw "else"
    ; kw "do"
    ; kw "while"
    ; kw "try"
    ; kw "catch"
    ; punct ~name:"lbrace" "{"
    ; punct ~name:"rbrace" "}"
    ; punct ~space_after:Hug ~name:"lparen" "("
    ; punct ~space_before:Hug ~name:"rparen" ")"
    ; punct ~space_before:Hug ~name:"comma" ","
    ; punct ~space_before:Hug ~name:"semi" ";"
    ; punct ~space_before:Hug ~name:"colon" ":"
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
          [ Rule "Def"; Rule "Val"; Rule "Arm"; Rule "If"; Rule "Block"; Token "name" ]
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
  let names =
    prod "Names" [ child_rep1 "name" (Token "name") ] |> with_separator ~sep:"comma"
  in
  let body =
    prod
      "Body"
      [ child_alt_rules
          ~modifier:Exactly_one
          "kind"
          [ "Block"; "Params"; "If"; "Loop"; "Try"; "Names" ]
      ]
  in
  let def =
    prod
      "Def"
      [ child_req "kw" (Token "def")
      ; child_req ~break:Never "name" (Token "name")
      ; child_req ~break:Never ~space:false "params" (Rule "Params")
      ; child_req ~break:Never "colon" (Token "colon")
      ; child_req ~break:Never "ty" (Token "name")
      ; child_req ~break:Never "body" (Rule "Block")
      ]
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
  let if_ =
    prod
      "If"
      [ child_req "kw" (Token "if")
      ; child_req ~break:Never "cond" (Token "name")
      ; child_req ~break:Never "then_" (Rule "Block")
      ; child_req ~break:Never "else_kw" (Token "else")
      ; child_req ~break:Never "else_" (Rule "Block")
      ]
  in
  (* A body with tokens after it. *)
  let loop =
    prod
      "Loop"
      [ child_req "kw" (Token "do")
      ; child_req ~break:Never "body" (Rule "Block")
      ; child_req ~break:Never "while_kw" (Token "while")
      ; child_req ~break:Never "cond" (Token "name")
      ]
  in
  (* A body with a hard break after it. *)
  let try_ =
    prod
      "Try"
      [ child_req "kw" (Token "try")
      ; child_req ~break:Never "body" (Rule "Block")
      ; child_req ~break:(always 1) "catch_kw" (Token "catch")
      ; child_req ~break:Never "handler" (Rule "Block")
      ]
  in
  (* The one gap here that may break, in front of a body. *)
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
    [ file; stmt; params; block; names; body; def; arm; if_; loop; try_; val_ ]
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
      failwith "the grammar of test/units/yields.ml does not check"
  in
  let plan, _ = Plan.Lower.of_facts f in
  let layout = Layout.Lower.of_facts f in
  let boundary = Lex.boundary f in
  let format (width : int) (src : string) : string =
    let tree, _ = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
    Lingo_runtime.Layout.format layout ~boundary ~width tree
  in
  (* -- a list, then a body --

     23 columns in all, and the header up to its brace is 16. *)
  let def = "def f(a, b): t { x; y }" in
  check "a definition that fits lies on one line" ~expected:def (format 23 def);
  check
    "one column short, the body opens and the header stays whole"
    ~expected:"def f(a, b): t {\n  x;\n  y;\n}"
    (format 22 def);
  check
    "the header stays whole down to its own width"
    ~expected:"def f(a, b): t {\n  x;\n  y;\n}"
    (format 16 def);
  check
    "below that the list opens, and the body fits on the line that closes it"
    ~expected:"def f(\n  a,\n  b\n): t { x; y }"
    (format 15 def);
  (* -- a body behind a rule that only holds it --

     24 columns, and 17 up to the brace. [Body] stands between the arm and
     its block. *)
  let arm = "case p(a, b) => { x; y }" in
  check
    "an arm's block opens and its pattern stays whole"
    ~expected:"case p(a, b) => {\n  x;\n  y;\n}"
    (format 23 arm);
  check
    "the pattern opens where the arm's head does not fit"
    ~expected:"case p(\n  a,\n  b\n) => { x; y }"
    (format 16 arm);
  (* The same with a body whose group is a plain one. 28 columns, and 17 up to
     the bracket. *)
  let tuple = "case p(a, b) => (xx, yy, zz)" in
  check
    "an arm's bracketed list opens and its pattern stays whole"
    ~expected:"case p(a, b) => (\n  xx,\n  yy,\n  zz\n)"
    (format 27 tuple);
  (* -- two bodies --

     A rule yields from its first body. 46 columns in all, 22 up to the first
     brace and 42 up to the second. At 41 the line cannot run to the second
     brace, and at 22 it still reaches the first. *)
  let chain = "case p(a, b) => if c { xx; yy; zz } else { w }" in
  check
    "a long first body opens, and the pattern in front of it stays whole"
    ~expected:"case p(a, b) => if c {\n  xx;\n  yy;\n  zz;\n} else { w }"
    (format 41 chain);
  check
    "down to the width of the line that ends at the first brace"
    ~expected:"case p(a, b) => if c {\n  xx;\n  yy;\n  zz;\n} else { w }"
    (format 22 chain);
  check
    "and one column short of it the pattern opens"
    ~expected:"case p(\n  a,\n  b\n) => if c {\n  xx;\n  yy;\n  zz;\n} else { w }"
    (format 21 chain);
  (* The last body opens first where the first one fits: 26 columns in all and
     17 up to the second brace. *)
  let last = "if c { x } else { yy; zz }" in
  check
    "of two bodies on a line the last opens first"
    ~expected:"if c { x } else {\n  yy;\n  zz;\n}"
    (format 25 last);
  (* -- what follows the body in its rule --

     A rule that yields goes on yielding past its body, whether tokens follow
     it or a line of its own does. 37 columns with the loop, and 34 on the
     first line with the handler. *)
  let loop = "case p(a, b) => do { xx; yy } while c" in
  check
    "a body with tokens after it opens in front of them"
    ~expected:"case p(a, b) => do {\n  xx;\n  yy;\n} while c"
    (format 36 loop);
  let handled = "case p(a, b) => try { xx; yy; zz }\ncatch { w }" in
  check
    "a body with a hard break after it lies flat"
    ~expected:handled
    (format 34 handled);
  check
    "and opens where its line does not fit"
    ~expected:"case p(a, b) => try {\n  xx;\n  yy;\n  zz;\n}\ncatch { w }"
    (format 33 handled);
  (* -- a separated list --

     It has no opener to end the line on, so it counts flat and the list in
     front of it opens. 26 columns. *)
  let bare = "case p(a, b) => xx, yy, zz" in
  check
    "a separated list is counted flat"
    ~expected:"case p(\n  a,\n  b\n) => xx, yy, zz"
    (format 25 bare);
  (* -- a gap that may break, in front of a body --

     The group that settles a gap holds the child after it, so the gap is
     measured with the body flat and opens before the body does. 18 columns. *)
  let value = "val r = { xx; yy }" in
  check
    "a gap that may break opens before the body behind it"
    ~expected:"val r =\n  { xx; yy }"
    (format 17 value);
  Law.summarise "yields"
;;
