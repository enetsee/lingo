(* -- Rust ---------------------------------------------------------------------

   The first grammar here big enough that a corpus does not reach all of it by
   accident: twenty-five productions, seven binding powers, four postfix
   operators.

   What it carries that the smaller grammars do not:

   - [eq] is right-associative at the bottom and [eqeq] left at the level
     above, so one operator table holds both climbs;
   - the four postfix operators share one binding power above the infix
     operators, so the loop leaves a ceiling behind on some steps and not on
     others;
   - [Block] is an atom of the expression block and the body of a function, so
     the rule graph recurses through the block rather than around it;
   - [Fn] and [Block] take [Lookahead 3], which nothing else here does.

   The line comment is [Preserve] trivia, so the sampler draws comments into a
   grammar with a deep tree. grammars/comments_grammar.ml has them in a flat
   one, and the two say different things about where a comment lands.

   Source it parses:

   {v
     struct Point { x: int, y: int }

     enum Color { Red, Rgb(int, int, int), Named { name: int } }

     trait Drawable { fn area() -> int; }

     fn main() -> int {
       let x = 1 + 2 * 3;
       let p = compute(1).field?;
       match x { 0 => 1, n => n * 2 };
     }
   v}
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let ident =
    Redfa.Regex.(
      seq
        (one_of_char ~ranges:[ 'a', 'z'; 'A', 'Z' ] ~singles:[ '_' ] ())
        (star (one_of_char ~ranges:[ 'a', 'z'; 'A', 'Z'; '0', '9' ] ~singles:[ '_' ] ())))
  in
  let line = Redfa.Regex.(seq (str "//") (star (not_singleton_char '\n'))) in
  let tokens =
    [ kw "struct"
    ; kw "enum"
    ; kw "trait"
    ; kw "fn"
    ; kw "let"
    ; kw "match"
    ; punct_tight ~name:"lbrace" "{"
    ; punct_tight ~name:"rbrace" "}"
    ; punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct_tight ~name:"lbracket" "["
    ; punct_tight ~name:"rbracket" "]"
    ; punct ~space_before:false ~name:"comma" ","
    ; punct ~space_before:false ~name:"semi" ";"
    ; punct ~space_before:false ~name:"colon" ":"
    ; punct_tight ~name:"dot" "."
    ; punct_tight ~name:"question" "?"
    ; punct ~name:"arrow" "->"
    ; punct ~name:"fat_arrow" "=>"
    ; punct ~name:"eq" "="
    ; punct ~name:"plus" "+"
    ; punct ~name:"minus" "-"
    ; punct ~name:"star" "*"
    ; punct ~name:"slash" "/"
    ; punct ~name:"eqeq" "=="
    ; punct ~name:"bangeq" "!="
    ; punct ~space_after:false ~name:"bang" "!"
    ; pat "ident" ident
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
      (* The comment runs to the end of its line and stops there. The break
         after it belongs to the document, and a newline welded in here would
         be bytes inside a text node that the column model cannot see. *)
    ; pat ~trivia:Preserve "line" line
    ]
  in
  let file =
    prod
      ~break_style:Always
      ~separator_lines:2
      ~indent_width:0
      "File"
      [ child_rep "item" (Rule "Item") ]
  in
  let item =
    prod
      "Item"
      [ child_alt_rules ~modifier:Exactly_one "kind" [ "Struct"; "Enum"; "Trait"; "Fn" ] ]
  in
  (* -- struct -- *)
  let type_ = prod "Type" [ child_req "name" (Token "ident") ] |> with_no_hole in
  let field =
    prod
      "Field"
      [ child_req "name" (Token "ident")
      ; child_req "colon" (Token "colon")
      ; child_req "ty" (Rule "Type")
      ]
  in
  let struct_body =
    prod "StructBody" [ child_rep "field" (Rule "Field") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let struct_ =
    prod
      "Struct"
      [ child_req "kw" (Token "struct")
      ; child_req "name" (Token "ident")
      ; child_req "body" (Rule "StructBody")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_messages [ "name", "expected a struct name" ]
  in
  (* -- enum --

     A variant's payload is optional and starts with [(] or [{]; what follows a
     variant in the body is [,] or [}]. The two sets are disjoint, so the
     optional child needs no [greedy]. *)
  let tuple_payload =
    prod "TuplePayload" [ child_rep "ty" (Rule "Type") ]
    |> with_delimited_sep ~open_tok:"lparen" ~close_tok:"rparen" ~sep:"comma"
  in
  let struct_payload =
    prod "StructPayload" [ child_rep "field" (Rule "Field") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let variant_payload =
    prod
      "VariantPayload"
      [ child_alt_rules ~modifier:Exactly_one "shape" [ "TuplePayload"; "StructPayload" ]
      ]
  in
  let variant =
    prod
      "Variant"
      [ child_req "name" (Token "ident"); child_opt "payload" (Rule "VariantPayload") ]
  in
  let enum_body =
    prod "EnumBody" [ child_rep "variant" (Rule "Variant") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let enum_ =
    prod
      "Enum"
      [ child_req "kw" (Token "enum")
      ; child_req "name" (Token "ident")
      ; child_req "body" (Rule "EnumBody")
      ]
    |> with_committed
    |> with_identity "name"
  in
  (* -- trait -- *)
  let param =
    prod
      "Param"
      [ child_req "name" (Token "ident")
      ; child_req "colon" (Token "colon")
      ; child_req "ty" (Rule "Type")
      ]
  in
  let param_list =
    prod "ParamList" [ child_rep "param" (Rule "Param") ]
    |> with_delimited_sep
         ~open_tok:"lparen"
         ~close_tok:"rparen"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let method_sig =
    prod
      "MethodSig"
      [ child_req "kw" (Token "fn")
      ; child_req "name" (Token "ident")
      ; child_req "params" (Rule "ParamList")
      ; child_req "arrow" (Token "arrow")
      ; child_req "ret_ty" (Rule "Type")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
  in
  let trait_body =
    prod ~break_style:Always "TraitBody" [ child_rep "method_" (Rule "MethodSig") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
  in
  let trait =
    prod
      "Trait"
      [ child_req "kw" (Token "trait")
      ; child_req "name" (Token "ident")
      ; child_req "body" (Rule "TraitBody")
      ]
    |> with_committed
    |> with_identity "name"
  in
  (* -- fn --

     [Lookahead 3] is here and on [Block] and on nothing else in grammars/. A
     stray token between a name and its parameter list is skipped into an error
     node rather than unwinding the whole item. *)
  let fn_ =
    prod
      "Fn"
      [ child_req "kw" (Token "fn")
      ; child_req "name" (Token "ident")
      ; child_req "params" (Rule "ParamList")
      ; child_req "arrow" (Token "arrow")
      ; child_req "ret_ty" (Rule "Type")
      ; child_req "body" (Rule "Block")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_recovery_strategy (Lookahead (lookahead_n 3))
    |> with_messages
         [ "name", "expected a function name after `fn`"
         ; "params", "expected a parameter list"
         ; "ret_ty", "expected a return type after `->`"
         ; "body", "expected a function body"
         ]
  in
  (* -- statements and blocks -- *)
  let let_stmt =
    prod
      "Let"
      [ child_req "kw" (Token "let")
      ; child_req "name" (Token "ident")
      ; child_req "eq" (Token "eq")
      ; child_req "value" (Rule "Expr")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_messages [ "value", "expected an expression after `=`" ]
  in
  let expr_stmt =
    prod "ExprStmt" [ child_req "expr" (Rule "Expr"); child_req "semi" (Token "semi") ]
  in
  let stmt =
    prod "Stmt" [ child_alt_rules ~modifier:Exactly_one "kind" [ "Let"; "ExprStmt" ] ]
  in
  let block =
    prod ~break_style:Always ~indent_width:2 "Block" [ child_rep "stmt" (Rule "Stmt") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
    |> with_recovery_strategy (Lookahead (lookahead_n 3))
  in
  (* -- match --

     The arm's pattern is a token alternative, so the block's own atoms are not
     what decides it. No node can stand in that slot, which makes it the one
     place a mutator has to build a token to reach an arm. *)
  let match_arm =
    prod
      "MatchArm"
      [ child_alt ~modifier:Exactly_one "pat" [ Token "ident"; Token "int" ]
      ; child_req "fat_arrow" (Token "fat_arrow")
      ; child_req "body" (Rule "Expr")
      ]
  in
  let match_body =
    prod ~break_style:Always "MatchBody" [ child_rep "arm" (Rule "MatchArm") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let match_ =
    prod
      "Match"
      [ child_req "kw" (Token "match")
      ; child_req "scrutinee" (Rule "Expr")
      ; child_req "body" (Rule "MatchBody")
      ]
    |> with_committed
  in
  (* -- expressions --

     [eq] binds loosest and to the right, so [a = b = c] is [a = (b = c)]. The
     comparisons sit above it to the left and the arithmetic above those. The
     four postfix operators share one binding power well over all of them. *)
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "int"; Token "ident"; Rule "Block"; Rule "Match" ]
      ~prefix_ops:[ prefix ~token:"bang" ~bp:60 () ]
      ~infix_ops:
        [ infix ~assoc:Right ~token:"eq" ~bp:1 ()
        ; infix ~token:"eqeq" ~bp:5 ()
        ; infix ~token:"bangeq" ~bp:5 ()
        ; infix ~token:"plus" ~bp:10 ()
        ; infix ~token:"minus" ~bp:10 ()
        ; infix ~token:"star" ~bp:20 ()
        ; infix ~token:"slash" ~bp:20 ()
        ]
      ~postfix:
        [ postfix_simple ~kind_suffix:"try" ~token:"question" ~bp:100 ()
        ; postfix_access ~kind_suffix:"field" ~token:"dot" ~rhs:(Token "ident") ~bp:100 ()
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
        ]
      ~infix_recovery:true
      ~operator_position:Op_after
      ~continuation_indent:2
      ()
  in
  create
    ~expr:[ expr ]
    ~tokens
    ~roots:[ "File" ]
    [ file
    ; item
    ; struct_
    ; struct_body
    ; field
    ; enum_
    ; enum_body
    ; variant
    ; variant_payload
    ; tuple_payload
    ; struct_payload
    ; trait
    ; trait_body
    ; method_sig
    ; fn_
    ; param_list
    ; param
    ; type_
    ; block
    ; stmt
    ; let_stmt
    ; expr_stmt
    ; match_
    ; match_body
    ; match_arm
    ]
;;
