(* -- postfix operators --------------------------------------------------------

   The five postfix forms, and nothing else that could fail. Every other
   grammar here leaves them out, so the whole postfix half of a Pratt parse
   goes unexercised without this one.

   {v
     x?        postfix_simple   the lead token is the whole operator
     x.f       postfix_access   a child follows the lead
     x[i]      postfix_index    a matched pair with one child inside
     x{ i }    postfix_brace    the same shape under a different pair
     x(a, b)   postfix_call     a matched pair with a separated list inside
   v}

   [postfix_access] takes a rule rather than a token, because the lowering
   keeps only the child's FIRST set where a token would do and needs the rule
   id where it would not. A grammar that reaches [x.f] with a token alone
   cannot tell the two apart.

   One left-associative infix operator sits beside them, so a postfix binding
   tighter than an infix is observable. There is no prefix operator and no
   right-associative one: grammars/calc_grammar.ml and
   grammars/rassoc_grammar.ml carry those, and a failure here should have one
   cause.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let letter = Redfa.Regex.range_char ~lo:'a' ~hi:'z' in
  let tokens =
    [ punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct_tight ~name:"lbracket" "["
    ; punct_tight ~name:"rbracket" "]"
    ; punct_tight ~name:"lbrace" "{"
    ; punct_tight ~name:"rbrace" "}"
    ; punct_tight ~name:"dot" "."
    ; punct_tight ~name:"comma" ","
    ; punct_tight ~name:"question" "?"
    ; punct ~name:"plus" "+"
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat "name" (Redfa.Regex.plus letter)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_req "expr" (Rule "Expr") ] in
  (* The right-hand side of [x.f] is a production, so the plan has to carry
     what to run there and not only what to dispatch on. *)
  let field = prod "Field" [ child_req "name" (Token "name") ] in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "int"; Token "name" ]
      ~infix_ops:[ infix ~token:"plus" ~bp:10 () ]
      ~postfix:
        [ postfix_simple ~kind_suffix:"try" ~token:"question" ~bp:90 ()
        ; postfix_access ~kind_suffix:"field" ~token:"dot" ~rhs:(Rule "Field") ~bp:90 ()
        ; postfix_index
            ~kind_suffix:"index"
            ~open_tok:"lbracket"
            ~close_tok:"rbracket"
            ~index:(Rule "Expr")
            ~bp:90
            ()
        ; postfix_brace
            ~kind_suffix:"block"
            ~open_tok:"lbrace"
            ~close_tok:"rbrace"
            ~body:(Rule "Expr")
            ~bp:90
            ()
        ; postfix_call
            ~kind_suffix:"call"
            ~open_tok:"lparen"
            ~close_tok:"rparen"
            ~elem:(Rule "Expr")
            ~sep_policy:(with_sep "comma")
            ~bp:90
            ()
        ]
      ()
  in
  create ~expr:[ expr ] ~tokens ~roots:[ "File" ] [ file; field ]
;;
