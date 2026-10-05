(* -- binding powers that tie ----------------------------------------------------

   A prefix and a postfix operator whose binding powers equal an infix
   operator's, for the parentheses a role constructor puts in. None of the
   grammars in grammars/ has a tie like these, and each one is where a
   comparison that is off by one gives a different answer:

   - prefix [-] binds its operand at 20, and [*] has a left binding power
     of 20. So [-a * b] is [-(a * b)], with no parentheses.
   - postfix [!] has binding power 30, and [^] binds its right operand at
     30. So [(a ^ b)!] keeps its parentheses, and [a ^ b!] is [a ^ (b!)].
   - postfix [?] has binding power 21, and [*] binds its right operand at
     21. So [(a * b)?] keeps its parentheses too.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let tokens =
    [ punct ~space_after:Hug ~name:"lparen" "("
    ; punct ~space_before:Hug ~name:"rparen" ")"
    ; punct ~name:"plus" "+"
    ; punct ~name:"star" "*"
    ; punct ~name:"caret" "^"
    ; punct ~space_after:Hug ~name:"minus" "-"
    ; punct ~space_before:Hug ~name:"bang" "!"
    ; punct ~space_before:Hug ~name:"question" "?"
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_req "expr" (Rule "Expr") ] in
  let paren =
    prod "Paren" [ child_req "inner" (Rule "Expr") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "int"; Rule "Paren" ]
      ~prefix_ops:[ prefix ~assoc:Right ~token:"minus" ~bp:20 () ]
      ~infix_ops:
        [ infix ~token:"plus" ~bp:10 ()
        ; infix ~token:"star" ~bp:20 ()
        ; infix ~assoc:Right ~token:"caret" ~bp:30 ()
        ]
      ~postfix:
        [ postfix_simple ~kind_suffix:"bang" ~token:"bang" ~bp:30 ()
        ; postfix_simple ~kind_suffix:"question" ~token:"question" ~bp:21 ()
        ]
      ()
  in
  create ~expr:[ expr ] ~tokens ~roots:[ "File" ] [ file; paren ]
;;
