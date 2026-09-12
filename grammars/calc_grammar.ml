(* -- a calculator -------------------------------------------------------------

   The smallest grammar with an expression block in it:

   - [Expr] is a Pratt block, so precedence is a binding power rather than a
     production per level;
   - [minus] is a prefix operator and an infix one, which the parser tells
     apart by where the token sits;
   - [Parens] is a delimited production used as an atom, so the block reaches
     back into ordinary productions.

   Every infix operator here is left-associative. The right-associative case
   is grammars/rassoc_grammar.ml.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let tokens =
    [ punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct ~name:"plus" "+"
    ; punct ~name:"minus" "-"
    ; punct ~name:"star" "*"
    ; punct ~name:"slash" "/"
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_req "expr" (Rule "Expr") ] in
  let parens =
    prod "Parens" [ child_req "inner" (Rule "Expr") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "int"; Rule "Parens" ]
      ~prefix_ops:[ prefix ~token:"minus" ~bp:50 () ]
      ~infix_ops:
        [ infix ~token:"plus" ~bp:10 ()
        ; infix ~token:"minus" ~bp:10 ()
        ; infix ~token:"star" ~bp:20 ()
        ; infix ~token:"slash" ~bp:20 ()
        ]
      ()
  in
  create ~expr:[ expr ] ~tokens ~roots:[ "File" ] [ file; parens ]
;;
