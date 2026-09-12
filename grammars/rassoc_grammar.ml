(* -- right associativity ------------------------------------------------------

   This grammar exists for one threshold. An associativity is a pair of
   binding powers: [Left] gives [(bp, bp + 1)] and [Right] gives [(bp, bp)].
   A parser tells them apart with [left_bp >= min_bp], and that test only
   changes its answer where an operator is right-associative.

   [caret] is right-associative, so [a ^ b ^ c] groups to the right. [plus]
   is left-associative beside it, so one grammar carries both answers.

   Keep it minimal. Every other grammar with a right-associative operator
   also has postfix operators, and then a failure has two possible causes.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let tokens =
    [ punct ~name:"plus" "+"
    ; punct ~name:"caret" "^"
    ; punct ~name:"minus" "-"
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_req "expr" (Rule "Expr") ] in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "int" ]
      ~prefix_ops:[ prefix ~token:"minus" ~bp:50 () ]
      ~infix_ops:
        [ infix ~token:"plus" ~bp:10 (); infix ~assoc:Right ~token:"caret" ~bp:30 () ]
      ()
  in
  create ~expr:[ expr ] ~tokens ~roots:[ "File" ] [ file ]
;;
