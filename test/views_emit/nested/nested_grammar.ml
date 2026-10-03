(* -- two shapes for the views alone ---------------------------------------------

   None of the grammars in grammars/ has either, and each is a branch of the
   views emitter that nothing else reaches:

   - [Expr] has rule atoms and no token atom, so its block has no base node.
     A position over it holds the infix role and the two atoms, and nothing
     wraps a token.
   - [Item]'s child is [Expr] or [Block], so its variant has an arm that holds
     a block's position rather than a node of its own.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let letter = Redfa.Regex.range_char ~lo:'a' ~hi:'z' in
  let tokens =
    [ punct ~space_after:Hug ~name:"lparen" "("
    ; punct ~space_before:Hug ~name:"rparen" ")"
    ; punct ~space_after:Hug ~name:"lbrace" "{"
    ; punct ~space_before:Hug ~name:"rbrace" "}"
    ; punct ~name:"plus" "+"
    ; punct ~space_before:Hug ~name:"semi" ";"
    ; pat "ident" (Redfa.Regex.plus letter)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_rep "items" (Rule "Item") ] in
  let item =
    prod
      "Item"
      [ child_alt ~modifier:Exactly_one "body" [ Rule "Expr"; Rule "Block" ]
      ; child_req "semi" (Token "semi")
      ]
  in
  let block =
    prod "Block" [ child_rep "items" (Rule "Item") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
  in
  let name = prod "Name" [ child_req "id" (Token "ident") ] in
  let paren =
    prod "Paren" [ child_req "inner" (Rule "Expr") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Rule "Name"; Rule "Paren" ]
      ~infix_ops:[ infix ~token:"plus" ~bp:10 () ]
      ()
  in
  create ~expr:[ expr ] ~tokens ~roots:[ "File" ] [ file; item; block; name; paren ]
;;

let good : string list = [ "a;"; "a + b;"; "(a + b) + c;"; "{ a; { b; } };"; "{ };" ]

let broken : string list =
  [ "a +;"; "+ b;"; "a b;"; "(;"; ";"; "{ ; }"; "{ a"; "a"; "a + (b;"; "} a;" ]
;;
