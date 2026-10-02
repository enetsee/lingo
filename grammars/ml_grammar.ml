(* -- a small ML ---------------------------------------------------------------

   Written for the features the other twelve leave at zero. A probe that reads
   the corpus as values, rather than as source, found seven settings no grammar
   in grammars/ ever reached, so every law over the corpus was silent about
   them. Each one here sits on the construct that wants it in a real language:

   - two expression blocks. [Type] climbs [->] and [*], [Expr] climbs the
     arithmetic and the comparisons. A language with types and terms has two
     precedence tables, and they are not the same table;
   - [Op_before] and a continuation indent of four, on [Type]. That is how an
     ML signature breaks a long arrow chain, with the arrow leading the line;
   - [break_style Never] on [Import]. A dotted path stays on its line;
   - a trailing block, [f { s; s; }], which is an [Enclosed] postfix whose
     elements carry their own [;] and so take no separator;
   - two roots, [File] and [Sig]. The second is the signature file, and
     [TypeDecl] belongs to both;
   - [~textmate] and [~treesitter] on the comment. Its body excludes the
     closing delimiter and the newline, which is an intersection with a
     complement, and neither backend's dialect has a form for one.

   It doubles up the settings that had one witness: [greedy] on the dangling
   [else], [recover_to] on a [let] body, [trailing_sep Always] on the list
   literal, [~space:false] on a labelled argument's value, resync anchors on
   the list.

   Source it parses:

   {v
     import std.list.core;

     type Handler = int -> string -> int;

     data Color {
       | Red
       | Rgb(int, int, int)
     }

     let map (f : int -> int) (xs : int) : int =
       f(~by: 1, xs) {
         xs.head;
         !xs;
       };
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
  (* The body is every string that holds neither the closing delimiter nor a
     newline. That is an intersection with a complement, which is what a DFA
     wants and what an Oniguruma engine has no term for, so the TextMate
     spelling is written out by hand. The newline is excluded so the token
     stays one lexeme on one line: a [Preserve] token that spans a break leaves
     the layout engine's column model a line short. *)
  let comment =
    Redfa.Regex.(
      seqs
        [ str "(*"
        ; inter
            (complement (seqs [ star any; str "*)"; star any ]))
            (star (not_singleton_char '\n'))
        ; str "*)"
        ])
  in
  let tokens =
    [ kw "import"
    ; kw "type"
    ; kw "data"
    ; kw "let"
    ; kw "val"
    ; kw "if"
    ; kw "then"
    ; kw "else"
    ; punct ~space_after:false ~name:"lbrace" "{"
    ; punct ~space_before:false ~name:"rbrace" "}"
    ; punct ~space_after:false ~name:"lparen" "("
    ; punct ~space_before:false ~name:"rparen" ")"
    ; punct ~space_after:false ~name:"lbracket" "["
    ; punct ~space_before:false ~name:"rbracket" "]"
    ; punct_tight ~name:"dot" "."
    ; punct ~space_after:false ~name:"tilde" "~"
    ; punct ~space_after:false ~name:"bang" "!"
    ; punct ~space_before:false ~name:"comma" ","
    ; punct ~space_before:false ~name:"semi" ";"
    ; punct ~space_before:false ~name:"colon" ":"
    ; punct ~name:"arrow" "->"
    ; punct ~name:"eq" "="
    ; punct ~name:"bar" "|"
    ; punct ~name:"plus" "+"
    ; punct ~name:"minus" "-"
    ; punct ~name:"star" "*"
    ; punct ~name:"slash" "/"
    ; punct ~name:"eqeq" "=="
    ; punct ~name:"lt" "<"
    ; pat "ident" ident
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ; pat
        ~trivia:Preserve
        ~textmate:{|\(\*(?:(?!\*\))[^\n])*\*\)|}
        ~treesitter:{|\(\*(?:(?!\*\))[^\n])*\*\)|}
        "comment"
        comment
    ]
  in
  (* -- files -- *)
  let file =
    prod
      ~indent_width:0
      "File"
      [ child_rep ~break:(always 1) ~between:(always 2) "item" (Rule "Decl") ]
  in
  let decl =
    prod
      "Decl"
      [ child_alt_rules
          ~modifier:Exactly_one
          "kind"
          [ "Import"; "TypeDecl"; "DataDecl"; "LetDecl" ]
      ]
  in
  (* The signature file. [TypeDecl] is an item of both roots, so the two entry
     points share a subtree rather than sitting in separate halves of the rule
     graph. *)
  let sig_ =
    prod
      ~indent_width:0
      "Sig"
      [ child_rep ~break:(always 1) ~between:(always 1) "item" (Rule "SigItem") ]
  in
  let sig_item =
    prod
      "SigItem"
      [ child_alt_rules ~modifier:Exactly_one "kind" [ "TypeDecl"; "ValDecl" ] ]
  in
  let val_decl =
    prod
      "ValDecl"
      [ child_req "kw" (Token "val")
      ; child_req ~break:Never "name" (Token "ident")
      ; child_req ~break:Never "colon" (Token "colon")
      ; child_req ~break:Never "ty" (Rule "Type")
      ; child_req ~break:Never "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_messages [ "ty", "expected a type after `:`" ]
  in
  (* -- import --

     [Never] keeps the path on one line however long it runs. A break inside a
     dotted path reads as two paths. *)
  let path_seg =
    prod
      "PathSeg"
      [ child_req "dot" (Token "dot"); child_req ~break:Never "name" (Token "ident") ]
    |> with_no_hole
  in
  let path =
    prod
      "Path"
      [ child_req "head" (Token "ident")
      ; child_rep ~break:Never ~between:Never "rest" (Rule "PathSeg")
      ]
    |> with_no_hole
  in
  let import =
    prod
      "Import"
      [ child_req ~break:Never "kw" (Token "import")
      ; child_req ~break:Never "path" (Rule "Path")
      ; child_req ~break:Never "semi" (Token "semi")
      ]
    |> with_committed
  in
  (* -- types -- *)
  let type_group =
    prod "TypeGroup" [ child_req "ty" (Rule "Type") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  let type_decl =
    prod
      "TypeDecl"
      [ child_req "kw" (Token "type")
      ; child_req ~break:Never "name" (Token "ident")
      ; child_req ~break:Never "eq" (Token "eq")
        (* The one boundary that may break. A long type goes to its own
           line and [type Name =] stays together. *)
      ; child_req "ty" (Rule "Type")
      ; child_req ~break:Never "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_messages
         [ "name", "expected a type name after `type`"
         ; "ty", "expected a type after `=`"
         ]
  in
  (* -- data --

     A constructor leads with [|] and ends where the next one starts, so the
     body takes no separator. *)
  let ctor_payload =
    prod "CtorPayload" [ child_rep "ty" (Rule "Type") ]
    |> with_delimited_sep ~open_tok:"lparen" ~close_tok:"rparen" ~sep:"comma"
  in
  let ctor =
    prod
      "Ctor"
      [ child_req "bar" (Token "bar")
      ; child_req ~break:Never "name" (Token "ident")
      ; child_opt ~break:Never ~space:false "payload" (Rule "CtorPayload")
      ]
    |> with_binder "name"
  in
  let data_body =
    prod
      "DataBody"
      [ child_rep ~break:(always 1) ~between:(always 1) "ctor" (Rule "Ctor") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
  in
  let data_decl =
    (* No boundary here breaks, so an indent of its own would only push the
       body in twice: a nest reaches every break below it, not just the ones
       at this production's own boundaries. *)
    prod
      ~indent_width:0
      "DataDecl"
      [ child_req "kw" (Token "data")
      ; child_req ~break:Never "name" (Token "ident")
      ; child_req ~break:Never "body" (Rule "DataBody")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  (* -- let --

     [recover_to] on the body stops a broken expression at the [;] rather than
     letting it run into the next declaration. The parameters are a bare loop:
     one starts with [(] or [~] and the loop stops at [:], so nothing needs a
     lookahead to end it. *)
  let plain_param =
    prod
      "PlainParam"
      [ child_req "lparen" (Token "lparen")
      ; child_req ~break:Never "name" (Token "ident")
      ; child_req ~break:Never "colon" (Token "colon")
      ; child_req ~break:Never "ty" (Rule "Type")
      ; child_req ~break:Never "rparen" (Token "rparen")
      ]
    |> with_binder "name"
  in
  let labelled_param =
    prod
      "LabelledParam"
      [ child_req "tilde" (Token "tilde"); child_req ~break:Never "name" (Token "ident") ]
    |> with_binder "name"
  in
  let param =
    prod
      "Param"
      [ child_alt_rules ~modifier:Exactly_one "kind" [ "PlainParam"; "LabelledParam" ] ]
  in
  let let_decl =
    prod
      "LetDecl"
      (* The header is a space throughout. Only the boundary in front of the
         body may break, so a long body does not scatter [let], the name and
         the type over six lines. *)
      [ child_req "kw" (Token "let")
      ; child_req ~break:Never "name" (Token "ident")
      ; child_rep ~break:Never ~between:Never "param" (Rule "Param")
      ; child_req ~break:Never "colon" (Token "colon")
      ; child_req ~break:Never "ty" (Rule "Type")
      ; child_req ~break:Never "eq" (Token "eq")
      ; child ~recover_to:[ "semi" ] ~modifier:Exactly_one "body" (Rule "Expr")
      ; child_req ~break:Never "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
    |> with_recovery_strategy (Lookahead (lookahead_n 3))
    |> with_messages
         [ "name", "expected a name after `let`"
         ; "ty", "expected a type after `:`"
         ; "body", "expected an expression after `=`"
         ]
  in
  (* -- expressions --

     [List] carries the resync anchors. A missing [\]] would otherwise let the
     element loop swallow the rest of the declaration; neither [;] nor [}] can
     start an expression, so both end the loop and are left to whatever
     encloses it. *)
  let list_ =
    prod "List" [ child_rep "elem" (Rule "Expr") ]
    |> with_delimited_sep
         ~open_tok:"lbracket"
         ~close_tok:"rbracket"
         ~sep:"comma"
         ~trailing_sep:Always
    |> with_resync_to [ "semi"; "rbrace" ]
  in
  (* The dangling [else]. [greedy] attaches it to the nearer [if]. *)
  let else_ =
    prod "Else" [ child_req "kw" (Token "else"); child_req "e" (Rule "Expr") ]
  in
  let if_ =
    prod
      "If"
      [ child_req "kw" (Token "if")
      ; child_req "cond" (Rule "Expr")
      ; child_req "then_kw" (Token "then")
      ; child_req "t" (Rule "Expr")
      ; child_opt ~greedy:true "else_" (Rule "Else")
      ]
    |> with_committed
  in
  let stmt =
    prod
      "Stmt"
      [ child_req "e" (Rule "Expr"); child_req ~break:Never "semi" (Token "semi") ]
  in
  let label =
    prod
      "Label"
      [ child_req "tilde" (Token "tilde")
      ; child_req ~break:Never "name" (Token "ident")
      ; child_req ~break:Never "colon" (Token "colon")
      ]
    |> with_no_hole
  in
  (* [~x:] hugs the value it labels. [colon] leaves a space after it, which is
     right everywhere else it appears. *)
  let arg =
    prod
      "Arg"
      [ child_opt "label" (Rule "Label"); child_req ~space:false "value" (Rule "Expr") ]
  in
  let type_expr =
    expr_block
      ~rule_name:"Type"
      ~atoms:[ Token "ident"; Rule "TypeGroup" ]
      ~infix_ops:
        [ infix ~assoc:Right ~token:"arrow" ~bp:1 (); infix ~token:"star" ~bp:10 () ]
      ~operator_position:Op_before
      ~continuation_indent:4
      ()
  in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "int"; Token "ident"; Rule "List"; Rule "If" ]
      ~prefix_ops:[ prefix ~token:"bang" ~bp:60 () ]
      ~infix_ops:
        [ infix ~token:"eqeq" ~bp:5 ()
        ; infix ~token:"lt" ~bp:5 ()
        ; infix ~token:"plus" ~bp:10 ()
        ; infix ~token:"minus" ~bp:10 ()
        ; infix ~token:"star" ~bp:20 ()
        ; infix ~token:"slash" ~bp:20 ()
        ]
      ~postfix:
        [ postfix_access ~kind_suffix:"field" ~token:"dot" ~rhs:(Token "ident") ~bp:100 ()
        ; postfix_call
            ~kind_suffix:"call"
            ~open_tok:"lparen"
            ~close_tok:"rparen"
            ~elem:(Rule "Arg")
            ~sep_policy:(with_sep ~trailing:On_break "comma")
            ~bp:100
            ()
          (* The trailing block. Each statement ends in its own [;], so the
             loop takes no separator. *)
        ; postfix_call
            ~kind_suffix:"block"
            ~open_tok:"lbrace"
            ~close_tok:"rbrace"
            ~elem:(Rule "Stmt")
            ~bp:100
            ()
        ]
      ~infix_recovery:true
      ~operator_position:Op_after
      ~continuation_indent:2
      ()
  in
  create
    ~expr:[ type_expr; expr ]
    ~tokens
    ~roots:[ "File"; "Sig" ]
    [ file
    ; decl
    ; sig_
    ; sig_item
    ; val_decl
    ; import
    ; path
    ; path_seg
    ; type_decl
    ; type_group
    ; data_decl
    ; data_body
    ; ctor
    ; ctor_payload
    ; let_decl
    ; param
    ; plain_param
    ; labelled_param
    ; list_
    ; if_
    ; else_
    ; stmt
    ; label
    ; arg
    ]
;;
