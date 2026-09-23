(* -- Effekt -------------------------------------------------------------------

   Trivia and running to the end of a line usually coincide. In every other
   grammar here one bit covers both. [doc_comment] splits them: it runs to the
   end of its line and it is meaningful. A production names it, the parse
   keeps it, and the formatter may not touch its bytes. A predicate that read
   one of those properties off the other would break on this grammar alone.

   The [///] against [//] split falls out of longest match. [line] is [//]
   followed by a character other than a slash, or [//] alone, so [///x] lexes
   as one doc comment.

   What else it carries:

   - nine definition forms under one dispatch, each committed on its own
     keyword;
   - types stratified by hand, [Type] over [BoxedType] over [AtomicType], so
     the corpus covers a precedence climb the Pratt desugaring never built;
   - [postfix_brace], the trailing-block form, which nothing else here
     declares;
   - [<{] and [}>] as a matched pair of two-character delimiters. [}>] is also
     a [}] beside a [>], so a space has to sit between them for the output to
     read back, and the lexer settles that boundary;
   - [and] as an infix operator and as the keyword joining guards, so one token
     is read two ways in two places.

   Source it parses:

   {v
     module tree;

     /// A labelled node.
     record Node(label: Int, kids: List)

     interface Emit { def emit(x: Int): Unit }

     def walk(t: Node) = {
       val n = t.label + 1;
       try { do emit(n) } with Emit { def emit(x) = resume(unit) };
       walk(t) { x => x };
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
  let string_ =
    Redfa.Regex.(
      seqs
        [ singleton_char '"'
        ; star
            (alt
               (not_chars (Ucharset.of_char_list [ '"'; '\\' ]))
               (seq (singleton_char '\\') any))
        ; singleton_char '"'
        ])
  in
  (* Three slashes and then the rest of the line. A meaningful token, so the
     formatter may not reshape its bytes. The line break after it belongs to
     the document. *)
  let doc = Redfa.Regex.(seq (str "///") (star (not_singleton_char '\n'))) in
  (* [//] and then a character other than a slash, or [//] on its own. The
     first alternative keeps [///] out of this token's language. *)
  let line =
    Redfa.Regex.(
      alt
        (seqs
           [ str "//"
           ; not_chars (Ucharset.of_list [ Char.code '/'; Char.code '\n' ])
           ; star (not_singleton_char '\n')
           ])
        (str "//"))
  in
  let block =
    Redfa.Regex.(
      seqs
        [ str "/*"
        ; star
            (alt
               (not_singleton_char '*')
               (seq
                  (plus (singleton_char '*'))
                  (not_chars (Ucharset.of_char_list [ '*'; '/' ]))))
        ; plus (singleton_char '*')
        ; singleton_char '/'
        ])
  in
  let tokens =
    [ kw "module"
    ; kw "import"
    ; kw "def"
    ; kw "val"
    ; kw "var"
    ; kw "type"
    ; kw "record"
    ; kw "interface"
    ; kw "effect"
    ; kw "namespace"
    ; kw "if"
    ; kw "else"
    ; kw "while"
    ; kw "try"
    ; kw "with"
    ; kw "case"
    ; kw "match"
    ; kw "do"
    ; kw "return"
    ; kw "box"
    ; kw "unbox"
    ; kw "fn"
    ; kw "at"
    ; kw "and"
    ; kw "resume"
    ; kw "true"
    ; kw "false"
    ; punct_tight ~name:"lbrace" "{"
    ; punct_tight ~name:"rbrace" "}"
    ; punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct_tight ~name:"lbracket" "["
    ; punct_tight ~name:"rbracket" "]"
      (* A matched pair of two-character delimiters. [<{] is also a [<] beside
         a [{], and [}>] a [}] beside a [>], so longest match in the lexer
         settles both edges. *)
    ; punct_tight ~name:"lhole" "<{"
    ; punct_tight ~name:"rhole" "}>"
    ; punct ~space_before:false ~name:"comma" ","
    ; punct ~space_before:false ~name:"semi" ";"
    ; punct ~space_before:false ~name:"colon" ":"
    ; punct_tight ~name:"dot" "."
    ; punct_tight ~name:"coloncolon" "::"
    ; punct ~name:"arrow" "->"
    ; punct ~name:"fat_arrow" "=>"
    ; punct ~name:"eq" "="
    ; punct ~name:"eqeq" "=="
    ; punct ~name:"bangeq" "!="
    ; punct ~name:"lt" "<"
    ; punct ~name:"gt" ">"
    ; punct ~name:"le" "<="
    ; punct ~name:"ge" ">="
    ; punct ~name:"plus" "+"
    ; punct ~name:"minus" "-"
    ; punct ~name:"star" "*"
    ; punct ~name:"slash" "/"
    ; punct ~name:"pipe" "|"
    ; punct ~space_after:false ~name:"bang" "!"
    ; pat "ident" ident
    ; pat "int" (Redfa.Regex.plus digit)
    ; pat "string" string_
    ; pat "doc_comment" doc
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
      (* The [_comment] suffix keeps these clear of the production called
         [Block]. A tree-sitter query matches on node names, and a rule and a
         token share one namespace. *)
    ; pat ~trivia:Preserve "line_comment" line
      (* [/*], then everything up to the first [*/]. *)
    ; pat ~trivia:Preserve "block_comment" block
    ]
  in
  (* -- the file and its items -- *)
  let file =
    prod
      ~break_style:Always
      ~separator_lines:2
      ~indent_width:0
      "File"
      [ child_opt "header" (Rule "Module"); child_rep "item" (Rule "Item") ]
  in
  let module_ =
    prod
      "Module"
      [ child_req "kw" (Token "module")
      ; child_req "path" (Rule "Path")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "path"
  in
  (* A definition may carry doc comment lines in front of it. They sit in
     their own production, so the definition's first child is that whole
     block. *)
  let doc_block = prod "DocBlock" [ child_rep1 "line" (Token "doc_comment") ] in
  let item =
    prod "Item" [ child_opt "doc" (Rule "DocBlock"); child_req "def" (Rule "Definition") ]
  in
  let definition =
    prod
      "Definition"
      [ child_alt_rules
          ~modifier:Exactly_one
          "kind"
          [ "Import"
          ; "Def"
          ; "ValDef"
          ; "VarDef"
          ; "TypeDef"
          ; "Record"
          ; "Interface"
          ; "Effect"
          ; "Namespace"
          ]
      ]
  in
  let path =
    prod
      "Path"
      [ child_req "first" (Token "ident"); child_rep "rest" (Rule "PathSegment") ]
  in
  let path_segment =
    prod
      "PathSegment"
      [ child_req "sep" (Token "coloncolon"); child_req "name" (Token "ident") ]
  in
  let import =
    prod
      "Import"
      [ child_req "kw" (Token "import")
      ; child_req "path" (Rule "Path")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "path"
  in
  (* -- definitions -- *)
  let param =
    prod "Param" [ child_req "name" (Token "ident"); child_opt "ann" (Rule "TypeAnn") ]
    |> with_binder "name"
  in
  let type_ann =
    prod "TypeAnn" [ child_req "colon" (Token "colon"); child_req "ty" (Rule "Type") ]
  in
  let params =
    prod "Params" [ child_rep "param" (Rule "Param") ]
    |> with_delimited_sep
         ~open_tok:"lparen"
         ~close_tok:"rparen"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let def =
    prod
      "Def"
      [ child_req "kw" (Token "def")
      ; child_req "name" (Token "ident")
      ; child_opt "params" (Rule "Params")
      ; child_opt "ret" (Rule "TypeAnn")
      ; child_req "eq" (Token "eq")
      ; child_req "body" (Rule "Expr")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
    |> with_messages [ "body", "expected a definition body after `=`" ]
  in
  let val_def =
    prod
      "ValDef"
      [ child_req "kw" (Token "val")
      ; child_req "name" (Token "ident")
      ; child_opt "ann" (Rule "TypeAnn")
      ; child_req "eq" (Token "eq")
      ; child_req "value" (Rule "Expr")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
  in
  let var_def =
    prod
      "VarDef"
      [ child_req "kw" (Token "var")
      ; child_req "name" (Token "ident")
      ; child_opt "ann" (Rule "TypeAnn")
      ; child_req "eq" (Token "eq")
      ; child_req "value" (Rule "Expr")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
  in
  let type_def =
    prod
      "TypeDef"
      [ child_req "kw" (Token "type")
      ; child_req "name" (Token "ident")
      ; child_req "eq" (Token "eq")
      ; child_req "ty" (Rule "Type")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
  in
  let record =
    prod
      "Record"
      [ child_req "kw" (Token "record")
      ; child_req "name" (Token "ident")
      ; child_req "fields" (Rule "Params")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  (* An interface body repeats an operation, and an operation carries its own
     doc comment. A meaningful comment sits here as well as at the top level,
     and here it sits inside a delimited body. *)
  let operation =
    prod
      "Operation"
      [ child_opt "doc" (Rule "DocBlock")
      ; child_req "kw" (Token "def")
      ; child_req "name" (Token "ident")
      ; child_req "params" (Rule "Params")
      ; child_opt "ret" (Rule "TypeAnn")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  let interface_body =
    prod ~break_style:Always "InterfaceBody" [ child_rep "op" (Rule "Operation") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
  in
  let interface =
    prod
      "Interface"
      [ child_req "kw" (Token "interface")
      ; child_req "name" (Token "ident")
      ; child_req "body" (Rule "InterfaceBody")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  let effect_ =
    prod
      "Effect"
      [ child_req "kw" (Token "effect")
      ; child_req "name" (Token "ident")
      ; child_req "params" (Rule "Params")
      ; child_opt "ret" (Rule "TypeAnn")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  let namespace_body =
    prod ~break_style:Always "NamespaceBody" [ child_rep "item" (Rule "Item") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
  in
  let namespace =
    prod
      "Namespace"
      [ child_req "kw" (Token "namespace")
      ; child_req "name" (Token "ident")
      ; child_req "body" (Rule "NamespaceBody")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  (* -- types, stratified by hand --

     [Type] is a boxed type, or a function arrow over one. [BoxedType] is an
     atomic type with an optional capture set after it. Three rules do the
     work one expression block would have desugared into. *)
  let type_rule =
    prod
      "Type"
      [ child_req "from" (Rule "BoxedType"); child_opt "to_" (Rule "TypeArrow") ]
  in
  let type_arrow =
    prod
      "TypeArrow"
      [ child_req "arrow" (Token "arrow"); child_req "result" (Rule "BoxedType") ]
  in
  let boxed_type =
    prod
      "BoxedType"
      [ child_req "base" (Rule "AtomicType"); child_opt "capture" (Rule "CaptureSet") ]
  in
  let capture_set =
    prod
      "CaptureSet"
      [ child_req "kw" (Token "at"); child_req "set" (Rule "CaptureList") ]
  in
  let capture_list =
    prod "CaptureList" [ child_rep "name" (Token "ident") ]
    |> with_delimited_sep ~open_tok:"lbrace" ~close_tok:"rbrace" ~sep:"comma"
  in
  let atomic_type =
    prod
      "AtomicType"
      [ child_req "name" (Token "ident"); child_opt "args" (Rule "TypeArgs") ]
  in
  let type_args =
    prod "TypeArgs" [ child_rep1 "arg" (Rule "Type") ]
    |> with_delimited_sep ~open_tok:"lbracket" ~close_tok:"rbracket" ~sep:"comma"
  in
  (* -- statements -- *)
  let block_body =
    prod ~break_style:Always ~indent_width:2 "Block" [ child_rep "stmt" (Rule "Stmt") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
    |> with_scope
    |> with_recovery_strategy (Lookahead (lookahead_n 3))
  in
  let stmt =
    prod
      "Stmt"
      [ child_alt_rules
          ~modifier:Exactly_one
          "kind"
          [ "ValDef"; "VarDef"; "Def"; "ExprStmt" ]
      ]
  in
  let expr_stmt =
    prod "ExprStmt" [ child_req "expr" (Rule "Expr"); child_req "semi" (Token "semi") ]
  in
  (* -- expression forms led by a keyword -- *)
  let if_ =
    prod
      "If"
      [ child_req "kw" (Token "if")
      ; child_req "cond" (Rule "Paren")
      ; child_req "then_" (Rule "Expr")
        (* The dangling else. [Else] is in the optional child's FIRST and in
           what may follow it, so the parse cannot separate the two and
           [greedy] settles it on the nearer [if]. *)
      ; child_opt ~greedy:true "else_" (Rule "Else")
      ]
    |> with_committed
  in
  let else_ =
    prod "Else" [ child_req "kw" (Token "else"); child_req "body" (Rule "Expr") ]
  in
  let while_ =
    prod
      "While"
      [ child_req "kw" (Token "while")
      ; child_req "cond" (Rule "Paren")
      ; child_req "body" (Rule "Expr")
      ]
    |> with_committed
  in
  let paren =
    prod "Paren" [ child_req "inner" (Rule "Expr") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  let do_ =
    prod
      "Do"
      [ child_req "kw" (Token "do")
      ; child_req "name" (Token "ident")
      ; child_req "args" (Rule "Args")
      ]
    |> with_committed
    |> with_identity "name"
  in
  let args =
    prod "Args" [ child_rep "arg" (Rule "Expr") ]
    |> with_delimited_sep
         ~open_tok:"lparen"
         ~close_tok:"rparen"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let return =
    prod "Return" [ child_req "kw" (Token "return"); child_req "value" (Rule "Expr") ]
    |> with_committed
  in
  let resume =
    prod "Resume" [ child_req "kw" (Token "resume"); child_req "args" (Rule "Args") ]
    |> with_committed
  in
  let box =
    prod "Box" [ child_req "kw" (Token "box"); child_req "value" (Rule "Expr") ]
    |> with_committed
  in
  let unbox =
    prod "Unbox" [ child_req "kw" (Token "unbox"); child_req "value" (Rule "Expr") ]
    |> with_committed
  in
  let fn_ =
    prod
      "Fn"
      [ child_req "kw" (Token "fn")
      ; child_req "params" (Rule "Params")
      ; child_req "arrow" (Token "fat_arrow")
      ; child_req "body" (Rule "Expr")
      ]
    |> with_committed
    |> with_scope
  in
  (* [try] takes a block and then one or more handlers, each naming an
     interface and holding operation definitions. *)
  let try_ =
    prod
      "Try"
      [ child_req "kw" (Token "try")
      ; child_req "body" (Rule "Block")
      ; child_rep1 "handler" (Rule "Handler")
      ]
    |> with_committed
  in
  let handler =
    prod
      "Handler"
      [ child_req "kw" (Token "with")
      ; child_req "name" (Token "ident")
      ; child_req "body" (Rule "HandlerBody")
      ]
    |> with_identity "name"
  in
  let handler_body =
    prod ~break_style:Always "HandlerBody" [ child_rep "clause" (Rule "Clause") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
  in
  let clause =
    prod
      "Clause"
      [ child_req "kw" (Token "def")
      ; child_req "name" (Token "ident")
      ; child_req "params" (Rule "Params")
      ; child_req "eq" (Token "eq")
      ; child_req "body" (Rule "Expr")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_scope
  in
  (* A match arm takes one or more patterns and an optional guard, so [and] is
     read here as well as in the operator table. *)
  let match_ =
    prod
      "Match"
      [ child_req "kw" (Token "match")
      ; child_req "scrutinee" (Rule "Paren")
      ; child_req "body" (Rule "MatchBody")
      ]
    |> with_committed
  in
  let match_body =
    prod ~break_style:Always "MatchBody" [ child_rep "arm" (Rule "MatchArm") ]
    |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
  in
  let match_arm =
    prod
      "MatchArm"
      [ child_req "kw" (Token "case")
      ; child_req "pat" (Rule "Pattern")
      ; child_rep "extra" (Rule "AltPattern")
      ; child_opt "guard" (Rule "Guard")
      ; child_req "arrow" (Token "fat_arrow")
      ; child_req "body" (Rule "Expr")
      ]
    |> with_committed
  in
  let alt_pattern =
    prod "AltPattern" [ child_req "bar" (Token "pipe"); child_req "pat" (Rule "Pattern") ]
  in
  let guard =
    prod "Guard" [ child_req "kw" (Token "and"); child_req "cond" (Rule "Expr") ]
  in
  let pattern =
    prod
      "Pattern"
      [ child_alt
          ~modifier:Exactly_one
          "head"
          [ Token "ident"; Token "int"; Token "string" ]
      ; child_opt "args" (Rule "PatternArgs")
      ]
  in
  let pattern_args =
    prod "PatternArgs" [ child_rep "arg" (Rule "Pattern") ]
    |> with_delimited_sep
         ~open_tok:"lparen"
         ~close_tok:"rparen"
         ~sep:"comma"
         ~trailing_sep:Never
  in
  (* The hole. [<{] and [}>] around statements. A formatter glues that pair
     without a bracket's spacing rules. *)
  let hole =
    prod "Hole" [ child_rep "stmt" (Rule "Stmt") ]
    |> with_delimited ~open_tok:"lhole" ~close_tok:"rhole"
  in
  (* -- expressions --

     [and] is the loosest infix and the comparisons sit above it, then the
     arithmetic. The postfix operators share one binding power above all of
     them, and [postfix_brace] is the trailing block: [f(x) { s; }]. *)
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:
        [ Token "int"
        ; Token "string"
        ; Token "ident"
        ; Token "true"
        ; Token "false"
        ; Rule "Block"
        ; Rule "Paren"
        ; Rule "Hole"
        ; Rule "If"
        ; Rule "While"
        ; Rule "Try"
        ; Rule "Match"
        ; Rule "Do"
        ; Rule "Return"
        ; Rule "Resume"
        ; Rule "Box"
        ; Rule "Unbox"
        ; Rule "Fn"
        ]
      ~prefix_ops:[ prefix ~token:"bang" ~bp:70 (); prefix ~token:"minus" ~bp:70 () ]
      ~infix_ops:
        [ infix ~token:"and" ~bp:5 ()
        ; infix ~token:"eqeq" ~bp:10 ()
        ; infix ~token:"bangeq" ~bp:10 ()
        ; infix ~token:"lt" ~bp:15 ()
        ; infix ~token:"gt" ~bp:15 ()
        ; infix ~token:"le" ~bp:15 ()
        ; infix ~token:"ge" ~bp:15 ()
        ; infix ~token:"plus" ~bp:20 ()
        ; infix ~token:"minus" ~bp:20 ()
        ; infix ~token:"star" ~bp:30 ()
        ; infix ~token:"slash" ~bp:30 ()
        ]
      ~postfix:
        [ postfix_access ~kind_suffix:"select" ~token:"dot" ~rhs:(Token "ident") ~bp:90 ()
        ; postfix_index
            ~kind_suffix:"typed"
            ~open_tok:"lbracket"
            ~close_tok:"rbracket"
            ~index:(Rule "Type")
            ~bp:90
            ()
        ; postfix_call
            ~kind_suffix:"call"
            ~open_tok:"lparen"
            ~close_tok:"rparen"
            ~elem:(Rule "Expr")
            ~sep_policy:(with_sep ~trailing:On_break "comma")
            ~bp:90
            ()
        ; postfix_brace
            ~kind_suffix:"block"
            ~open_tok:"lbrace"
            ~close_tok:"rbrace"
            ~body:(Rule "Stmt")
            ~bp:90
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
    ; module_
    ; doc_block
    ; item
    ; definition
    ; path
    ; path_segment
    ; import
    ; param
    ; type_ann
    ; params
    ; def
    ; val_def
    ; var_def
    ; type_def
    ; record
    ; operation
    ; interface_body
    ; interface
    ; effect_
    ; namespace_body
    ; namespace
    ; type_rule
    ; type_arrow
    ; boxed_type
    ; capture_set
    ; capture_list
    ; atomic_type
    ; type_args
    ; block_body
    ; stmt
    ; expr_stmt
    ; if_
    ; else_
    ; while_
    ; paren
    ; do_
    ; args
    ; return
    ; resume
    ; box
    ; unbox
    ; fn_
    ; try_
    ; handler
    ; handler_body
    ; clause
    ; match_
    ; match_body
    ; match_arm
    ; alt_pattern
    ; guard
    ; pattern
    ; pattern_args
    ; hole
    ]
;;
