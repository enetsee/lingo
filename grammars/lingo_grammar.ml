(* -- lingo, in lingo ----------------------------------------------------------

   The surface syntax a [.lingo] file is written in, as a grammar. It is what
   the frontend's parser is generated from, and writing it here is how the
   syntax in scratch/docs/06-SURFACE.md was checked: a shape the checker
   rejects is a shape the syntax cannot have.

   What it carries that the other grammars do not:

   - a name may be any of the declaration keywords, so [Name] is an
     alternation over the identifier pattern and every keyword. That is what
     keeps [rule], [pattern] and [root] usable as child names, which the
     corpus needs: [by], [end], [open], [close], [from], [where], [order],
     [limit], [offset], [set], [line] and [root] are all child names in
     grammars/ today;
   - three delimited bodies with three different separators, [semi] inside
     braces, [comma] inside brackets and parentheses, and [bar] inside the
     parentheses of an alternation;
   - [@\[] as a two-character opener, which only effekt's [<{] otherwise
     reaches.

   No declaration carries a binder yet. Every one of them names itself by a
   child holding [Name], and [Name] is a rule, so [binder-not-pattern-token]
   rejects it. The check asks for a single pattern token where the property
   it stands for is that the child's text is one lexeme, and a rule that
   spans exactly one token has that property too. See
   scratch/docs/06-SURFACE.md §8.

   Source it parses:

   {v
     punct   lbrace "{" @[tight];
     pattern ident `[a-z]+`;

     rule Struct = {
       keyword : struct;
       name    : ident @[identity, binds];
       body    : StructBody;
     } committed;

     expression Expr = {
       atoms = int | ident;
       infix(1, right): eq;
       postfix(100): lbracket closed_by(rbracket), holds(Expr) @[kind(index)];
     } operators(after);

     start File;
   v}
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  (* A name may be spelled in single quotes. A declaration keyword is a token
     of its own, so a bare [start] or [keyword] is that keyword wherever it
     appears; the quoted form is always a name. One token either way, so a
     binder still points at a single pattern token. *)
  let bare =
    Redfa.Regex.(
      seq
        (one_of_char ~ranges:[ 'a', 'z'; 'A', 'Z' ] ~singles:[ '_' ] ())
        (star (one_of_char ~ranges:[ 'a', 'z'; 'A', 'Z'; '0', '9' ] ~singles:[ '_' ] ())))
  in
  let ident =
    Redfa.Regex.(alt bare (seqs [ singleton_char '\''; bare; singleton_char '\'' ]))
  in
  (* A quoted literal and a backtick regex differ only in their delimiter. A
     backslash escapes the delimiter in both, which is also redfa's own rule
     for a printable non-alphanumeric. *)
  let quoted (delim : char) =
    Redfa.Regex.(
      seqs
        [ singleton_char delim
        ; star
            (alt
               (not_chars (Ucharset.of_char_list [ delim; '\\' ]))
               (seq (singleton_char '\\') any))
        ; singleton_char delim
        ])
  in
  (* [///] against [//] falls out of longest match: a line comment is [//]
     followed by a character that is not a slash, or [//] alone. *)
  let line_comment =
    Redfa.Regex.(
      alt
        (seqs
           [ str "//"
           ; not_chars (Ucharset.of_list [ Char.code '/'; Char.code '\n' ])
           ; star (not_singleton_char '\n')
           ])
        (str "//"))
  in
  let doc_comment = Redfa.Regex.(seq (str "///") (star (not_singleton_char '\n'))) in
  let block_comment =
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
    [ kw "rule"
    ; kw "token"
    ; kw "expression"
    ; kw "start"
    ; kw "atoms"
    ; kw "prefix"
    ; kw "infix"
    ; kw "postfix"
    ; kw "scopes"
    ; punct_tight ~name:"at_lbracket" "@["
    ; punct ~space_before:false ~name:"rbracket" "]"
    ; punct ~space_after:false ~name:"lbrace" "{"
    ; punct ~space_before:false ~name:"rbrace" "}"
    ; punct ~space_after:false ~name:"lparen" "("
    ; punct ~space_before:false ~name:"rparen" ")"
    ; punct ~space_before:false ~name:"semi" ";"
    ; punct ~space_before:false ~name:"comma" ","
    ; punct ~space_before:false ~name:"colon" ":"
    ; punct ~name:"eq" "="
    ; punct ~name:"bar" "|"
    ; punct_tight ~name:"star" "*"
    ; punct_tight ~name:"plus" "+"
    ; punct_tight ~name:"question" "?"
    ; punct_tight ~name:"dot" "."
    ; pat "ident" ident
    ; pat "number" (Redfa.Regex.plus digit)
    ; pat "string" (quoted '"')
    ; pat "regex" (quoted '`')
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ; pat ~trivia:Preserve "doc" doc_comment
    ; pat ~trivia:Preserve "line" line_comment
    ; pat ~trivia:Preserve "block" block_comment
    ]
  in
  (* -- names --

     A declaration keyword is still a name. The FIRST sets are disjoint
     because they are different token kinds, so the cascade settles it. *)
  (* -- annotations, shared by a modifier and an attribute -- *)
  let arg =
    prod
      "Arg"
      [ child_alt
          ~modifier:Exactly_one
          "value"
          [ Token "ident"; Token "number"; Token "string" ]
      ; child_opt "many" (Rule "Sigil")
      ]
  in
  let args =
    prod "Args" [ child_rep "arg" (Rule "Arg") ]
    |> with_delimited_sep ~open_tok:"lparen" ~close_tok:"rparen" ~sep:"comma"
  in
  let ann =
    prod "Ann" [ child_req "name" (Token "ident"); child_opt "args" (Rule "Args") ]
  in
  let attrs =
    prod "Attrs" [ child_rep1 "attr" (Rule "Ann") ]
    |> with_delimited_sep
         ~open_tok:"at_lbracket"
         ~close_tok:"rbracket"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let modifiers =
    prod "Modifiers" [ child_rep1 "modifier" (Rule "Ann") ] |> with_separator ~sep:"comma"
  in
  (* -- a child's right-hand side -- *)
  let sigil =
    prod
      "Sigil"
      [ child_alt
          ~modifier:Exactly_one
          "mark"
          [ Token "star"; Token "plus"; Token "question" ]
      ]
    |> with_no_hole
  in
  let paren_alt =
    prod "ParenAlt" [ child_rep1 "alt" (Token "ident") ]
    |> with_delimited_sep ~open_tok:"lparen" ~close_tok:"rparen" ~sep:"bar"
  in
  let rhs =
    prod
      "Rhs"
      [ child_alt ~modifier:Exactly_one "symbol" [ Token "ident"; Rule "ParenAlt" ]
      ; child_opt "many" (Rule "Sigil")
      ]
  in
  let child_ =
    prod
      "Child"
      [ child_req "name" (Token "ident")
      ; child_req "colon" (Token "colon")
      ; child_req "rhs" (Rule "Rhs")
      ; child_opt "attrs" (Rule "Attrs")
      ]
  in
  (* -- token declarations -- *)
  let token_value =
    prod
      "TokenValue"
      [ child_req "eq" (Token "eq")
      ; child_alt ~modifier:Exactly_one "literal" [ Token "string"; Token "regex" ]
      ]
  in
  (* One form for all three token classes. A backtick literal is a pattern. A
     quoted literal is a keyword unless [@\[punctuation\]] says otherwise, and
     the spacing rides with it because only punctuation has any. *)
  let token_decl =
    prod
      "TokenDecl"
      [ child_req "name" (Token "ident")
      ; child_opt "value" (Rule "TokenValue")
      ; child_opt "attrs" (Rule "Attrs")
      ]
    |> with_identity "name"
    |> with_binder "name"
  in
  (* Every token in one block, because declaration order is max-munch
     priority. A rule needs no such block: its order settles kind numbering
     and nothing about what parses. *)
  let tokens_block =
    prod
      "TokenList"
      [ child_rep ~break:(always 1) ~between:(always 1) "token" (Rule "TokenDecl") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"semi"
         ~trailing_sep:Always
  in
  let tokens_decl =
    prod
      "TokensDecl"
      [ child_req "kw" (Token "token")
      ; child_req "block" (Rule "TokenList")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
  in
  (* -- a rule -- *)
  let body =
    prod "Body" [ child_rep ~break:(always 1) ~between:(always 1) "child" (Rule "Child") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"semi"
         ~trailing_sep:Always
  in
  let rule_decl =
    prod
      "RuleDecl"
      [ child_req "kw" (Token "rule")
      ; child_req "name" (Token "ident")
      ; child_req "eq" (Token "eq")
      ; child_req "body" (Rule "Body")
      ; child_opt "modifiers" (Rule "Modifiers")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  (* -- an expression block -- *)
  let level =
    prod "Level" [ child_rep1 "part" (Rule "Arg") ]
    |> with_delimited_sep ~open_tok:"lparen" ~close_tok:"rparen" ~sep:"comma"
  in
  (* A separated body wraps exactly one child, so the list is a rule of its
     own. [Modifiers] and [ParenAlt] are the same shape. *)
  let atom_list =
    prod "AtomList" [ child_rep1 "atom" (Token "ident") ] |> with_separator ~sep:"bar"
  in
  let atoms_entry =
    prod
      "AtomsEntry"
      [ child_req "kw" (Token "atoms")
      ; child_req "eq" (Token "eq")
      ; child_req "symbols" (Rule "AtomList")
      ]
    |> with_committed
  in
  let prefix_entry =
    prod
      "PrefixEntry"
      [ child_req "kw" (Token "prefix")
      ; child_req "level" (Rule "Level")
      ; child_req "colon" (Token "colon")
      ; child_req "op" (Token "ident")
      ]
    |> with_committed
  in
  let infix_entry =
    prod
      "InfixEntry"
      [ child_req "kw" (Token "infix")
      ; child_req "level" (Rule "Level")
      ; child_req "colon" (Token "colon")
      ; child_req "op" (Token "ident")
      ]
    |> with_committed
  in
  let postfix_entry =
    prod
      "PostfixEntry"
      [ child_req "kw" (Token "postfix")
      ; child_req "level" (Rule "Level")
      ; child_req "colon" (Token "colon")
      ; child_req "op" (Token "ident")
      ; child_opt "modifiers" (Rule "Modifiers")
      ; child_opt "attrs" (Rule "Attrs")
      ]
    |> with_committed
  in
  let entry =
    prod
      "Entry"
      [ child_alt_rules
          ~modifier:Exactly_one
          "kind"
          [ "AtomsEntry"; "PrefixEntry"; "InfixEntry"; "PostfixEntry" ]
      ]
  in
  let entries =
    prod
      "Entries"
      [ child_rep ~break:(always 1) ~between:(always 1) "entry" (Rule "Entry") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"semi"
         ~trailing_sep:Always
  in
  let expr_decl =
    prod
      "ExprDecl"
      [ child_req "kw" (Token "expression")
      ; child_req "name" (Token "ident")
      ; child_req "eq" (Token "eq")
      ; child_req "entries" (Rule "Entries")
      ; child_opt "modifiers" (Rule "Modifiers")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
    |> with_identity "name"
    |> with_binder "name"
    |> with_scope
  in
  (* -- scopes --

     A colour is not part of the grammar, so [Scopes.override] is a list
     beside it rather than a field in [Grammar.t]. The block here builds that
     list.

     A scope is written as the dotted path TextMate uses, which is what
     [Scope.to_string] prints. [Custom] takes its own form, because
     [Custom "keyword.control.struct"] and [Keyword_control (Some "struct")]
     print the same path and are different values. *)
  let segment =
    prod "Segment" [ child_req "dot" (Token "dot"); child_req "name" (Token "ident") ]
    |> with_no_hole
  in
  let dotted =
    prod "Dotted" [ child_req "head" (Token "ident"); child_rep "rest" (Rule "Segment") ]
    |> with_no_hole
  in
  let scope_value =
    prod "ScopeValue" [ child_req "path" (Rule "Dotted"); child_opt "args" (Rule "Args") ]
  in
  let scope_entry =
    prod
      "ScopeEntry"
      [ child_req "target" (Rule "Dotted")
      ; child_req "eq" (Token "eq")
      ; child_req "value" (Rule "ScopeValue")
      ]
  in
  let scope_list =
    prod
      "ScopeList"
      [ child_rep ~break:(always 1) ~between:(always 1) "entry" (Rule "ScopeEntry") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"semi"
         ~trailing_sep:Always
  in
  let scopes_decl =
    prod
      "ScopesDecl"
      [ child_req "kw" (Token "scopes")
      ; child_req "block" (Rule "ScopeList")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
  in
  (* -- roots -- *)
  let root_decl =
    prod
      "RootDecl"
      [ child_req "kw" (Token "start")
      ; child_rep1 "name" (Token "ident")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
  in
  let decl =
    prod
      "Decl"
      [ child_alt_rules
          ~modifier:Exactly_one
          "kind"
          [ "TokensDecl"; "RuleDecl"; "ExprDecl"; "ScopesDecl"; "RootDecl" ]
      ]
  in
  let file =
    prod
      ~indent_width:0
      "File"
      [ child_rep ~break:(always 1) ~between:(always 1) "decl" (Rule "Decl") ]
  in
  create
    ~tokens
    ~roots:[ "File" ]
    [ file
    ; decl
    ; tokens_decl
    ; tokens_block
    ; token_decl
    ; token_value
    ; rule_decl
    ; body
    ; child_
    ; rhs
    ; paren_alt
    ; sigil
    ; expr_decl
    ; entries
    ; entry
    ; atom_list
    ; atoms_entry
    ; prefix_entry
    ; infix_entry
    ; postfix_entry
    ; level
    ; scopes_decl
    ; scope_list
    ; scope_entry
    ; scope_value
    ; dotted
    ; segment
    ; root_decl
    ; modifiers
    ; attrs
    ; ann
    ; args
    ; arg
    ]
;;
