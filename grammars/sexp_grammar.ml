(* -- s-expressions ------------------------------------------------------------

   The smallest grammar that takes a delimited production, recursion and a
   pattern token together:

   - [Group] is delimited, with a repeated body and no separator;
   - [Group] holds [Sexp]s which can be [Group]s, so the rule graph recurses
     and the FIRST fixpoint reaches through it;
   - [ident] and [number] are pattern tokens, so the lexer automaton comes
     from regexes rather than literals;
   - [ws] is trivia.

   The facts it produces are pinned in test/units/sexp_facts.ml.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  (* Lisp-flavoured identifiers. The punctuation that is not structural
     here is an identifier character. *)
  let ident_head =
    Redfa.Regex.one_of_char
      ~ranges:[ 'a', 'z'; 'A', 'Z' ]
      ~singles:[ '_'; '+'; '-'; '*'; '/'; '!'; '?'; '<'; '>'; '=' ]
      ()
  in
  let ident_cont =
    Redfa.Regex.one_of_char
      ~ranges:[ 'a', 'z'; 'A', 'Z'; '0', '9' ]
      ~singles:[ '_'; '+'; '-'; '*'; '/'; '!'; '?'; '<'; '>'; '=' ]
      ()
  in
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let tokens =
    [ punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; pat "ident" Redfa.Regex.(seq ident_head (star ident_cont))
    ; pat
        "number"
        Redfa.Regex.(
          seq
            (opt (singleton_char '-'))
            (seq (plus digit) (opt (seq (singleton_char '.') (plus digit)))))
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_req "root" (Rule "Sexp") ] in
  let sexp =
    prod
      "Sexp"
      [ child_alt
          ~modifier:Exactly_one
          "kind"
          [ Token "ident"; Token "number"; Rule "Group" ]
      ]
  in
  (* [lparen] and [rparen] are tight, so nothing sits between a group and what
     is beside it: [(a (b 12) c)] comes out [(a(b 12)c)]. The spacing wanted
     here belongs to the production rather than to either token, because two
     parentheses still touch. *)
  let group =
    prod "Group" [ child_rep "elt" (Rule "Sexp") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
    |> with_leading_space true
    |> with_trailing_space true
  in
  create ~tokens ~roots:[ "File" ] [ file; sexp; group ]
;;
