(* -- a grammar the lexer must read by codepoint --------------------------------

   Every other grammar here is ASCII, so nothing separates a lexer that reads
   codepoints from one that reads bytes. Both give the same for every input
   the rest of the corpus offers.

   This one separates them. Each of its non-ASCII tokens is more than one byte
   in UTF-8, so a scan that steps a byte at a time never matches the set the
   automaton holds:

   - [«] and [»] are the delimiters, two bytes each;
   - [word] is a pattern over a codepoint range above U+007F, so the range
     lives in the automaton rather than in a literal;
   - [→] is a three-byte separator, which is a different width again.

   A lexer that reads bytes turns every one of those into error tokens, and
   the parse then reports on input the grammar accepts.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  (* Latin-1 letters and Greek, both above U+007F. A literal could not carry
     these: the range has to reach the automaton as a range. *)
  let letter =
    Redfa.Regex.(
      alt
        (range_char ~lo:'a' ~hi:'z')
        (alt (range ~lo:0x00C0 ~hi:0x00FF) (range ~lo:0x0391 ~hi:0x03C9)))
  in
  let tokens =
    [ punct_tight ~name:"laquo" "\xc2\xab"
    ; punct_tight ~name:"raquo" "\xc2\xbb"
    ; punct ~name:"arrow" "\xe2\x86\x92"
    ; pat "word" (Redfa.Regex.plus letter)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_req "doc" (Rule "Doc") ] in
  let doc =
    prod "Doc" [ child_rep "words" (Token "word") ]
    |> with_delimited_sep ~open_tok:"laquo" ~close_tok:"raquo" ~sep:"arrow"
  in
  create ~tokens ~roots:[ "File" ] [ file; doc ]
;;
