(* -- comments, and the boundaries they make -----------------------------------

   The grammar the formatter needs and the parser does not. Every grammar
   before it is whitespace-only, so three of the layout's steps had no input
   that reached them.

   - [line] runs to the end of its line. Anything written after one on that
     line is inside it when the output is read back, so the only glue that
     works at that boundary is a line break. That comes from the lexer, and
     this is the grammar that lets a law reach it.
   - [line] and [block] are [Preserve] trivia. A comment keeps the line the
     source put it on rather than taking one from the production, because the
     production's flat or broken state moves between passes and the comment
     would move with it.
   - [List] takes [trailing_sep:On_break], so the separator after the last
     element appears only when the body breaks. That is one [flat_alt] in the
     document rather than something read back off the renderer afterwards.

   [Field] is here for the max-munch floor rather than for the syntax. [number]
   takes a decimal point, so on ["\[1 .5\]" ] recovery leaves ["1"], ["."] and
   ["5"] as three tokens the source had apart. No pair of them joins; the three
   of them are one number. The floor reads the run back to the last blank for
   that reason.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let number =
    Redfa.Regex.(seq (plus digit) (opt (seq (singleton_char '.') (plus digit))))
  in
  let name = Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z')) in
  let line = Redfa.Regex.(seq (str "//") (star (not_singleton_char '\n'))) in
  (* [/*], then anything that is not a [*/], then [*/]. The middle takes a run
     of stars only where a slash does not follow it. *)
  let block =
    Redfa.Regex.(
      seqs
        [ str "/*"
        ; star
            (alt
               (not_singleton_char '*')
               (seq
                  (plus (singleton_char '*'))
                  (inter any (complement (chars_of_char_list [ '*'; '/' ])))))
        ; plus (singleton_char '*')
        ; singleton_char '/'
        ])
  in
  let tokens =
    [ punct_tight ~name:"lbrack" "["
    ; punct_tight ~name:"rbrack" "]"
    ; punct ~space_before:false ~name:"comma" ","
    ; punct_tight ~name:"dot" "."
    ; pat "name" name
    ; pat "number" number
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ; pat ~trivia:Preserve "line" line
    ; pat ~trivia:Preserve "block" block
    ]
  in
  let file = prod "File" [ child_req "body" (Rule "List") ] in
  let list_ =
    prod "List" [ child_rep "elt" (Rule "Item") ]
    |> with_delimited_sep
         ~open_tok:"lbrack"
         ~close_tok:"rbrack"
         ~sep:"comma"
         ~trailing_sep:On_break
  in
  let item =
    prod
      "Item"
      [ child_alt
          ~modifier:Exactly_one
          "head"
          [ Token "name"; Token "number"; Rule "List" ]
      ; child_rep "field" (Rule "Field")
      ]
  in
  let field =
    prod
      "Field"
      [ child_req "dot" (Token "dot")
      ; child_alt ~modifier:Exactly_one "tail" [ Token "name"; Token "number" ]
      ]
  in
  create ~tokens ~roots:[ "File" ] [ file; list_; item; field ]
;;
