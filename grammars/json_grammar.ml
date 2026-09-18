(* -- JSON ---------------------------------------------------------------------

   JSON carries four shapes the others here do not:

   - [Value] is an alternative over five tokens and two rules, so dispatch
     is a cascade over mixed kinds;
   - [Object] and [Array] sit side by side as delimited-with-separator
     bodies, sharing a separator and taking different elements;
   - [Member] is committed, so a missing colon stays inside the member
     instead of unwinding to the object;
   - JSON forbids a trailing comma, so both bodies take [trailing_sep:Never]
     and the parser reports one where the source has it.

   [Value] holds [Object], [Object] holds [Member], and [Member] holds
   [Value], so the rule graph recurses through three rules rather than one.

   The string regex takes a backslash followed by any codepoint, where JSON
   names the escapes it allows. The extra strings it accepts are a lexing
   question rather than a parsing one, and nothing here decodes an escape.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let nonzero = Redfa.Regex.range_char ~lo:'1' ~hi:'9' in
  let int_part = Redfa.Regex.(alt (singleton_char '0') (seq nonzero (star digit))) in
  let frac = Redfa.Regex.(opt (seq (singleton_char '.') (plus digit))) in
  let exp_part =
    Redfa.Regex.(
      opt
        (seq
           (chars_of_char_list [ 'e'; 'E' ])
           (seq (opt (chars_of_char_list [ '+'; '-' ])) (plus digit))))
  in
  let number =
    Redfa.Regex.(seq (opt (singleton_char '-')) (seqs [ int_part; frac; exp_part ]))
  in
  (* One codepoint of a string body. [complement] is the complement of the
     language, so it takes in the empty string and strings of any length.
     Meeting it with [any] leaves the single codepoints, which is the
     character class this wants. *)
  let plain = Redfa.Regex.(inter any (complement (chars_of_char_list [ '"'; '\\' ]))) in
  let escape = Redfa.Regex.(seq (singleton_char '\\') any) in
  let string_ =
    Redfa.Regex.(seqs [ singleton_char '"'; star (alt plain escape); singleton_char '"' ])
  in
  let tokens =
    [ kw "true"
    ; kw "false"
    ; kw "null"
    ; punct_tight ~name:"lbrace" "{"
    ; punct_tight ~name:"rbrace" "}"
    ; punct_tight ~name:"lbracket" "["
    ; punct_tight ~name:"rbracket" "]"
    ; punct ~space_before:false ~name:"comma" ","
    ; punct ~space_before:false ~name:"colon" ":"
    ; pat "number" number
    ; pat "string" string_
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod "File" [ child_req "value" (Rule "Value") ] in
  let value =
    prod
      "Value"
      [ child_alt
          ~modifier:Exactly_one
          "kind"
          [ Token "true"
          ; Token "false"
          ; Token "null"
          ; Token "number"
          ; Token "string"
          ; Rule "Object"
          ; Rule "Array"
          ]
      ]
  in
  let member =
    prod
      "Member"
      [ child_req "key" (Token "string")
      ; child_req "colon" (Token "colon")
      ; child_req "value" (Rule "Value")
      ]
    |> with_committed
    |> with_messages
         [ "colon", "expected ':' after object key"; "value", "expected a JSON value" ]
  in
  let object_ =
    prod ~break_style:Always "Object" [ child_rep "member" (Rule "Member") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"comma"
         ~trailing_sep:Never
  in
  let array =
    prod "Array" [ child_rep "elt" (Rule "Value") ]
    |> with_delimited_sep
         ~open_tok:"lbracket"
         ~close_tok:"rbracket"
         ~sep:"comma"
         ~trailing_sep:Never
  in
  create ~tokens ~roots:[ "File" ] [ file; value; member; object_; array ]
;;
