(* -- recovery sets ------------------------------------------------------------

   The parts of a recovery set the other grammars leave unexercised, each in
   a position where dropping it changes a parse.
   test/parse_emit/law_parse.ml reads the same over those grammars whether
   a commit's resume set, a rule's boundary or a rule's adds are there or
   not, so a differential over them says nothing about the three.

   What each production is here for:

   - [Triple] has two children after its committed one. A commit's resume set
     holds the FIRST set of every later child, and its recovery set stops at
     the first later child that is not nullable, so two children after the
     commit is where the two sets part.
   - [Group] is committed and a boundary, and its body names none of the
     tokens its caller passes down. shapes' [Block] is a boundary too, and
     its own body already stops on everything a caller contributes, so
     dropping the inbound set there changes nothing.
   - [Sig] declares a [recover_to] on a child, which no other grammar does.
     The override replaces the computed set, and the resume set is computed
     without reading it, so the two part here as well.
   - [Fields] is a separated list of a rule. A separated body has no closer,
     so its loop adds nothing to what it passes its elements, and the
     separator reaches them through the rule's adds alone. shapes' [Names] is
     separated too, but its elements are tokens, so nothing there reads what
     is passed down.

   Every token is spelled so the FIRST sets stay disjoint and the interesting
   token is reachable at the position that reads it. That is what the grammar
   is for; it is not a language anyone would write.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let letter = Redfa.Regex.range_char ~lo:'a' ~hi:'z' in
  let tokens =
    [ kw "let"
    ; kw "sig"
    ; kw "in"
    ; kw "end"
    ; punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct_tight ~name:"comma" ","
    ; punct_tight ~name:"semi" ";"
    ; punct ~name:"colon" ":"
    ; pat "name" (Redfa.Regex.plus letter)
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
      [ child_alt_rules ~modifier:Exactly_one "item" [ "Triple"; "Group"; "Sig" ] ]
  in
  (* Two children follow the committed one, and they start with different
     tokens. [resume] holds both and [recover] holds the first. *)
  let triple =
    prod
      "Triple"
      [ child_req "kw" (Token "let")
      ; child_req "head" (Rule "Leaf")
      ; child_req "sep" (Token "in")
      ; child_req "tail" (Token "end")
      ]
  in
  (* The body names [rparen] and nothing else, and a caller passes down
     [let], [lparen] and [sig]. Dropping the inbound set is then the
     difference between stopping at the close and stopping at the next
     item. *)
  let group =
    prod
      "Group"
      [ child_req "open" (Token "lparen")
      ; child_req "inner" (Rule "Triple")
      ; child_req "close" (Token "rparen")
      ]
    |> with_committed ~boundary:true
  in
  (* The override is [end], and the computed set at that child is [in]. *)
  let sig_ =
    prod
      "Sig"
      [ child_req "kw" (Token "sig")
      ; child_req ~recover_to:[ "end" ] "fields" (Rule "Fields")
      ; child_req "tail" (Token "in")
      ]
  in
  let fields =
    prod "Fields" [ child_rep1 "items" (Rule "Field") ]
    |> with_separator ~sep:"comma" ~trailing_sep:Never
  in
  (* A lead token, so the element is entered on [colon] and its committed
     child can then meet something else. A commit on a rule's first child
     never fails, because the caller dispatched on the same set. *)
  let field =
    prod
      "Field"
      [ child_req "lead" (Token "colon")
      ; child_req "value" (Rule "Leaf")
      ; child_req "tail" (Token "semi")
      ]
  in
  let leaf = prod "Leaf" [ child_req "n" (Token "name") ] in
  create
    ~tokens
    ~roots:[ "File" ]
    [ file; item; triple; group; sig_; fields; field; leaf ]
;;
