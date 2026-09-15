(* -- production shapes --------------------------------------------------------

   The production framings the other grammars here leave out. Each one is a
   different body loop or a different recovery set, and none of them is
   reachable through sexp, json, calc or rassoc.

   - [Program] is a root with a repeated child and no frame, so the body loop
     has no closer to stop at and ends on a kind its element cannot start
     with;
   - [Names] is [with_separator]: a separated list with nothing around it;
   - [Block] is delimited, committed and a boundary, so a failure inside it
     resumes on its own [rbrace] rather than on a caller's delimiter;
   - [Block] declares a resync anchor, so a broken body stops at [end] rather
     than sweeping it up and carrying on;
   - [Let] has optional children, which are silent when absent where a
     required one reports;
   - [Block]'s separator allows a trailing one, where json's forbids it. The
     two answers need two grammars, because the policy is per production.

   There is no expression block. grammars/calc_grammar.ml, rassoc and postfix
   carry those, and a failure here should have one cause.
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let letter = Redfa.Regex.range_char ~lo:'a' ~hi:'z' in
  let tokens =
    [ kw "let"
    ; punct_tight ~name:"lbrace" "{"
    ; punct_tight ~name:"rbrace" "}"
    ; punct_tight ~name:"semi" ";"
    ; punct_tight ~name:"comma" ","
    ; punct ~name:"equals" "="
    ; punct_tight ~name:"at" "@"
    ; kw "end"
    ; pat "name" (Redfa.Regex.plus letter)
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  (* A repeated child with no frame around it. The loop ends where the cursor
     is on something no declaration starts with. *)
  let program = prod "Program" [ child_rep "decls" (Rule "Decl") ] in
  let decl =
    prod "Decl" [ child_alt_rules ~modifier:Required "decl" [ "Let"; "Block" ] ]
  in
  (* Both trailing children are optional, so their absence is silent.

     [at] appears in this grammar and nowhere else in it, so the wording the
     lowering would give the [marker] child is a wording no other position
     shares. That matters to test/laws/law_lower.ml part (c): the catalogue
     dedupes on the text, so a lowering that asked for a message at a child
     that never reports would hide the spare entry behind a live one
     everywhere the wording is shared. Here it cannot hide. *)
  let let_ =
    prod
      "Let"
      [ child_req "kw" (Token "let")
      ; child_req "name" (Token "name")
      ; child_opt "marker" (Token "at")
      ; child_opt "init" (Rule "Init")
      ]
  in
  let init =
    prod "Init" [ child_req "eq" (Token "equals"); child_req "value" (Rule "Names") ]
  in
  (* A separated list with nothing around it. Its first element is required
     whatever the child slot says, so the loop differs from a delimited one. *)
  let names =
    prod "Names" [ child_rep "items" (Token "name") ]
    |> with_separator ~sep:"comma" ~trailing_sep:Never
  in
  (* Committed and a boundary, so a failure inside resumes on this rule's own
     delimiters.

     [end] is the resync anchor. Nothing in this grammar starts with it, so
     without the anchor a body would sweep it up and carry on; with it the body
     stops there and the token is left to whatever encloses it. An anchor that
     a body element could start with would end the body before it ever took
     one, so it has to be a token like this. *)
  let block =
    prod "Block" [ child_rep "items" (Rule "Decl") ]
    |> with_delimited_sep
         ~open_tok:"lbrace"
         ~close_tok:"rbrace"
         ~sep:"semi"
         ~trailing_sep:Always
         ~boundary:true
    |> with_resync_to [ "end" ]
  in
  create ~tokens ~roots:[ "Program" ] [ program; decl; let_; init; names; block ]
;;
