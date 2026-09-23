(* -- witness grammars, at least one per rejection code -------------------------

      Each provokes its own code and no other, so whichever fired first cannot
      hide a second. [law_validate.ml] checks both halves: every code in
      [Error.codes] appears here, and each grammar's rejection set is the one
      code it is filed under.

      Several carry a [~greedy:true] or an awkward shape for that reason, and
      the comment says which other code it keeps quiet.

      A code carries a second grammar where one shape cannot reach what the
      check owes. [left_recursion] has [left_recursion_pair], the first cycle
      length that separates one finding from many, and [invalid_name] has
      [invalid_name_kind_suffix], a name in a position the first leaves out.
      The comment on each second grammar says what it adds. Both halves above
      are quantified per entry, so a shared code costs nothing.
   -------------------------------------------------------------------------- *)

open Core.Grammar

let t_a = punct_tight ~name:"ta" "a"
let t_b = punct_tight ~name:"tb" "b"
let t_t = punct_tight ~name:"t" "%"
let t_m = punct_tight ~name:"m" "!"
let t_lp = punct_tight ~name:"lp" "("
let t_rp = punct_tight ~name:"rp" ")"
let t_comma = punct_tight ~name:"comma" ","
let base_tokens = [ t_a; t_b; t_t; t_m; t_lp; t_rp; t_comma ]

(* A root that is clean on its own, so a witness adds one problem beside
   it. *)
let clean_root = prod "Root" [ child_req "x" (Token "ta") ]

let with_root ?(tokens = base_tokens) ?(expr = []) (ps : production list) : Grammar.t =
  create ~expr ~tokens ~roots:[ "Root" ] (clean_root :: ps)
;;

let only ?(tokens = base_tokens) ?(expr = []) ?(roots = [ "Root" ]) (ps : production list)
  : Grammar.t
  =
  create ~expr ~tokens ~roots ps
;;

(* -- stage 1 --------------------------------------------------------------- *)

let empty_grammar = create ~tokens:[] ~roots:[] []

let invalid_name =
  (* A child name, so nothing else about the production is disturbed. *)
  only [ prod "Root" [ child_req "bad name" (Token "ta") ] ]
;;

let invalid_name_kind_suffix =
  (* A postfix operator's [kind_suffix]. It is spliced into a kind
     constructor, a view module and a formatter binding, and the grammar was
     accepted while it gave all three [K_E_POSTFIX_A-B] to trip over.
     The block declares one postfix operator, so [postfix-suffix-missing] stays
     quiet about a missing or duplicated suffix, and every other name in the
     grammar is clean. *)
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~postfix:[ postfix_simple ~kind_suffix:"a-b" ~token:"m" ~bp:10 () ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

let invalid_token_literal =
  only ~tokens:(punct_tight ~name:"bad" "\xff\xfe" :: base_tokens) [ clean_root ]
;;

let dup_kind_name =
  (* [Foo]'s hole kind is [N_FOO_HOLE] and so is [Foo_hole]'s own kind.
     They collide in the kind enum and nowhere else: [parse_foo] against
     [parse_foo_hole], [format_foo] against [format_foo_hole], and the view
     modules [Foo] against [Foo_hole] all stay apart. *)
  with_root
    [ prod "Foo" [ child_req "x" (Token "ta") ]
    ; prod "Foo_hole" [ child_req "x" (Token "tb") ]
    ]
;;

let reserved_name =
  (* [format_node] is emitted for every grammar. *)
  with_root [ prod "Node" [ child_req "x" (Token "ta") ] ]
;;

let name_collision =
  (* [Match] and [Match_] both mangle to [match_] once the keyword escape
     has run, so both emit [parse_match_] into one [let rec] group. Their
     kinds, [N_MATCH] and [N_MATCH_], stay apart, so this namespace needs a
     check of its own. *)
  with_root
    [ prod "Match" [ child_req "x" (Token "ta") ]
    ; prod "Match_" [ child_req "x" (Token "tb") ]
    ]
;;

let dup_child_name =
  with_root
    [ prod "P" [ child_req "fooBar" (Token "ta"); child_req "foo_bar" (Token "tb") ] ]
;;

let unknown_rule = only [ prod "Root" [ child_req "x" (Rule "Nope") ] ]
let unknown_token = only [ prod "Root" [ child_req "x" (Token "nope") ] ]

let unknown_op_token =
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~infix_ops:[ infix ~token:"nope" ~bp:10 () ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

let unknown_delimiter_token =
  only
    [ prod "Root" [ child_rep "x" (Token "ta") ]
      |> with_delimited ~open_tok:"nope" ~close_tok:"rp"
    ]
;;

let unknown_recover_to_token =
  only [ prod "Root" [ child_req ~recover_to:[ "nope" ] "x" (Token "ta") ] ]
;;

let unknown_resync_anchor =
  only [ prod "Root" [ child_rep "x" (Token "ta") ] |> with_resync_to [ "nope" ] ]
;;

let unknown_identity_child =
  only [ prod "Root" [ child_req "x" (Token "ta") ] |> with_identity "nope" ]
;;

let unknown_binder_child =
  only [ prod "Root" [ child_req "x" (Token "ta") ] |> with_binder "nope" ]
;;

(* A binder on a child holding a punctuation token. Its text is the same
   wherever it appears, so it introduces one name over and over. *)
let binder_not_pattern_token =
  only [ prod "Root" [ child_req "x" (Token "ta") ] |> with_binder "x" ]
;;

(* A binder on a child holding a rule. Its text is the whole subtree. *)
let binder_holds_a_rule =
  only
    [ prod "Root" [ child_req "x" (Rule "Inner") ] |> with_binder "x"
    ; prod "Inner" [ child_req "y" (Token "ta") ]
    ]
;;

(* A binder on a child that takes either of two pattern tokens. Only one of
   them is a name, and nothing here says which. The two patterns are disjoint
   from each other and from the punctuation, so the lexer is clean. *)
let binder_holds_alternatives =
  only
    ~tokens:
      [ pat "word" Redfa.Regex.(plus (range_char ~lo:'c' ~hi:'z'))
      ; pat "num" Redfa.Regex.(plus (range_char ~lo:'0' ~hi:'9'))
      ]
    [ prod "Root" [ child_alt ~modifier:Exactly_one "x" [ Token "word"; Token "num" ] ]
      |> with_binder "x"
    ]
;;

let unknown_message_child =
  only
    [ prod "Root" [ child_req "x" (Token "ta") ]
      |> with_messages [ "nope", "expected something" ]
    ]
;;

(* A wording for a child that never reports. The child is repeated rather than
   optional so that [Root] stays non-nullable, which keeps the shape stage
   quiet. *)
let unused_message_child =
  only
    [ prod "Root" [ child_req "x" (Token "ta"); child_rep "y" (Token "tb") ]
      |> with_messages [ "y", "expected something" ]
    ]
;;

(* A recovery set on a child that never recovers, for the same reason. *)
let unused_recover_to =
  only
    [ prod
        "Root"
        [ child_req "x" (Token "ta"); child_rep ~recover_to:[ "tb" ] "y" (Token "tb") ]
    ]
;;

(* Resync anchors on a production that repeats nothing, so there is no body
   loop for one to end. *)
let unused_resync_anchors =
  only [ prod "Root" [ child_req "x" (Token "ta") ] |> with_resync_to [ "tb" ] ]
;;

(* The same on a separated list, which has ended wherever an anchor could sit:
   its loop runs while the cursor is on the separator. *)
let unused_resync_anchors_separated =
  only
    [ prod "Root" [ child_rep "x" (Token "ta") ]
      |> with_separator ~sep:"comma"
      |> with_resync_to [ "tb" ]
    ]
;;

let empty_alternatives = only [ prod "Root" [ child_alt ~modifier:Exactly_one "x" [] ] ]
let unknown_root = only ~roots:[ "Nope" ] [ clean_root ]
let dup_root = only ~roots:[ "Root"; "Root" ] [ clean_root ]

(* No roots at all. The grammar has a production, so [empty-grammar] does not
   fire first and stop the stage. *)
let no_roots = only ~roots:[] [ clean_root ]

(* A root naming an expression block. The block is a rule, so the reference
   resolves and only the "a root must be a production" arm fires. *)
let root_is_block =
  only
    ~roots:[ "E" ]
    ~expr:[ expr_block ~rule_name:"E" ~atoms:[ Token "ta" ] () ]
    [ clean_root ]
;;

let postfix_kind_suffix =
  (* Two postfix operators with an empty kind_suffix. Distinct leads, so
     the duplicate check stays quiet. *)
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~postfix:
            [ postfix_simple ~token:"m" ~bp:10 (); postfix_simple ~token:"t" ~bp:20 () ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

(* Two postfix operators sharing one kind_suffix. Both carry a suffix, so
   the missing-suffix arm stays quiet. *)
let postfix_suffix_duplicate =
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~postfix:
            [ postfix_simple ~kind_suffix:"same" ~token:"m" ~bp:10 ()
            ; postfix_simple ~kind_suffix:"same" ~token:"t" ~bp:20 ()
            ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

let nullable_token =
  only
    ~tokens:(pat "maybe" Redfa.Regex.(opt (singleton_char 'z')) :: base_tokens)
    [ clean_root ]
;;

let empty_pratt_atoms =
  only
    ~expr:[ expr_block ~rule_name:"E" ~atoms:[] () ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

let dup_pratt_op =
  (* One token twice in one category. Same binding power and associativity,
     so the mixed-associativity check stays quiet. *)
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~infix_ops:[ infix ~token:"m" ~bp:10 (); infix ~token:"m" ~bp:10 () ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

let mixed_pratt_role =
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~infix_ops:[ infix ~token:"m" ~bp:10 () ]
          ~postfix:[ postfix_simple ~token:"m" ~bp:20 () ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

let mixed_assoc_at_bp =
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~infix_ops:
            [ infix ~assoc:Left ~token:"m" ~bp:10 ()
            ; infix ~assoc:Right ~token:"t" ~bp:10 ()
            ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

(* -- stage 2 --------------------------------------------------------------- *)

let delimited_arity =
  (* Two children of different kinds, both required, so the view checks
     stay quiet and the arity is the one complaint. *)
  only
    [ prod "Root" [ child_req "a" (Token "ta"); child_req "b" (Token "tb") ]
      |> with_delimited ~open_tok:"lp" ~close_tok:"rp"
    ]
;;

let separated_arity =
  only
    [ prod "Root" [ child_req "a" (Token "ta"); child_req "b" (Token "tb") ]
      |> with_separator ~sep:"comma"
    ]
;;

let repeated_vs_single =
  only [ prod "Root" [ child_rep "xs" (Token "ta"); child_req "x" (Token "ta") ] ]
;;

(* The same hazard across buckets. The two children spell different symbol
   sets, so they land in separate slot families, and the kinds they admit
   still intersect on [ta]: the repeated accessor's skip passes over the
   single one and returns its node too. *)
let repeated_vs_single_kinds =
  only
    [ prod
        "Root"
        [ child_alt ~modifier:Zero_or_more "xs" [ Token "ta"; Token "tb" ]
        ; child_req "x" (Token "ta")
        ]
    ]
;;

let ambiguous_same_kind_child =
  (* Same bucket, no repeated child, and the earlier single is optional.
     Where it is absent the later accessor's rank points at the wrong
     node. *)
  only
    [ prod "Root" [ child_opt ~greedy:true "a" (Token "ta"); child_req "b" (Token "ta") ]
    ]
;;

let overlapping_single_kinds =
  (* Different buckets, since [(ta|tb)] and [ta] are spelled differently,
     so each accessor counts from zero and both admit a [ta] node. *)
  only
    [ prod
        "Root"
        [ child_alt ~modifier:Exactly_one "a" [ Token "ta"; Token "tb" ]
        ; child_req "b" (Token "ta")
        ]
    ]
;;

(* -- stage 3 --------------------------------------------------------------- *)

let first_first_conflict =
  only
    [ prod "Root" [ child_alt ~modifier:Exactly_one "x" [ Rule "A"; Rule "B" ] ]
    ; prod "A" [ child_req "x" (Token "ta") ]
    ; prod "B" [ child_req "x" (Token "ta") ]
    ]
;;

let first_follow_conflict =
  (* Two rules rather than two tokens. Two same-kind children are reported by
     the shape stage, which runs first. *)
  only
    [ prod "Root" [ child_opt "o" (Rule "A"); child_req "r" (Rule "B") ]
    ; prod "A" [ child_req "x" (Token "ta") ]
    ; prod "B" [ child_req "x" (Token "ta") ]
    ]
;;

let left_recursion =
  (* [~greedy:true] keeps the FIRST/FOLLOW complaint quiet, which the
     optional self-reference would otherwise raise first. It says the author
     meant the ambiguity, and the parser still makes no progress. *)
  only
    [ prod "Root" [ child_opt ~greedy:true "l" (Rule "Root"); child_req "t" (Token "ta") ]
    ]
;;

let left_recursion_pair =
  (* The same shape one rule wider. A cycle of one is the single length at
     which reporting cycles and reporting components agree, so the self-loop
     above stayed green while a cycle of two reported one finding per
     rotation: two findings for one left recursion. [~greedy:true] is for the
     reason given above.

     [law_validate] part (f) holds the count. Part (a) compares sets of
     codes, and part (e) rejects identical findings where rotations differ in
     both site and message, so a left recursion reported k times gets past
     both. *)
  only
    [ prod
        "Root"
        [ child_opt ~greedy:true "l" (Rule "Inner"); child_req "t" (Token "ta") ]
    ; prod
        "Inner"
        [ child_opt ~greedy:true "l" (Rule "Root"); child_req "t" (Token "tb") ]
    ]
;;

let nullable_repeated =
  only
    [ prod "Root" [ child_rep "xs" (Rule "Opt") ]
    ; prod "Opt" [ child_opt ~greedy:true "x" (Token "ta") ]
    ]
;;

let nullable_pratt_atom =
  only
    ~expr:[ expr_block ~rule_name:"E" ~atoms:[ Rule "Opt" ] () ]
    [ prod "Root" [ child_req "e" (Rule "E") ]
    ; prod "Opt" [ child_opt ~greedy:true "x" (Token "ta") ]
    ]
;;

let nullable_separated_element =
  only
    [ prod "Root" [ child_rep "e" (Rule "Opt") ] |> with_separator ~sep:"comma"
    ; prod "Opt" [ child_opt ~greedy:true "x" (Token "ta") ]
    ]
;;

let empty_first_set =
  only
    [ prod "Root" [ child_req "e" (Rule "Empty"); child_req "t" (Token "ta") ]
    ; prod "Empty" []
    ]
;;

let pratt_atom_conflict =
  only
    ~expr:[ expr_block ~rule_name:"E" ~atoms:[ Token "ta"; Rule "A" ] () ]
    [ prod "Root" [ child_req "e" (Rule "E") ]; prod "A" [ child_req "x" (Token "ta") ] ]
;;

(* The prefix operator is itself an atom of the block. The prefix table is read
   first, so the atom never reads the token. *)
let prefix_is_atom =
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "m" ]
          ~prefix_ops:[ prefix ~token:"m" ~bp:10 () ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ] ]
;;

(* A delimited body anchored on the very token its elements start with. The
   anchor ends the body wherever an element could begin. *)
let resync_anchor_conflict =
  only
    [ prod "Root" [ child_rep "items" (Token "ta") ]
      |> with_delimited ~open_tok:"lp" ~close_tok:"rp"
      |> with_resync_to [ "ta" ]
    ]
;;

let prefix_atom_conflict =
  only
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Rule "A" ]
          ~prefix_ops:[ prefix ~token:"m" ~bp:10 () ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ]; prod "A" [ child_req "x" (Token "m") ] ]
;;

let token_unreachable =
  (* Two tokens spelled the same. Declaration order is max-munch priority
     order, so the second wins from no state. *)
  only ~tokens:(kw "if" :: kw ~name:"if2" "if" :: base_tokens) [ clean_root ]
;;

(* -- the table ------------------------------------------------------------- *)

let all : (string * Grammar.t) list =
  [ "empty-grammar", empty_grammar
  ; "invalid-name", invalid_name
  ; "invalid-name", invalid_name_kind_suffix
  ; "invalid-token-literal", invalid_token_literal
  ; "dup-kind-name", dup_kind_name
  ; "reserved-name", reserved_name
  ; "name-collision", name_collision
  ; "dup-child-name", dup_child_name
  ; "unknown-rule", unknown_rule
  ; "unknown-token", unknown_token
  ; "unknown-op-token", unknown_op_token
  ; "unknown-delimiter-token", unknown_delimiter_token
  ; "unknown-recover-to-token", unknown_recover_to_token
  ; "unknown-resync-anchor", unknown_resync_anchor
  ; "unknown-identity-child", unknown_identity_child
  ; "unknown-binder-child", unknown_binder_child
  ; "binder-not-pattern-token", binder_not_pattern_token
  ; "binder-not-pattern-token", binder_holds_a_rule
  ; "binder-not-pattern-token", binder_holds_alternatives
  ; "unknown-message-child", unknown_message_child
  ; "unused-message-child", unused_message_child
  ; "unused-recover-to", unused_recover_to
  ; "unused-resync-anchors", unused_resync_anchors
  ; "unused-resync-anchors", unused_resync_anchors_separated
  ; "empty-alternatives", empty_alternatives
  ; "no-roots", no_roots
  ; "root-is-block", root_is_block
  ; "unknown-root", unknown_root
  ; "dup-root", dup_root
  ; "postfix-suffix-missing", postfix_kind_suffix
  ; "postfix-suffix-duplicate", postfix_suffix_duplicate
  ; "nullable-token", nullable_token
  ; "empty-pratt-atoms", empty_pratt_atoms
  ; "dup-pratt-op", dup_pratt_op
  ; "mixed-pratt-role", mixed_pratt_role
  ; "mixed-assoc-at-bp", mixed_assoc_at_bp
  ; "delimited-arity", delimited_arity
  ; "separated-arity", separated_arity
  ; "repeated-vs-single", repeated_vs_single
  ; "repeated-vs-single-kinds", repeated_vs_single_kinds
  ; "ambiguous-same-kind-child", ambiguous_same_kind_child
  ; "overlapping-single-kinds", overlapping_single_kinds
  ; "first-first-conflict", first_first_conflict
  ; "first-follow-conflict", first_follow_conflict
  ; "left-recursion", left_recursion
  ; "left-recursion", left_recursion_pair
  ; "nullable-repeated", nullable_repeated
  ; "nullable-pratt-atom", nullable_pratt_atom
  ; "nullable-separated-element", nullable_separated_element
  ; "empty-first-set", empty_first_set
  ; "pratt-atom-conflict", pratt_atom_conflict
  ; "prefix-atom-conflict", prefix_atom_conflict
  ; "prefix-atom-conflict", prefix_is_atom
  ; "resync-anchor-conflict", resync_anchor_conflict
  ; "token-unreachable", token_unreachable
  ]
;;

(** Grammars the checker accepts. A checker that rejects everything passes
    the completeness law, so the other side needs a corpus of its own: these
    take the shapes nearest to each rejection and stop short of them. *)
let accepted : (string * Grammar.t) list =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar
  ; "calc", Lingo_grammars.Calc_grammar.grammar
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar
  ; "json", Lingo_grammars.Json_grammar.grammar
  ; ( "delimited-with-sep"
    , only
        [ prod "Root" [ child_rep "e" (Rule "Item") ]
          |> with_delimited_sep ~open_tok:"lp" ~close_tok:"rp" ~sep:"comma"
        ; prod "Item" [ child_req "x" (Token "ta") ]
        ] )
  ; ( "separated"
    , only
        [ prod "Root" [ child_rep "e" (Rule "Item") ] |> with_separator ~sep:"comma"
        ; prod "Item" [ child_req "x" (Token "ta") ]
        ] )
  ; ( "all-required-same-kind"
    , (* Every slot required. All of them are present on a clean parse, so
         the two ranks agree. *)
      only [ prod "Root" [ child_req "a" (Token "ta"); child_req "b" (Token "ta") ] ] )
  ; ( "pratt"
    , only
        ~expr:
          [ expr_block
              ~rule_name:"E"
              ~atoms:[ Token "ta"; Rule "Paren" ]
              ~prefix_ops:[ prefix ~token:"m" ~bp:70 () ]
              ~infix_ops:
                [ infix ~assoc:Left ~token:"t" ~bp:10 ()
                ; infix ~assoc:Right ~token:"tb" ~bp:20 ()
                ]
              ()
          ]
        [ prod "Root" [ child_req "e" (Rule "E") ]
        ; prod "Paren" [ child_req "inner" (Rule "E") ]
          |> with_delimited ~open_tok:"lp" ~close_tok:"rp"
        ] )
  ; ( "pratt-postfix"
    , (* One block with three postfix shapes, each with its own kind_suffix,
         so the desugaring, the kind naming and the frame are exercised
         together. *)
      only
        ~expr:
          [ expr_block
              ~rule_name:"E"
              ~atoms:[ Token "ta" ]
              ~postfix:
                [ postfix_simple ~kind_suffix:"bang" ~token:"m" ~bp:80 ()
                ; postfix_access ~kind_suffix:"dot" ~token:"t" ~rhs:(Token "tb") ~bp:90 ()
                ; postfix_call
                    ~kind_suffix:"call"
                    ~open_tok:"lp"
                    ~close_tok:"rp"
                    ~elem:(Rule "E")
                    ~sep_policy:(with_sep "comma")
                    ~bp:100
                    ()
                ]
              ()
          ]
        [ prod "Root" [ child_req "e" (Rule "E") ] ] )
  ]
;;
