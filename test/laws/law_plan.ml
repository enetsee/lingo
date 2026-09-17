(* -- the plan IR --------------------------------------------------------------

      (a) [parse] reads what [pp] wrote, and gets the same plan back.
      (b) [pp] is deterministic at a fixed width, and printing a plan read
          back gives the same bytes.
      (c) [check] accepts a well-formed plan, and reports each malformed one
          with the problem that names what is wrong.

      Mechanism. All three are oracles over one plan built by hand, which
      reaches every instruction, every postfix body and every atom form. A
      round trip over a plan holding three of them would say nothing about
      the rest, so a new instruction goes in this plan.

      Part (c) works over a deliberately broken plan per thing the checker
      can report, and more than one where a problem has several sources.
      Each is the good plan with one thing changed, so the finding is
      attributable.

      Part (a)'s oracle is not a second printer. It reads the text back into
      a plan and compares the plans, so a field the printer drops and the
      reader defaults would have to be dropped and defaulted to the same
      value to stay hidden.

      Coverage. One plan, and twenty-one broken ones. The plan's loop reaches
      both exit policies: it may end after an element, and ending after a
      separator reports the separator as extra. The round-trip says
      nothing about a plan no [of_facts] would build, and [of_facts] does not
      exist yet: the goldens over the example grammars land with it. It is
      stated over plans that pass [check], because an empty [resume] set and
      [resume = None] print the same and [check] is what rules the first one
      out.

      Reaching every form is not the same as reaching every value. The plan
      once carried only plain names, so the printer and the reader disagreed
      about escaping and the round-trip stayed green. One child name now
      carries every character the printer escapes, and one that it does not:
      a quote, a backslash, the three named whitespace escapes, a byte below
      32, byte 127, and a byte above 127 that goes out as it stands.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        M1  In [Check.run], drop the ascending test on a set of kinds.
            -> part (c), the case with a repeated kind, and nothing else.
        M2  In [Check.run], leave an [Alt]'s arms unwalked.
            -> part (c), two cases: the empty arm and the two arms on one
               kind. The empty-alt case still reports, because that test is
               on the [Alt] itself.
        M3  In [Check.run], answer [Ok ()] whatever it found.
            -> part (c), all twenty-two broken plans.
        M4  In [Text.sexp_of_instr], drop the [at-child] field of an [expect].
            -> part (a), on the reader: an expect has five fields and four
               arrived, so the form is not an instruction.
        M5  In [Text.sexp_of_instr], print a [commit]'s [recover] set where its
            [first] set goes.
            -> part (a). The plans differ, and the output still parses, which
               is the case a reader-side check alone would miss.
        M6  In [Sexp.pp], print every atom bare.
            -> part (a). The child name in this plan has spaces in it, so the
               reader takes it as three atoms and the field has too many
               values. The plan carries that name for this reason and for no
               other.
        M7  In [Sexp.of_string], stop treating [;] as the start of a comment.
            -> part (a), the commented plan. A printed plan carries no
               comment, so only the hand-edited case reaches this.
        M8  In [Check.run], test the net depth alone and drop the lowest
            depth a branch passed through.
            -> part (c), the close-before-open case. Its opens and closes
               come to nothing overall, which is why counting them is not
               enough.
        M9  In [Check.run], leave the infix and prefix tables unwalked.
            -> part (c), four cases: the two with an operator on a kind below
               zero, and the two declaring one operator twice.
        M10 In [Check.run], leave the delimiter pairs unwalked.
            -> part (c), three cases: the two out of order and the one on a
               kind below zero. Dropping the order test alone reddens the
               first two.
        M11 In [Check.run], let [taker] record a kind without reporting one
            an earlier entry took.
            -> part (c), six cases, one per dispatch: an [Alt]'s arms, a loop
               state's accepts, and a block's atoms, infix, prefix and
               postfix tables. Nothing else, which is the point: the six are
               separate cascades and a token may sit in more than one.
        M12 In [Sexp.of_string], drop the branch that reads a [\ddd] escape.
            -> part (a), both round trips: "an escape this does not know:
               \0". The child name holds a byte below 32 and byte 127, and
               the printer has no other way to write either.

        M13 In [Text], print a [May_exit_reporting] exit as a bare [may].
            -> part (a), both round trips. The plans differ and the output
               still parses, so a reader-side check alone would miss it.

      One that reddens nothing, recorded as a finding.

        M14 In [Sexp.pp], quote with [%S] again, which is what it did before.
            -> nothing reddens. [%S] writes a byte above 127 as [\ddd], and
               the reader takes that now, so the two agree either way. The
               defect this fixed was in the reader. The writer is here so a
               name in UTF-8 appears in a dump as itself rather than as a run
               of escapes, which is a claim about reading a diff rather than
               about correctness.

   -------------------------------------------------------------------------- *)

open Plan

let failures = ref 0

let fail fmt =
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt
let msg n = Ir.Message.of_int n

(* -- one plan that reaches every form -------------------------------------- *)

(* Kinds are this law's own. 0 and 1 are the two a plan always names, 2 is
   trivia, 3 to 11 are tokens and 12 upwards are nodes. *)
let k_error = 0
let k_missing = 1
let k_ws = 2
let k_lparen = 3
let k_rparen = 4
let k_lbrack = 5
let k_rbrack = 6
let k_word = 7
let k_comma = 8
let k_plus = 9
let k_minus = 10
let k_dot = 11
let k_question = 12
let n_file = 13
let n_list = 14
let n_item = 15
let n_paren = 16
let n_base = 17
let n_prefix = 18
let n_infix = 19
let n_hole = 20
let n_index = 21
let n_access = 22
let n_try = 23

let expect ?at_child ?hole ?placeholder tok =
  Ir.Plan.Expect { tok; message = msg 1; at_child; hole; placeholder }
;;

let file : Ir.Plan.rule =
  { name = "File"
  ; kind = n_file
  ; first = [| k_lparen |]
  ; adds = [||]
  ; boundary = false
  ; body =
      Ir.Plan.Seq
        [| Ir.Plan.Open n_file; Ir.Plan.Call 1; Ir.Plan.Drain (msg 5); Ir.Plan.Close |]
  }
;;

(* A delimited body with a separator is three positions, and they admit
   different things. The states are what say so, and this one reaches both
   exit policies: the body may end after an element, and ending after a
   separator reports the separator as extra. *)
let list_rule : Ir.Plan.rule =
  { name = "List"
  ; kind = n_list
  ; first = [| k_lparen |]
  ; adds = [| k_rparen; k_comma |]
  ; boundary = false
  ; body =
      Ir.Plan.Seq
        [| Ir.Plan.Open n_list
         ; expect k_lparen
         ; Ir.Plan.Loop
             { entry = 0
             ; ends_on = Some [| k_rparen |]
             ; states =
                 [| { accepts = [| [| k_lbrack; k_word |], 1 |]
                    ; exit = Ir.Plan.May_exit
                    ; when_missing = None
                    ; emits = Ir.Plan.Call 2
                    }
                  ; { accepts = [| [| k_comma |], 2 |]
                    ; exit = Ir.Plan.May_exit
                    ; when_missing = None
                    ; emits = Ir.Plan.Bump
                    }
                  ; { accepts = [| [| k_lbrack; k_word |], 1 |]
                    ; exit = Ir.Plan.May_exit_reporting (msg 4)
                    ; when_missing = None
                    ; emits = Ir.Plan.Call 2
                    }
                 |]
             }
         ; expect k_rparen ~placeholder:k_rparen
         ; Ir.Plan.Close
        |]
  }
;;

let item : Ir.Plan.rule =
  { name = "Item"
  ; kind = n_item
  ; first = [| k_lbrack; k_word |]
  ; adds = [| k_rbrack |]
  ; boundary = true
  ; body =
      Ir.Plan.Seq
        [| Ir.Plan.Open n_item
         ; Ir.Plan.Alt
             { arms =
                 [| ( [| k_lbrack |]
                    , Ir.Plan.Seq
                        [| expect k_lbrack
                         ; Ir.Plan.Commit
                             { first = [| k_lparen; k_word |]
                             ; recover =
                                 [| k_rbrack |]
                                 (* Every escape the printer can write, so
                                    the reader has to undo every one: a
                                    quote, a backslash, the three named
                                    whitespace escapes, a byte below 32 and
                                    byte 127 as [\ddd], and a byte above 127
                                    that goes out as it stands. Spaces are
                                    what make the whole thing need quoting. *)
                             ; at_child =
                                 "the \"inner\" expression,\nna\239vely\t\r \\ \001 \127"
                             ; message = msg 2
                             ; hole = Some n_hole
                             ; placeholder = n_hole
                             ; resume = Some [| k_rbrack |]
                             ; body = Ir.Plan.Pratt { block = 0; min_bp = 0 }
                             }
                         ; expect k_rbrack ~placeholder:k_rbrack
                        |] )
                  ; [| k_word |], Ir.Plan.Seq [| Ir.Plan.Trivia; Ir.Plan.Bump |]
                 |]
             }
         ; Ir.Plan.Close
        |]
  }
;;

let paren : Ir.Plan.rule =
  { name = "Paren"
  ; kind = n_paren
  ; first = [| k_lparen |]
  ; adds = [| k_rparen |]
  ; boundary = false
  ; body =
      Ir.Plan.Seq
        [| Ir.Plan.Open n_paren
         ; expect k_lparen
         ; Ir.Plan.Pratt { block = 0; min_bp = 0 }
         ; expect k_rparen ~placeholder:k_rparen ~hole:n_hole ~at_child:"close"
         ; Ir.Plan.Close
        |]
  }
;;

let block : Ir.Plan.block =
  { name = "Expr"
  ; infix = [| k_plus, (10, 11) |]
  ; prefix = [| k_minus, 50 |]
  ; postfix =
      (* The three shapes a postfix body takes: an enclosed one, a child after
         the lead, and a lead that is the whole operator. *)
      [| { lead = k_lbrack
         ; bp = 90
         ; kind = n_index
         ; body =
             Ir.Plan.Seq
               [| Ir.Plan.Pratt { block = 0; min_bp = 0 }
                ; expect k_rbrack ~placeholder:k_rbrack
               |]
         }
       ; { lead = k_dot; bp = 80; kind = n_access; body = Ir.Plan.Bump }
       ; { lead = k_question; bp = 85; kind = n_try; body = Ir.Plan.Seq [||] }
      |]
  ; atoms = [| [| k_word |], Ir.Plan.Atom_token; [| k_lparen |], Ir.Plan.Atom_rule 3 |]
  ; base_kind = n_base
  ; prefix_kind = Some n_prefix
  ; infix_kind = Some n_infix
  ; hole_kind = n_hole
  ; expected = [| k_lparen; k_word |]
  ; message = msg 3
  }
;;

let good : Ir.Plan.t =
  { rules = [| file; list_rule; item; paren |]
  ; blocks = [| block |]
  ; roots = [| 0 |]
  ; pairs = [| k_lparen, k_rparen; k_lbrack, k_rbrack |]
  ; trivia = [| k_ws |]
  ; error_kind = k_error
  ; missing_kind = k_missing
  }
;;

(* -- (a) and (b) ----------------------------------------------------------- *)

let () =
  match Check.run good with
  | Error problems ->
    fail
      "check rejected the good plan: %a"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "; ") Check.pp_problem)
      problems
  | Ok () -> pass "check accepts a well-formed plan"
;;

(* [Text.pp] breaks a form that does not fit, and the width it fits to comes
   from the formatter. Pinning it here keeps part (b) about the printer. *)
let render (p : Ir.Plan.t) =
  let b = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt 78;
  Text.pp fmt p;
  Format.pp_print_flush fmt ();
  Buffer.contents b
;;

let text = render good

let () =
  match Text.parse text with
  | Error m -> fail "(a) the printer's own output did not parse: %s" m
  | Ok back when back <> good -> fail "(a) the plan that came back is not the one printed"
  | Ok back ->
    let again = render back in
    if not (String.equal again text)
    then fail "(b) printing the plan a second time gave different bytes"
    else
      pass
        "a plan prints, reads back and prints the same, over %d bytes"
        (String.length text)
;;

(* A plan is read back with comments and odd whitespace in it, because a
   plan someone edits by hand has both. The printer writes neither. *)
let () =
  let edited = "; a comment\n" ^ text ^ "\n  ; and another\n" in
  match Text.parse edited with
  | Error m -> fail "(a) a commented plan did not parse: %s" m
  | Ok back when back <> good -> fail "(a) a commented plan read back as a different plan"
  | Ok _ -> pass "comments and surrounding whitespace do not change what is read"
;;

(* -- (c) one broken plan per problem --------------------------------------- *)

(* Each is the good plan with one thing changed. The other three rules stay,
   so a finding names the change rather than what went missing with it. *)
let with_body body : Ir.Plan.t =
  { good with rules = [| { file with body }; list_rule; item; paren |] }
;;

let with_file f : Ir.Plan.t = { good with rules = [| f; list_rule; item; paren |] }
let open_ body = Ir.Plan.Seq [| Ir.Plan.Open n_file; body; Ir.Plan.Close |]

let broken =
  [ ( "a call to a rule that is not there"
    , with_body (open_ (Ir.Plan.Call 99))
    , function
      | Check.Rule_out_of_range _ -> true
      | _ -> false )
  ; ( "a pratt call to a block that is not there"
    , with_body (open_ (Ir.Plan.Pratt { block = 99; min_bp = 0 }))
    , function
      | Check.Block_out_of_range _ -> true
      | _ -> false )
  ; ( "a set of kinds that repeats one"
    , with_file { file with first = [| k_lparen; k_lparen |] }
    , function
      | Check.Kinds_unordered _ -> true
      | _ -> false )
  ; ( "a negative kind"
    , with_file { file with kind = -1 }
    , function
      | Check.Negative_kind _ -> true
      | _ -> false )
  ; ( "an alt with no arms"
    , with_body (open_ (Ir.Plan.Alt { arms = [||] }))
    , function
      | Check.Empty_alt _ -> true
      | _ -> false )
  ; ( "an arm no kind can take"
    , with_body (open_ (Ir.Plan.Alt { arms = [| [||], Ir.Plan.Bump |] }))
    , function
      | Check.Empty_arm _ -> true
      | _ -> false )
  ; ( "two alt arms on the same kind"
    , with_body
        (open_
           (Ir.Plan.Alt
              { arms = [| [| k_word |], Ir.Plan.Bump; [| k_word |], Ir.Plan.Bump |] }))
    , function
      | Check.Kind_taken_twice _ -> true
      | _ -> false )
  ; ( "one loop state accepting a kind twice"
    , with_body
        (open_
           (Ir.Plan.Loop
              { entry = 0
              ; ends_on = None
              ; states =
                  [| { accepts = [| [| k_word |], 0; [| k_word |], 0 |]
                     ; exit = Ir.Plan.May_exit
                     ; when_missing = None
                     ; emits = Ir.Plan.Bump
                     }
                  |]
              }))
    , function
      | Check.Kind_taken_twice _ -> true
      | _ -> false )
  ; ( "two atoms on the same kind"
    , { good with
        blocks =
          [| { block with
               atoms =
                 [| [| k_word |], Ir.Plan.Atom_token; [| k_word |], Ir.Plan.Atom_rule 3 |]
             }
          |]
      }
    , function
      | Check.Kind_taken_twice _ -> true
      | _ -> false )
  ; ( "the same infix operator twice"
    , { good with
        blocks = [| { block with infix = [| k_plus, (10, 11); k_plus, (20, 21) |] } |]
      }
    , function
      | Check.Kind_taken_twice _ -> true
      | _ -> false )
  ; ( "the same prefix operator twice"
    , { good with blocks = [| { block with prefix = [| k_minus, 50; k_minus, 60 |] } |] }
    , function
      | Check.Kind_taken_twice _ -> true
      | _ -> false )
  ; ( "two postfix operators on the same lead"
    , { good with
        blocks =
          [| { block with
               postfix =
                 [| { lead = k_dot; bp = 80; kind = n_access; body = Ir.Plan.Bump }
                  ; { lead = k_dot; bp = 85; kind = n_try; body = Ir.Plan.Seq [||] }
                 |]
             }
          |]
      }
    , function
      | Check.Kind_taken_twice _ -> true
      | _ -> false )
  ; ( "a loop entering a state that is not there"
    , with_body
        (open_
           (Ir.Plan.Loop
              { entry = 9
              ; ends_on = None
              ; states =
                  [| { accepts = [||]
                     ; exit = Ir.Plan.May_exit
                     ; when_missing = None
                     ; emits = Ir.Plan.Bump
                     }
                  |]
              }))
    , function
      | Check.Loop_state_out_of_range _ -> true
      | _ -> false )
  ; ( "a resume set with nothing in it"
    , with_body
        (open_
           (Ir.Plan.Commit
              { first = [| k_word |]
              ; recover = [||]
              ; at_child = "x"
              ; message = msg 1
              ; hole = None
              ; placeholder = n_hole
              ; resume = Some [||]
              ; body = Ir.Plan.Bump
              }))
    , function
      | Check.Empty_resume _ -> true
      | _ -> false )
  ; ( "a node opened and not closed"
    , with_body (Ir.Plan.Seq [| Ir.Plan.Open n_file; Ir.Plan.Bump |])
    , function
      | Check.Unbalanced _ -> true
      | _ -> false )
  ; ( "a node closed before any was opened"
    , with_body (Ir.Plan.Seq [| Ir.Plan.Close; Ir.Plan.Open n_file |])
    , function
      | Check.Close_without_open _ -> true
      | _ -> false )
  ; ( "an infix operator on a kind below zero"
    , { good with blocks = [| { block with infix = [| -5, (10, 11) |] } |] }
    , function
      | Check.Negative_kind _ -> true
      | _ -> false )
  ; ( "a prefix operator on a kind below zero"
    , { good with blocks = [| { block with prefix = [| -5, 50 |] } |] }
    , function
      | Check.Negative_kind _ -> true
      | _ -> false )
  ; ( "delimiter pairs the wrong way round"
    , { good with pairs = [| k_lbrack, k_rbrack; k_lparen, k_rparen |] }
    , function
      | Check.Pairs_unordered _ -> true
      | _ -> false )
  ; ( "the same delimiter pair twice"
    , { good with
        pairs = [| k_lparen, k_rparen; k_lparen, k_rparen; k_lbrack, k_rbrack |]
      }
    , function
      | Check.Pairs_unordered _ -> true
      | _ -> false )
  ; ( "a delimiter pair on a kind below zero"
    , { good with pairs = [| -1, k_rparen; k_lbrack, k_rbrack |] }
    , function
      | Check.Negative_kind _ -> true
      | _ -> false )
  ]
;;

let () =
  let wrong = ref 0 in
  List.iter
    (fun (what, plan, is_it) ->
       match Check.run plan with
       | Ok () ->
         incr wrong;
         fail "(c) check accepted %s" what
       | Error problems ->
         if not (List.exists is_it problems)
         then (
           incr wrong;
           fail
             "(c) check reported the wrong thing about %s: %a"
             what
             (Format.pp_print_list
                ~pp_sep:(fun f () -> Format.fprintf f "; ")
                Check.pp_problem)
             problems))
    broken;
  if !wrong = 0
  then
    pass
      "check reports each of %d broken plans with the right problem"
      (List.length broken)
;;

let () =
  if !failures = 0
  then print_endline "law_plan: 0 failures"
  else (
    Printf.printf "law_plan: %d failures\n" !failures;
    exit 1)
;;
