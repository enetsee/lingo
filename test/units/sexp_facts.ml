(* -- sexp's facts, by hand ----------------------------------------------------

      Literal expected values, worked out on paper from grammars/sexp_grammar.ml
      and typed in. Nothing regenerates this file, and nothing in it came from
      running the code it checks. Where the checker and this file disagree, one
      of them is wrong and the argument has to be made.

      What it pins:

      - the kind numbering, in full and in order. The numbering follows the
        manifest's declaration order, so changing that order renumbers every
        kind in every emitted artefact and shows up here.
      - FIRST, FOLLOW and nullability for all three rules.
      - the recovery set inside the delimited body.

      The derivation, so the numbers can be checked without running anything.

        nullable   File   root:Sexp is required and Sexp takes a token  -> false
                   Sexp   kind is required and no alternative is nullable -> false
                   Group  a delimited frame takes its open token         -> false

        FIRST      Group  delimited: the open token              = {lparen}
                   Sexp   the union over its alternatives:
                            ident, number, FIRST(Group)
                                           = {ident, number, lparen}
                   File   root is required and takes a token, so FIRST(Sexp)
                                           = {ident, number, lparen}

        FOLLOW     File   a root, referenced nowhere             = {}
                   Sexp   from File.root: nothing remains and File is plain,
                            so follow(File)                      = {}
                          from Group.elt: repeated with no separator, so the
                            next thing is another element, FIRST(Sexp); nothing
                            remains after it and Group is delimited, so its
                            close token bounds the body
                                           = {ident, number, lparen, rparen}
                   Group  from Sexp.kind: nothing remains and Sexp is plain, so
                            follow(Sexp)
                                           = {ident, number, lparen, rparen}

        recovery at Group.elt
                   nothing remains inside the rule       local  = {}
                   the frame's closer                    frame  = {rparen}
                   the position is trailing, so follow(Group)
                                           = {ident, number, lparen, rparen}
                                    union  = {ident, number, lparen, rparen}

        recovery at File.root
                   nothing remains, no frame, follow(File) = {}
                                           = {}
                   A root's only child has nowhere to resume, so a failure
                   there drains to end of input. Pinned because this is the one
                   position where an empty recovery set is the answer.
   -------------------------------------------------------------------------- *)

open Core

let failures = ref 0

let fail fmt =
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt

let f =
  match Facts.of_grammar Lingo_grammars.Sexp_grammar.grammar with
  | Ok f -> f
  | Error es ->
    Format.printf "FAIL sexp was rejected:@\n%a@." Error.pp_list es;
    exit 1
;;

(* -- the kind numbering ---------------------------------------------------- *)

let expected_kinds =
  [ "N_FILE"
  ; "N_SEXP"
  ; "N_GROUP"
  ; "T_LPAREN"
  ; "T_RPAREN"
  ; "T_IDENT"
  ; "T_NUMBER"
  ; "T_WS"
  ; "N_FILE_HOLE"
  ; "N_SEXP_HOLE"
  ; "N_GROUP_HOLE"
  ; "T_ERROR"
  ; "N_ERROR"
  ; "N_MISSING"
  ; "T_UNTERMINATED"
  ]
;;

let () =
  let got = List.map Kind.Name.to_string (Kind.Table.names f.kinds) in
  if got = expected_kinds
  then pass "kind numbering: 15 kinds, in the order written down"
  else
    fail
      "kind numbering differs.@\n  expected: %s@\n  got:      %s"
      (String.concat " " expected_kinds)
      (String.concat " " got)
;;

(* Over the table's own names, since the list above has just been pinned equal
   to them. Reaching [find] with a literal would want a way to build a
   [Kind.Name.t] from a spelling, and there is none. *)
let () =
  List.iteri
    (fun i n ->
       let spelled = Kind.Name.to_string n in
       match Facts.find_kind f n with
       | Some k when Kind.to_int k = i -> ()
       | Some k -> fail "%s numbers %d, not %d" spelled (Kind.to_int k) i
       | None -> fail "%s is missing from the table" spelled)
    (Kind.Table.names f.kinds)
;;

(* -- the rules ------------------------------------------------------------- *)

let rule name =
  match Facts.find_kind f (Kind.Name.node name) with
  | None ->
    fail "no kind for rule %s" name;
    exit 1
  | Some k ->
    (match Facts.rule_of_kind f k with
     | Some d -> d
     | None ->
       fail "no rule for kind of %s" name;
       exit 1)
;;

let file = rule "File"
let sexp = rule "Sexp"
let group = rule "Group"

let () =
  if file.id = 0 && sexp.id = 1 && group.id = 2
  then pass "rule ids: File=0 Sexp=1 Group=2"
  else fail "rule ids are %d %d %d, not 0 1 2" file.id sexp.id group.id
;;

let set_names s =
  List.sort
    String.compare
    (List.map (fun k -> Kind.Name.to_string (Facts.kind_name f k)) (Kind.Set.elements s))
;;

let expect what got want =
  let want = List.sort String.compare want in
  if got = want
  then pass "%s = {%s}" what (String.concat ", " want)
  else
    fail
      "%s = {%s}, expected {%s}"
      what
      (String.concat ", " got)
      (String.concat ", " want)
;;

let () =
  List.iter
    (fun ((d : Rule.def), want) ->
       if Facts.is_nullable f d.id = want
       then pass "nullable(%s) = %b" (Grammar.Name.Rule.to_string d.name) want
       else
         fail
           "nullable(%s) = %b, expected %b"
           (Grammar.Name.Rule.to_string d.name)
           (not want)
           want)
    [ file, false; sexp, false; group, false ]
;;

let () =
  expect
    "FIRST(File)"
    (set_names (Facts.first_of f file.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN" ];
  expect
    "FIRST(Sexp)"
    (set_names (Facts.first_of f sexp.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN" ];
  expect "FIRST(Group)" (set_names (Facts.first_of f group.id)) [ "T_LPAREN" ]
;;

let () =
  expect "FOLLOW(File)" (set_names (Facts.follow_of f file.id)) [];
  expect
    "FOLLOW(Sexp)"
    (set_names (Facts.follow_of f sexp.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN"; "T_RPAREN" ];
  expect
    "FOLLOW(Group)"
    (set_names (Facts.follow_of f group.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN"; "T_RPAREN" ]
;;

(* -- shape and framing ----------------------------------------------------- *)

let () =
  (match group.frame with
   | Rule.Delimited { open_; close; sep = None; boundary = false }
     when Kind.Name.equal (Facts.kind_name f open_) (Kind.Name.token "lparen")
          && Kind.Name.equal (Facts.kind_name f close) (Kind.Name.token "rparen") ->
     pass "Group is delimited by lparen .. rparen with no separator"
   | _ -> fail "Group's frame is not the delimited lparen .. rparen it was declared as");
  if group.body_from = 0
  then pass "Group's body starts at child 0, as every production's does"
  else fail "Group's body_from is %d, not 0" group.body_from;
  if Array.length group.children = 1 && group.children.(0).modifier = Grammar.Repeated
  then pass "Group wraps one repeated child"
  else fail "Group does not wrap exactly one repeated child"
;;

let () =
  match sexp.children.(0).alts with
  | [| a; b; c |] ->
    let ns = List.map (fun k -> Kind.Name.to_string (Facts.kind_name f k)) [ a; b; c ] in
    if ns = [ "T_IDENT"; "T_NUMBER"; "N_GROUP" ]
    then pass "Sexp.kind keeps its alternatives in declaration order"
    else
      fail
        "Sexp.kind's alternatives are %s, not the declared order"
        (String.concat " " ns)
  | _ -> fail "Sexp.kind does not have three alternatives"
;;

(* -- trivia ---------------------------------------------------------------- *)

let () =
  let ws = Option.get (Facts.find_kind f (Kind.Name.token "ws")) in
  if Facts.is_trivia_kind f ws && Kind.Set.cardinal f.trivia = 1
  then pass "trivia is exactly {T_WS}"
  else fail "trivia is %s, expected {T_WS}" (String.concat ", " (set_names f.trivia))
;;

(* -- the recovery equation ------------------------------------------------- *)

let () =
  expect
    "recover(Group.elt)"
    (set_names (Facts.recovery_set f group.id ~child:0))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN"; "T_RPAREN" ];
  expect "recover(File.root)" (set_names (Facts.recovery_set f file.id ~child:0)) []
;;

let () =
  if !failures = 0
  then print_endline "sexp_facts: 0 failures"
  else (
    Printf.printf "sexp_facts: %d failures\n" !failures;
    exit 1)
;;
