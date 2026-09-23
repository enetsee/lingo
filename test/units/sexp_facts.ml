(* -- sexp's facts, by hand ----------------------------------------------------

      Literal expected values, worked out on paper from grammars/sexp_grammar.ml
      and typed in. Nothing regenerates this file, and nothing in it came from
      running the code it checks. Where the checker and this file disagree, one
      of them is wrong and the argument has to be made.

      What it checks:

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
                   there drains to end of input. Checked because this is the
                   one position where an empty recovery set is right.
   -------------------------------------------------------------------------- *)

let f =
  match Core.Facts.of_grammar Lingo_grammars.Sexp_grammar.grammar with
  | Ok f -> f
  | Error es ->
    Format.printf "FAIL sexp was rejected:@\n%a@." Core.Error.pp_list es;
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
  let got = List.map Core.Kind.Name.to_string (Core.Kind.Table.names f.kinds) in
  if got = expected_kinds
  then Law.pass "kind numbering: 15 kinds, in the order written down"
  else
    Law.fail
      "kind numbering differs.@\n  expected: %s@\n  got:      %s"
      (String.concat " " expected_kinds)
      (String.concat " " got)
;;

(* Over the table's own names, since the list above has just been checked
   equal to them. Reaching [find] with a literal would need a way to build a
   [Kind.Name.t] from a spelling, and there is none. *)
let () =
  List.iteri
    (fun i n ->
       let spelled = Core.Kind.Name.to_string n in
       match Core.Facts.find_kind f n with
       | Some k when Core.Kind.to_int k = i -> ()
       | Some k -> Law.fail "%s numbers %d, not %d" spelled (Core.Kind.to_int k) i
       | None -> Law.fail "%s is missing from the table" spelled)
    (Core.Kind.Table.names f.kinds)
;;

(* -- the rules ------------------------------------------------------------- *)

let rule (name : string) : Core.Rule.def =
  match Core.Facts.find_kind f (Core.Kind.Name.node name) with
  | None ->
    Law.fail "no kind for rule %s" name;
    exit 1
  | Some k ->
    (match Core.Facts.rule_of_kind f k with
     | Some d -> d
     | None ->
       Law.fail "no rule for kind of %s" name;
       exit 1)
;;

let file = rule "File"
let sexp = rule "Sexp"
let group = rule "Group"

let () =
  if file.id = 0 && sexp.id = 1 && group.id = 2
  then Law.pass "rule ids: File=0 Sexp=1 Group=2"
  else Law.fail "rule ids are %d %d %d, not 0 1 2" file.id sexp.id group.id
;;

let set_names (s : Core.Kind.Set.t) : string list =
  List.sort
    String.compare
    (List.map
       (fun k -> Core.Kind.Name.to_string (Core.Facts.kind_name f k))
       (Core.Kind.Set.elements s))
;;

let expect (what : string) (got : string list) (expected : string list) : unit =
  let expected = List.sort String.compare expected in
  if got = expected
  then Law.pass "%s = {%s}" what (String.concat ", " expected)
  else
    Law.fail
      "%s = {%s}, expected {%s}"
      what
      (String.concat ", " got)
      (String.concat ", " expected)
;;

let () =
  List.iter
    (fun ((d : Core.Rule.def), expected) ->
       if Core.Facts.is_nullable f d.id = expected
       then Law.pass "nullable(%s) = %b" (Grammar.Name.Rule.to_string d.name) expected
       else
         Law.fail
           "nullable(%s) = %b, expected %b"
           (Grammar.Name.Rule.to_string d.name)
           (not expected)
           expected)
    [ file, false; sexp, false; group, false ]
;;

let () =
  expect
    "FIRST(File)"
    (set_names (Core.Facts.first_of f file.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN" ];
  expect
    "FIRST(Sexp)"
    (set_names (Core.Facts.first_of f sexp.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN" ];
  expect "FIRST(Group)" (set_names (Core.Facts.first_of f group.id)) [ "T_LPAREN" ]
;;

let () =
  expect "FOLLOW(File)" (set_names (Core.Facts.follow_of f file.id)) [];
  expect
    "FOLLOW(Sexp)"
    (set_names (Core.Facts.follow_of f sexp.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN"; "T_RPAREN" ];
  expect
    "FOLLOW(Group)"
    (set_names (Core.Facts.follow_of f group.id))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN"; "T_RPAREN" ]
;;

(* -- shape and framing ----------------------------------------------------- *)

let () =
  (match group.frame with
   | Core.Rule.Delimited { open_; close; sep = None; boundary = false }
     when Core.Kind.Name.equal
            (Core.Facts.kind_name f open_)
            (Core.Kind.Name.token "lparen")
          && Core.Kind.Name.equal
               (Core.Facts.kind_name f close)
               (Core.Kind.Name.token "rparen") ->
     Law.pass "Group is delimited by lparen .. rparen with no separator"
   | _ ->
     Law.fail "Group's frame is not the delimited lparen .. rparen it was declared as");
  if group.body_from = 0
  then Law.pass "Group's body starts at child 0, as every production's does"
  else Law.fail "Group's body_from is %d, not 0" group.body_from;
  if Array.length group.children = 1 && group.children.(0).modifier = Grammar.Zero_or_more
  then Law.pass "Group wraps one repeated child"
  else Law.fail "Group does not wrap exactly one repeated child"
;;

let () =
  match sexp.children.(0).alts with
  | [| a; b; c |] ->
    let ns =
      List.map (fun k -> Core.Kind.Name.to_string (Core.Facts.kind_name f k)) [ a; b; c ]
    in
    if ns = [ "T_IDENT"; "T_NUMBER"; "N_GROUP" ]
    then Law.pass "Sexp.kind keeps its alternatives in declaration order"
    else
      Law.fail
        "Sexp.kind's alternatives are %s rather than the declared order"
        (String.concat " " ns)
  | _ -> Law.fail "Sexp.kind does not have three alternatives"
;;

(* -- trivia ---------------------------------------------------------------- *)

let () =
  let ws = Option.get (Core.Facts.find_kind f (Core.Kind.Name.token "ws")) in
  if Core.Facts.is_trivia_kind f ws && Core.Kind.Set.cardinal f.trivia = 1
  then Law.pass "trivia is exactly {T_WS}"
  else Law.fail "trivia is %s, expected {T_WS}" (String.concat ", " (set_names f.trivia))
;;

(* -- the recovery equation ------------------------------------------------- *)

let () =
  expect
    "recover(Group.elt)"
    (set_names (Core.Facts.recovery_set f group.id ~child:0))
    [ "T_IDENT"; "T_NUMBER"; "T_LPAREN"; "T_RPAREN" ];
  expect "recover(File.root)" (set_names (Core.Facts.recovery_set f file.id ~child:0)) []
;;

let () = Law.summarise "sexp_facts"
