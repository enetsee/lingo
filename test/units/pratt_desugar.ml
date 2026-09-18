(* -- desugaring a postfix operator --------------------------------------------

      Literal expected values for the shape an expression block desugars into.

      A production delimited by [lp .. rp] with a comma separator, and a
      postfix call written with the same tokens, produce the same frame value
      and the same body for a fold to walk. The one difference is where the
      body starts: at child 0 for a production, at child 1 for a postfix, since
      the operand the operator applies to sits before the frame opens. A fold
      taking that index serves both.

      This file was written because a mutation found nothing. Changing the
      desugared postfix frame from [Delimited] to [Plain], and its [body_from]
      from 1 to 0, both left the suite green, and those two fields carry the
      whole of the shared frame.

      Mutations. All four were applied, run and reverted, and the result
      recorded is the one observed.

        M1  In [Stage.shape], give an [Enclosed] postfix [Rule.Plain] in place
            of the delimited frame.
            -> this file. The frames stop being equal.
        M2  In [Stage.shape], set an [Enclosed] postfix's [body_from] to 0.
            -> this file. The operand lands inside the body.
        M3  In [Role.kind_suffix], drop the suffix from a postfix
            role's kind name, so all four collide.
            -> law_validate, law_manifest, law_first_follow and law_facts:
               every corpus grammar with more than one postfix operator stops
               being accepted, so this file does not run. The kind names are
               pinned by the roles assertion here, which M4 reddens.
        M4  In [Stage.names], stop filtering role slots by [role_is_active], so
            a block with no infix operators gains an [EBin] rule whose operator
            child has no alternatives.
            -> this file. Two rules appear that describe no node.

      Coverage. A single grammar, which carries all four postfix bodies and a
      production
      framed with the same delimiters as one of them. It pins the shape of the
      desugaring. Whether a parser built from that shape parses [f(a, b)] is a
      question for the machine that consumes it.
   -------------------------------------------------------------------------- *)

open Core
open Core.Grammar

let failures = ref 0

let fail fmt =
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt

let grammar =
  create
    ~tokens:
      [ punct_tight ~name:"lp" "("
      ; punct_tight ~name:"rp" ")"
      ; punct_tight ~name:"lb" "["
      ; punct_tight ~name:"rb" "]"
      ; punct_tight ~name:"comma" ","
      ; punct_tight ~name:"dot" "."
      ; punct_tight ~name:"bang" "!"
      ; punct_tight ~name:"ta" "a"
      ]
    ~roots:[ "Root"; "List" ]
    ~expr:
      [ expr_block
          ~rule_name:"E"
          ~atoms:[ Token "ta" ]
          ~postfix:
            [ postfix_call
                ~kind_suffix:"call"
                ~open_tok:"lp"
                ~close_tok:"rp"
                ~elem:(Rule "E")
                ~sep_policy:(with_sep "comma")
                ~bp:100
                ()
            ; postfix_index
                ~kind_suffix:"idx"
                ~open_tok:"lb"
                ~close_tok:"rb"
                ~index:(Rule "E")
                ~bp:90
                ()
            ; postfix_access ~kind_suffix:"dot" ~token:"dot" ~rhs:(Token "ta") ~bp:95 ()
            ; postfix_simple ~kind_suffix:"bang" ~token:"bang" ~bp:110 ()
            ]
          ()
      ]
    [ prod "Root" [ child_req "e" (Rule "E") ]
      (* The same delimiters and the same separator as the postfix call
         above, so the two frames are directly comparable. *)
    ; prod "List" [ child_rep "items" (Rule "E") ]
      |> with_delimited_sep ~open_tok:"lp" ~close_tok:"rp" ~sep:"comma"
    ]
;;

let f =
  match Facts.of_grammar grammar with
  | Ok f -> f
  | Error es ->
    Format.printf "FAIL the desugaring grammar was rejected:@\n%a@." Error.pp_list es;
    exit 1
;;

let rule_named (n : string) =
  let n = Grammar.Name.Rule.of_string n in
  let r = ref None in
  Array.iter
    (fun (d : Rule.def) ->
       if Grammar.Name.Rule.equal d.name n && !r = None then r := Some d)
    f.rules;
  match !r with
  | Some d -> d
  | None ->
    fail "no rule named %s" (Grammar.Name.Rule.to_string n);
    exit 1
;;

(* -- the roles the block desugars into ------------------------------------- *)

let () =
  let roles =
    Array.to_list f.rules
    |> List.filter (fun (d : Rule.def) -> Rule.is_synthetic d)
    |> List.map (fun (d : Rule.def) ->
      Name.Rule.to_string d.name, Kind.Name.to_string (Facts.kind_name f d.kind))
  in
  let expected =
    [ "EPostfixCall", "N_E_POSTFIX_CALL"
    ; "EPostfixIdx", "N_E_POSTFIX_IDX"
    ; "EPostfixDot", "N_E_POSTFIX_DOT"
    ; "EPostfixBang", "N_E_POSTFIX_BANG"
    ]
  in
  if roles = expected
  then
    pass
      "one rule per postfix operator, named by its kind_suffix, and none for the \
       inactive Bin and Prefix roles"
  else
    fail
      "roles are %s, expected %s"
      (String.concat " " (List.map (fun (a, b) -> a ^ "/" ^ b) roles))
      (String.concat " " (List.map (fun (a, b) -> a ^ "/" ^ b) expected))
;;

(* The block itself carries the base role: a block's base kind and the kind a
   token atom produces are the same kind, so it takes no role of its own. *)
let () =
  let e = rule_named "E" in
  if e.origin = Rule.Pratt_block && Kind.Name.to_string (Facts.kind_name f e.kind) = "N_E"
  then pass "the block rule carries the base role"
  else fail "the block rule is not the base role"
;;

(* -- the claim ------------------------------------------------------------- *)

let list_rule = rule_named "List"
let call_rule = rule_named "EPostfixCall"

let () =
  if list_rule.frame = call_rule.frame
  then
    pass
      "a delimited production and an enclosed postfix written with the same tokens \
       produce the same frame"
  else
    fail
      "the frames differ: %s vs %s"
      (Format.asprintf "%a" Facts.pp f |> fun _ -> "List")
      "EPostfixCall"
;;

let () =
  if list_rule.body_from = 0 && call_rule.body_from = 1
  then
    pass
      "body_from is 0 for the production and 1 for the postfix, and that is the whole \
       difference"
  else
    fail
      "body_from is %d for List and %d for EPostfixCall, expected 0 and 1"
      list_rule.body_from
      call_rule.body_from
;;

let describe (c : Rule.child) =
  Printf.sprintf
    "%s%s:%s"
    (Name.Child.to_string c.child_name)
    (match c.modifier with
     | Exactly_one -> "1"
     | Zero_or_one -> "?"
     | Zero_or_more -> "*"
     | One_or_more -> "+")
    (String.concat
       "|"
       (List.map
          (fun k -> Kind.Name.to_string (Facts.kind_name f k))
          (Array.to_list c.alts)))
;;

let () =
  (* What a fold walking the frame sees. The names differ -- an author's
     [items] against a synthesised [args] -- and nothing downstream reads
     them; the shape is what the fold consumes and it is identical. *)
  let body d = List.map (fun c -> (describe c : string)) (Rule.body_children d) in
  let l = body list_rule
  and c = body call_rule in
  let strip s =
    String.sub s (String.index s ':') (String.length s - String.index s ':')
  in
  if List.map strip l = List.map strip c && List.length l = 1
  then
    pass
      "both frames wrap one repeated child of the same kind: %s / %s"
      (List.hd l)
      (List.hd c)
  else
    fail
      "the framed bodies differ: [%s] vs [%s]"
      (String.concat " " l)
      (String.concat " " c)
;;

let () =
  match Array.to_list call_rule.children with
  | [ operand; _ ] when describe operand = "operand1:N_E" ->
    pass "the postfix operand sits before the frame, at child 0"
  | cs ->
    fail "EPostfixCall's children are [%s]" (String.concat " " (List.map describe cs))
;;

(* -- the other three bodies ------------------------------------------------ *)

let () =
  let expect name want =
    let got = List.map describe (Array.to_list (rule_named name).children) in
    if got = want
    then pass "%s = [%s]" name (String.concat " " want)
    else
      fail
        "%s = [%s], expected [%s]"
        name
        (String.concat " " got)
        (String.concat " " want)
  in
  expect "EPostfixBang" [ "operand1:N_E"; "op1:T_BANG" ];
  expect "EPostfixDot" [ "operand1:N_E"; "op1:T_DOT"; "rhs1:T_TA" ];
  expect "EPostfixIdx" [ "operand1:N_E"; "body1:N_E" ];
  expect "EPostfixCall" [ "operand1:N_E"; "args*:N_E" ]
;;

let () =
  (* [Nothing] and [Then] are not enclosed, so they carry no frame and their
     lead token is an ordinary child. That is the other half of the collapse:
     the shape says which of the two it is, and nothing has to remember. *)
  let plain n =
    if (rule_named n).frame = Rule.Plain && (rule_named n).body_from = 0
    then pass "%s is plain and starts at child 0" n
    else fail "%s should carry no frame" n
  in
  plain "EPostfixBang";
  plain "EPostfixDot";
  match (rule_named "EPostfixIdx").frame with
  | Rule.Delimited { open_; close; sep = None; _ }
    when Kind.Name.equal (Facts.kind_name f open_) (Kind.Name.token "lb")
         && Kind.Name.equal (Facts.kind_name f close) (Kind.Name.token "rb") ->
    pass "EPostfixIdx is delimited by lb .. rb with no separator"
  | _ -> fail "EPostfixIdx does not carry the lb .. rb frame"
;;

let () =
  if !failures = 0
  then print_endline "pratt_desugar: 0 failures"
  else (
    Printf.printf "pratt_desugar: %d failures\n" !failures;
    exit 1)
;;
