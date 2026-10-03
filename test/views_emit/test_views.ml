(* -- the views, read through their types ----------------------------------------

      What test/views_emit/law_views.ml leaves out: that an accessor reads its
      slot right. Which arm of a variant a node becomes, which token a token
      child gives, that a placeholder or a hole reads as [None], and what a
      variant's module gives back for each arm.

      Each input is parsed by the emitted parser and read through the emitted
      views with their interface, which is how a consumer holds them. The
      nested grammar has no emitted parser, so its inputs go through the
      interpreter, which test/parse_emit/law_parse.ml holds equal to one.
   -------------------------------------------------------------------------- *)

let parse (grammar : Core.Grammar.t) parse_tokens (src : string) : Siesta.Syntax.t =
  match Core.Facts.of_grammar grammar with
  | Error _ -> failwith "the grammar does not check"
  | Ok facts ->
    let green, _ = parse_tokens ?cache:None (Lex.run facts src) in
    Siesta.Syntax.of_root green
;;

let check (what : string) (ok : bool) : unit =
  if ok then Law.pass "%s" what else Law.fail "%s" what
;;

let text (token : Siesta.Syntax.token_cursor option) : string option =
  Option.map Siesta.Syntax.Token.text token
;;

(* -- json: a variant over tokens and rules, a list, a missing token ----------- *)

let json_file (src : string) : Emitted_views.Json_views.File.t =
  parse Lingo_grammars.Json_grammar.grammar Emitted_parsers.Json_parser.parse_tokens src
  |> Emitted_views.Json_views.File.cast
  |> Option.get
;;

let json_members (src : string) : Emitted_views.Json_views.Member.t list =
  let open Emitted_views.Json_views in
  match Option.bind (File.value (json_file src)) Value.kind with
  | Some (Value_kind_object o) -> Object.member o
  | _ -> []
;;

let () =
  let open Emitted_views.Json_views in
  match json_members {|{"a": 1, "b": [true]}|} with
  | [ a; b ] ->
    check "json: a member's key is its string" (text (Member.key a) = Some {|"a"|});
    check
      "json: a number value reads as the number arm"
      (match Option.bind (Member.value a) Value.kind with
       | Some (Value_kind_number n) -> Siesta.Syntax.Token.text n = "1"
       | _ -> false);
    check
      "json: an array value reads as the array arm, and its elements as a list"
      (match Option.bind (Member.value b) Value.kind with
       | Some (Value_kind_array arr) ->
         (match List.map Value.kind (Array.elt arr) with
          | [ Some (Value_kind_true _) ] -> true
          | _ -> false)
       | _ -> false)
  | members -> Law.fail "json: two members expected, %d read" (List.length members)
;;

let () =
  let open Emitted_views.Json_views in
  match json_members {|{"a" 1}|} with
  | [ m ] ->
    check "json: a missing colon reads as None" (Member.colon m = None);
    check
      "json: the value after a missing colon is still the value"
      (match Option.bind (Member.value m) Value.kind with
       | Some (Value_kind_number _) -> true
       | _ -> false)
  | members -> Law.fail "json: one member expected, %d read" (List.length members)
;;

let () =
  let open Emitted_views.Json_views in
  match json_members {|{"a": }|} with
  | [ m ] ->
    check "json: a colon before a missing value is still read" (Member.colon m <> None);
    check "json: a missing value reads as None" (Member.value m = None)
  | members -> Law.fail "json: one member expected, %d read" (List.length members)
;;

(* -- calc: the position variant, the base, a hole ------------------------------ *)

let calc_expr (src : string) : Emitted_views.Calc_views.expr_position option =
  let open Emitted_views.Calc_views in
  parse Lingo_grammars.Calc_grammar.grammar Emitted_parsers.Calc_parser.parse_tokens src
  |> File.cast
  |> Fun.flip Option.bind File.expr
;;

let () =
  let open Emitted_views.Calc_views in
  match calc_expr "1+2*3" with
  | Some (Expr_position_expr_bin b) ->
    check
      "calc: an infix operand that is a token reads as the base, holding the atom"
      (match Expr_bin.lhs b with
       | Some (Expr_position_expr e) -> text (Expr.atom e) = Some "1"
       | _ -> false);
    check "calc: the operator reads as its token" (text (Expr_bin.op b) = Some "+");
    check
      "calc: the right operand is the tighter application"
      (match Expr_bin.rhs b with
       | Some (Expr_position_expr_bin inner) -> text (Expr_bin.op inner) = Some "*"
       | _ -> false)
  | _ -> Law.fail "calc: 1+2*3 does not read as an infix application"
;;

let () =
  let open Emitted_views.Calc_views in
  match calc_expr "1+*2" with
  | Some (Expr_position_expr_bin outer) ->
    (match Expr_bin.rhs outer with
     | Some (Expr_position_expr_bin inner) ->
       check "calc: a missing left operand reads as None" (Expr_bin.lhs inner = None);
       check
         "calc: the right operand beside a missing left one stays on the right"
         (match Expr_bin.rhs inner with
          | Some (Expr_position_expr e) -> text (Expr.atom e) = Some "2"
          | _ -> false)
     | _ -> Law.fail "calc: 1+*2 has no inner application")
  | _ -> Law.fail "calc: 1+*2 does not read as an infix application"
;;

let () =
  let open Emitted_views.Calc_views in
  check
    "calc: a rule atom reads as its own arm"
    (match calc_expr "(1+2)" with
     | Some (Expr_position_parens p) ->
       (match Parens.inner p with
        | Some (Expr_position_expr_bin _) -> true
        | _ -> false)
     | _ -> false)
;;

(* -- postfix: a frame, a separated list, a missing child after a lead ---------- *)

let postfix_expr (src : string) : Emitted_views.Postfix_views.expr_position option =
  let open Emitted_views.Postfix_views in
  parse
    Lingo_grammars.Postfix_grammar.grammar
    Emitted_parsers.Postfix_parser.parse_tokens
    src
  |> File.cast
  |> Fun.flip Option.bind File.expr
;;

let () =
  let open Emitted_views.Postfix_views in
  check
    "postfix: a call's arguments read as a list, without the frame's tokens"
    (match postfix_expr "a(1, 2)" with
     | Some (Expr_position_expr_postfix_call c) ->
       List.length (Expr_postfix_call.args c) = 2 && Expr_postfix_call.operand c <> None
     | _ -> false);
  check
    "postfix: a missing child after the lead reads as None"
    (match postfix_expr "a." with
     | Some (Expr_position_expr_postfix_field f) ->
       Expr_postfix_field.rhs f = None
       && text (Expr_postfix_field.op f) = Some "."
       && Expr_postfix_field.operand f <> None
     | _ -> false)
;;

(* -- the variant modules ----------------------------------------------------------- *)

let () =
  let open Emitted_views.Calc_views in
  check
    "calc: a position's syntax is the node its arm holds"
    (match calc_expr "1+2" with
     | Some (Expr_position_expr_bin b as p) ->
       Siesta.Syntax.equal (Expr_position.syntax p) (Expr_bin.syntax b)
       &&
         (match Expr_position.elem p with
         | Siesta.Syntax.Node n -> Siesta.Syntax.equal n (Expr_bin.syntax b)
         | Siesta.Syntax.Token _ -> false)
     | _ -> false)
;;

let () =
  let open Emitted_views.Json_views in
  check
    "json: a token arm's element is the token"
    (match Option.bind (File.value (json_file "true")) Value.kind with
     | Some (Value_kind_true t as k) ->
       (match Value_kind.elem k with
        | Siesta.Syntax.Token u -> Siesta.Syntax.Token.equal t u
        | Siesta.Syntax.Node _ -> false)
     | _ -> false)
;;

(* -- nested: a block with no base, and a position as an arm ----------------------- *)

let nested_bodies (src : string) : Emitted_views.Nested_views.item_body option list =
  let open Emitted_views.Nested_views in
  match Core.Facts.of_grammar Nested_grammar.grammar with
  | Error _ -> failwith "the grammar does not check"
  | Ok facts ->
    let plan, _ = Plan.Lower.of_facts facts in
    let green, _ = Interp.run plan plan.roots.(0) (Lex.run facts src) in
    (match File.cast (Siesta.Syntax.of_root green) with
     | Some file -> List.map Item.body (File.items file)
     | None -> [])
;;

let () =
  let open Emitted_views.Nested_views in
  match nested_bodies "a + b; { c; };" with
  | [ Some (Item_body_expr (Expr_position_expr_bin b) as e)
    ; Some (Item_body_block k as body)
    ] ->
    check
      "nested: an expression arm holds the position, and its operands are rule atoms"
      (match Expr_bin.lhs b, Expr_bin.rhs b with
       | Some (Expr_position_name l), Some (Expr_position_name r) ->
         text (Name.id l) = Some "a" && text (Name.id r) = Some "b"
       | _ -> false);
    check
      "nested: a variant's syntax goes through the position it nests"
      (Siesta.Syntax.equal (Item_body.syntax e) (Expr_bin.syntax b));
    check
      "nested: the other arm is the block, and its items read through the same views"
      (Siesta.Syntax.equal (Item_body.syntax body) (Block.syntax k)
       && List.length (Block.items k) = 1)
  | bodies -> Law.fail "nested: two items expected, %d read" (List.length bodies)
;;

let () =
  let open Emitted_views.Nested_views in
  check
    "nested: an expression missing its right operand keeps the left one"
    (match nested_bodies "a +;" with
     | [ Some (Item_body_expr (Expr_position_expr_bin b)) ] ->
       Expr_bin.rhs b = None
       &&
         (match Expr_bin.lhs b with
         | Some (Expr_position_name _) -> true
         | _ -> false)
     | _ -> false)
;;

let () = Law.exit_on_failure ()
