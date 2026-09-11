(* -- name mangling ------------------------------------------------------------

      Literal expected strings. [snake_case] is not injective, so what there is
      to check is that it does what it says, over the cases that have been
      wrong before.

      The pairs at the bottom are the interesting ones. They are why
      {!Core.Manifest} groups by the emitted string, and if any of them
      stopped colliding the collision checks would have nothing to catch.
   -------------------------------------------------------------------------- *)

open Core

let failures = ref 0

let check what got want =
  if got = want
  then Printf.printf "PASS %s = %S\n" what want
  else (
    incr failures;
    Printf.printf "FAIL %s = %S, expected %S\n" what got want)
;;

let () =
  check "snake_case FooBar" (Mangle.snake_case "FooBar") "foo_bar";
  check "snake_case foo" (Mangle.snake_case "foo") "foo";
  check "snake_case Foo" (Mangle.snake_case "Foo") "foo";
  check "snake_case F" (Mangle.snake_case "F") "f";
  check "snake_case (empty)" (Mangle.snake_case "") "";
  check "snake_case Foo_Bar" (Mangle.snake_case "Foo_Bar") "foo__bar";
  check "snake_case URLPattern" (Mangle.snake_case "URLPattern") "u_r_l_pattern"
;;

let () =
  (* The acronym-aware variant exists for dotted scopes, where a run of
     capitals is one word. *)
  check
    "snake_case_acronym URLPattern"
    (Mangle.snake_case_acronym "URLPattern")
    "url_pattern";
  check
    "snake_case_acronym MatchBody"
    (Mangle.snake_case_acronym "MatchBody")
    "match_body";
  check "snake_case_acronym HTTP" (Mangle.snake_case_acronym "HTTP") "http"
;;

let () =
  check "safe_snake Match" (Mangle.safe_snake "Match") "match_";
  check "safe_snake Type" (Mangle.safe_snake "Type") "type_";
  check "safe_snake Foo" (Mangle.safe_snake "Foo") "foo";
  check "escape_reserved end" (Mangle.escape_reserved "end") "end_";
  check "escape_reserved ending" (Mangle.escape_reserved "ending") "ending"
;;

let () =
  check "upper_first FooBar" (Mangle.upper_first "FooBar") "Foo_bar";
  check "upper_first foo" (Mangle.upper_first "foo") "Foo";
  check "upper_first (empty)" (Mangle.upper_first "") "";
  check "screaming_snake FooBar" (Mangle.screaming_snake "FooBar") "FOOBAR";
  check "screaming_snake foo_bar" (Mangle.screaming_snake "foo_bar") "FOO_BAR"
;;

let () =
  let ident s want =
    let got = Mangle.is_ident s in
    if got = want
    then Printf.printf "PASS is_ident %S = %b\n" s want
    else (
      incr failures;
      Printf.printf "FAIL is_ident %S = %b, expected %b\n" s got want)
  in
  ident "foo" true;
  ident "Foo_bar'" true;
  ident "_x9" true;
  ident "" false;
  ident "9foo" false;
  ident "foo bar" false;
  ident "foo.bar" false;
  ident "foo-bar" false
;;

let () =
  (* The pairs the collision checks exist for. Each is a real mangling
     collision, and each is the reason one of the manifest's scopes is
     grouped the way it is. *)
  let collide f a b what =
    if f a = f b
    then Printf.printf "PASS %S and %S collide in %s\n" a b what
    else (
      incr failures;
      Printf.printf "FAIL %S and %S no longer collide in %s\n" a b what)
  in
  collide Mangle.safe_snake "Match" "Match_" "the parser cluster";
  collide Mangle.snake_case "FooBar" "Foo_bar" "the parser cluster";
  collide Mangle.screaming_snake "Foo_hole" "FOO_HOLE" "the kind enum";
  (* A production [Expr_Root] and a root production [Expr]: [parse_expr__root]
     from two different derivations. Grouping only [parse_fn] compared
     [parse_expr] against [parse_expr__root], found them distinct, and
     accepted a grammar whose emitted parser binds one name twice. *)
  if Mangle.snake_case "Expr_Root" = "expr__root"
  then
    Printf.printf "PASS \"Expr_Root\" snakes to the same string as Expr's root variant\n"
  else (
    incr failures;
    Printf.printf
      "FAIL \"Expr_Root\" no longer snakes to %S\n"
      (Mangle.snake_case "Expr_Root"))
;;

let () =
  if !failures = 0
  then print_endline "mangle_test: 0 failures"
  else (
    Printf.printf "mangle_test: %d failures\n" !failures;
    exit 1)
;;
