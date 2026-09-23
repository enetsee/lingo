(* -- name mangling ------------------------------------------------------------

      Literal expected strings. [snake_case] is not injective, so what there is
      to check is that it does what it says, over the cases that have been
      wrong before.

      The pairs at the bottom are the interesting ones. They are why
      {!Core.Manifest} groups by the emitted string, and if any of them
      stopped colliding the collision checks would have nothing to catch.
   -------------------------------------------------------------------------- *)

let check (what : string) (got : string) (expected : string) : unit =
  if got = expected
  then Law.pass "%s = %S" what expected
  else Law.fail "%s = %S, expected %S" what got expected
;;

let () =
  check "snake_case FooBar" (Core.Mangle.snake_case "FooBar") "foo_bar";
  check "snake_case foo" (Core.Mangle.snake_case "foo") "foo";
  check "snake_case Foo" (Core.Mangle.snake_case "Foo") "foo";
  check "snake_case F" (Core.Mangle.snake_case "F") "f";
  check "snake_case (empty)" (Core.Mangle.snake_case "") "";
  check "snake_case Foo_Bar" (Core.Mangle.snake_case "Foo_Bar") "foo__bar";
  check "snake_case URLPattern" (Core.Mangle.snake_case "URLPattern") "u_r_l_pattern"
;;

let () =
  (* The acronym-aware variant exists for dotted scopes, where a run of
     capitals is one word. *)
  check
    "snake_case_acronym URLPattern"
    (Core.Mangle.snake_case_acronym "URLPattern")
    "url_pattern";
  check
    "snake_case_acronym MatchBody"
    (Core.Mangle.snake_case_acronym "MatchBody")
    "match_body";
  check "snake_case_acronym HTTP" (Core.Mangle.snake_case_acronym "HTTP") "http"
;;

let () =
  check "safe_snake Match" (Core.Mangle.safe_snake "Match") "match_";
  check "safe_snake Type" (Core.Mangle.safe_snake "Type") "type_";
  check "safe_snake Foo" (Core.Mangle.safe_snake "Foo") "foo";
  check "escape_reserved end" (Core.Mangle.escape_reserved "end") "end_";
  check "escape_reserved ending" (Core.Mangle.escape_reserved "ending") "ending"
;;

let () =
  check "upper_first FooBar" (Core.Mangle.upper_first "FooBar") "Foo_bar";
  check "upper_first foo" (Core.Mangle.upper_first "foo") "Foo";
  check "upper_first (empty)" (Core.Mangle.upper_first "") "";
  check "screaming_snake FooBar" (Core.Mangle.screaming_snake "FooBar") "FOOBAR";
  check "screaming_snake foo_bar" (Core.Mangle.screaming_snake "foo_bar") "FOO_BAR"
;;

let () =
  let ident s expected =
    let got = Core.Mangle.is_ident s in
    if got = expected
    then Law.pass "is_ident %S = %b" s expected
    else Law.fail "is_ident %S = %b, expected %b" s got expected
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
    then Law.pass "%S and %S collide in %s" a b what
    else Law.fail "%S and %S no longer collide in %s" a b what
  in
  collide Core.Mangle.safe_snake "Match" "Match_" "the parser cluster";
  collide Core.Mangle.snake_case "FooBar" "Foo_bar" "the parser cluster";
  collide Core.Mangle.screaming_snake "Foo_hole" "FOO_HOLE" "the kind enum";
  (* A production [Expr_Root] and a root production [Expr]: [parse_expr__root]
     from two different derivations. Grouping only [parse_fn] compared
     [parse_expr] against [parse_expr__root], found them distinct, and
     accepted a grammar whose emitted parser binds one name twice. *)
  if Core.Mangle.snake_case "Expr_Root" = "expr__root"
  then Law.pass "\"Expr_Root\" snakes to the same string as Expr's root variant"
  else
    Law.fail "\"Expr_Root\" no longer snakes to %S" (Core.Mangle.snake_case "Expr_Root")
;;

let () = Law.summarise "mangle_test"
