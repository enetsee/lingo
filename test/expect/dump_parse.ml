(* -- what a parse makes of a broken input -------------------------------------

      The tree and the diagnostics, for inputs chosen to reach a recovery path.
      The dune rules beside this diff the output against a file in the tree, so
      a change in how a parse recovers is a diff rather than a difference
      nobody sees.

      test/laws/law_interp.ml states what holds for every input: the bytes come
      back, the parse stops, a grammar's own inputs report nothing, no node but
      the root begins with trivia. None of that reads the shape of what
      recovery built, and four mutations have now reddened nothing because of
      it. This is the reader for that.

      Generated. [dune promote] writes it, so a moved line is cheap to accept
      and the reviewing happens in the diff.
   -------------------------------------------------------------------------- *)

let grammars =
  [ ( "sexp"
    , Lingo_grammars.Sexp_grammar.grammar
    , [ "(a b)"; "(a"; "  )"; "(a ) b"; "(()"; "( a  )  )" ] )
  ; ( "json"
    , Lingo_grammars.Json_grammar.grammar
      (* A missing separator, junk mid-body, a trailing separator, and a
         closer an outer frame is waiting for. *)
    , [ "[1, 2]"
      ; "[1 2]"
      ; "[1 : 2]"
      ; "[1, 2,]"
      ; "[1"
      ; "{\"a\" 1}"
      ; "{\"a\": 1 \"b\": 2}"
      ; "[{1 ]"
      ; "[1] junk"
        (* This one ends inside a string, so the lexer leaves one
           unterminated token and nothing follows it. *)
      ; "{\"a\": \"b"
      ] )
  ; ( "shapes"
    , Lingo_grammars.Shapes_grammar.grammar
      (* A root of repeated items recovers to the end of the input; [end] is a
         resync anchor, so a body stops there rather than sweeping it up. *)
    , [ "let a ; let b"; "@ let a"; "{ let a end }"; "{ let a; }"; "{ let }" ] )
  ; "calc", Lingo_grammars.Calc_grammar.grammar, [ "1+2*3"; "1+"; "1+*2"; "(1+2"; "1 2" ]
  ; ( "postfix"
    , Lingo_grammars.Postfix_grammar.grammar
    , [ "a.b[2]?+1"; "a."; "a(1"; "a(1,)"; "a(1 2)" ] )
    (* One input per part of the recovery set, each at the position that
       reads it. The valid ones are here so the shapes can be read against a
       parse that reports nothing. *)
  ; ( "recovery"
    , Lingo_grammars.Recovery_grammar.grammar
    , [ "let a in end"
      ; "( let a in end )"
      ; "sig : a ; in"
      ; "sig : a ; , : b ; in"
        (* [end] is in the commit's resume set and not in its recovery set,
           so the parse declines the skip and keeps the token. *)
      ; "let end"
        (* The caller passes down [sig], and the boundary drops it, so the
           skip runs to this rule's own close. *)
      ; "( sig )"
        (* The child declares [recover_to \[end\]], which replaces the [in]
           the position would otherwise have computed. *)
      ; "sig end in"
        (* The separator reaches the element through the rule's adds, and a
           separated body's loop contributes nothing. *)
      ; "sig : , : a ; in"
      ] )
  ]
;;

let () =
  let name = Sys.argv.(1) in
  match List.find_opt (fun (n, _, _) -> String.equal n name) grammars with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some (_, g, inputs) ->
    (match Core.Facts.of_grammar g with
     | Error es ->
       List.iter (fun e -> Format.printf "%a@." Core.Error.pp e) es;
       exit 1
     | Ok f ->
       let plan, msgs = Plan.Lower.of_facts f in
       let kinds = Core.Kind.Table.kinds f.kinds in
       let kind_name k =
         Core.Kind.Name.to_string (Core.Facts.kind_name f (List.nth kinds k))
       in
       List.iter
         (fun src ->
            let root, diags = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
            Printf.printf "%s\n" (String.make 70 '-');
            Printf.printf "%S\n" src;
            let rec go indent (n : Siesta.Green.node) =
              Printf.printf
                "%s%s\n"
                (String.make indent ' ')
                (kind_name (Siesta.Green.kind n));
              Array.iter
                (function
                  | Siesta.Green.Node m -> go (indent + 2) m
                  | Siesta.Green.Token t ->
                    Printf.printf
                      "%s%S\n"
                      (String.make (indent + 2) ' ')
                      (Siesta.Green.Token.text t))
                (Siesta.Green.children_array n)
            in
            go 2 root;
            List.iter
              (fun (d : Lingo_runtime.Diagnostic.t) ->
                 let lo, hi = d.range in
                 Printf.printf
                   "  %d-%d %s\n"
                   lo
                   hi
                   (match d.kind with
                    | Missing m ->
                      Printf.sprintf
                        "missing %s: %s"
                        (Option.value m.at_child ~default:"-")
                        (Plan.Messages.text msgs m.expected)
                    | Extra id -> "extra: " ^ Plan.Messages.text msgs id
                    | Unexpected -> "unexpected"))
              diags)
         inputs)
;;
