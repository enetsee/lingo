(* -- what the formatter writes ------------------------------------------------

      Every input from test/inputs, formatted at two widths. The broken half
      matters most: an editor formats input that does not parse all day, and
      the predecessor's fixed point was only ever checked on input that did.

      test/laws/law_layout.ml states what holds for every one of these. This is
      the reader for it, because no law says whether the output is any good.

      Generated. [dune promote] writes it.
   -------------------------------------------------------------------------- *)

let grammars =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar, Inputs.sexp
  ; "json", Lingo_grammars.Json_grammar.grammar, Inputs.json
  ; "calc", Lingo_grammars.Calc_grammar.grammar, Inputs.calc
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar, Inputs.rassoc
  ; "postfix", Lingo_grammars.Postfix_grammar.grammar, Inputs.postfix
  ; "shapes", Lingo_grammars.Shapes_grammar.grammar, Inputs.shapes
  ; "unicode", Lingo_grammars.Unicode_grammar.grammar, Inputs.unicode
  ; "recovery", Lingo_grammars.Recovery_grammar.grammar, Inputs.recovery
  ; "comments", Lingo_grammars.Comments_grammar.grammar, Inputs.comments
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
       let plan, _ = Plan.Lower.of_facts f in
       let l = Layout.Lower.of_facts f in
       let boundary = Lex.boundary f in
       List.iter
         (fun src ->
            let root, diags = Interp.run plan plan.Ir.Plan.roots.(0) (Lex.run f src) in
            Printf.printf "%s\n" (String.make 70 '-');
            Printf.printf "%S%s\n" src (if diags = [] then "" else "   (recovered)");
            List.iter
              (fun width ->
                 Printf.printf "@%d\n" width;
                 let out = Lingo_runtime.Layout.format l ~boundary ~width root in
                 List.iter
                   (fun line -> Printf.printf "  | %s\n" line)
                   (String.split_on_char '\n' out))
              [ 80; 16 ])
         (Inputs.all inputs))
;;
