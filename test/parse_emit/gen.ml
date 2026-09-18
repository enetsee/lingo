(* -- the emitted parser, as source ---------------------------------------------

      Writes one grammar's parser module, or its interface. The dune rules
      beside this run it once per grammar and give the result to a library
      linking [lingo_runtime] and nothing else, so a generated parser that
      reached for anything else of ours would not build.
   -------------------------------------------------------------------------- *)

let grammars : (string * Core.Grammar.t) list =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar
  ; "json", Lingo_grammars.Json_grammar.grammar
  ; "calc", Lingo_grammars.Calc_grammar.grammar
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar
  ; "postfix", Lingo_grammars.Postfix_grammar.grammar
  ; "shapes", Lingo_grammars.Shapes_grammar.grammar
  ; "unicode", Lingo_grammars.Unicode_grammar.grammar
  ; "recovery", Lingo_grammars.Recovery_grammar.grammar
  ]
;;

let () =
  let name = Sys.argv.(1) in
  match List.assoc_opt name grammars with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some grammar ->
    (match Core.Facts.of_grammar grammar with
     | Error errors ->
       List.iter
         (fun (error : Core.Error.t) -> Format.eprintf "%a@." Core.Error.pp error)
         errors;
       exit 1
     | Ok facts ->
       let plan, _ = Plan.Lower.of_facts facts in
       (match Sys.argv.(2) with
        | "mli" ->
          print_string (Ocaml.Emit.render_signature (Ocaml.Parser.signature plan))
        | "residual" -> print_string (Ocaml.Emit.render (Ocaml.Residual.generate plan))
        | "residual-mli" ->
          print_string (Ocaml.Emit.render_signature (Ocaml.Residual.signature plan))
        | _ -> print_string (Ocaml.Emit.render (Ocaml.Parser.generate plan))))
;;
