(* -- the emitted lexer, as source ---------------------------------------------

      Writes one grammar's lexer module, or its interface. The dune rules
      beside this run it once per grammar and give the result to a library
      linking [lingo_runtime] and nothing else, so a generated lexer that
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
       (match Sys.argv.(2) with
        | "mli" -> print_string (Ocaml.Emit.render_signature Ocaml.Lexer.signature)
        | "match" ->
          print_string (Ocaml.Emit.render (Ocaml.Lexer.generate ~shape:Match facts))
        | _ -> print_string (Ocaml.Emit.render (Ocaml.Lexer.generate facts))))
;;
