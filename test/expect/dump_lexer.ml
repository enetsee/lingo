(* -- the lexer table, as text -------------------------------------------------

      Prints one grammar's token automaton the way an emitter reads it. The
      dune rules beside this diff the output against a file in the tree, so a
      change in the table is a diff rather than a difference nobody sees.

      test/laws/law_lexer.ml says the table agrees with the automaton,
      whatever the partition turns out to be. How coarse the partition is
      sets how big every emitted lexer gets, and the counts at the top are
      where that shows.

      Generated. [dune promote] writes it, so a moved line is cheap to accept
      and the reviewing happens in the diff.
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
         (fun (error : Core.Error.t) -> Format.printf "%a@." Core.Error.pp error)
         errors;
       exit 1
     | Ok facts ->
       (* A table names kinds as integers, and a diff of integers says
          nothing. The legend above makes the accept column readable. *)
       print_endline "; -- token kinds --------------------------------------------------";
       Array.iter
         (fun (token : Core.Token.def) ->
            Printf.printf
              "; %3d %s\n"
              (Core.Kind.to_int token.kind)
              (Core.Kind.Name.to_string (Core.Facts.kind_name facts token.kind)))
         facts.tokens;
       print_endline "";
       let buffer = Buffer.create 8192 in
       let formatter = Format.formatter_of_buffer buffer in
       Core.Lexer.pp formatter (Core.Lexer.of_facts facts);
       Format.pp_print_flush formatter ();
       print_string (Buffer.contents buffer))
;;
