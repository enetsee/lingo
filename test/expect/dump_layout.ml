(* -- the layout, as the fold reads it -----------------------------------------

      One file per grammar, with a legend so the kinds read as names. A change
      in where a formatter may end a line is a diff here before it is a
      difference in anyone's output.

      Generated. [dune promote] writes it.
   -------------------------------------------------------------------------- *)

let grammars =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar
  ; "json", Lingo_grammars.Json_grammar.grammar
  ; "calc", Lingo_grammars.Calc_grammar.grammar
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar
  ; "postfix", Lingo_grammars.Postfix_grammar.grammar
  ; "shapes", Lingo_grammars.Shapes_grammar.grammar
  ; "unicode", Lingo_grammars.Unicode_grammar.grammar
  ; "recovery", Lingo_grammars.Recovery_grammar.grammar
  ; "comments", Lingo_grammars.Comments_grammar.grammar
  ]
;;

let () =
  let name = Sys.argv.(1) in
  match List.assoc_opt name grammars with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some g ->
    (match Core.Facts.of_grammar g with
     | Error es ->
       List.iter (fun e -> Format.printf "%a@." Core.Error.pp e) es;
       exit 1
     | Ok f ->
       let l = Layout.Lower.of_facts f in
       (match Layout.Check.run l with
        | Ok () -> ()
        | Error ps ->
          List.iter (fun p -> Format.printf "%a@." Layout.Check.pp_problem p) ps;
          exit 1);
       List.iter
         (fun k ->
            Printf.printf
              "; %d %s\n"
              (Core.Kind.to_int k)
              (Core.Kind.Name.to_string (Core.Facts.kind_name f k)))
         (Core.Kind.Table.kinds f.kinds);
       Format.printf "%a" Layout.Text.pp l)
;;
