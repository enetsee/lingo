(* -- the tree-sitter grammar and its queries, recorded ------------------------

      One file per grammar per output. A change in the second parser, or in
      what an editor would colour or fold, is a diff here first.

      Generated. [dune promote] writes it.
   -------------------------------------------------------------------------- *)

let () =
  let name = Sys.argv.(1) in
  let which = Sys.argv.(2) in
  match Editor_corpus.find name with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some entry ->
    (match Treesitter.generate (Editor_corpus.scopes entry) ~language:name () with
     | Error ps ->
       List.iter (fun p -> Format.printf "%a@." Treesitter.Check.pp_problem p) ps;
       exit 1
     | Ok out ->
       print_string
         (match which with
          | "grammar" -> out.grammar_js
          | "highlights" -> out.highlights
          | "folds" -> out.folds
          | "locals" -> out.locals
          | other ->
            Printf.eprintf "no output named %s\n" other;
            exit 1))
;;
