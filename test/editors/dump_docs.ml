(* -- the reference page, recorded ---------------------------------------------

      The body alone, without the page around it. A stylesheet is a matter of
      taste, and a diff over one says nothing. The listing, the diagrams, the
      tables and the coloured examples all land here.

      Generated. [dune promote] writes it.
   -------------------------------------------------------------------------- *)

let () =
  let name = Sys.argv.(1) in
  match Editor_corpus.find name with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some entry ->
    let out =
      Docs.generate
        entry.grammar
        (Editor_corpus.scopes entry)
        ~title:name
        ~examples:(Editor_corpus.examples entry)
        ()
    in
    print_string out.body
;;
