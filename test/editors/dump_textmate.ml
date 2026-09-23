(* -- the TextMate grammar, recorded -------------------------------------------

      One file per grammar. A change in what an editor would colour is a diff
      here before it is a difference in anyone's window.

      Generated. [dune promote] writes it.
   -------------------------------------------------------------------------- *)

let () =
  let name = Sys.argv.(1) in
  match Editor_corpus.find name with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some entry ->
    (match Textmate.generate (Editor_corpus.scopes entry) ~language:name () with
     | Error ps ->
       List.iter (fun p -> Format.printf "%a@." Textmate.Check.pp_problem p) ps;
       exit 1
     | Ok json -> print_string json)
;;
