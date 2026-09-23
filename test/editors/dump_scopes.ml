(* -- the scopes, recorded -----------------------------------------------------

      What each token and each child position is called, before any backend
      renders it. Three backends read this, so a change here moves all three,
      and this file shows it first.

      Generated. [dune promote] writes it.
   -------------------------------------------------------------------------- *)

let () =
  let name = Sys.argv.(1) in
  match Editor_corpus.find name with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some entry -> Format.printf "%a" Scopes.pp (Editor_corpus.scopes entry)
;;
