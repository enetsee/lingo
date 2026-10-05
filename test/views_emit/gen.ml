(* -- the emitted views, as source -----------------------------------------------

      Writes one grammar's views module or its rewrite module, or the
      interface of either. The dune rules
      beside this compile the module twice. One copy has the interface and
      links [siesta] alone, so a view module that reached for anything else
      would not build. The other has no interface, so the law can reach the
      [Slots] module the interface hides.
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
  ; "comments", Lingo_grammars.Comments_grammar.grammar
  ; "rust", Lingo_grammars.Rust_grammar.grammar
  ; "effekt", Lingo_grammars.Effekt_grammar.grammar
  ; "wide", Lingo_grammars.Wide_grammar.grammar
  ; "ml", Lingo_grammars.Ml_grammar.grammar
  ; "nested", Nested_grammar.grammar
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
        | "mli" ->
          print_string (Ocaml.Emit.render_signature (Ocaml.Views.signature facts))
        | "rewrite" ->
          (* The views are compiled into a wrapped library beside this. *)
          let views = "Emitted_views." ^ String.capitalize_ascii name ^ "_views" in
          print_string (Ocaml.Emit.render (Ocaml.Rewrite.generate ~views facts))
        | "rewrite-mli" ->
          print_string (Ocaml.Emit.render_signature (Ocaml.Rewrite.signature facts))
        | _ -> print_string (Ocaml.Emit.render (Ocaml.Views.generate facts))))
;;
