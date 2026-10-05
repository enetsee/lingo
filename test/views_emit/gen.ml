(* -- the emitted views, as source -----------------------------------------------

      Writes one grammar's views module, its rewrite module, or the interface
      of either, and the lexer and parser its templates are read with. The
      dune rules beside this compile the views twice. One copy has the
      interface and links [siesta] alone, so a view module that reached for
      anything else would not build. The other has no interface, so the law
      can reach the [Slots] module the interface hides.

      Every grammar is given the metavariables [$x] and [$$xs] here. None of
      the corpus grammars declares any, and the tests read templates in all
      of them.
   -------------------------------------------------------------------------- *)

let letters = Redfa.Regex.plus (Redfa.Regex.range_char ~lo:'a' ~hi:'z')

let metavariables : Core.Grammar.metavariables =
  { single = Core.Grammar.pat "lingo_metavariable" Redfa.Regex.(seq (str "$") letters)
  ; sequence = Core.Grammar.pat "lingo_metavariables" Redfa.Regex.(seq (str "$$") letters)
  }
;;

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
  ; "ties", Ties_grammar.grammar
  ]
;;

let () =
  let name = Sys.argv.(1) in
  match List.assoc_opt name grammars with
  | None ->
    Printf.eprintf "no grammar named %s\n" name;
    exit 1
  | Some grammar ->
    let grammar = { grammar with metavariables = Some metavariables } in
    (match Core.Facts.of_grammar grammar with
     | Error errors ->
       List.iter
         (fun (error : Core.Error.t) -> Format.eprintf "%a@." Core.Error.pp error)
         errors;
       exit 1
     | Ok facts ->
       (* The views are compiled into a wrapped library beside this. *)
       let views = "Emitted_views." ^ String.capitalize_ascii name ^ "_views" in
       let capital = String.capitalize_ascii name in
       let template : Ocaml.Rewrite.template =
         { grammar
         ; lexer = "Emitted_template." ^ capital ^ "_template_lexer"
         ; parser = "Emitted_template." ^ capital ^ "_template_parser"
         }
       in
       let template_facts () =
         match Core.Facts.of_template grammar with
         | Some (Ok facts) -> facts
         | Some (Error _) | None ->
           prerr_endline "the template grammar does not check";
           exit 1
       in
       (match Sys.argv.(2) with
        | "template-lexer" ->
          print_string (Ocaml.Emit.render (Ocaml.Lexer.generate (template_facts ())))
        | "template-lexer-mli" ->
          print_string (Ocaml.Emit.render_signature Ocaml.Lexer.signature)
        | "template-parser" ->
          let plan, _ = Plan.Lower.of_facts (template_facts ()) in
          print_string (Ocaml.Emit.render (Ocaml.Parser.generate plan))
        | "template-parser-mli" ->
          let plan, _ = Plan.Lower.of_facts (template_facts ()) in
          print_string (Ocaml.Emit.render_signature (Ocaml.Parser.signature plan))
        | "mli" ->
          print_string (Ocaml.Emit.render_signature (Ocaml.Views.signature facts))
        | "rewrite" ->
          print_string (Ocaml.Emit.render (Ocaml.Rewrite.generate ~views ~template facts))
        | "rewrite-mli" ->
          print_string
            (Ocaml.Emit.render_signature (Ocaml.Rewrite.signature ~views ~template facts))
        | _ -> print_string (Ocaml.Emit.render (Ocaml.Views.generate facts))))
;;
