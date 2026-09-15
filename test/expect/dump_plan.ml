(* -- the lowered plan, as text ------------------------------------------------

      Prints one grammar's plan and its message catalogue. The dune rules beside
      this diff the output against a file in the tree, so a change to the
      lowering is a diff rather than a silent difference in what every backend
      reads.

      Unlike test/units/sexp_facts.ml, this file is generated: [dune promote]
      writes it. That makes a moved line cheap to accept, so the reviewing
      happens in the diff and nowhere else.

      A plan names kinds as integers, and a diff of integers says nothing. The
      legend at the top is what makes the rest readable, and it moves whenever
      the numbering does.
   -------------------------------------------------------------------------- *)

let grammars =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar
  ; "json", Lingo_grammars.Json_grammar.grammar
  ; "calc", Lingo_grammars.Calc_grammar.grammar
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar
  ]
;;

let render (p : Ir.Plan.t) =
  let b = Buffer.create 8192 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt 78;
  Plan.Text.pp fmt p;
  Format.pp_print_flush fmt ();
  Buffer.contents b
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
       let plan, msgs = Plan.Lower.of_facts f in
       print_endline "; -- kinds --------------------------------------------------------";
       List.iter
         (fun k ->
            Printf.printf
              "; %3d %s\n"
              (Core.Kind.to_int k)
              (Core.Kind.Name.to_string (Core.Facts.kind_name f k)))
         (Core.Kind.Table.kinds f.kinds);
       print_endline "";
       print_string (render plan);
       print_endline "";
       print_endline "; -- messages -----------------------------------------------------";
       Array.iteri
         (fun i s -> Printf.printf "; %3d %s\n" i s)
         (Plan.Messages.entries msgs))
;;
