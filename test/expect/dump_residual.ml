(* -- the residual tables, as text ---------------------------------------------

      Prints the points the emitter writes into a grammar's generated module,
      one node kind at a time. The dune rules beside this diff the output
      against a file in the tree.

      What it is for: reading them. Two faults in these tables were found by
      eye and not by any check, because a table that is wrong in a shape the
      corpus never reaches still passes every law. A file in the tree is what
      puts the next one in a diff.

      Kinds are named rather than numbered. The numbers are in
      test/expect/*.plan, and a diff of integers says nothing.

      A point prints its number, then [may-end] where the node above carries
      on from it, then [next] and the kinds that may appear, then one [on] line
      per child it can take and the point that child leaves it at. A point with
      no [next] admits nothing, which is what the end of a rule looks like.
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
  ]
;;

(* Wrapped, so a long set stays inside a terminal and a diff stays narrow. *)
let wrapped (indent : string) (names : string list) =
  let width = 78 in
  let line = Buffer.create 80 in
  Buffer.add_string line indent;
  let start = String.length indent in
  List.iter
    (fun name ->
       if
         Buffer.length line > start && Buffer.length line + 1 + String.length name > width
       then (
         print_endline (Buffer.contents line);
         Buffer.clear line;
         Buffer.add_string line (indent ^ "  "))
       else if Buffer.length line > start
       then Buffer.add_char line ' ';
       Buffer.add_string line name)
    names;
  if Buffer.length line > start then print_endline (Buffer.contents line)
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
       List.iter (fun e -> Format.printf "%a@." Core.Error.pp e) errors;
       exit 1
     | Ok facts ->
       let plan, _ = Plan.Lower.of_facts facts in
       let named (kind : Ir.Kind.t) =
         match
           List.find_opt
             (fun k -> Core.Kind.to_int k = kind)
             (Core.Kind.Table.kinds facts.kinds)
         with
         | Some k -> Core.Kind.Name.to_string (Core.Facts.kind_name facts k)
         | None -> string_of_int kind
       in
       List.iter
         (fun (kind, points) ->
            print_endline (named kind);
            Array.iteri
              (fun at (p : Ir.Residual.Table.point) ->
                 Printf.printf "  %d%s\n" at (if p.may_end then " may-end" else "");
                 (match Array.to_list p.first with
                  | [] -> ()
                  | kinds -> wrapped "    next " (List.map named kinds));
                 Array.iter
                   (fun (on, dest) ->
                      wrapped
                        "    on "
                        (List.map named (Array.to_list on) @ [ "->"; string_of_int dest ]))
                   p.on)
              points;
            print_endline "")
         (Ir.Residual.Table.of_plan plan))
;;
