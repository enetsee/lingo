(* [snake_case_acronym] splits a run of capitals before its last letter, so
   [URLPattern] gives [url_pattern]. [safe_snake] would append an underscore
   for an OCaml keyword, and a tree-sitter node called [struct_] reads as a
   mistake in every query that names it. *)
let of_rule (name : Core.Grammar.Name.Rule.t) : string =
  Core.Mangle.snake_case_acronym (Core.Grammar.Name.Rule.to_string name)
;;

let of_token (name : Core.Grammar.Name.Token.t) : string =
  Core.Mangle.snake_case_acronym (Core.Grammar.Name.Token.to_string name)
;;

let dispatch (name : Core.Grammar.Name.Rule.t) : string = "_" ^ of_rule name

(* tree-sitter parses from the first rule in the map, so a grammar with
   several roots needs one made up to choose between them. Hand-written
   grammars call it [source_file]. *)
let start = "source_file"
