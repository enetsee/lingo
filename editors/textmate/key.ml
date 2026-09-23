open StdLabels

(* [Core.Mangle.snake_case_acronym] splits a run of capitals before its last
   letter, so [URLPattern] gives [url_pattern]. [safe_snake] would append an
   underscore for an OCaml keyword, and [struct_] leaks a detail of the
   OCaml emitter into a file a person reads. *)
let of_rule (name : Core.Grammar.Name.Rule.t) : string =
  Core.Mangle.snake_case_acronym (Core.Grammar.Name.Rule.to_string name)
;;

let dotted (name : Core.Grammar.Name.Rule.t) : string =
  String.map (of_rule name) ~f:(fun c ->
    match c with
    | '_' -> '.'
    | c -> c)
;;
