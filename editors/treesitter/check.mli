(** What this backend rejects, over and above what a grammar has passed.

    A tree-sitter grammar adds constraints of its own. Its rules and its
    tokens share one namespace, so two that mangle alike would be one rule.
    Its regexes are read by two engines, a JavaScript one at generate time
    and a Rust one after, so a term has to write out in both or in neither.
    Its own name goes into the C identifiers tree-sitter generates. *)

type problem =
  | No_js_regex of
      { token : Core.Grammar.Name.Token.t
      ; reason : string
      }
  | Node_collision of
      { node : string
      ; sources : string list
      }
  | Bad_setting of
      { field : string
      ; value : string
      ; reason : string
      }

val pp_problem : Format.formatter -> problem -> unit
val problem_to_string : problem -> string
val run : Scopes.t -> language:string -> (unit, problem list) result
