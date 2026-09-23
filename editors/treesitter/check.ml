open StdLabels

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

let problem_to_string (problem : problem) : string =
  match problem with
  | No_js_regex { token; reason } ->
    Printf.sprintf
      "token %S has no JavaScript regex (%s)"
      (Core.Grammar.Name.Token.to_string token)
      reason
  | Node_collision { node; sources } ->
    Printf.sprintf
      "the node name %S is claimed by %s"
      node
      (String.concat ~sep:" and " sources)
  | Bad_setting { field; value; reason } -> Printf.sprintf "%s %S %s" field value reason
;;

let pp_problem (fmt : Format.formatter) (problem : problem) : unit =
  Format.pp_print_string fmt (problem_to_string problem)
;;

let compare_problem (a : problem) (b : problem) : int =
  String.compare (problem_to_string a) (problem_to_string b)
;;

let check_regexes (facts : Core.Facts.t) : problem list =
  Array.to_list Core.Facts.(facts.tokens)
  |> List.filter_map ~f:(fun (token : Core.Token.def) ->
    match token.klass with
    | Core.Grammar.Keyword _ | Core.Grammar.Punctuation _ -> None
    | Core.Grammar.Pattern { lexer; _ } ->
      (match Js.regex lexer with
       | Ok _ -> None
       | Error reason -> Some (No_js_regex { token = token.name; reason })))
;;

(* Every name the rules map will hold. A pattern token becomes a rule so that
   a query can name it, so a rule and a token that mangle alike would be one
   entry and the second would win. *)
let check_nodes (facts : Core.Facts.t) : problem list =
  let claimed =
    (Array.to_list Core.Facts.(facts.rules)
     |> List.concat_map ~f:(fun (rule : Core.Rule.def) ->
       let named = Core.Grammar.Name.Rule.to_string rule.name in
       (Node.of_rule rule.name, Printf.sprintf "the rule %S" named)
       ::
       (match rule.origin with
        | Core.Rule.Pratt_block ->
          [ ( Node.dispatch rule.name
            , Printf.sprintf "the hidden choice emitted for the block %S" named )
          ]
        | Core.Rule.User | Core.Rule.Pratt_role _ -> [])))
    @ (Array.to_list Core.Facts.(facts.tokens)
       |> List.filter_map ~f:(fun (token : Core.Token.def) ->
         match token.klass with
         | Core.Grammar.Pattern _ ->
           Some
             ( Node.of_token token.name
             , Printf.sprintf
                 "the token %S"
                 (Core.Grammar.Name.Token.to_string token.name) )
         | _ -> None))
    @
    match Core.Facts.(facts.roots) with
    | [] | [ _ ] -> []
    | _ -> [ Node.start, "the rule made up to choose between the grammar's roots" ]
  in
  let by_node = Hashtbl.create 64 in
  List.iter claimed ~f:(fun (node, source) ->
    Hashtbl.replace
      by_node
      node
      (source :: Option.value (Hashtbl.find_opt by_node node) ~default:[]));
  Hashtbl.fold
    (fun node sources acc ->
       match sources with
       | _ :: _ :: _ -> Node_collision { node; sources = List.rev sources } :: acc
       | _ -> acc)
    by_node
    []
;;

(* tree-sitter puts the grammar's name into the C identifiers it generates,
   so the name has to be a C identifier. *)
let check_language (language : string) : problem list =
  let bad (reason : string) : problem list =
    [ Bad_setting { field = "the language"; value = language; reason } ]
  in
  if String.equal language ""
  then bad "is empty"
  else if
    not
      (match language.[0] with
       | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
       | _ -> false)
  then bad "does not start with a letter or an underscore"
  else if
    not
      (String.for_all language ~f:(fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
         | _ -> false))
  then bad "holds something that is not a letter, a digit or an underscore"
  else []
;;

let run (scopes : Scopes.t) ~(language : string) : (unit, problem list) result =
  let facts = Scopes.facts scopes in
  match
    List.sort_uniq
      ~cmp:compare_problem
      (check_language language @ check_regexes facts @ check_nodes facts)
  with
  | [] -> Ok ()
  | problems -> Error problems
;;
