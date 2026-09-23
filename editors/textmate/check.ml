open StdLabels

type problem =
  | No_oniguruma of
      { token : Core.Grammar.Name.Token.t
      ; reason : string
      }
  | Key_collision of
      { key : string
      ; sources : string list
      }
  | Shared_opener of
      { rule : Core.Grammar.Name.Rule.t
      ; token : Core.Grammar.Name.Token.t
      ; children : Core.Grammar.Name.Child.t list
      }
  | Shared_postfix_lead of
      { block : Core.Grammar.Name.Rule.t
      ; token : Core.Grammar.Name.Token.t
      }
  | Forwarding_cycle of { rules : Core.Grammar.Name.Rule.t list }
  | Unattachable_scope of { rule : Core.Grammar.Name.Rule.t }
  | Bad_raw_pattern of
      { rule : Core.Grammar.Name.Rule.t
      ; reason : string
      }
  | Bad_setting of
      { field : string
      ; value : string
      ; reason : string
      }

let problem_to_string (problem : problem) : string =
  match problem with
  | No_oniguruma { token; reason } ->
    Printf.sprintf
      "token %S has no Oniguruma form (%s); write one with ~textmate"
      (Core.Grammar.Name.Token.to_string token)
      reason
  | Key_collision { key; sources } ->
    Printf.sprintf
      "repository key %S is claimed by %s"
      key
      (String.concat ~sep:" and " sources)
  | Shared_opener { rule; token; children } ->
    Printf.sprintf
      "production %S has children %s opening on %S"
      (Core.Grammar.Name.Rule.to_string rule)
      (String.concat
         ~sep:" and "
         (List.map children ~f:(fun child ->
            Printf.sprintf "%S" (Core.Grammar.Name.Child.to_string child))))
      (Core.Grammar.Name.Token.to_string token)
  | Shared_postfix_lead { block; token } ->
    Printf.sprintf
      "expression block %S has more than one postfix operator leading on %S"
      (Core.Grammar.Name.Rule.to_string block)
      (Core.Grammar.Name.Token.to_string token)
  | Forwarding_cycle { rules } ->
    Printf.sprintf
      "%s forward to each other and add nothing"
      (String.concat
         ~sep:" to "
         (List.map rules ~f:(fun rule ->
            Printf.sprintf "%S" (Core.Grammar.Name.Rule.to_string rule))))
  | Unattachable_scope { rule } ->
    Printf.sprintf
      "%S is scoped, and what it emits is a list of patterns, which has nowhere to carry \
       a scope"
      (Core.Grammar.Name.Rule.to_string rule)
  | Bad_raw_pattern { rule; reason } ->
    Printf.sprintf
      "the pattern written for %S %s"
      (Core.Grammar.Name.Rule.to_string rule)
      reason
  | Bad_setting { field; value; reason } -> Printf.sprintf "%s %S %s" field value reason
;;

let pp_problem (fmt : Format.formatter) (problem : problem) : unit =
  Format.pp_print_string fmt (problem_to_string problem)
;;

let compare_problem (a : problem) (b : problem) : int =
  String.compare (problem_to_string a) (problem_to_string b)
;;

(* -- the hand-written patterns --------------------------------------------- *)

let json_kind (json : Yojson.Basic.t) : string =
  match json with
  | `Null -> "null"
  | `Bool _ -> "a boolean"
  | `Int _ -> "an integer"
  | `Float _ -> "a float"
  | `String _ -> "a string"
  | `List _ -> "an array"
  | `Assoc _ -> "an object"
;;

let known_fields =
  [ "match"
  ; "begin"
  ; "end"
  ; "include"
  ; "patterns"
  ; "name"
  ; "captures"
  ; "beginCaptures"
  ; "endCaptures"
  ; "contentName"
  ; "applyEndPatternLast"
  ; "while"
  ; "whileCaptures"
  ; "comment"
  ; "disabled"
  ]
;;

(* The four forms a reference takes: [#entry] in this grammar, [$self] or
   [$base] for the root of this one or the outermost, a scope name for
   another grammar, and [scope#entry] for an entry inside one. A scope name
   always holds a dot. Requiring one still catches the common slip of
   writing [trivia] where [#trivia] was meant. *)
let is_reference (s : string) : bool =
  let width = String.length s in
  if width = 0
  then false
  else if String.equal s "$self" || String.equal s "$base"
  then true
  else if s.[0] = '#'
  then width > 1
  else (
    let ok = ref true in
    let seen_hash = ref false in
    let seen_dot = ref false in
    let after_dot = ref false in
    (match s.[0] with
     | 'A' .. 'Z' | 'a' .. 'z' -> ()
     | _ -> ok := false);
    String.iter s ~f:(fun c ->
      if !ok
      then (
        match c with
        | '#' ->
          if !after_dot || !seen_hash then ok := false else seen_hash := true;
          after_dot := false
        | '.' when not !seen_hash ->
          if !after_dot then ok := false;
          seen_dot := true;
          after_dot := true
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> after_dot := false
        | _ -> ok := false));
    !ok && !seen_dot && (not !after_dot) && not (s.[width - 1] = '#'))
;;

(* Walks the pattern at every depth. A [begin] without an [end] opens a
   region that never closes, and the colouring then runs to the end of the
   file. *)
let rec check_pattern (json : Yojson.Basic.t) : string option =
  let first (reasons : string option list) : string option =
    List.fold_left reasons ~init:None ~f:(fun acc reason ->
      match acc with
      | Some _ -> acc
      | None -> reason)
  in
  match json with
  | `Assoc fields ->
    let has (key : string) : bool = List.mem_assoc key ~map:fields in
    let paired =
      match has "begin", has "end" with
      | true, false -> Some "has a begin and no end, and a region needs both"
      | false, true -> Some "has an end and no begin, and a region needs both"
      | _ -> None
    in
    let structural =
      if List.exists [ "match"; "begin"; "include"; "patterns" ] ~f:has
      then None
      else Some "names no match, begin, include or patterns, so it scopes nothing"
    in
    let per_field =
      List.map fields ~f:(fun (key, value) ->
        if not (List.mem key ~set:known_fields)
        then Some (Printf.sprintf "has an unknown field %S" key)
        else (
          match key, value with
          | ("match" | "begin" | "end" | "name" | "contentName" | "comment" | "while"), v
            when match v with
                 | `String _ -> false
                 | _ -> true ->
            Some
              (Printf.sprintf "has %S as %s, and it has to be a string" key (json_kind v))
          | "include", `String s when not (is_reference s) ->
            Some
              (Printf.sprintf
                 "has include %S, which is none of #entry, $self, $base, a scope name or \
                  scope#entry"
                 s)
          | ("captures" | "beginCaptures" | "endCaptures" | "whileCaptures"), `Assoc pairs
            ->
            first
              (List.map pairs ~f:(fun (index, entry) ->
                 match int_of_string_opt index, entry with
                 | None, _ ->
                   Some
                     (Printf.sprintf
                        "has %S keyed by %S, which is not a capture number"
                        key
                        index)
                 | _, `Assoc _ -> None
                 | _, other ->
                   Some
                     (Printf.sprintf
                        "has %S entry %S as %s, and it has to be an object"
                        key
                        index
                        (json_kind other))))
          | ("captures" | "beginCaptures" | "endCaptures" | "whileCaptures"), other ->
            Some
              (Printf.sprintf
                 "has %S as %s, and it has to be an object of capture numbers"
                 key
                 (json_kind other))
          | "patterns", `List items -> first (List.map items ~f:check_pattern)
          | "patterns", other ->
            Some
              (Printf.sprintf
                 "has patterns as %s, and it has to be an array"
                 (json_kind other))
          | ("applyEndPatternLast" | "disabled"), v
            when match v with
                 | `Bool _ -> false
                 | _ -> true ->
            (* TextMate spells these as booleans. Some hand-written grammars
               use 0 and 1, and accepting both would let two patterns in one
               file disagree about how to spell the same flag. *)
            Some
              (Printf.sprintf
                 "has %S as %s, and it has to be a boolean"
                 key
                 (json_kind v))
          | _ -> None))
    in
    first (paired :: structural :: per_field)
  | other -> Some (Printf.sprintf "is %s, and it has to be an object" (json_kind other))
;;

(* -- the checks ------------------------------------------------------------ *)

let is_emitted (rule : Core.Rule.def) : bool =
  match rule.origin with
  | Core.Rule.User | Core.Rule.Pratt_block -> true
  | Core.Rule.Pratt_role _ -> false
;;

let check_oniguruma (facts : Core.Facts.t) : problem list =
  Array.to_list Core.Facts.(facts.tokens)
  |> List.filter_map ~f:(fun (token : Core.Token.def) ->
    match Oniguruma.of_token token with
    | Ok _ -> None
    | Error reason -> Some (No_oniguruma { token = token.name; reason }))
;;

let check_keys (facts : Core.Facts.t) : problem list =
  let claimed =
    Array.to_list Core.Facts.(facts.rules)
    |> List.filter ~f:is_emitted
    |> List.map ~f:(fun (rule : Core.Rule.def) ->
      ( Key.of_rule rule.name
      , Printf.sprintf "production %S" (Core.Grammar.Name.Rule.to_string rule.name) ))
  in
  let synthetic =
    ("tokens", "the token entry")
    :: (if Emit.has_trivia facts then [ "trivia", "the trivia entry" ] else [])
  in
  let by_key = Hashtbl.create 64 in
  List.iter (claimed @ synthetic) ~f:(fun (key, source) ->
    Hashtbl.replace
      by_key
      key
      (source :: Option.value (Hashtbl.find_opt by_key key) ~default:[]));
  Hashtbl.fold
    (fun key sources acc ->
       match sources with
       | _ :: _ :: _ -> Key_collision { key; sources = List.rev sources } :: acc
       | _ -> acc)
    by_key
    []
;;

let opener (facts : Core.Facts.t) (child : Core.Rule.child) : Core.Kind.t option =
  match Shape.rule_of_child facts child with
  | None -> None
  | Some target ->
    (match target.frame with
     | Core.Rule.Delimited { open_; _ } -> Some open_
     | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> None)
;;

let check_openers (facts : Core.Facts.t) : problem list =
  Array.to_list Core.Facts.(facts.rules)
  |> List.filter ~f:is_emitted
  |> List.concat_map ~f:(fun (rule : Core.Rule.def) ->
    let groups = Hashtbl.create 4 in
    Array.iter rule.children ~f:(fun (child : Core.Rule.child) ->
      match opener facts child with
      | None -> ()
      | Some kind ->
        let key = Core.Kind.to_int kind in
        let children =
          match Hashtbl.find_opt groups key with
          | Some (_, children) -> children
          | None -> []
        in
        Hashtbl.replace groups key (kind, child.child_name :: children));
    Hashtbl.fold
      (fun _ (kind, children) acc ->
         match children, Core.Facts.token_of_kind facts kind with
         | _ :: _ :: _, Some token ->
           Shared_opener
             { rule = rule.name; token = token.name; children = List.rev children }
           :: acc
         | _ -> acc)
      groups
      [])
;;

let check_postfix_leads (facts : Core.Facts.t) : problem list =
  Array.to_list Core.Facts.(facts.blocks)
  |> List.concat_map ~f:(fun (block : Core.Block.def) ->
    let seen = Hashtbl.create 4 in
    Array.to_list block.postfix
    |> List.filter_map ~f:(fun (postfix : Core.Block.postfix) ->
      match postfix.p_body with
      | Core.Block.Nothing -> None
      | Core.Block.Then _ | Core.Block.Enclosed _ ->
        let key = Core.Kind.to_int postfix.p_lead in
        if Hashtbl.mem seen key
        then (
          match Core.Facts.token_of_kind facts postfix.p_lead with
          | None -> None
          | Some token ->
            Some (Shared_postfix_lead { block = block.name; token = token.name }))
        else (
          Hashtbl.replace seen key ();
          None)))
;;

let check_forwarding
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      ~(raw : Core.Rule.id -> bool)
  : problem list
  =
  let rules = Core.Facts.(facts.rules) in
  let target =
    Array.map rules ~f:(fun (rule : Core.Rule.def) ->
      if is_emitted rule then Shape.forwards_to facts scopes ~raw rule else None)
  in
  let cycles = ref [] in
  Array.iter rules ~f:(fun (rule : Core.Rule.def) ->
    let rec walk ~(seen : Core.Rule.id list) (id : Core.Rule.id) : unit =
      if List.mem id ~set:seen
      then (
        let names =
          List.rev_map (id :: seen) ~f:(fun id -> (Core.Facts.rule facts id).name)
        in
        cycles := Forwarding_cycle { rules = names } :: !cycles)
      else (
        match target.(id) with
        | None -> ()
        | Some next -> walk ~seen:(id :: seen) next)
    in
    walk ~seen:[] rule.id);
  !cycles
;;

(* Where a rule's own scope can land.

   A postfix operator with no body is the odd case. [x?] emits no pattern of
   its own, because the lead token is the whole operator and the
   grammar-wide token entry already scopes it. There is nothing to name. *)
let carries_a_scope
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      ~(raw : Core.Rule.id -> bool)
  : Core.Rule.def -> bool
  =
  fun (rule : Core.Rule.def) ->
  match rule.origin with
  | Core.Rule.Pratt_block -> false
  | Core.Rule.Pratt_role { block; role } ->
    (match role with
     | Core.Role.Base | Core.Role.Bin | Core.Role.Prefix -> false
     | Core.Role.Postfix index ->
       (match
          Array.find_opt
            Core.Facts.(facts.blocks)
            ~f:(fun (def : Core.Block.def) -> def.rule_id = block)
        with
        | None -> false
        | Some def ->
          index < Array.length def.postfix
          &&
            (match def.postfix.(index).p_body with
            | Core.Block.Nothing -> false
            | Core.Block.Then _ | Core.Block.Enclosed _ -> true)))
  | Core.Rule.User ->
    (* A rule that forwards to another emits nothing at all, and a rule
       carrying a scope is never treated as a forwarder. So [Shape] alone
       settles it. *)
    ignore raw;
    (match Shape.of_rule facts scopes rule with
     | Shape.Flat -> false
     | Shape.Delimited _ | Shape.Region _ | Shape.Capture _ -> true)
;;

let check_attachable
      (facts : Core.Facts.t)
      (scopes : Scopes.t)
      ~(raw : Core.Rule.id -> bool)
  : problem list
  =
  let carries = carries_a_scope facts scopes ~raw in
  Array.to_list Core.Facts.(facts.rules)
  |> List.filter_map ~f:(fun (rule : Core.Rule.def) ->
    match Scopes.rule scopes rule.id with
    | None -> None
    | Some _ ->
      if carries rule then None else Some (Unattachable_scope { rule = rule.name }))
;;

let check_no_whitespace ~(field : string) (value : string) : problem list =
  if String.equal value ""
  then [ Bad_setting { field; value; reason = "is empty" } ]
  else if
    String.exists value ~f:(fun c ->
      match c with
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false)
  then [ Bad_setting { field; value; reason = "holds whitespace" } ]
  else []
;;

let run
      (scopes : Scopes.t)
      ~(language : string)
      ~(scope_prefix : string)
      ~(file_types : string list)
      ~(raw : (Core.Grammar.Name.Rule.t * string) list)
  : (Yojson.Basic.t list array, problem list) result
  =
  let facts = Scopes.facts scopes in
  let rules = Core.Facts.(facts.rules) in
  let parsed = Array.make (Array.length rules) [] in
  let problems = ref [] in
  let report (problem : problem) : unit = problems := problem :: !problems in
  List.iter raw ~f:(fun (name, source) ->
    match
      Array.find_opt rules ~f:(fun (rule : Core.Rule.def) ->
        Core.Grammar.Name.Rule.equal rule.name name)
    with
    | None ->
      report
        (Bad_raw_pattern { rule = name; reason = "names no production in this grammar" })
    | Some rule ->
      (match Yojson.Basic.from_string source with
       | exception Yojson.Json_error reason ->
         report (Bad_raw_pattern { rule = name; reason = "is not JSON: " ^ reason })
       | json ->
         (match check_pattern json with
          | Some reason -> report (Bad_raw_pattern { rule = name; reason })
          | None -> parsed.(rule.id) <- parsed.(rule.id) @ [ json ])));
  let has_raw (id : Core.Rule.id) : bool = parsed.(id) <> [] in
  List.iter
    ~f:report
    (check_no_whitespace ~field:"the language" language
     @ check_no_whitespace ~field:"the scope prefix" scope_prefix
     @ List.concat_map file_types ~f:(fun file_type ->
       check_no_whitespace ~field:"the file type" file_type
       @
       if String.length file_type > 0 && file_type.[0] = '.'
       then
         [ Bad_setting
             { field = "the file type"
             ; value = file_type
             ; reason = "starts with a dot, and a file type is a bare extension"
             }
         ]
       else [])
     @ check_oniguruma facts
     @ check_keys facts
     @ check_openers facts
     @ check_postfix_leads facts
     @ check_attachable facts scopes ~raw:has_raw
     @ check_forwarding facts scopes ~raw:has_raw);
  match List.sort_uniq ~cmp:compare_problem !problems with
  | [] -> Ok parsed
  | problems -> Error problems
;;
