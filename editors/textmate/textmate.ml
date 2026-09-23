open StdLabels
module Key = Key
module Oniguruma = Oniguruma
module Shape = Shape
module Emit = Emit
module Check = Check

(* -- forwarding ------------------------------------------------------------ *)

(* Where each rule's references end up. A rule that forwards to another and
   adds nothing is dropped, and every [#wrapper] becomes [#target].

   [Check] has already rejected a cycle, so the walk terminates. *)
let aliases (facts : Core.Facts.t) (scopes : Scopes.t) ~(raw : Core.Rule.id -> bool)
  : Core.Rule.id -> Core.Rule.id
  =
  let rules = Core.Facts.(facts.rules) in
  let direct =
    Array.map rules ~f:(fun (rule : Core.Rule.def) ->
      match rule.origin with
      | Core.Rule.User | Core.Rule.Pratt_block -> Shape.forwards_to facts scopes ~raw rule
      | Core.Rule.Pratt_role _ -> None)
  in
  let resolved = Array.make (Array.length rules) None in
  let rec resolve (id : Core.Rule.id) : Core.Rule.id =
    match resolved.(id) with
    | Some target -> target
    | None ->
      let target =
        match direct.(id) with
        | None -> id
        | Some next -> resolve next
      in
      resolved.(id) <- Some target;
      target
  in
  resolve
;;

(* -- include cycles -------------------------------------------------------- *)

(* An entry that is nothing but a list of references. A TextMate engine
   flattens one into whichever list includes it, and it does that by
   recursion with no cycle guard: two such entries that reference each other
   leave the engine's cache half built, and the region that was being
   compiled never closes.

   A region entry breaks the recursion. Its body is compiled when the
   region opens, rather than when the reference to it is read. *)
let is_flat (json : Yojson.Basic.t) : bool =
  match json with
  | `Assoc fields ->
    let has (key : string) : bool = List.mem_assoc key ~map:fields in
    has "patterns" && not (has "begin" || has "end" || has "match")
  | _ -> false
;;

let references (json : Yojson.Basic.t) : string list =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "patterns" fields with
     | Some (`List patterns) ->
       List.filter_map patterns ~f:(fun pattern ->
         match pattern with
         | `Assoc [ ("include", `String key) ] when String.length key > 0 && key.[0] = '#'
           -> Some (String.sub key ~pos:1 ~len:(String.length key - 1))
         | _ -> None)
     | _ -> [])
  | _ -> []
;;

(* Drops the reference that closes each cycle.

   Nothing is lost by dropping one. Every list that could have reached the
   target through the cycle also includes the target's own entry, or
   includes something that does. A cycle is reachable only through such a
   list.

   [seeded] is dropped before the walk starts, so a cycle it already breaks
   never reaches the walk's own choice. That choice depends on the order the
   entries come in, and a grammar edit elsewhere would otherwise flip which
   side of a cycle gets dropped. *)
let break_cycles
      ~(seeded : (string * string) list)
      (entries : (string * Yojson.Basic.t) list)
  : (string * Yojson.Basic.t) list
  =
  let module Keys = Set.Make (String) in
  let flat =
    Keys.of_list
      (List.filter_map entries ~f:(fun (key, json) ->
         if is_flat json then Some key else None))
  in
  let dropped = Hashtbl.create 16 in
  List.iter seeded ~f:(fun edge -> Hashtbl.replace dropped edge ());
  let visited = ref Keys.empty in
  let on_stack = ref Keys.empty in
  let rec walk (key : string) : unit =
    if Keys.mem key !visited || Keys.mem key !on_stack
    then ()
    else (
      visited := Keys.add key !visited;
      on_stack := Keys.add key !on_stack;
      (match List.assoc_opt key entries with
       | None -> ()
       | Some json ->
         List.iter (references json) ~f:(fun target ->
           if Keys.mem target flat && not (Hashtbl.mem dropped (key, target))
           then
             if Keys.mem target !on_stack
             then Hashtbl.replace dropped (key, target) ()
             else walk target));
      on_stack := Keys.remove key !on_stack)
  in
  List.iter entries ~f:(fun (key, _) -> if Keys.mem key flat then walk key);
  List.map entries ~f:(fun (key, json) ->
    if not (Keys.mem key flat)
    then key, json
    else (
      match json with
      | `Assoc fields ->
        ( key
        , `Assoc
            (List.map fields ~f:(fun (field, value) ->
               match field, value with
               | "patterns", `List patterns ->
                 ( field
                 , `List
                     (List.filter patterns ~f:(fun pattern ->
                        match pattern with
                        | `Assoc [ ("include", `String reference) ]
                          when String.length reference > 0 && reference.[0] = '#' ->
                          let target =
                            String.sub reference ~pos:1 ~len:(String.length reference - 1)
                          in
                          not (Hashtbl.mem dropped (key, target))
                        | _ -> true)) )
               | _ -> field, value)) )
      | other -> key, other))
;;

(* Every reference an entry makes, at any depth, including one inside a
   region's body and one inside a hand-written pattern. *)
let rec all_references (json : Yojson.Basic.t) : string list =
  match json with
  | `Assoc fields ->
    List.concat_map fields ~f:(fun (key, value) ->
      match key, value with
      | "include", `String reference
        when String.length reference > 1 && reference.[0] = '#' ->
        [ String.sub reference ~pos:1 ~len:(String.length reference - 1) ]
      | _ -> all_references value)
  | `List items -> List.concat_map items ~f:all_references
  | _ -> []
;;

(* Drops the entries nothing reaches.

   A rule can be emitted and referenced by nothing: the right-hand side of an
   access operator is folded into the operator's own regex, so [x.f] leaves
   the entry for whatever [f] is with no reference to it. Keeping it would
   put a block in the file that no editor ever consults and no reader can
   account for. *)
let prune ~(roots : string list) (entries : (string * Yojson.Basic.t) list)
  : (string * Yojson.Basic.t) list
  =
  let module Keys = Set.Make (String) in
  let reached = ref Keys.empty in
  let rec walk (key : string) : unit =
    if not (Keys.mem key !reached)
    then (
      reached := Keys.add key !reached;
      match List.assoc_opt key entries with
      | None -> ()
      | Some json -> List.iter (all_references json) ~f:walk)
  in
  List.iter roots ~f:walk;
  List.filter entries ~f:(fun (key, _) -> Keys.mem key !reached)
;;

(* An atom of an expression block whose own entry references the block back.

   The block reaches into the atom as the dispatch itself. The atom reaches
   back into the block as a convenience the enclosing region already
   provides. So the edge to drop is always the atom's. *)
let seeded_drops
      (facts : Core.Facts.t)
      (alias : Core.Rule.id -> Core.Rule.id)
      (entries : (string * Yojson.Basic.t) list)
  : (string * string) list
  =
  Array.to_list Core.Facts.(facts.blocks)
  |> List.concat_map ~f:(fun (block : Core.Block.def) ->
    let block_key = Key.of_rule (Core.Facts.rule facts block.rule_id).name in
    Array.to_list block.atoms
    |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
      match Core.Facts.rule_of_kind facts kind with
      | None -> None
      | Some atom ->
        let atom_key = Key.of_rule (Core.Facts.rule facts (alias atom.id)).name in
        (match List.assoc_opt atom_key entries with
         | Some json when List.mem block_key ~set:(references json) ->
           Some (atom_key, block_key)
         | _ -> None)))
;;

(* -- the document ---------------------------------------------------------- *)

let schema =
  "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json"
;;

let generate
      (scopes : Scopes.t)
      ?name
      ?(file_types = [])
      ?(scope_prefix = "source")
      ?(raw = [])
      ~(language : string)
      ()
  : (string, Check.problem list) result
  =
  match Check.run scopes ~language ~scope_prefix ~file_types ~raw with
  | Error problems -> Error problems
  | Ok parsed ->
    let facts = Scopes.facts scopes in
    let raw_of (id : Core.Rule.id) : Yojson.Basic.t list = parsed.(id) in
    let has_raw (id : Core.Rule.id) : bool = parsed.(id) <> [] in
    let alias = aliases facts scopes ~raw:has_raw in
    let ctx : Emit.ctx = { facts; scopes; language; alias; raw = raw_of } in
    let block_of (rule : Core.Rule.id) : Core.Block.def option =
      Array.find_opt
        Core.Facts.(facts.blocks)
        ~f:(fun (block : Core.Block.def) -> block.rule_id = rule)
    in
    let entries =
      Array.to_list Core.Facts.(facts.rules)
      |> List.filter_map ~f:(fun (rule : Core.Rule.def) ->
        if alias rule.id <> rule.id
        then None
        else (
          match rule.origin with
          | Core.Rule.User -> Some (Key.of_rule rule.name, Emit.entry ctx rule)
          | Core.Rule.Pratt_block ->
            (match block_of rule.id with
             | Some block -> Some (Key.of_rule rule.name, Emit.block_entry ctx block)
             | None -> None)
          | Core.Rule.Pratt_role _ -> None))
    in
    let trivia =
      match Emit.trivia_entry ctx with
      | Some json -> [ "trivia", json ]
      | None -> []
    in
    let entries =
      break_cycles
        ~seeded:(seeded_drops facts alias entries)
        (entries @ trivia @ [ "tokens", Emit.tokens_entry ctx ])
    in
    let root =
      match Core.Facts.(facts.roots) with
      | first :: _ -> Key.of_rule (Core.Facts.rule facts (alias first)).name
      | [] -> "tokens"
    in
    let entries =
      prune
        ~roots:(root :: "tokens" :: (if Emit.has_trivia facts then [ "trivia" ] else []))
        entries
    in
    let root_patterns =
      [ `Assoc [ "include", `String ("#" ^ root) ] ]
      @ (if Emit.has_trivia facts then [ `Assoc [ "include", `String "#trivia" ] ] else [])
      @ [ `Assoc [ "include", `String "#tokens" ] ]
    in
    let header =
      [ "$schema", `String schema
      ; ( "name"
        , `String
            (match name with
             | Some name -> name
             | None -> String.capitalize_ascii language) )
      ; "scopeName", `String (scope_prefix ^ "." ^ language)
      ]
      @ (match file_types with
         | [] -> []
         | types -> [ "fileTypes", `List (List.map types ~f:(fun t -> `String t)) ])
      @ [ "patterns", `List root_patterns; "repository", `Assoc entries ]
    in
    Ok (Yojson.Basic.pretty_to_string (`Assoc header) ^ "\n")
;;
