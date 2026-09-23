open StdLabels

let escape = Render.escape

let kind_text (facts : Core.Facts.t) (kind : Core.Kind.t) : string =
  match Core.Facts.token_of_kind facts kind with
  | Some token ->
    (match Core.Token.text token with
     | Some text -> text
     | None -> Core.Grammar.Name.Token.to_string token.name)
  | None ->
    (match Core.Facts.rule_of_kind facts kind with
     | Some rule -> Core.Grammar.Name.Rule.to_string rule.name
     | None -> Core.Kind.Name.to_string (Core.Facts.kind_name facts kind))
;;

let row (cells : string list) : string =
  "    <tr>"
  ^ String.concat ~sep:"" (List.map cells ~f:(fun c -> "<td>" ^ c ^ "</td>"))
  ^ "</tr>\n"
;;

(* A row a link can land on. The diagram and the listing both point at a
   token's row, so the row carries the anchor they name. *)
let anchored_row ~(anchor : string) (cells : string list) : string =
  Printf.sprintf "    <tr id=%S>" anchor
  ^ String.concat ~sep:"" (List.map cells ~f:(fun c -> "<td>" ^ c ^ "</td>"))
  ^ "</tr>\n"
;;

(* A production's name, pointing at that production's own section. *)
let rule_link (name : string) : string =
  Printf.sprintf
    "<a href=\"#%s\"><code>%s</code></a>"
    (escape (Mark.rule_anchor name))
    (escape name)
;;

let table ~(id : string) ~(headings : string list) (rows : string list) : string =
  match rows with
  | [] -> ""
  | rows ->
    Printf.sprintf "  <table id=%S>\n" id
    ^ "    <tr>"
    ^ String.concat ~sep:"" (List.map headings ~f:(fun h -> "<th>" ^ escape h ^ "</th>"))
    ^ "</tr>\n"
    ^ String.concat ~sep:"" rows
    ^ "  </table>\n"
;;

let code (s : string) : string = "<code>" ^ escape s ^ "</code>"

(* -- tokens ---------------------------------------------------------------- *)

let tokens (facts : Core.Facts.t) : string =
  let rows =
    Array.to_list Core.Facts.(facts.tokens)
    |> List.map ~f:(fun (token : Core.Token.def) ->
      let what, matches =
        match token.klass with
        | Core.Grammar.Keyword text -> "keyword", code text
        | Core.Grammar.Punctuation text -> "punctuation", code text
        | Core.Grammar.Pattern { lexer; _ } ->
          "pattern", code (Redfa.Regex.to_string lexer)
      in
      let what =
        match token.trivia with
        | None -> what
        | Some Core.Grammar.Reformat -> what ^ ", skipped"
        | Some Core.Grammar.Preserve -> what ^ ", kept as written"
      in
      let named = Core.Grammar.Name.Token.to_string token.name in
      anchored_row ~anchor:(Mark.token_anchor named) [ code named; escape what; matches ])
  in
  table ~id:"tokens" ~headings:[ "token"; "kind"; "matches" ] rows
;;

(* -- precedence ------------------------------------------------------------ *)

let precedence (grammar : Core.Grammar.t) : string =
  let rows =
    List.concat_map grammar.expr ~f:(fun (block : Core.Grammar.expr_def) ->
      let gathered = Hashtbl.create 16 in
      let note (bp : int) (what : string) (text : string) : unit =
        let here = Option.value (Hashtbl.find_opt gathered bp) ~default:[] in
        Hashtbl.replace gathered bp (here @ [ what, text ])
      in
      let literal (name : Core.Grammar.Name.Token.t) : string =
        let name = Core.Grammar.Name.Token.to_string name in
        match
          List.find_opt grammar.tokens ~f:(fun (token : Core.Grammar.token_def) ->
            String.equal (Core.Grammar.Name.Token.to_string token.token_name) name)
        with
        | Some { token_class = Core.Grammar.Keyword text; _ }
        | Some { token_class = Core.Grammar.Punctuation text; _ } -> text
        | _ -> name
      in
      List.iter block.prefix_ops ~f:(fun (op : Core.Grammar.operator) ->
        note op.bp "prefix" (literal op.op_token));
      List.iter block.infix_ops ~f:(fun (op : Core.Grammar.operator) ->
        note
          op.bp
          (match op.op_assoc with
           | Core.Grammar.Left -> "infix, left"
           | Core.Grammar.Right -> "infix, right")
          (literal op.op_token));
      List.iter block.postfix ~f:(fun (op : Core.Grammar.postfix_op) ->
        note op.bp "postfix" (literal op.lead));
      Hashtbl.fold (fun bp entries acc -> (bp, entries) :: acc) gathered []
      |> List.sort ~cmp:(fun (a, _) (b, _) -> Int.compare a b)
      |> List.map ~f:(fun (bp, entries) ->
        row
          [ string_of_int bp
          ; escape
              (String.concat
                 ~sep:", "
                 (List.sort_uniq ~cmp:String.compare (List.map entries ~f:fst)))
          ; String.concat ~sep:" " (List.map entries ~f:(fun (_, text) -> code text))
          ]))
  in
  table ~id:"precedence" ~headings:[ "binds"; "shape"; "operators" ] rows
;;

(* -- first and follow ------------------------------------------------------ *)

let set_text (facts : Core.Facts.t) (set : Core.Kind.Set.t) : string =
  match Core.Kind.Set.elements set with
  | [] -> "<em>nothing</em>"
  | kinds ->
    String.concat ~sep:" " (List.map kinds ~f:(fun k -> code (kind_text facts k)))
;;

let first_follow (facts : Core.Facts.t) : string =
  let rows =
    Array.to_list Core.Facts.(facts.rules)
    |> List.filter ~f:(fun (rule : Core.Rule.def) -> not (Core.Rule.is_synthetic rule))
    |> List.map ~f:(fun (rule : Core.Rule.def) ->
      row
        [ rule_link (Core.Grammar.Name.Rule.to_string rule.name)
        ; set_text facts (Core.Facts.first_of facts rule.id)
        ; set_text facts (Core.Facts.follow_of facts rule.id)
        ; (if Core.Facts.is_nullable facts rule.id then "yes" else "")
        ])
  in
  table
    ~id:"first-follow"
    ~headings:[ "production"; "starts with"; "followed by"; "may be empty" ]
    rows
;;
