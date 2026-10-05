open StdLabels

let names (g : Grammar.t) : string list =
  List.map g.productions ~f:(fun (p : Grammar.production) ->
    Grammar.Name.Rule.to_string p.kind_name)
  @ List.map g.expr ~f:(fun (e : Grammar.expr_def) ->
    Grammar.Name.Rule.to_string e.rule_name)
;;

let entry (g : Grammar.t) (rule : string) : string =
  let taken = names g in
  let rec fresh (name : string) =
    if List.mem name ~set:taken then fresh (name ^ "_") else name
  in
  fresh (rule ^ "Template")
;;

let regex (t : Grammar.token_def) : Redfa.Regex.t =
  match t.token_class with
  | Grammar.Keyword text | Grammar.Punctuation text -> Redfa.Regex.str text
  | Grammar.Pattern spec -> spec.lexer
;;

let typed_name (m : Grammar.metavariables) (rule : string) : string =
  Grammar.Name.Token.to_string m.single.token_name ^ "_" ^ rule
;;

(* The single metavariable followed by [:] and the rule's name. *)
let typed (m : Grammar.metavariables) (rule : string) : Grammar.token_def =
  { m.single with
    token_name = Grammar.Name.Token.of_string (typed_name m rule)
  ; token_class =
      Grammar.Pattern
        { lexer = Redfa.Regex.seq (regex m.single) (Redfa.Regex.str (":" ^ rule))
        ; textmate = None
        ; treesitter = None
        }
  }
;;

let grammar (g : Grammar.t) : Grammar.t option =
  match g.metavariables with
  | None -> None
  | Some m ->
    let token (name : string) = Grammar.Token name in
    let single = token (Grammar.Name.Token.to_string m.single.token_name) in
    let sequence = token (Grammar.Name.Token.to_string m.sequence.token_name) in
    let blocks =
      List.map g.expr ~f:(fun (e : Grammar.expr_def) ->
        Grammar.Name.Rule.to_string e.rule_name)
    in
    let rules = names g in
    let patterns =
      List.filter_map g.tokens ~f:(fun (t : Grammar.token_def) ->
        match t.token_class with
        | Grammar.Pattern _ -> Some (Grammar.Name.Token.to_string t.token_name)
        | Grammar.Keyword _ | Grammar.Punctuation _ -> None)
    in
    let child (c : Grammar.child) : Grammar.child =
      let symbols = c.head :: c.rest in
      let repeats, optional =
        match c.modifier with
        | Grammar.Zero_or_more _ | Grammar.One_or_more _ -> true, false
        | Grammar.Zero_or_one -> false, true
        | Grammar.Exactly_one -> false, false
      in
      (* A rule takes its own typed metavariable. A block's atoms take its
         typed one already, and a child that can be a pattern token takes the
         plain one. A token with fixed text is the grammar's own structure,
         as an operator is, and no metavariable stands for one. *)
      let typed_rules =
        List.filter_map symbols ~f:(fun (s : Grammar.symbol) ->
          match s with
          | Grammar.Rule r when not (List.mem r ~set:blocks) ->
            Some (token (typed_name m r))
          | Grammar.Rule _ | Grammar.Token _ -> None)
      in
      let plain =
        if
          List.exists symbols ~f:(fun (s : Grammar.symbol) ->
            match s with
            | Grammar.Token name -> List.mem name ~set:patterns
            | Grammar.Rule _ -> false)
        then [ single ]
        else []
      in
      (* The sequence metavariable goes first. The element's own rule may
         begin with it too, through an expression atom, and the dispatch
         takes the first arm that admits it. *)
      let head, rest = if repeats then sequence, c.head :: c.rest else c.head, c.rest in
      { c with
        head
      ; rest = rest @ typed_rules @ plain
      ; c_parse = { c.c_parse with greedy = c.c_parse.greedy || optional }
      }
    in
    (* A root takes whatever input is left, so a rule is no root of its own.
       A wrapper per rule is the root a fragment is parsed at. *)
    let wrappers =
      List.map rules ~f:(fun (rule : string) ->
        Grammar.prod (entry g rule) [ Grammar.child_req "fragment" (Grammar.Rule rule) ])
    in
    let productions =
      List.map g.productions ~f:(fun (p : Grammar.production) ->
        { p with children = List.map p.children ~f:child })
      @ wrappers
    in
    Some
      { productions
      ; expr =
          List.map g.expr ~f:(fun (e : Grammar.expr_def) ->
            let name = Grammar.Name.Rule.to_string e.rule_name in
            { e with atoms = e.atoms @ [ single; sequence; token (typed_name m name) ] })
      ; tokens = g.tokens @ [ m.single; m.sequence ] @ List.map rules ~f:(typed m)
      ; roots =
          g.roots @ List.map wrappers ~f:(fun (p : Grammar.production) -> p.kind_name)
      ; metavariables = None
      }
;;
