open StdLabels
module Mark = Mark
module Render = Render
module Listing = Listing
module Railroad = Railroad
module Tables = Tables
module Highlight = Highlight

type output =
  { page : string
  ; body : string
  ; stylesheet : string
  ; diagrams : (string * string) list
  }

let escape = Render.escape

(* A scope is a dotted path and a span carries a class per prefix, so a rule
   written for [lg-s-keyword] reaches a span carrying
   [lg-s-keyword-control-let]. Only the scope roots are styled here. A page
   drawing a finer distinction writes the longer class of its own.

   Some classes the backend emits have no rule here. [lg-arrow] carries its
   fill as an attribute, and [lg-listing] and [lg-example] take the [pre]
   rule. *)
let stylesheet =
  {css|  .lingo-reference { font-family: system-ui, sans-serif; line-height: 1.5; }
  .lingo-reference pre { font-family: ui-monospace, monospace; font-size: 0.9rem;
    overflow-x: auto; padding: 0.6rem 0.8rem; background: #f6f7f9; border-radius: 4px; }
  .lingo-reference table { border-collapse: collapse; margin: 0.6rem 0; }
  .lingo-reference th, .lingo-reference td {
    border-bottom: 1px solid #dde; padding: 0.25rem 0.8rem 0.25rem 0; text-align: left;
    vertical-align: top; }
  .lingo-reference th { font-weight: 600; }
  .lingo-reference section { margin: 1.4rem 0; }
  .lingo-reference a { color: inherit; text-decoration-color: #bbc; }
  .lingo-reference a:hover { text-decoration-color: currentColor; }
  .lg-contents { columns: 12rem; list-style: none; padding: 0; margin: 0.6rem 0; }
  .lg-diagram a rect { transition: fill 0.1s; }
  .lg-diagram a:hover rect { fill: #eef1f8; }

  .lg-rule { font-weight: 600; }
  .lg-ref { color: #2a4b8d; }
  .lg-token { color: #6b4fa0; }
  .lg-literal { color: #14713d; }
  .lg-child { color: #777; }
  .lg-notation { color: #999; }

  .lg-diagram { display: block; margin: 0.4rem 0 0.2rem; max-width: 100%; }
  .lg-track { fill: none; stroke: #444; stroke-width: 1.4; }
  .lg-terminal { fill: #eaf6ee; stroke: #14713d; stroke-width: 1.2; }
  .lg-nonterminal { fill: #eef1f8; stroke: #2a4b8d; stroke-width: 1.2; }
  .lg-terminal-text, .lg-nonterminal-text {
    font-family: ui-monospace, monospace; font-size: 12px; text-anchor: middle; }
  .lg-slot { font-family: system-ui, sans-serif; font-size: 10px; fill: #777; }

  .lg-s-keyword { color: #a02020; font-weight: 600; }
  .lg-s-storage { color: #a02020; }
  .lg-s-entity { color: #2a4b8d; font-weight: 600; }
  .lg-s-variable { color: #333; }
  .lg-s-string { color: #14713d; }
  .lg-s-comment { color: #888; font-style: italic; }
  .lg-s-constant { color: #8a5000; }
  .lg-s-punctuation { color: #666; }
|css}
;;

let section ~(id : string) ~(heading : string) (body : string) : string =
  if String.trim body = ""
  then ""
  else Printf.sprintf "  <h2 id=%S>%s</h2>\n" id (escape heading) ^ body
;;

let listing_html ~(width : int) (document : Render.document) : string =
  "  <pre class=\"lg-listing\">" ^ Render.html ~width document ^ "</pre>\n"
;;

let generate
      (grammar : Core.Grammar.t)
      (scopes : Scopes.t)
      ?title
      ?(examples = [])
      ?(width = 76)
      ()
  : output
  =
  let facts = Scopes.facts scopes in
  let title =
    match title with
    | Some title -> title
    | None ->
      (match grammar.roots with
       | root :: _ -> Core.Grammar.Name.Rule.to_string root
       | [] -> "grammar")
  in
  let diagrams =
    List.map grammar.productions ~f:(fun (production : Core.Grammar.production) ->
      let named = Core.Grammar.Name.Rule.to_string production.kind_name in
      named, Railroad.svg ~title:named (Railroad.of_production grammar production))
    @ List.map grammar.expr ~f:(fun (block : Core.Grammar.expr_def) ->
      let named = Core.Grammar.Name.Rule.to_string block.rule_name in
      named, Railroad.svg ~title:named (Railroad.of_block grammar block))
  in
  let one ~(name : string) ~(document : Render.document) : string =
    Printf.sprintf "  <section id=%S>\n" (escape (Mark.rule_anchor name))
    ^ Printf.sprintf "  <h3>%s</h3>\n" (escape name)
    ^ listing_html ~width document
    ^ Option.value (List.assoc_opt name diagrams) ~default:""
    ^ "  </section>\n"
  in
  (* Every production, in one list at the top, so a reader can jump straight
     to one. *)
  let contents : string =
    let entry (name : string) : string =
      Printf.sprintf
        "    <li><a href=\"#%s\"><code>%s</code></a></li>\n"
        (escape (Mark.rule_anchor name))
        (escape name)
    in
    let names =
      List.map grammar.productions ~f:(fun (production : Core.Grammar.production) ->
        Core.Grammar.Name.Rule.to_string production.kind_name)
      @ List.map grammar.expr ~f:(fun (block : Core.Grammar.expr_def) ->
        Core.Grammar.Name.Rule.to_string block.rule_name)
    in
    match names with
    | [] -> ""
    | names ->
      "  <ul class=\"lg-contents\">\n"
      ^ String.concat ~sep:"" (List.map names ~f:entry)
      ^ "  </ul>\n"
  in
  let productions =
    String.concat
      ~sep:""
      (List.map grammar.productions ~f:(fun (production : Core.Grammar.production) ->
         one
           ~name:(Core.Grammar.Name.Rule.to_string production.kind_name)
           ~document:(Listing.production grammar production))
       @ List.map grammar.expr ~f:(fun (block : Core.Grammar.expr_def) ->
         one
           ~name:(Core.Grammar.Name.Rule.to_string block.rule_name)
           ~document:(Listing.block grammar block)))
  in
  let examples_html =
    String.concat
      ~sep:""
      (List.map examples ~f:(fun (caption, source) ->
         Printf.sprintf "  <section>\n  <h3>%s</h3>\n" (escape caption)
         ^ "  <pre class=\"lg-example\">"
         ^ Render.html ~width:1_000_000 (Highlight.example scopes source)
         ^ "</pre>\n  </section>\n"))
  in
  let body =
    Printf.sprintf "<article class=\"lingo-reference\">\n  <h1>%s</h1>\n" (escape title)
    ^ section ~id:"productions" ~heading:"Productions" (contents ^ productions)
    ^ section ~id:"operators" ~heading:"Operators" (Tables.precedence grammar)
    ^ section ~id:"tokens" ~heading:"Tokens" (Tables.tokens facts)
    ^ section
        ~id:"sets"
        ~heading:"What starts and follows a production"
        (Tables.first_follow facts)
    ^ section ~id:"examples" ~heading:"Examples" examples_html
    ^ "</article>\n"
  in
  let page =
    "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
    ^ Printf.sprintf "<title>%s</title>\n<style>\n" (escape title)
    ^ stylesheet
    ^ "</style>\n</head>\n<body>\n"
    ^ body
    ^ "</body>\n</html>\n"
  in
  { page; body; stylesheet; diagrams }
;;
