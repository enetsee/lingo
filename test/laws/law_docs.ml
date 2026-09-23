(* -- the documentation backend ------------------------------------------------

      (a) No document holds a newline inside a text node.
      (b) Every tag the page opens is closed by its own closer.
      (c) Every production and every expression block has a section and a
          diagram.
      (d) Every diagram holds together: it stays inside the box it declares,
          and no rail crosses the inside of a station.
      (e) A listing holds every name its production does.
      (f) A coloured example gives its own bytes back.
      (g) Every coloured span is text a token with that scope matches.
      (h) Every token has a row in the token table.
      (i) A grammar gives the same page every time.
      (j) Every link lands on an anchor the page carries.

      Mechanism. Parts (b), (c), (e) to (h) and (j) read the emitted output
      back and walk it. A reader gets that output. A walk over the
      generator's own values would pass over anything that never reached the
      page.

      Part (a) is the claim handsome's [check] was written for, and this
      backend is the consumer. A listing is laid out to a width and then
      marked up. Those are two passes over one document. A newline inside a
      text node leaves every column after it counted wrong. That shows up as
      a line breaking in the wrong place, and a golden file records a wrong
      break as readily as a right one.

      Part (d) takes coordinates from {!Docs.Railroad.place} and checks the
      geometry as data. Reading the drawing code settles nothing here. Every
      coordinate it produces is sensible on its own. Add them up and the
      track may still leave the box. A browser clips whatever falls outside
      and shows no error.

      Parts (f) and (g) work as a pair. Part (f) says colouring leaves the
      text alone. Part (g) says the colour sits over the right text, and (f)
      cannot reach that. A scan that takes one byte too many emits the same
      bytes under a wider span, and the two readings are identical once the
      markup is off. So (g) requires a token with that scope whose regex
      matches the span's text whole. A check through the lexer would compare
      the scan with itself.

      Coverage. The twelve grammars in test/editors/editor_corpus.ml, with
      the examples that corpus draws from test/inputs: the first two, and the
      first that spans more than a line. Every example was one line until the
      third was added. Two mutations that should have reddened read zero over
      a corpus where the highlighter never broke a line. The wide grammar has
      no example in test/inputs, so parts (f) and (g) read nothing for it.

      What this says nothing about. Whether the page reads well. A diagram
      inside its box can still be unreadable. A reader judges that by eye, in
      test/editors/*.docs.html.

      Falsification. Every mutation was applied, run and reverted, and every
      count below was observed.

        M1  In [Highlight.lines], put the whole text in one node, newlines
            and all.
            -> part (a), 4 findings: sexp, comments, rust and effekt. Those
               four grammars have an example that runs past one line.
        M2  In [Highlight.example], advance one byte further than the scan
            matched.
            -> part (g), 22 findings across six grammars. Part (f) reads zero
               under it. The byte the scan took is still emitted, so the text
               comes back whole and only the colour has moved.
        M3  In [Render.fold], write the text without escaping it.
            -> part (b), 12 findings, all effekt, whose examples hold the only
               angle brackets in the corpus. Each reads as a tag the page
               never opened.
        M4  In [Tables.tokens], leave the trivia out.
            -> part (h), 17 findings, one per trivia token in the corpus.
        M5  In [Railroad.lay], rail a stack's entry row straight across
            instead of stopping at the row's edges.
            -> part (d), 55 findings. The part was rewritten for this fault.
               A loop puts its separator on the return row, and a rail run
               across that row goes through the separator. The drawing used to
               have a routine per shape. Only the loop's routine got it wrong,
               and one routine for every row closed it.
        M6  In [Mark.link], point a reference at the bare production name
            rather than at its anchor.
            -> part (j), 202 findings. Every reference in every listing then
               goes nowhere. A browser reports none of it.
        M7  In [Tables.tokens], drop the anchor from each row.
            -> part (j), 574 findings. Every token named in a listing or in a
               diagram points at that row.
        M8  In [Listing.child], leave the slot name off.
            -> part (e), 280 findings across every grammar. A reader cannot
               write a typed view against a manual that leaves a slot
               unnamed.
        M9  In [Docs.generate], draw no diagram for an expression block.
            -> part (c), 6 findings, one per grammar with a block.
   -------------------------------------------------------------------------- *)

let failures = ref 0

let fail : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt ->
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt -> Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt
;;

(* -- what the corpus emits ----------------------------------------------- *)

type page =
  { name : string
  ; grammar : Core.Grammar.t
  ; scopes : Scopes.t
  ; examples : (string * string) list
  ; out : Docs.output
  }

let pages : page list =
  List.map
    (fun (entry : Editor_corpus.entry) ->
       let scopes = Editor_corpus.scopes entry in
       let examples = Editor_corpus.examples entry in
       { name = entry.name
       ; grammar = entry.grammar
       ; scopes
       ; examples
       ; out = Docs.generate entry.grammar scopes ~title:entry.name ~examples ()
       })
    Editor_corpus.all
;;

(* -- (a) no newline inside a text node ----------------------------------- *)

(* This part checks the document itself, before layout and before markup. *)
let () =
  let checked = ref 0 in
  List.iter
    (fun (p : page) ->
       let check (what : string) (document : Docs.Render.document) : unit =
         incr checked;
         match Handsome.Utf8.check document with
         | Ok () -> ()
         | Error errors ->
           List.iter
             (fun error ->
                fail "(a) %s: %s holds %a" p.name what Handsome.Utf8.pp_error error)
             errors
       in
       List.iter
         (fun (production : Core.Grammar.production) ->
            check
              (Core.Grammar.Name.Rule.to_string production.kind_name)
              (Docs.Listing.production p.grammar production))
         p.grammar.productions;
       List.iter
         (fun (block : Core.Grammar.expr_def) ->
            check
              (Core.Grammar.Name.Rule.to_string block.rule_name)
              (Docs.Listing.block p.grammar block))
         p.grammar.expr;
       List.iter
         (fun (caption, source) -> check caption (Docs.Highlight.example p.scopes source))
         p.examples)
    pages;
  if !failures = 0
  then pass "(a) no document holds a newline in a text node, over %d of them" !checked
;;

(* -- (b) the markup balances --------------------------------------------- *)

(* Every tag name in the order it appears, openers and closers alike.
   Comments and self-closing tags are left out.

   The page comes from the generator, so a stack is enough. Each closer has
   to match the tag on top of the stack, and the stack has to empty. *)
let tags (html : string) : string list =
  let found = ref [] in
  let at = ref 0 in
  let width = String.length html in
  while !at < width do
    if html.[!at] = '<'
    then (
      match String.index_from_opt html !at '>' with
      | None -> at := width
      | Some close ->
        let inner = String.sub html (!at + 1) (close - !at - 1) in
        if
          String.length inner > 0
          && inner.[0] <> '!'
          && inner.[String.length inner - 1] <> '/'
        then (
          let stop = ref 0 in
          while
            !stop < String.length inner
            &&
            match inner.[!stop] with
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '/' -> true
            | _ -> false
          do
            incr stop
          done;
          found := String.sub inner 0 !stop :: !found);
        at := close + 1)
    else incr at
  done;
  List.rev !found
;;

let () =
  let before = !failures in
  let counted = ref 0 in
  List.iter
    (fun (p : page) ->
       let stack = ref [] in
       List.iter
         (fun tag ->
            incr counted;
            if String.length tag > 0 && tag.[0] = '/'
            then (
              let name = String.sub tag 1 (String.length tag - 1) in
              match !stack with
              | top :: rest when top = name -> stack := rest
              | top :: _ ->
                fail "(b) %s: </%s> closes <%s>" p.name name top;
                stack := []
              | [] -> fail "(b) %s: </%s> closes nothing" p.name name)
            else stack := tag :: !stack)
         (tags p.out.body);
       match !stack with
       | [] -> ()
       | open_ -> fail "(b) %s: %s left open" p.name (String.concat ", " (List.rev open_)))
    pages;
  if !failures = before
  then pass "(b) every tag is closed by its own closer, over %d of them" !counted
;;

(* -- (c) every production reaches the page ------------------------------- *)

let contains ~(needle : string) (haystack : string) : bool =
  let width = String.length needle in
  let rec look (at : int) : bool =
    if at + width > String.length haystack
    then false
    else if String.sub haystack at width = needle
    then true
    else look (at + 1)
  in
  width = 0 || look 0
;;

let () =
  let before = !failures in
  let counted = ref 0 in
  List.iter
    (fun (p : page) ->
       let named (name : string) : unit =
         incr counted;
         if not (contains ~needle:(Printf.sprintf "<h3>%s</h3>" name) p.out.body)
         then fail "(c) %s: the page has no section for %s" p.name name;
         if not (List.mem_assoc name p.out.diagrams)
         then fail "(c) %s: nothing drew a diagram for %s" p.name name
       in
       List.iter
         (fun (production : Core.Grammar.production) ->
            named (Core.Grammar.Name.Rule.to_string production.kind_name))
         p.grammar.productions;
       List.iter
         (fun (block : Core.Grammar.expr_def) ->
            named (Core.Grammar.Name.Rule.to_string block.rule_name))
         p.grammar.expr)
    pages;
  if !failures = before
  then pass "(c) every production has a section and a diagram, over %d of them" !counted
;;

(* -- (d) a diagram holds together ---------------------------------------- *)

(* A rail that crosses the inside of a station draws a line through a word.
   The paper this layout follows states the same condition over bounding
   boxes: a sublayout's box may not overlap an unrelated one.

   A loop with a separator on its return row used to draw the rail straight
   through the separator. *)
let () =
  let before = !failures in
  let counted = ref 0 in
  List.iter
    (fun (p : page) ->
       let one (name : string) (shape : Docs.Railroad.t) : unit =
         incr counted;
         let placed, extent = Docs.Railroad.place shape in
         List.iter
           (fun fault ->
              fail "(d) %s: %s, and %a" p.name name Docs.Railroad.pp_fault fault)
           (Docs.Railroad.check placed extent)
       in
       List.iter
         (fun (production : Core.Grammar.production) ->
            one
              (Core.Grammar.Name.Rule.to_string production.kind_name)
              (Docs.Railroad.of_production p.grammar production))
         p.grammar.productions;
       List.iter
         (fun (block : Core.Grammar.expr_def) ->
            one
              (Core.Grammar.Name.Rule.to_string block.rule_name)
              (Docs.Railroad.of_block p.grammar block))
         p.grammar.expr)
    pages;
  if !failures = before
  then pass "(d) every diagram holds together, over %d of them" !counted
;;

(* -- (e) a listing holds everything the production does ------------------ *)

let () =
  let before = !failures in
  let counted = ref 0 in
  List.iter
    (fun (p : page) ->
       let literal (name : string) : string =
         match
           List.find_opt
             (fun (token : Core.Grammar.token_def) ->
                Core.Grammar.Name.Token.to_string token.token_name = name)
             p.grammar.tokens
         with
         | Some { token_class = Core.Grammar.Keyword text; _ }
         | Some { token_class = Core.Grammar.Punctuation text; _ } -> "'" ^ text ^ "'"
         | _ -> name
       in
       List.iter
         (fun (production : Core.Grammar.production) ->
            let text =
              Docs.Render.text
                ~width:1_000_000
                (Docs.Listing.production p.grammar production)
            in
            let holds (what : string) : unit =
              incr counted;
              if not (contains ~needle:what text)
              then
                fail
                  "(e) %s: the listing of %s says nothing about %s"
                  p.name
                  (Core.Grammar.Name.Rule.to_string production.kind_name)
                  what
            in
            List.iter
              (fun (child : Core.Grammar.child) ->
                 holds (Core.Grammar.Name.Child.to_string child.name);
                 let symbol (one : Core.Grammar.symbol) : unit =
                   match one with
                   | Core.Grammar.Rule name -> holds name
                   | Core.Grammar.Token name -> holds (literal name)
                 in
                 match child.sym with
                 | Core.Grammar.Single one -> symbol one
                 | Core.Grammar.Alternatives many -> List.iter symbol many)
              production.children;
            match production.framing with
            | Core.Grammar.Delimited { open_tok; close_tok; _ } ->
              holds (literal (Core.Grammar.Name.Token.to_string open_tok));
              holds (literal (Core.Grammar.Name.Token.to_string close_tok))
            | Core.Grammar.Separated { sep; _ } ->
              holds (literal (Core.Grammar.Name.Token.to_string sep))
            | Core.Grammar.Plain | Core.Grammar.Committed _ -> ())
         p.grammar.productions)
    pages;
  if !failures = before
  then pass "(e) a listing holds everything its production does, over %d parts" !counted
;;

(* -- (f) an example keeps its own bytes ---------------------------------- *)

(* Every byte of the source comes back out, in order, once the markup is
   taken off. A highlighter that drops a character or reorders two shows the
   reader a different program. *)
let strip_markup (html : string) : string =
  let buf = Buffer.create (String.length html) in
  let at = ref 0 in
  let width = String.length html in
  while !at < width do
    (match html.[!at] with
     | '<' ->
       (match String.index_from_opt html !at '>' with
        | None -> at := width
        | Some close -> at := close)
     | '&' ->
       (match String.index_from_opt html !at ';' with
        | None -> Buffer.add_char buf '&'
        | Some close ->
          (match String.sub html !at (close - !at + 1) with
           | "&amp;" -> Buffer.add_char buf '&'
           | "&lt;" -> Buffer.add_char buf '<'
           | "&gt;" -> Buffer.add_char buf '>'
           | "&quot;" -> Buffer.add_char buf '"'
           | "&#39;" -> Buffer.add_char buf '\''
           | other -> Buffer.add_string buf other);
          at := close)
     | c -> Buffer.add_char buf c);
    incr at
  done;
  Buffer.contents buf
;;

let () =
  let before = !failures in
  let bytes = ref 0 in
  List.iter
    (fun (p : page) ->
       List.iter
         (fun (caption, source) ->
            let rendered =
              Docs.Render.html ~width:1_000_000 (Docs.Highlight.example p.scopes source)
            in
            let back = strip_markup rendered in
            bytes := !bytes + String.length source;
            if back <> source
            then
              fail
                "(f) %s: %s came back as %S where %S went in"
                p.name
                caption
                back
                source)
         p.examples)
    pages;
  if !failures = before
  then pass "(f) an example keeps its own bytes, over %d of them" !bytes
;;

(* -- (g) a coloured span is text its token matches ----------------------- *)

(* A span carries one class per prefix of its scope. The law requires a
   token whose own scope sits among those classes, and whose regex matches
   the span's text whole. The comparison goes through redfa, and the lexer
   takes no part in it.

   A comparison that runs past 2000 states counts as no match. *)
let spans (html : string) : (string * string) list =
  let found = ref [] in
  let at = ref 0 in
  let width = String.length html in
  let key = "<span class=\"" in
  while !at + String.length key <= width do
    if String.sub html !at (String.length key) = key
    then (
      match String.index_from_opt html !at '>' with
      | None -> at := width
      | Some open_close ->
        let classes =
          String.sub
            html
            (!at + String.length key)
            (open_close - !at - String.length key - 1)
        in
        (match String.index_from_opt html open_close '<' with
         | None -> at := width
         | Some close ->
           found
           := (classes, String.sub html (open_close + 1) (close - open_close - 1))
              :: !found;
           at := close))
    else incr at
  done;
  List.rev !found
;;

let () =
  let before = !failures in
  let counted = ref 0 in
  List.iter
    (fun (p : page) ->
       let facts = Scopes.facts p.scopes in
       List.iter
         (fun (caption, source) ->
            let rendered =
              Docs.Render.html ~width:1_000_000 (Docs.Highlight.example p.scopes source)
            in
            List.iter
              (fun (classes, text) ->
                 incr counted;
                 let text = strip_markup text in
                 let matched =
                   Array.exists
                     (fun (token : Core.Token.def) ->
                        match Scopes.token p.scopes token.id with
                        | None -> false
                        | Some scope ->
                          List.mem
                            (List.nth
                               (Docs.Mark.classes (Docs.Mark.Scoped scope))
                               (List.length (Docs.Mark.classes (Docs.Mark.Scoped scope))
                                - 1))
                            (String.split_on_char ' ' classes)
                          &&
                            (match Redfa.Regex.str text with
                            | exception Invalid_argument _ -> false
                            | literal ->
                              (match
                                 Redfa.Regex.equivalent_within
                                   ~max_states:2000
                                   (Redfa.Regex.inter literal token.regex)
                                   literal
                               with
                               | Some same -> same
                               | None -> false)))
                     Core.Facts.(facts.tokens)
                 in
                 if not matched
                 then
                   fail
                     "(g) %s: %s colours %S, and no token with that scope matches it"
                     p.name
                     caption
                     text)
              (spans rendered))
         p.examples)
    pages;
  if !failures = before
  then pass "(g) every coloured span is text its token matches, over %d of them" !counted
;;

(* -- (h) every token reaches the table ----------------------------------- *)

let () =
  let before = !failures in
  let counted = ref 0 in
  List.iter
    (fun (p : page) ->
       let table = Docs.Tables.tokens (Scopes.facts p.scopes) in
       Array.iter
         (fun (token : Core.Token.def) ->
            incr counted;
            let name = Core.Grammar.Name.Token.to_string token.name in
            if not (contains ~needle:("<code>" ^ name ^ "</code>") table)
            then fail "(h) %s: the token table has no row for %s" p.name name)
         Core.Facts.(Scopes.facts p.scopes).tokens)
    pages;
  if !failures = before then pass "(h) every token has a row, over %d of them" !counted
;;

(* -- (i) the same bytes every time --------------------------------------- *)

let () =
  let before = !failures in
  List.iter
    (fun (p : page) ->
       let again =
         Docs.generate p.grammar p.scopes ~title:p.name ~examples:p.examples ()
       in
       if again.page <> p.out.page
       then fail "(i) %s: two runs gave different bytes" p.name)
    pages;
  if !failures = before
  then pass "(i) a grammar gives the same page twice, over %d of them" (List.length pages)
;;

(* -- (j) every link lands somewhere -------------------------------------- *)

(* A reader follows a manual by its links. A link to an anchor the page
   lacks goes nowhere. The browser shows no error, and the reader stays
   where they were. *)
let anchors (html : string) : string list =
  let found = ref [] in
  let at = ref 0 in
  let key = "id=\"" in
  while !at + String.length key <= String.length html do
    if String.sub html !at (String.length key) = key
    then (
      let from = !at + String.length key in
      match String.index_from_opt html from '"' with
      | None -> at := String.length html
      | Some close ->
        found := String.sub html from (close - from) :: !found;
        at := close)
    else incr at
  done;
  !found
;;

let targets (html : string) : string list =
  let found = ref [] in
  let at = ref 0 in
  let key = "href=\"#" in
  while !at + String.length key <= String.length html do
    if String.sub html !at (String.length key) = key
    then (
      let from = !at + String.length key in
      match String.index_from_opt html from '"' with
      | None -> at := String.length html
      | Some close ->
        found := String.sub html from (close - from) :: !found;
        at := close)
    else incr at
  done;
  !found
;;

let () =
  let before = !failures in
  let counted = ref 0 in
  List.iter
    (fun (p : page) ->
       let carried = anchors p.out.body in
       List.iter
         (fun target ->
            incr counted;
            if not (List.mem target carried)
            then
              fail
                "(j) %s: a link goes to %S, and the page has no such anchor"
                p.name
                target)
         (targets p.out.body))
    pages;
  if !failures = before
  then pass "(j) every link lands on an anchor the page carries, over %d of them" !counted
;;

let () =
  if !failures = 0
  then print_endline "law_docs: 0 failures"
  else (
    Printf.printf "law_docs: %d failures\n" !failures;
    exit 1)
;;
