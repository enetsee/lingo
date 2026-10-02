(* -- the tree-sitter backend --------------------------------------------------

      (a) Every [$.name] in an emitted grammar names a rule it declares.
      (b) Every rule is reachable, from the start rule or from the extras.
      (c) The rule tree-sitter starts at is the grammar's root, wherever the
          author declared it.
      (d) Every node, field and literal a query names is one the grammar has.
      (e) One precedence per operator, and one per greedy child.
      (f) A grammar emits the same bytes every time.
      (g) Every scope outside the two escape hatches translates to a capture,
          and every capture it translates to is carried.
      (h) Every regex is balanced and has no capturing group.
      (i) The keyword-extraction directive names the token whose language
          holds every keyword. It is written only where exactly one token
          holds them all.
      (j) Every {!Treesitter.Check.problem} has a witness.
      (k) An intersection that denotes a character class comes out as one.
      (l) Every binder and every scope the grammar declares reaches
          [locals.scm], and nothing else in that file claims to be one.
      (m) In [highlights.scm], a capture on one child position comes after
          the capture on the token itself.

      Mechanism. The law reads the emitted text back and walks it. tree-sitter
      is handed four files, and a walk over the emitter's own values would
      miss a rule that never reached the page. So the JavaScript is scanned
      for its rule headings, its [$.] references, its [field] names and its
      regex literals. The queries are parsed back into the four shapes they
      take.

      Part (d) does the most work. A query can name a node the grammar does
      not have. It then matches nothing, and nothing anywhere says so: the
      file loads, the editor runs, and the colour is absent. The same goes for
      a field name and for a literal.

      Part (i) compares two languages. Keyword extraction matters where the
      identifier pattern would swallow a keyword, and redfa settles that
      exactly by meeting the two languages. The law settles it a second time,
      with no help from the emitter.

      Part (l) reads the corpus grammar rather than the facts derived from it.
      A binder is declared on a production, carried into [Rule.def], and
      printed. Reading the middle of that chain would compare the emitter
      against the value it was handed.

      Coverage. The thirteen grammars in test/editors/editor_corpus.ml. Parts
      (c), (j) and (k) build their own witnesses. The corpus holds no grammar
      this backend rejects and no longer holds a term that needs the reduction
      (k) is about. rust, effekt and ml are the three that declare binders and
      scopes, so every count in part (l) comes from them. rust, wide and ml
      are the three that scope a child position, so every count in part (m)
      comes from those.

      ml is the only one with several roots, so it alone reaches part (c)'s
      claim about the rule tree-sitter starts at: with one root that is the
      root's own node, and with several it is the one the emitter makes up to
      choose between them.

      What tree-sitter does, which the emitter has to match. It takes the last
      pattern that matches a node, so the highlight sections go catch-alls
      first and positions last: written the other way round, [(ident)
      @variable] beat every per-position capture and rust's type and function
      names rendered as variables. Checked against tree-sitter 0.26.9 both
      ways round. It also takes no part in recovery, so a query for a resync
      anchor can never fire, and a query file is UTF-8, so a separator is
      written as itself rather than as escapes.

      Why three parts carry a witness of their own. Every grammar in the
      corpus declares its root first, declares its identifier before its other
      pattern tokens, and writes no intersection to reduce. Parts (c), (i) and
      (k) are about those three, so the corpus cannot reach any of them and
      each has a witness instead.

      A law may not build what it expects from the value it is checking. Part
      (l) reads the grammar rather than [Rule.def.binders], because a law that
      read the field would agree with it however the field was built, and a
      binder dropped on the way there would leave both sides saying the same
      wrong thing. The same reasoning puts part (g) where it is: a scope that
      quietly translates to nothing leaves the trace of one that was never
      set, so the part has to separate them.

      What this says nothing about. Whether tree-sitter's generator finds a
      conflict. lingo checks LL(1) and tree-sitter builds an LR automaton, and
      a grammar can pass the first and fail the second. The binding powers
      carry over as precedences, and that covers most of it. The rest needs
      the generator in the loop.

   -------------------------------------------------------------------------- *)

(* The six blocks below are generated, and they are the evidence. assay derives
   a mutation from the code rather than from a sentence beside it, applies
   every one, and records what went red. Regenerate them with

     assay -config assay.conf -only treesitter

   and take the counts as they come: they move whenever the corpus grows, and
   asserting them exactly would train everyone to ignore a red suite. What
   they assert is that every mutant dies. A survivor is the finding, and the
   lines it names are where to look. *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      editors/treesitter/grammar_js.ml, 90 mutants, 87 killed, 3 survived.
        extreme     12  all killed, law_treesitter (a): 5, law_treesitter (e): 3, dump_treesitter: 1, law_treesitter (b): 1, law_treesitter (c): 1, law_treesitter (d): 1
        sbr         50  48 killed, law_treesitter (b): 17, dump_treesitter: 15, law_treesitter (d): 6, law_treesitter (a): 5, law_treesitter (c): 2, law_treesitter (e): 2, law_treesitter (i): 1; 2 survived
        ror         11  10 killed, dump_treesitter: 3, law_treesitter (a): 3, law_treesitter (b): 2, law_treesitter (e): 2; 1 survived
        lcr          2  all killed, (a) 1 (b) 1
        aor          2  all killed, law_treesitter: 2
        uoi         13  all killed, law_treesitter (e): 4, dump_treesitter: 3, law_treesitter (a): 3, law_treesitter (b): 3
      survived at lines 105 282 354
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      editors/treesitter/queries.ml, 50 mutants, 34 killed, 16 survived.
        extreme     17  16 killed, law_treesitter (g): 11, law_treesitter (l): 3, dump_treesitter: 2; 1 survived
        sbr         28  14 killed, law_treesitter (g): 6, dump_treesitter: 4, law_treesitter (d): 3, law_treesitter (l): 1; 14 survived
        ror          1  all killed, dump_treesitter: 1
        aor          1  0 killed; 1 survived
        uoi          3  all killed, dump_treesitter: 1, law_treesitter (d): 1, law_treesitter (l): 1
      survived at lines 50 55 92 95 96 97 98 99 104 108 150 177 179 233 242 289
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      editors/treesitter/js.ml, 73 mutants, 67 killed, 6 survived.
        extreme     14  all killed, law_treesitter (k): 7, law_treesitter (b): 3, dump_treesitter: 2, law_treesitter (j): 2
        sbr         29  27 killed, dump_treesitter: 11, law_treesitter (k): 5, law_treesitter (d): 3, law_treesitter (h): 3, law_treesitter (b): 2, law_treesitter (e): 2, law_treesitter (j): 1; 2 survived
        ror          9  7 killed, law_treesitter (k): 4, dump_treesitter: 3; 2 survived
        lcr          5  4 killed, law_treesitter (k): 3, dump_treesitter: 1; 1 survived
        aor          4  3 killed, dump_treesitter: 3; 1 survived
        uoi         12  all killed, law_treesitter (k): 8, dump_treesitter: 2, law_treesitter (d): 1, law_treesitter sexp:: 1
      survived at lines 11 16 45 46 99 115
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      editors/treesitter/check.ml, 13 mutants, 9 killed, 4 survived.
        extreme      6  4 killed, (j) 4; 2 survived
        sbr          4  2 killed, (j) 2; 2 survived
        uoi          3  all killed, sexp: 3
      survived at lines 33 37 65 82
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      editors/treesitter/treesitter.ml, 5 mutants, 4 killed, 1 survived.
        extreme      3  all killed, (i) 3
        sbr          1  0 killed; 1 survived
        lcr          1  all killed, (i) 1
      survived at lines 70
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      editors/treesitter/node.ml, 3 mutants, 3 killed.
        extreme      3  all killed, sexp: 2 ml: 1
   ---------------------------------------------------------------------- *)

(* -- what the corpus emits ----------------------------------------------- *)

type emitted =
  { name : string
  ; grammar : Core.Grammar.t
  ; facts : Core.Facts.t
  ; scopes : Scopes.t
  ; out : Treesitter.output
  }

let emitted : emitted list =
  List.map
    (fun (entry : Editor_corpus.entry) ->
       let scopes = Editor_corpus.scopes entry in
       let common =
         { name = entry.name
         ; grammar = entry.grammar
         ; facts = Scopes.facts scopes
         ; scopes
         ; out = { grammar_js = ""; highlights = ""; folds = ""; locals = "" }
         }
       in
       match Treesitter.generate scopes ~language:entry.name () with
       | Error problems ->
         List.iter
           (fun p -> Law.fail "%s: %a" entry.name Treesitter.Check.pp_problem p)
           problems;
         common
       | Ok out -> { common with out })
    Editor_corpus.all
;;

(* -- reading the emitted JavaScript back --------------------------------- *)

let lines (text : string) : string list = String.split_on_char '\n' text

let starts_with ~(prefix : string) (s : string) : bool =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix
;;

let is_name_char (c : char) : bool =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

(* Every occurrence of [needle] in [haystack], as the offsets just past it. *)
let occurrences ~(needle : string) (haystack : string) : int list =
  let width = String.length needle in
  let found = ref [] in
  for start = String.length haystack - width downto 0 do
    if String.sub haystack start width = needle then found := (start + width) :: !found
  done;
  !found
;;

let name_at (s : string) (from : int) : string =
  let stop = ref from in
  while !stop < String.length s && is_name_char s.[!stop] do
    incr stop
  done;
  String.sub s from (!stop - from)
;;

(* A rule opens a line at four spaces and closes where the next one opens. *)
let rule_bodies (text : string) : (string * string) list =
  let opens (line : string) : string option =
    if not (starts_with ~prefix:"    " line)
    then None
    else (
      let name = name_at line 4 in
      let after = 4 + String.length name in
      if
        String.length name > 0
        && String.length line >= after + 7
        && String.sub line after 7 = ": $ => "
      then Some name
      else None)
  in
  let rec gather (current : (string * Buffer.t) option) (acc : (string * string) list)
    : string list -> (string * string) list
    = function
    | [] ->
      (match current with
       | None -> List.rev acc
       | Some (name, buf) -> List.rev ((name, Buffer.contents buf) :: acc))
    | line :: rest ->
      (match opens line with
       | Some name ->
         let acc =
           match current with
           | None -> acc
           | Some (previous, buf) -> (previous, Buffer.contents buf) :: acc
         in
         let buf = Buffer.create 64 in
         Buffer.add_string buf line;
         gather (Some (name, buf)) acc rest
       | None ->
         (match current with
          | None -> gather None acc rest
          | Some (name, buf) ->
            if starts_with ~prefix:"  }" line
            then gather None ((name, Buffer.contents buf) :: acc) rest
            else (
              Buffer.add_char buf '\n';
              Buffer.add_string buf line;
              gather (Some (name, buf)) acc rest)))
  in
  gather None [] (lines text)
;;

let references (text : string) : string list =
  List.map (fun at -> name_at text at) (occurrences ~needle:"$." text)
;;

let fields (text : string) : string list =
  List.filter_map
    (fun at ->
       (* [field('name', ...] and [at] is just past the quote. *)
       match String.index_from_opt text at '\'' with
       | None -> None
       | Some close -> Some (String.sub text at (close - at)))
    (occurrences ~needle:"field('" text)
;;

(* Every single-quoted string in the emitted file. Those are the literal
   tokens the grammar matches, plus the field names and the language's own
   name. *)
let quoted (text : string) : string list =
  let found = ref [] in
  let at = ref 0 in
  let width = String.length text in
  while !at < width do
    if text.[!at] = '\''
    then (
      let stop = ref (!at + 1) in
      while !stop < width && text.[!stop] <> '\'' do
        if text.[!stop] = '\\' then incr stop;
        incr stop
      done;
      if !stop < width then found := String.sub text (!at + 1) (!stop - !at - 1) :: !found;
      at := !stop + 1)
    else incr at
  done;
  List.rev !found
;;

(* A regex is always a whole line's worth: the body of a token rule, or one
   entry of [extras]. *)
let regexes (text : string) : string list =
  List.filter_map
    (fun line ->
       let trimmed = String.trim line in
       let trimmed =
         if String.length trimmed > 0 && trimmed.[String.length trimmed - 1] = ','
         then String.sub trimmed 0 (String.length trimmed - 1)
         else trimmed
       in
       let body =
         match String.index_opt trimmed '/' with
         | Some 0 -> Some trimmed
         | _ ->
           (match String.index_opt trimmed '>' with
            | Some arrow
              when arrow + 2 < String.length trimmed && trimmed.[arrow + 2] = '/' ->
              Some (String.sub trimmed (arrow + 2) (String.length trimmed - arrow - 2))
            | _ -> None)
       in
       match body with
       | Some body
         when String.length body >= 2
              && body.[0] = '/'
              && body.[String.length body - 1] = '/' ->
         Some (String.sub body 1 (String.length body - 2))
       | _ -> None)
    (lines text)
;;

(* -- reading the queries back -------------------------------------------- *)

type query =
  { node : string option
  ; literal : string option
  ; field : string option
  ; inner : string option
  }

let parse_query (line : string) : query option =
  let line = String.trim line in
  if String.length line = 0 || line.[0] = ';'
  then None
  else if line.[0] = '"'
  then (
    match String.index_from_opt line 1 '"' with
    | None -> None
    | Some close ->
      Some
        { node = None
        ; literal = Some (String.sub line 1 (close - 1))
        ; field = None
        ; inner = None
        })
  else if line.[0] = '('
  then (
    let node = name_at line 1 in
    let after = 1 + String.length node in
    if String.length line <= after || line.[after] <> ' '
    then Some { node = Some node; literal = None; field = None; inner = None }
    else (
      let field = name_at line (after + 1) in
      let colon = after + 1 + String.length field in
      if String.length line <= colon || line.[colon] <> ':'
      then Some { node = Some node; literal = None; field = None; inner = None }
      else (
        let rest = colon + 2 in
        if String.length line > rest && line.[rest] = '('
        then
          Some
            { node = Some node
            ; literal = None
            ; field = Some field
            ; inner = Some (name_at line (rest + 1))
            }
        else if String.length line > rest && line.[rest] = '"'
        then (
          match String.index_from_opt line (rest + 1) '"' with
          | None -> None
          | Some close ->
            Some
              { node = Some node
              ; literal = Some (String.sub line (rest + 1) (close - rest - 1))
              ; field = Some field
              ; inner = None
              })
        else Some { node = Some node; literal = None; field = Some field; inner = None })))
  else None
;;

let queries (text : string) : query list = List.filter_map parse_query (lines text)

(* The [extras] list, as text. It runs from its own line to the line that
   closes the bracket. *)
let extras_of (text : string) : string =
  let rec collect (inside : bool) (acc : string list) : string list -> string list
    = function
    | [] -> List.rev acc
    | line :: rest ->
      if starts_with ~prefix:"  extras:" line
      then collect true acc rest
      else if inside && starts_with ~prefix:"  ]" line
      then List.rev acc
      else collect inside (if inside then line :: acc else acc) rest
  in
  String.concat "\n" (collect false [] (lines text))
;;

(* -- (a) every reference names a rule ------------------------------------ *)

let () =
  let checked = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let bodies = rule_bodies e.out.grammar_js in
       let names = List.map fst bodies in
       List.iter
         (fun (owner, body) ->
            List.iter
              (fun target ->
                 incr checked;
                 if not (List.mem target names)
                 then
                   Law.fail
                     "(a) %s: %s refers to $.%s, and no rule declares it"
                     e.name
                     owner
                     target)
              (references body))
         bodies)
    emitted;
  if Law.failures () = 0
  then Law.pass "(a) every reference names a rule, over %d of them" !checked
;;

(* -- (b) every rule is reachable, and (c) the first is the root ---------- *)

let () =
  let before = Law.failures () in
  let total = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let bodies = rule_bodies e.out.grammar_js in
       let node_of (root : Core.Rule.id) : string =
         Treesitter.Node.of_rule (Core.Facts.rule e.facts root).name
       in
       (match bodies, Core.Facts.(e.facts.roots) with
        | [], _ -> Law.fail "(c) %s: the grammar has no rules" e.name
        | _ :: _, [] -> ()
        | (first, _) :: _, roots ->
          (* One root is the rule tree-sitter starts at. Several need one made
             up to choose between them, which is what the emitter names
             {!Treesitter.Node.start}. *)
          let expected =
            match roots with
            | [ root ] -> node_of root
            | [] | _ :: _ :: _ -> Treesitter.Node.start
          in
          if first <> expected
          then
            Law.fail
              "(c) %s: tree-sitter would start at %S, and the grammar's root is %S"
              e.name
              first
              expected;
          (* The made-up rule is the only thing that names the roots, so a root
             it leaves out is one no parse can start at. *)
          (match roots with
           | [] | [ _ ] -> ()
           | _ :: _ :: _ ->
             let named =
               match List.assoc_opt first bodies with
               | Some body -> references body
               | None -> []
             in
             List.iter
               (fun root ->
                  let name = node_of root in
                  if not (List.mem name named)
                  then
                    Law.fail
                      "(c) %s: the start rule does not reach the root %S"
                      e.name
                      name)
               roots));
       let seen = Hashtbl.create 64 in
       let rec walk (name : string) : unit =
         if not (Hashtbl.mem seen name)
         then (
           Hashtbl.replace seen name ();
           match List.assoc_opt name bodies with
           | None -> ()
           | Some body -> List.iter walk (references body))
       in
       (* The walk starts twice. A rule body reaches what its production
          refers to. The [extras] list reaches the trivia rules, which no
          production refers to, because a parser takes trivia on its own. *)
       (match bodies with
        | (first, _) :: _ -> walk first
        | [] -> ());
       List.iter walk (references (extras_of e.out.grammar_js));
       List.iter
         (fun (name, _) ->
            incr total;
            if not (Hashtbl.mem seen name)
            then Law.fail "(b) %s: nothing reaches the rule %S" e.name name)
         bodies)
    emitted;
  if Law.failures () = before
  then
    Law.pass "(b) every rule is reachable and the first is the root, over %d rules" !total
;;

(* -- (d) every query names something that exists ------------------------- *)

let () =
  let before = Law.failures () in
  let checked = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let bodies = rule_bodies e.out.grammar_js in
       let names = List.map fst bodies in
       let literals = quoted e.out.grammar_js in
       let check (where : string) (query : query) : unit =
         incr checked;
         (match query.node with
          | Some node when not (List.mem node names) ->
            Law.fail
              "(d) %s: %s names the node %S, and the grammar has none"
              e.name
              where
              node
          | _ -> ());
         (match query.node, query.field with
          | Some node, Some field ->
            (match List.assoc_opt node bodies with
             | Some body when not (List.mem field (fields body)) ->
               Law.fail
                 "(d) %s: %s names the field %S of %S, and the rule has no such field"
                 e.name
                 where
                 field
                 node
             | _ -> ())
          | _ -> ());
         (match query.inner with
          | Some inner when not (List.mem inner names) ->
            Law.fail
              "(d) %s: %s names the node %S, and the grammar has none"
              e.name
              where
              inner
          | _ -> ());
         match query.literal with
         | Some literal when not (List.mem literal literals) ->
           Law.fail
             "(d) %s: %s names the literal %S, and the grammar matches no such text"
             e.name
             where
             literal
         | _ -> ()
       in
       List.iter (check "highlights.scm") (queries e.out.highlights);
       List.iter (check "folds.scm") (queries e.out.folds);
       List.iter (check "locals.scm") (queries e.out.locals))
    emitted;
  if Law.failures () = before
  then Law.pass "(d) every query names something that exists, over %d patterns" !checked
;;

(* -- (e) one precedence per operator ------------------------------------- *)

let () =
  let before = Law.failures () in
  let total = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let expected =
         Array.fold_left
           (fun acc (block : Core.Block.def) ->
              acc
              + Array.length block.infix
              + Array.length block.prefix
              + Array.length block.postfix)
           0
           Core.Facts.(e.facts.blocks)
         + Array.fold_left
             (fun acc (rule : Core.Rule.def) ->
                if Array.exists (fun (c : Core.Rule.child) -> c.greedy) rule.children
                then acc + 1
                else acc)
             0
             Core.Facts.(e.facts.rules)
       in
       let found =
         List.length (occurrences ~needle:"prec.left(" e.out.grammar_js)
         + List.length (occurrences ~needle:"prec.right(" e.out.grammar_js)
       in
       total := !total + found;
       if found <> expected
       then
         Law.fail
           "(e) %s: %d operators and a greedy child apiece need %d precedences, and the \
            grammar has %d"
           e.name
           expected
           expected
           found)
    emitted;
  if Law.failures () = before
  then Law.pass "(e) one precedence per operator, over %d of them" !total
;;

(* -- (f) the same bytes every time --------------------------------------- *)

let () =
  let before = Law.failures () in
  List.iter
    (fun (e : emitted) ->
       match Treesitter.generate e.scopes ~language:e.name () with
       | Error _ ->
         Law.fail "(f) %s: the second run rejected what the first emitted" e.name
       | Ok again ->
         if
           again.grammar_js <> e.out.grammar_js
           || again.highlights <> e.out.highlights
           || again.folds <> e.out.folds
         then Law.fail "(f) %s: two runs gave different bytes" e.name)
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(f) a grammar emits the same bytes twice, over %d of them"
      (List.length emitted)
;;

(* -- (g) every scope that translates reaches the queries ----------------- *)

let () =
  let before = Law.failures () in
  let carried = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let captures =
         List.filter_map
           (fun line ->
              match String.index_opt line '@' with
              | None -> None
              | Some at ->
                let stop = ref (at + 1) in
                while
                  !stop < String.length line
                  && (is_name_char line.[!stop]
                      || line.[!stop] = '.'
                      || line.[!stop] = '-')
                do
                  incr stop
                done;
                Some (String.sub line (at + 1) (!stop - at - 1)))
           (lines e.out.highlights)
       in
       let check (what : string) (scope : Scopes.Scope.t option) : unit =
         match scope with
         | None -> ()
         | Some scope ->
           (* Every arm of the vocabulary has a tree-sitter word. Two arms
              have none, on purpose. A scope that quietly translates to
              nothing is a colour the author asked for and will not see. *)
           (match scope, Treesitter.Queries.capture scope with
            | (Scopes.Scope.Meta _ | Scopes.Scope.Custom _), _ -> ()
            | _, None ->
              Law.fail
                "(g) %s: %s is scoped %a and nothing here translates it"
                e.name
                what
                Scopes.Scope.pp
                scope
            | _, Some _ -> ());
           (match Treesitter.Queries.capture scope with
            | None -> ()
            | Some capture ->
              incr carried;
              if not (List.mem capture captures)
              then
                Law.fail
                  "(g) %s: %s translates to @%s and nothing in highlights.scm carries it"
                  e.name
                  what
                  capture)
       in
       Array.iter
         (fun (token : Core.Token.def) ->
            check
              ("the token " ^ Core.Grammar.Name.Token.to_string token.name)
              (Scopes.token e.scopes token.id))
         Core.Facts.(e.facts.tokens);
       Array.iter
         (fun (rule : Core.Rule.def) ->
            let named = Core.Grammar.Name.Rule.to_string rule.name in
            check (named ^ ", by its identity child") (Scopes.identity e.scopes rule.id);
            Array.iteri
              (fun index (child : Core.Rule.child) ->
                 check
                   (named ^ "." ^ Core.Grammar.Name.Child.to_string child.child_name)
                   (Scopes.child e.scopes rule.id ~child:index))
              rule.children)
         Core.Facts.(e.facts.rules))
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(g) every scope that translates reaches the queries, over %d of them"
      !carried
;;

(* -- (h) every regex is groupless and balanced --------------------------- *)

(* A capturing group in a tree-sitter regex is legal and useless. tree-sitter
   reads the source and hands it to a second engine, and the two can number a
   group differently. Everything emitted here is [(?:...)]. *)
let capture_groups (regex : string) : int =
  let width = String.length regex in
  let count = ref 0 in
  let in_class = ref false in
  let at = ref 0 in
  while !at < width do
    (match regex.[!at] with
     | '\\' -> incr at
     | '[' when not !in_class -> in_class := true
     | ']' when !in_class -> in_class := false
     | '(' when not !in_class ->
       if not (!at + 1 < width && regex.[!at + 1] = '?') then incr count
     | _ -> ());
    incr at
  done;
  !count
;;

let unbalanced (regex : string) : bool =
  let depth = ref 0 in
  let in_class = ref false in
  let at = ref 0 in
  let bad = ref false in
  while !at < String.length regex do
    (match regex.[!at] with
     | '\\' -> incr at
     | '[' when not !in_class -> in_class := true
     | ']' when !in_class -> in_class := false
     | '(' when not !in_class -> incr depth
     | ')' when not !in_class ->
       decr depth;
       if !depth < 0 then bad := true
     | _ -> ());
    incr at
  done;
  !bad || !depth <> 0 || !in_class
;;

let () =
  let before = Law.failures () in
  let checked = ref 0 in
  List.iter
    (fun (e : emitted) ->
       List.iter
         (fun regex ->
            incr checked;
            (match capture_groups regex with
             | 0 -> ()
             | n -> Law.fail "(h) %s: the regex %S has %d capturing groups" e.name regex n);
            if unbalanced regex
            then Law.fail "(h) %s: the regex %S does not balance" e.name regex)
         (regexes e.out.grammar_js))
    emitted;
  if Law.failures () = before
  then Law.pass "(h) every regex is groupless and balanced, over %d of them" !checked
;;

(* -- (i) the keyword-extraction token ------------------------------------ *)

let () =
  let before = Law.failures () in
  let tested = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let keywords =
         Array.to_list Core.Facts.(e.facts.tokens)
         |> List.filter_map (fun (token : Core.Token.def) ->
           match token.klass with
           | Core.Grammar.Keyword text -> Some text
           | _ -> None)
       in
       let holds (token : Core.Token.def) (text : string) : bool =
         match token.klass with
         | Core.Grammar.Pattern { lexer; _ } ->
           (match
              Redfa.Regex.is_empty_language_within
                ~max_states:2000
                (Redfa.Regex.inter (Redfa.Regex.str text) lexer)
            with
            | Some empty -> not empty
            | None -> false)
         | _ -> false
       in
       let holders =
         Array.to_list Core.Facts.(e.facts.tokens)
         |> List.filter (fun (token : Core.Token.def) ->
           (not (Core.Token.is_trivia token))
           && keywords <> []
           && List.for_all (holds token) keywords)
       in
       let declared =
         List.exists
           (fun line -> starts_with ~prefix:"  word:" line)
           (lines e.out.grammar_js)
       in
       incr tested;
       match holders, declared with
       | [ only ], true ->
         let expected = "  word: $ => $." ^ Treesitter.Node.of_token only.name ^ "," in
         if not (List.exists (fun line -> line = expected) (lines e.out.grammar_js))
         then
           Law.fail
             "(i) %s: the token that holds every keyword is %S and the directive names \
              another"
             e.name
             (Core.Grammar.Name.Token.to_string only.name)
       | [ _ ], false ->
         Law.fail "(i) %s: one token holds every keyword and no directive names it" e.name
       | _, true ->
         Law.fail
           "(i) %s: %d tokens hold every keyword, so no directive should have been \
            written"
           e.name
           (List.length holders)
       | _, false -> ())
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(i) keyword extraction runs against the token that holds them, over %d grammars"
      !tested
;;

(* -- (j) a witness per problem ------------------------------------------- *)

let facts_of (grammar : Core.Grammar.t) : Core.Facts.t =
  match Core.Facts.of_grammar grammar with
  | Ok facts -> facts
  | Error es -> failwith (Format.asprintf "%a" Core.Error.pp_list es)
;;

let scopes_of (facts : Core.Facts.t) : Scopes.t =
  match Scopes.of_facts facts ~overrides:[] with
  | Ok scopes -> scopes
  | Error fs -> failwith (String.concat ", " (List.map Scopes.finding_to_string fs))
;;

let () =
  let before = Law.failures () in
  let expect
        (what : string)
        (result : (Treesitter.output, Treesitter.Check.problem list) result)
        (expected : Treesitter.Check.problem -> bool)
    : unit
    =
    match result with
    | Ok _ -> Law.fail "(j) %s: the grammar was accepted" what
    | Error problems ->
      if not (List.exists expected problems)
      then
        Law.fail
          "(j) %s: reported %s"
          what
          (String.concat ", " (List.map Treesitter.Check.problem_to_string problems))
  in
  (* A token holding a complement. JavaScript has no form for one.

     The complement sits inside a sequence. Alone it would match the empty
     string, and the grammar checker rejects a nullable token before this
     backend ever runs, so the witness would prove nothing. *)
  let no_regex =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "word" Redfa.Regex.(seq (singleton_char 'q') (complement (str "no")))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ]
  in
  (match Core.Facts.of_grammar no_regex with
   | Error es ->
     (* A silent skip here let this witness prove nothing for a while. *)
     Law.fail
       "(j) a token with no JavaScript regex: the grammar was rejected, %a"
       Core.Error.pp_list
       es
   | Ok facts ->
     expect
       "a token with no JavaScript regex"
       (Treesitter.generate (scopes_of facts) ~language:"witness" ())
       (function
         | Treesitter.Check.No_js_regex _ -> true
         | _ -> false));
  (* A rule and a token that mangle to one node name. *)
  let collision =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "item" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "one" (Rule "Item") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ; prod "Item" [ child_req "text" (Token "item") ]
      ]
  in
  (match Core.Facts.of_grammar collision with
   | Error _ ->
     Law.fail "(j) a rule and a token sharing a node name: the grammar was rejected"
   | Ok facts ->
     expect
       "a rule and a token sharing a node name"
       (Treesitter.generate (scopes_of facts) ~language:"witness" ())
       (function
         | Treesitter.Check.Node_collision _ -> true
         | _ -> false));
  expect
    "a language name that is not an identifier"
    (Treesitter.generate
       (scopes_of (facts_of Lingo_grammars.Sexp_grammar.grammar))
       ~language:"my language"
       ())
    (function
      | Treesitter.Check.Bad_setting _ -> true
      | _ -> false);
  if Law.failures () = before then Law.pass "(j) every problem has a witness"
;;

(* -- (c) the root is the rule tree-sitter starts at ---------------------- *)

(* Every grammar in the corpus declares its root first, so the reordering is
   invisible there. Another grammar could declare it anywhere, and tree-sitter
   parses from the first rule in the map whatever the author meant. *)
let () =
  let before = Law.failures () in
  let grammar =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "Second" ]
      [ prod "First" [ child_req "text" (Token "word") ]
      ; prod "Second" [ child_rep "one" (Rule "First") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ]
  in
  (match Treesitter.generate (scopes_of (facts_of grammar)) ~language:"witness" () with
   | Error problems ->
     List.iter
       (fun p -> Law.fail "(c) a root declared second: %a" Treesitter.Check.pp_problem p)
       problems
   | Ok out ->
     (match rule_bodies out.grammar_js with
      | (first, _) :: _ when first = "second" -> ()
      | (first, _) :: _ ->
        Law.fail "(c) a root declared second: tree-sitter would start at %S" first
      | [] -> Law.fail "(c) a root declared second: no rules were emitted"));
  if Law.failures () = before
  then Law.pass "(c) the root is the rule tree-sitter starts at, wherever it was declared"
;;

let negated_class = "[^*" ^ "\\" ^ "/]+"

(* -- (k) an intersection that denotes a class comes out as one ----------- *)

(* A grammar can write [inter any (complement [*/])] for "one character that
   is not a star or a slash". The term denotes a character class, and redfa's
   own emitter has no form for it. The four tokens in this repo's grammars
   that were written that way now write [not_chars]. That form is simpler,
   gives a smaller automaton and needs no reduction.

   The reduction stays because an author can still write the longer form. This
   part measures it. *)
let () =
  let before = Law.failures () in
  let grammar =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat
            "word"
            Redfa.Regex.(plus (inter any (complement (chars_of_char_list [ '*'; '/' ]))))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ]
  in
  (match Treesitter.generate (scopes_of (facts_of grammar)) ~language:"witness" () with
   | Error problems ->
     List.iter
       (fun p ->
          Law.fail
            "(k) an intersection over a complement: %a"
            Treesitter.Check.pp_problem
            p)
       problems
   | Ok out ->
     (match List.assoc_opt "word" (rule_bodies out.grammar_js) with
      | None ->
        Law.fail "(k) an intersection over a complement: no rule was emitted for it"
      | Some body ->
        (match regexes body with
         | [ one ] when one = negated_class -> ()
         | [ other ] ->
           Law.fail
             "(k) an intersection over a complement came out as %S rather than a negated \
              class"
             other
         | found ->
           Law.fail
             "(k) an intersection over a complement gave %d regexes"
             (List.length found))));
  if Law.failures () = before
  then Law.pass "(k) an intersection that denotes a class comes out as one"
;;

(* -- (m) the specific pattern comes after the catch-all it has to beat --- *)

(* tree-sitter takes the last pattern in the file that matches a node. So a
   capture on one child position has to sit after the capture on the token
   itself, or the token's own colour wins at every position and the specific
   pattern is dead text.

   Measured against tree-sitter 0.26.9 on rust. With [(ident) @variable] last
   in the file, every identifier renders as a variable. Move it to the front
   and [Point] renders as a type, [main] as a function.

   The node a pattern hangs its capture on is the inner one where it names a
   position, and the pattern's own node otherwise. Two patterns compete only
   where those are the same node. *)
let capture_target (q : query) : string option =
  match q.field, q.inner, q.literal with
  | Some _, Some inner, _ -> Some inner
  | Some _, None, Some literal -> Some literal
  | Some _, None, None -> None
  | None, _, Some literal -> Some literal
  | None, _, None -> q.node
;;

let () =
  let before = Law.failures () in
  let checked = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let numbered = List.mapi (fun index q -> index, q) (queries e.out.highlights) in
       let at_a_position = List.filter (fun (_, q) -> q.field <> None) numbered in
       let catch_alls = List.filter (fun (_, q) -> q.field = None) numbered in
       List.iter
         (fun (specific, q) ->
            match capture_target q with
            | None -> ()
            | Some target ->
              incr checked;
              List.iter
                (fun (general, other) ->
                   if general > specific && capture_target other = Some target
                   then
                     Law.fail
                       "(m) %s: highlights.scm captures %S at one position and then \
                        again for the node itself, so the second one wins everywhere"
                       e.name
                       target)
                catch_alls)
         at_a_position)
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(m) a specific pattern comes after the catch-all it has to beat, over %d of them"
      !checked
;;

(* -- (l) the binders and the scopes reach locals.scm ---------------------- *)

(* Read from the grammar the corpus declares, so the whole path is covered:
   the author's declaration, the field [Facts] carries it in, and the page.
   Reading [Rule.def.binders] here instead would compare the emitter against
   the value it was handed, and a binder dropped on the way would leave no
   trace.

   Both directions. A binder or a scope that never reaches the page leaves an
   editor with no way to follow a name. A pattern no declaration stands
   behind tags a position the author never called a binding site.

   A binder's node is its own token's, because [Check_names] rejects a binder
   on anything but a single pattern token. The [fail] below is what ties the
   two together. *)
let () =
  let before = Law.failures () in
  let total = ref 0 in
  let binder_token (prod : Core.Grammar.production) (name : Core.Grammar.Name.Child.t)
    : string option
    =
    match
      List.find_opt
        (fun (child : Core.Grammar.child) ->
           Core.Grammar.Name.Child.equal child.name name)
        prod.children
    with
    | None -> None
    | Some child ->
      (match child.head, child.rest with
       | Core.Grammar.Token token, [] -> Some token
       | Core.Grammar.Rule _, _ | _, _ :: _ -> None)
  in
  List.iter
    (fun (e : emitted) ->
       let declared =
         List.concat_map
           (fun (prod : Core.Grammar.production) ->
              let node = Treesitter.Node.of_rule prod.kind_name in
              let scope =
                if prod.opens_scope
                then [ Printf.sprintf "(%s) @local.scope" node ]
                else []
              in
              let definitions =
                List.concat_map
                  (fun (name : Core.Grammar.Name.Child.t) ->
                     let named = Core.Grammar.Name.Child.to_string name in
                     match binder_token prod name with
                     | None ->
                       Law.fail
                         "(l) %s: the binder %s.%s does not hold a single token"
                         e.name
                         node
                         named;
                       []
                     | Some token ->
                       [ Printf.sprintf
                           "(%s %s: (%s) @local.definition)"
                           node
                           named
                           (Treesitter.Node.of_token
                              (Core.Grammar.Name.Token.of_string token))
                       ])
                  (Core.Grammar.Name.Child.Set.elements prod.binders)
              in
              scope @ definitions)
           Core.Grammar.(e.grammar.productions)
       in
       let written =
         List.filter
           (fun line ->
              occurrences ~needle:"@local.scope" line <> []
              || occurrences ~needle:"@local.definition" line <> [])
           (lines e.out.locals)
       in
       List.iter
         (fun pattern ->
            incr total;
            if not (List.mem pattern written)
            then
              Law.fail
                "(l) %s: the grammar declares %s and locals.scm does not carry it"
                e.name
                pattern)
         declared;
       List.iter
         (fun pattern ->
            if not (List.mem pattern declared)
            then
              Law.fail
                "(l) %s: locals.scm carries %s and the grammar declares no such binder \
                 or scope"
                e.name
                pattern)
         written;
       (* A definition with no reference beside it resolves nothing. *)
       match Treesitter.word_token e.facts with
       | None -> ()
       | Some token ->
         incr total;
         let pattern =
           Printf.sprintf "(%s) @local.reference" (Treesitter.Node.of_token token.name)
         in
         if not (List.mem pattern (lines e.out.locals))
         then
           Law.fail
             "(l) %s: the language's identifier is %S and locals.scm carries no \
              reference to it"
             e.name
             (Core.Grammar.Name.Token.to_string token.name))
    emitted;
  if Law.failures () = before
  then
    Law.pass "(l) every binder and every scope reaches locals.scm, over %d of them" !total
;;

let () = Law.summarise "law_treesitter"
