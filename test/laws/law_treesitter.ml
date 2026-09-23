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

      Coverage. The twelve grammars in test/editors/editor_corpus.ml. Parts
      (c), (j) and (k) build their own witnesses. The corpus declares every
      root first, holds no grammar this backend rejects, and no longer holds a
      term that needs the reduction (k) is about. rust and effekt are the two
      that declare binders and scopes, so every count in part (l) comes from
      them. rust and wide are the two that scope a child position, so every
      count in part (m) comes from those.

      What this says nothing about. Whether tree-sitter's generator finds a
      conflict. lingo checks LL(1) and tree-sitter builds an LR automaton, and
      a grammar can pass the first and fail the second. The binding powers
      carry over as precedences, and that covers most of it. The rest needs
      the generator in the loop.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        M1  In [Grammar_js.symbol], send a reference to an expression block to
            the block's own node rather than to the hidden choice.
            -> part (b), 62 findings across the six grammars with a block:
               effekt 33, rust 10, postfix 8, wide 4, calc 4, rassoc 3. The
               hidden
               rule becomes unreachable, and so does everything only it
               reached. That is every shape the block's operators build.
        M2  In [Grammar_js.role_body], emit every role as its plain body.
            -> part (e), 6 findings, one per grammar with a block. The whole
               operator table collapses into one choice with no precedence.
               tree-sitter would report that as a conflict over the entire
               expression grammar.
        M3  In [Queries.token_patterns], stop filtering by what the grammar
            matches.
            -> part (d), 1 finding: shapes' [end]. It is a resync anchor and
               nothing else. It says where recovery stops, and no production
               holds it. tree-sitter takes no part in recovery, so a query for
               it can never fire.
        M4  In [Queries.token_patterns], write the literal the way OCaml
            writes one.
            -> part (d), 1 finding: unicode's separator, which comes out as
               three decimal escapes instead of an arrow. A query file is
               UTF-8, so the arrow is written as itself.
        M5  In [Js.regex], wrap a spliced alternation in a capturing group.
            -> part (h), 1 finding on json. Only json has an alternation
               inside a repetition inside a token, and the group survives
               into the emitted regex only in that shape.
        M6  In [Grammar_js.ordered], leave the rules in the order the facts
            hold them.
            -> part (c), 1 finding, on the law's own witness. Every grammar in
               the corpus declares its root first. The corpus cannot reach
               this, so the witness exists for it.
        M7  In [Queries.capture], translate a function name to nothing.
            -> part (g), 2 findings on rust, for [Fn] and [MethodSig].

               This read zero before part (g) said that every scope outside
               the two escape hatches must translate. A scope that quietly
               translates to nothing left the same trace as one that was never
               set. Part (g) separates the two.
        M8  In [Treesitter.word_token], take the first pattern token rather
            than the token that holds every keyword.
            -> part (i), 1 finding on json, whose first pattern token is
               [number] and whose keywords are [true], [false] and [null]. The
               other grammars declare the identifier first, so the wrong
               choice and the right one coincide.
        M9  In [Js.as_charset], never reduce an intersection.
            -> part (k), 1 finding: the witness is rejected with
               [No_js_regex]. The term has no JavaScript form at all without
               the reduction.

               Every one of these was run again after the four grammars that
               used to write [inter any (complement …)] were changed to write
               [not_chars]. Only this part's witness exercises the reduction
               now. The witness exists for that.
        M10 In [Queries.definition_patterns], write a definition for every
            child rather than for the binders.
            -> part (l), 312 findings: effekt 137, rust 57, wide 50, postfix
               16, recovery 16, shapes 9, calc 7, json 6, rassoc 6, comments
               4, sexp 2, unicode 2. Every child position with one symbol
               behind it becomes a binding site, in every grammar, whether or
               not the author declared one.
        M11 In [Queries.scope_patterns], write no scope.
            -> part (l), 13 findings: effekt 9, rust 4. An editor would put
               every name in one flat scope, so a local would shadow
               everything of its name in the file.
        M12 In [Stage.shape], build a user production's [binders] empty.
            -> part (l), 17 findings: effekt 10, rust 7.

               This read zero while part (l) built what it expected from
               [Rule.def.binders]. The emitter agreed with the field it was
               handed, and the field was the mutated one. Part (l) reads the
               grammar instead, so a binder dropped anywhere along the way
               reddens it.
        M13 In [Queries.definition_patterns], name the inner node by the
            child rather than by the token it holds.
            -> part (d), 17 findings, and part (l), 34. A field name is not a
               node name, so the pattern matches nothing and both directions
               of (l) fire on every binder.
        M14 In [Queries.locals], write no reference pattern.
            -> part (l), 5 findings: shapes, recovery, rust, effekt and wide,
               the grammars with a token that holds every keyword. A
               definition with nothing to resolve against resolves nothing.
        M15 In [Queries.highlights], write the sections in the order they
            were written in before part (m) existed: the positions first and
            the catch-alls last.
            -> part (m), 11 findings: rust 9, wide 2.

               That order was what the emitter wrote, and part (m) is the
               law that found it. tree-sitter takes the last pattern that
               matches a node, so [(ident) @variable] beat every per-position
               capture and rust's type and function names rendered as
               variables. Checked against tree-sitter 0.26.9 both ways round
               before the order was reversed.
   -------------------------------------------------------------------------- *)

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
       (match bodies, Core.Facts.(e.facts.roots) with
        | [], _ -> Law.fail "(c) %s: the grammar has no rules" e.name
        | (first, _) :: _, root :: _ ->
          let expected = Treesitter.Node.of_rule (Core.Facts.rule e.facts root).name in
          if first <> expected
          then
            Law.fail
              "(c) %s: tree-sitter would start at %S, and the grammar's root is %S"
              e.name
              first
              expected
        | _ :: _, [] -> ());
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
