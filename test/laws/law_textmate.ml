(* -- the TextMate backend -----------------------------------------------------

      (a) Every [#key] in an emitted grammar names a repository entry.
      (b) Every repository entry is reachable from the root patterns.
      (c) No cycle runs through entries that are nothing but reference lists.
      (d) Every [begin] has an [end], at every depth, and every capture index
          a pattern scopes is a group its regex actually has.
      (e) Every scope is a well formed path ending in the language.
      (f) A grammar emits the same bytes every time.
      (g) A [begin] appears exactly where the framing says a region is, and
          nowhere else.
      (h) Every scope, derived or written, is carried somewhere in the
          document.
      (i) Every {!Scopes.finding} has a witness override that provokes it
          alone.
      (j) Every {!Textmate.Check.problem} a grammar can reach has a witness.
      (k) A part spliced into a single-regex emission contributes exactly one
          capture group.
      (l) A pattern list tries the specific before the general: [#tokens]
          last, a literal before any literal it is a prefix of, and every
          literal before every pattern token.
      (m) Two guards the twelve grammars cannot reach, each on a grammar
          written for it.

      Mechanism. Parts (a) to (h) and (l) read the emitted JSON back and walk
      it. They
      share no code with the emitter. The reference walk, the capture counter
      and the scope-path check are written here a second time. A law that
      calls the function it is checking proves only that the function agrees
      with itself.

      Part (g) is the claim {!Core.Rule.frame} is for. In the predecessor the
      author wrote each region by hand, once per production, and named the
      anchor regexes. Here the region comes out of the frame, so the law is
      that the two agree: a rule framed by a matched pair is a region, a rule
      that contains its own errors may be one, and nothing else ever is.

      Part (h) is the silence check, and it carries the resolution chain. A
      scope nothing carries is a colour the author asked for and will not see.
      No editor reports it.

      Part (k) exists because part (d) cannot reach the bug it is about. Every
      single-regex emission wraps each part in its own group and scopes
      capture [n] as part [n]. A capturing group inside a spliced part shifts
      every index after it. An index that has shifted is still an index the
      regex has, so it is in range and it is wrong. Part (k) counts the groups
      instead, over a token deliberately written with one inside it.

      Coverage. The thirteen grammars in test/editors/editor_corpus.ml, with
      the overrides an author would write on json, rust, effekt, wide and ml.
      Counts print beside each result. Parts (i) to (k) build their own
      witnesses.

      What this says nothing about. Whether a regex compiles under Oniguruma,
      and whether an editor colours what a reader expects. The first needs the
      engine. The second needs a person, who reads test/editors/*.textmate by
      eye.

      What the corpus does not reach, and what the witnesses are for. Nothing
      in the thirteen grammars splices a capturing group -- redfa emits [(?:]
      throughout and the trivia separator is built non-capturing -- so part (k)
      is written over a witness rather than over the corpus. No rule in them
      contains its own errors, opens on a literal and ends on a child that may
      be absent, and no token in them is both half of a matched pair and a child
      somewhere else. Part (m) carries a witness for each of those two.

      Part (l) is about order alone. A TextMate engine takes the leftmost
      match, and among rules that match at the same position it takes the
      first in the list. So a general pattern written ahead of a specific one
      is the specific one deleted, and nothing says so: the file loads, the
      editor runs, and the colour is the general one everywhere. Parts (a) to
      (h) read what is in the document and never what order it is in.
      [law_treesitter] part (m) is the same claim for the other engine, where
      the rule runs the other way and the last pattern wins. That one found a
      real defect; this one reads zero and holds the order in place.

      Facts the parts lean on. [Field] is the one entry in the corpus that
      nothing reaches, and it has a reason: it is the right-hand side of an
      access operator, and the operator folds the token at the end of it into
      its own regex. Only effekt has blocks and statements that reference each
      other flat all the way round, which is what a cycle break is for. A
      delimiter's scope lives in a region's [beginCaptures], so a framed rule
      that emits no region takes its delimiter scopes down with it. And an
      [entity.name.*] scope lands only where a rule's identity child is folded
      into the region's opening pattern: a rule that names itself by a child
      does it once, where a body pattern fires everywhere.

      Where the same name means different things. json's [Member.key], rust's
      [Field.name], [Variant.name], [Param.name] and [Type.name], wide's
      [Alias.name] and [Table.name], and five positions in ml. A per-position
      scope exists for exactly those, and reading the token's scope ahead of the
      child's override is what loses them.

   -------------------------------------------------------------------------- *)

(* The six blocks below are generated, and they are the evidence. assay derives
   a mutation from the code rather than from a sentence beside it, applies every
   one, and records what went red. Regenerate them with

     assay -config assay.conf -only textmate

   and take the counts as they come: they move whenever the corpus grows, and
   asserting them exactly would train everyone to ignore a red suite. What they
   assert is that every mutant dies. A survivor is the finding, and the lines it
   names are where to look. *)

(* -- mutation testing, generated by assay on 2026-10-01 ---------------------
      editors/textmate/textmate.ml, 71 mutants, 46 killed, 24 survived, 1 timed out.
        extreme     12  10 killed, law_textmate (a): 5, law_textmate (c): 3, dump_textmate: 1, law_textmate a: 1; 2 survived
        sbr         29  18 killed, dump_textmate: 8, law_textmate (a): 7, law_textmate (b): 3; 10 survived; 1 timed out
        ror          9  6 killed, law_textmate (a): 3, law_textmate (c): 2, dump_textmate: 1; 3 survived
        lcr          8  2 killed, dump_textmate: 2; 6 survived
        aor          3  all killed, law_textmate: 3
        uoi         10  7 killed, dump_textmate: 2, law_textmate (a): 2, law_textmate (c): 2, law_textmate (h): 1; 3 survived
      survived at lines 34 54 65 98 99 102 103 104 107 108 115 132 150 228 267 286
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-01 ---------------------
      editors/textmate/emit.ml, 85 mutants, 72 killed, 13 survived.
        extreme     21  20 killed, law_textmate (h): 14, dump_textmate: 3, law_textmate (a): 1, law_textmate (e): 1, law_textmate (l): 1; 1 survived
        sbr         40  35 killed, law_textmate (h): 13, dump_textmate: 11, law_textmate (d): 8, law_textmate (m): 3; 5 survived
        ror          5  2 killed, (h) 1 (l) 1; 3 survived
        lcr          4  3 killed, dump_textmate: 2, law_textmate: 1; 1 survived
        aor          9  6 killed, law_textmate: 5, law_textmate (k): 1; 3 survived
        uoi          6  all killed, law_textmate (h): 3, dump_textmate: 2, law_textmate: 1
      survived at lines 74 75 77 93 177 186 369 472 475
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-01 ---------------------
      editors/textmate/shape.ml, 56 mutants, 48 killed, 8 survived.
        extreme     17  all killed, dump_textmate: 10, law_textmate (k): 3, law_textmate (j): 2, law_textmate (g): 1, law_textmate (h): 1
        sbr          9  8 killed, dump_textmate: 4, law_textmate (h): 4; 1 survived
        ror          7  4 killed, dump_textmate: 3, law_textmate (k): 1; 3 survived
        lcr          5  2 killed, (h) 2; 3 survived
        aor          3  2 killed, law_textmate: 2; 1 survived
        uoi         15  all killed, dump_textmate: 8, law_textmate (h): 5, law_textmate (k): 1, law_textmate (m): 1
      survived at lines 70 81 252 256 372 382
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-01 ---------------------
      editors/textmate/check.ml, 91 mutants, 73 killed, 18 survived.
        extreme     19  12 killed, (j) 9 a 2 (n) 1; 7 survived
        sbr         39  32 killed, (n) 20 (j) 6 a 6; 7 survived
        ror          9  7 killed, law_textmate (n): 2, law_textmate a: 2, law_textmate: 1, law_textmate (j): 1, law_textmate rust:: 1; 2 survived
        lcr          7  6 killed, law_textmate (n): 5, law_textmate: 1; 1 survived
        aor          1  all killed, law_textmate: 1
        uoi         16  15 killed, a 5 (n) 3 sexp: 3 (j) 2 postfix: 1 rust: 1; 1 survived
      survived at lines 79 83 89 137 148 266 301 327 344 348 357 360 361 402 403 411 473
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-01 ---------------------
      editors/textmate/oniguruma.ml, 51 mutants, 33 killed, 17 survived, 1 timed out.
        extreme      7  6 killed, dump_textmate: 5, law_textmate (m): 1; 1 survived
        sbr          8  6 killed, dump_textmate: 3, law_textmate (m): 2, law_textmate (d): 1; 2 survived
        ror         11  6 killed, law_textmate (k): 3, dump_textmate: 1, law_textmate: 1, law_textmate (d): 1; 5 survived
        lcr          9  6 killed, law_textmate (k): 3, dump_textmate: 2, law_textmate (d): 1; 3 survived
        aor          9  3 killed, law_textmate: 3; 5 survived; 1 timed out
        uoi          7  6 killed, dump_textmate: 2, law_textmate (k): 2, law_textmate: 1, law_textmate (m): 1; 1 survived
      survived at lines 10 30 34 35 36 63 77 98 103 109 111 113
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-01 ---------------------
      editors/textmate/key.ml, 2 mutants, 2 killed.
        extreme      2  all killed, (e) 1 sexp: 1
   ---------------------------------------------------------------------- *)

(* -- what the corpus emits ----------------------------------------------- *)

type emitted =
  { name : string
  ; facts : Core.Facts.t
  ; scopes : Scopes.t
  ; text : string
  ; document : Yojson.Basic.t
  }

let emitted : emitted list =
  List.map
    (fun (entry : Editor_corpus.entry) ->
       let scopes = Editor_corpus.scopes entry in
       match Textmate.generate scopes ~language:entry.name () with
       | Error problems ->
         List.iter
           (fun p -> Law.fail "%s: %a" entry.name Textmate.Check.pp_problem p)
           problems;
         { name = entry.name
         ; facts = Scopes.facts scopes
         ; scopes
         ; text = ""
         ; document = `Null
         }
       | Ok text ->
         { name = entry.name
         ; facts = Scopes.facts scopes
         ; scopes
         ; text
         ; document = Yojson.Basic.from_string text
         })
    Editor_corpus.all
;;

let field (key : string) (json : Yojson.Basic.t) : Yojson.Basic.t option =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None
;;

let entries (e : emitted) : (string * Yojson.Basic.t) list =
  match field "repository" e.document with
  | Some (`Assoc fields) -> fields
  | _ -> []
;;

let root_patterns (e : emitted) : Yojson.Basic.t list =
  match field "patterns" e.document with
  | Some (`List patterns) -> patterns
  | _ -> []
;;

(* Every pattern in a value, and the value itself where it is one. *)
let rec patterns_in (json : Yojson.Basic.t) : Yojson.Basic.t list =
  match json with
  | `Assoc fields ->
    json
    ::
    (match List.assoc_opt "patterns" fields with
     | Some (`List items) -> List.concat_map patterns_in items
     | _ -> [])
  | _ -> []
;;

let reference_of (json : Yojson.Basic.t) : string option =
  match field "include" json with
  | Some (`String key) when String.length key > 1 && key.[0] = '#' ->
    Some (String.sub key 1 (String.length key - 1))
  | _ -> None
;;

let references_of (json : Yojson.Basic.t) : string list =
  List.filter_map reference_of (patterns_in json)
;;

(* The references a TextMate engine follows while it flattens one entry into
   whichever list includes it. It stops at a region and at a match. Those
   compile when they fire rather than when the reference to them is read, so a
   cycle through one terminates. *)
let rec flat_references (json : Yojson.Basic.t) : string list =
  match json with
  | `Assoc fields ->
    let has key = List.mem_assoc key fields in
    if has "begin" || has "end" || has "match"
    then []
    else (
      match List.assoc_opt "patterns" fields with
      | Some (`List items) ->
        List.concat_map
          (fun item ->
             match reference_of item with
             | Some key -> [ key ]
             | None -> flat_references item)
          items
      | _ -> [])
  | _ -> []
;;

(* -- (a) every reference resolves ---------------------------------------- *)

let () =
  let checked = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let known = List.map fst (entries e) in
       let check (where : string) (json : Yojson.Basic.t) : unit =
         List.iter
           (fun key ->
              incr checked;
              if not (List.mem key known)
              then
                Law.fail
                  "(a) %s: %s references #%s, which nothing declares"
                  e.name
                  where
                  key)
           (references_of json)
       in
       List.iter (check "the root") (root_patterns e);
       List.iter (fun (key, json) -> check key json) (entries e))
    emitted;
  if Law.failures () = 0
  then Law.pass "(a) every reference names an entry, over %d of them" !checked
;;

(* -- (b) every entry is reachable ---------------------------------------- *)

let () =
  let before = Law.failures () in
  let total = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let table = entries e in
       let seen = Hashtbl.create 64 in
       let rec walk (key : string) : unit =
         if not (Hashtbl.mem seen key)
         then (
           Hashtbl.replace seen key ();
           match List.assoc_opt key table with
           | None -> ()
           | Some json -> List.iter walk (references_of json))
       in
       List.iter (fun json -> List.iter walk (references_of json)) (root_patterns e);
       List.iter
         (fun (key, _) ->
            incr total;
            if not (Hashtbl.mem seen key)
            then Law.fail "(b) %s: nothing reaches the entry %S" e.name key)
         table)
    emitted;
  if Law.failures () = before
  then Law.pass "(b) every entry is reachable from the root, over %d of them" !total
;;

(* -- (c) no cycle among reference-only entries --------------------------- *)

(* A TextMate engine flattens a reference-only entry into whichever list
   includes it, by a recursion with no cycle guard. A region entry is compiled
   when the region opens instead, so it breaks the recursion. *)
let is_flat (json : Yojson.Basic.t) : bool =
  match json with
  | `Assoc fields ->
    let has key = List.mem_assoc key fields in
    has "patterns" && not (has "begin" || has "end" || has "match")
  | _ -> false
;;

let () =
  let before = Law.failures () in
  List.iter
    (fun (e : emitted) ->
       let table = entries e in
       let flat key =
         match List.assoc_opt key table with
         | Some json -> is_flat json
         | None -> false
       in
       let reported = Hashtbl.create 8 in
       let rec walk ~(stack : string list) (key : string) : unit =
         if List.mem key stack
         then (
           let cycle = String.concat " to " (List.rev (key :: stack)) in
           if not (Hashtbl.mem reported cycle)
           then (
             Hashtbl.replace reported cycle ();
             Law.fail "(c) %s: the reference-only entries %s form a cycle" e.name cycle))
         else (
           match List.assoc_opt key table with
           | None -> ()
           | Some json ->
             List.iter
               (fun target -> if flat target then walk ~stack:(key :: stack) target)
               (flat_references json))
       in
       List.iter (fun (key, _) -> if flat key then walk ~stack:[] key) table)
    emitted;
  if Law.failures () = before
  then Law.pass "(c) no cycle runs through a reference-only entry"
;;

(* -- (d) regions are paired and captures exist --------------------------- *)

(* Capturing groups in an Oniguruma regex: a [(] that is not escaped, not
   inside a character class, and not the start of a [(?...)] form.

   This counts the groups a second time. The emitter has its own scanner for
   them, and a count taken from that scanner would check the scanner against
   itself. *)
let capture_groups (regex : string) : int =
  let width = String.length regex in
  let count = ref 0 in
  let in_class = ref false in
  let i = ref 0 in
  while !i < width do
    (match regex.[!i] with
     | '\\' -> incr i
     | '[' when not !in_class -> in_class := true
     | ']' when !in_class -> in_class := false
     | '(' when not !in_class ->
       if not (!i + 1 < width && regex.[!i + 1] = '?') then incr count
     | _ -> ());
    incr i
  done;
  !count
;;

let () =
  let before = Law.failures () in
  let checked = ref 0 in
  let groups = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let check (where : string) (json : Yojson.Basic.t) : unit =
         match json with
         | `Assoc fields ->
           let has key = List.mem_assoc key fields in
           (match has "begin", has "end" with
            | true, false -> Law.fail "(d) %s: %s has a begin and no end" e.name where
            | false, true -> Law.fail "(d) %s: %s has an end and no begin" e.name where
            | _ -> ());
           List.iter
             (fun (map_key, regex_key) ->
                match List.assoc_opt map_key fields, List.assoc_opt regex_key fields with
                | Some (`Assoc indexed), Some (`String regex) ->
                  let available = capture_groups regex in
                  groups := !groups + available;
                  List.iter
                    (fun (index, _) ->
                       incr checked;
                       match int_of_string_opt index with
                       | Some index when index <= available -> ()
                       | Some index ->
                         Law.fail
                           "(d) %s: %s scopes capture %d of %S, which has %d groups"
                           e.name
                           where
                           index
                           regex
                           available
                       | None ->
                         Law.fail
                           "(d) %s: %s scopes %S, which is no capture"
                           e.name
                           where
                           index)
                    indexed
                | Some (`Assoc _), _ ->
                  Law.fail
                    "(d) %s: %s has a %s map and no %s"
                    e.name
                    where
                    map_key
                    regex_key
                | _ -> ())
             [ "captures", "match"; "beginCaptures", "begin"; "endCaptures", "end" ]
         | _ -> ()
       in
       List.iter (fun (key, json) -> List.iter (check key) (patterns_in json)) (entries e))
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(d) every region is paired and every capture exists, over %d captures and %d \
       groups"
      !checked
      !groups
;;

(* -- (e) every scope is a path ------------------------------------------- *)

let () =
  let before = Law.failures () in
  let checked = ref 0 in
  let valid (c : char) : bool =
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
    | _ -> false
  in
  List.iter
    (fun (e : emitted) ->
       let rec walk (json : Yojson.Basic.t) : unit =
         match json with
         | `Assoc fields ->
           List.iter
             (fun (key, value) ->
                (match key, value with
                 | "name", `String scope when String.length e.name > 0 ->
                   incr checked;
                   if not (String.for_all valid scope)
                   then Law.fail "(e) %s: the scope %S is not a path" e.name scope;
                   if String.split_on_char '.' scope |> List.exists (String.equal "")
                   then Law.fail "(e) %s: the scope %S has an empty segment" e.name scope;
                   if not (Filename.check_suffix scope ("." ^ e.name))
                   then
                     Law.fail
                       "(e) %s: the scope %S does not end in the language"
                       e.name
                       scope
                 | _ -> ());
                walk value)
             fields
         | `List items -> List.iter walk items
         | _ -> ()
       in
       (* The header's own [name] is the language's display name. Only the
          repository and the root patterns hold scopes. *)
       List.iter walk (root_patterns e);
       List.iter (fun (_, json) -> walk json) (entries e))
    emitted;
  if Law.failures () = before
  then Law.pass "(e) every scope is a path, over %d of them" !checked
;;

(* -- (f) the same bytes every time --------------------------------------- *)

let () =
  let before = Law.failures () in
  List.iter
    (fun (e : emitted) ->
       match Textmate.generate e.scopes ~language:e.name () with
       | Error _ ->
         Law.fail "(f) %s: the second run rejected what the first emitted" e.name
       | Ok again ->
         if not (String.equal again e.text)
         then Law.fail "(f) %s: two runs gave different bytes" e.name)
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(f) a grammar emits the same bytes twice, over %d of them"
      (List.length emitted)
;;

(* -- (g) a region is a frame --------------------------------------------- *)

let () =
  let before = Law.failures () in
  let regions = ref 0 in
  let plain = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let table = entries e in
       Array.iter
         (fun (rule : Core.Rule.def) ->
            match rule.origin with
            | Core.Rule.Pratt_role _ | Core.Rule.Pratt_block -> ()
            | Core.Rule.User ->
              (match List.assoc_opt (Textmate.Key.of_rule rule.name) table with
               | None -> ()
               | Some json ->
                 let is_region = field "begin" json <> None in
                 if is_region then incr regions else incr plain;
                 (match rule.frame with
                  | Core.Rule.Delimited _ ->
                    if not is_region
                    then
                      Law.fail
                        "(g) %s: %s is framed by a matched pair and emits no region"
                        e.name
                        (Core.Grammar.Name.Rule.to_string rule.name)
                  | Core.Rule.Committed _ -> ()
                  | Core.Rule.Plain | Core.Rule.Separated _ ->
                    if is_region
                    then
                      Law.fail
                        "(g) %s: %s emits a region and its framing contains nothing"
                        e.name
                        (Core.Grammar.Name.Rule.to_string rule.name))))
         Core.Facts.(e.facts.rules))
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(g) a region is a frame, over %d regions and %d entries beside them"
      !regions
      !plain
;;

(* -- (h) every scope reaches the document -------------------------------- *)

let () =
  let before = Law.failures () in
  let carried = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let names = Hashtbl.create 64 in
       let rec walk (json : Yojson.Basic.t) : unit =
         match json with
         | `Assoc fields ->
           List.iter
             (fun (key, value) ->
                (match key, value with
                 | "name", `String scope -> Hashtbl.replace names scope ()
                 | _ -> ());
                walk value)
             fields
         | `List items -> List.iter walk items
         | _ -> ()
       in
       List.iter (fun (_, json) -> walk json) (entries e);
       let check (what : string) (scope : Scopes.Scope.t option) : unit =
         match scope with
         | None -> ()
         | Some scope ->
           incr carried;
           let rendered = Scopes.Scope.to_string ~language:e.name scope in
           if not (Hashtbl.mem names rendered)
           then
             Law.fail
               "(h) %s: %s is scoped %s and nothing in the document carries it"
               e.name
               what
               rendered
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
            check named (Scopes.rule e.scopes rule.id);
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
  then Law.pass "(h) every scope reaches the document, over %d of them" !carried
;;

(* -- (i) a witness per finding ------------------------------------------- *)

let facts_of (grammar : Core.Grammar.t) : Core.Facts.t =
  match Core.Facts.of_grammar grammar with
  | Ok facts -> facts
  | Error es -> failwith (Format.asprintf "%a" Core.Error.pp_list es)
;;

let () =
  let before = Law.failures () in
  let facts = facts_of Lingo_grammars.Rust_grammar.grammar in
  let one (what : string) (override : Scopes.override) (expected : Scopes.finding) : unit =
    match Scopes.of_facts facts ~overrides:[ override ] with
    | Ok _ -> Law.fail "(i) %s: the override was accepted" what
    | Error [ finding ] ->
      if
        not
          (String.equal
             (Scopes.finding_to_string finding)
             (Scopes.finding_to_string expected))
      then
        Law.fail
          "(i) %s: reported %S where %S was expected"
          what
          (Scopes.finding_to_string finding)
          (Scopes.finding_to_string expected)
    | Error findings ->
      Law.fail
        "(i) %s: reported %d findings where one was expected"
        what
        (List.length findings)
  in
  let token = Core.Grammar.Name.Token.of_string "nosuchtoken" in
  let rule = Core.Grammar.Name.Rule.of_string "NoSuchRule" in
  let child = Core.Grammar.Name.Child.of_string "nosuchchild" in
  one
    "an unknown token"
    (Scopes.Token { token; scope = Scopes.Scope.Variable_other })
    (Scopes.Unknown_token token);
  one
    "an unknown rule"
    (Scopes.Rule { rule; scope = Scopes.Scope.Variable_other })
    (Scopes.Unknown_rule rule);
  one
    "an unknown child"
    (Scopes.Child
       { rule = Core.Grammar.Name.Rule.of_string "Field"
       ; child
       ; scope = Scopes.Scope.Variable_other
       })
    (Scopes.Unknown_child { rule = Core.Grammar.Name.Rule.of_string "Field"; child });
  one
    "an identity on a rule that has none"
    (Scopes.Identity
       { rule = Core.Grammar.Name.Rule.of_string "Field"
       ; scope = Scopes.Scope.Variable_other
       })
    (Scopes.No_identity_child (Core.Grammar.Name.Rule.of_string "Field"));
  (match
     Scopes.of_facts
       facts
       ~overrides:
         [ Scopes.Token
             { token = Core.Grammar.Name.Token.of_string "ident"
             ; scope = Scopes.Scope.Custom "entity name"
             }
         ]
   with
   | Ok _ -> Law.fail "(i) a malformed scope: the override was accepted"
   | Error [ Scopes.Malformed_scope _ ] -> ()
   | Error findings ->
     Law.fail
       "(i) a malformed scope: reported %s"
       (String.concat ", " (List.map Scopes.finding_to_string findings)));
  if Law.failures () = before then Law.pass "(i) every finding has a witness override"
;;

(* -- (j) a witness per problem ------------------------------------------- *)

let scopes_of (facts : Core.Facts.t) : Scopes.t =
  match Scopes.of_facts facts ~overrides:[] with
  | Ok scopes -> scopes
  | Error fs -> failwith (String.concat ", " (List.map Scopes.finding_to_string fs))
;;

let () =
  let before = Law.failures () in
  let expect
        (what : string)
        (result : (string, Textmate.Check.problem list) result)
        (expected : Textmate.Check.problem -> bool)
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
          (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
  in
  (* A token holding a complement, with no [~textmate] beside it to say how
     to write it out.

     The complement sits inside a sequence. Alone it would match the empty
     string, and the grammar checker rejects a nullable token before this
     backend ever runs. An intersection over a complement would not do
     either: redfa reads that as the character class it denotes and emits
     one. *)
  let no_oniguruma =
    let open Core.Grammar in
    let body = Redfa.Regex.(seq (singleton_char 'q') (complement (str "no"))) in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "word" body
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ]
  in
  let scopes = scopes_of (facts_of no_oniguruma) in
  expect
    "a token with no Oniguruma form"
    (Textmate.generate scopes ~language:"witness" ())
    (function
    | Textmate.Check.No_oniguruma _ -> true
    | _ -> false);
  (* A production whose repository key is the trivia entry's own. *)
  let collision =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ; pat
            ~trivia:Reformat
            "ws"
            Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
        ]
      ~roots:[ "Trivia" ]
      [ prod "Trivia" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ]
  in
  expect
    "a production named after the trivia entry"
    (Textmate.generate (scopes_of (facts_of collision)) ~language:"witness" ())
    (function
      | Textmate.Check.Key_collision _ -> true
      | _ -> false);
  (* Two children whose matched pairs open on the same token. *)
  let shared =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; punct_tight ~name:"rbrace" "}"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_req "one" (Rule "A"); child_req "two" (Rule "B") ]
      ; prod "A" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ; prod "B" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrace"
      ]
  in
  (match Core.Facts.of_grammar shared with
   | Error _ ->
     (* The grammar checker may reject this before the backend runs. The
        backend keeps its own check all the same. A grammar that reaches the
        backend by another shape would otherwise emit a silently unreachable
        rule. *)
     ()
   | Ok facts ->
     expect
       "two children opening on one token"
       (Textmate.generate (scopes_of facts) ~language:"witness" ())
       (function
         | Textmate.Check.Shared_opener _ -> true
         | _ -> false));
  let rust = scopes_of (facts_of Lingo_grammars.Rust_grammar.grammar) in
  expect
    "a hand-written pattern that is not JSON"
    (Textmate.generate
       rust
       ~raw:[ Core.Grammar.Name.Rule.of_string "Block", "{" ]
       ~language:"rust"
       ())
    (function
      | Textmate.Check.Bad_raw_pattern _ -> true
      | _ -> false);
  expect
    "a hand-written pattern with a begin and no end"
    (Textmate.generate
       rust
       ~raw:
         [ Core.Grammar.Name.Rule.of_string "Block", {|{ "begin": "a", "name": "x" }|} ]
       ~language:"rust"
       ())
    (function
      | Textmate.Check.Bad_raw_pattern _ -> true
      | _ -> false);
  expect
    "a file type written with its dot"
    (Textmate.generate rust ~file_types:[ ".rs" ] ~language:"rust" ())
    (function
    | Textmate.Check.Bad_setting _ -> true
    | _ -> false);
  (* [Item] dispatches over four alternatives, so it emits a list of
     patterns, and a list has nowhere to carry a name. *)
  (match
     Scopes.of_facts
       (facts_of Lingo_grammars.Rust_grammar.grammar)
       ~overrides:
         [ Scopes.Rule
             { rule = Core.Grammar.Name.Rule.of_string "Item"
             ; scope = Scopes.Scope.Meta "item"
             }
         ]
   with
   | Error _ -> Law.fail "(j) a scope with nowhere to go: the override was rejected"
   | Ok scopes ->
     expect
       "a scope on a rule that emits a list"
       (Textmate.generate scopes ~language:"rust" ())
       (function
       | Textmate.Check.Unattachable_scope _ -> true
       | _ -> false));
  if Law.failures () = before then Law.pass "(j) every reachable problem has a witness"
;;

(* -- (k) a spliced part is one capture group ----------------------------- *)

(* Every single-regex emission wraps each part in its own group and scopes
   capture [n] as part [n]. A hand-written token regex is spliced in as
   written. A capturing group inside one shifts every index after it: the
   scope meant for the third child would land on whatever the group inside the
   first child matched.

   Part (d) cannot reach it. An index that has shifted is still an index the
   regex has, so it is in range and it is wrong. This counts the groups
   instead, over a token written with a group in it on purpose. *)
let () =
  let before = Law.failures () in
  let grammar =
    let open Core.Grammar in
    create
      ~tokens:
        [ pat
            ~textmate:"(?:x)?(a|b)"
            "word"
            Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ; pat
            ~trivia:Reformat
            "ws"
            Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
        ]
      ~roots:[ "Pair" ]
      [ prod "Pair" [ child_req "one" (Token "word"); child_req "two" (Token "word") ] ]
  in
  let overrides =
    [ Scopes.Child
        { rule = Core.Grammar.Name.Rule.of_string "Pair"
        ; child = Core.Grammar.Name.Child.of_string "one"
        ; scope = Scopes.Scope.Variable_parameter
        }
    ; Scopes.Child
        { rule = Core.Grammar.Name.Rule.of_string "Pair"
        ; child = Core.Grammar.Name.Child.of_string "two"
        ; scope = Scopes.Scope.Variable_other
        }
    ]
  in
  match Scopes.of_facts (facts_of grammar) ~overrides with
  | Error fs -> List.iter (fun f -> Law.fail "(k) %s" (Scopes.finding_to_string f)) fs
  | Ok scopes ->
    (match Textmate.generate scopes ~language:"witness" () with
     | Error problems ->
       List.iter (fun p -> Law.fail "(k) %a" Textmate.Check.pp_problem p) problems
     | Ok text ->
       let document = Yojson.Basic.from_string text in
       (match
          Option.bind (field "repository" document) (fun repository ->
            field "pair" repository)
        with
        | None -> Law.fail "(k) the witness emitted no entry for its one production"
        | Some entry ->
          (match field "match" entry, field "captures" entry with
           | Some (`String regex), Some (`Assoc indexed) ->
             let groups = capture_groups regex in
             if groups <> 2
             then
               Law.fail
                 "(k) two parts were spliced and the regex %S has %d groups"
                 regex
                 groups;
             let named = List.sort compare (List.map fst indexed) in
             if named <> [ "1"; "2" ]
             then Law.fail "(k) the capture map names %s" (String.concat ", " named)
           | _ -> Law.fail "(k) the witness did not emit a single match with captures")));
    if Law.failures () = before
    then Law.pass "(k) a spliced part contributes exactly one capture group"
;;

(* -- (l) a pattern list tries the specific before the general ------------ *)

(* A TextMate patterns list has no precedence beyond the order it is written
   in. The engine takes the leftmost match, and among rules that match at the
   same position it takes the first in the list. So a general pattern written
   ahead of a specific one is the specific one deleted, and nothing anywhere
   says so: the file loads, the editor runs, and the colour is the general
   one everywhere.

   Three orderings carry that weight, and each is stated in a comment in
   [Emit] with nothing checking it.

   - [#tokens] is the grammar-wide catch-all, so it comes last in any list
     that includes it.
   - Inside [#tokens], a literal comes before any literal it is a prefix of.
     [=] written first would take the [=] of an [==].
   - Inside [#tokens], every literal comes before every pattern token. An
     identifier pattern reaches the text of a keyword, so the keyword has to
     be tried first.

   The sibling law for tree-sitter is [law_treesitter] part (m), where the
   rule runs the other way: that engine takes the last pattern rather than
   the first. *)

let token_regex (token : Core.Token.def) : string option =
  match Textmate.Oniguruma.of_token token with
  | Ok regex -> Some regex
  | Error _ -> None
;;

(* Where a token's own pattern sits in [#tokens], by the regex it emits. *)
let position_in (patterns : Yojson.Basic.t list) (regex : string) : int option =
  let rec look (index : int) (rest : Yojson.Basic.t list) : int option =
    match rest with
    | [] -> None
    | pattern :: tl ->
      (match field "match" pattern with
       | Some (`String found) when found = regex -> Some index
       | _ -> look (index + 1) tl)
  in
  look 0 patterns
;;

let () =
  let before = Law.failures () in
  let checked = ref 0 in
  List.iter
    (fun (e : emitted) ->
       (* [#tokens] last, in every list that has it. *)
       List.iter
         (fun (key, entry) ->
            List.iter
              (fun pattern ->
                 match field "patterns" pattern with
                 | Some (`List items) ->
                   let count = List.length items in
                   List.iteri
                     (fun index item ->
                        if reference_of item = Some "tokens"
                        then (
                          incr checked;
                          if index <> count - 1
                          then
                            Law.fail
                              "(l) %s: %s includes #tokens at %d of %d, and every \
                               pattern after it is unreachable wherever #tokens matches"
                              e.name
                              key
                              index
                              count))
                     items
                 | _ -> ())
              (patterns_in entry))
         (entries e);
       (* Inside [#tokens], the specific literal and then the general one. *)
       match List.assoc_opt "tokens" (entries e) with
       | None -> ()
       | Some entry ->
         let items =
           match field "patterns" entry with
           | Some (`List items) -> items
           | _ -> []
         in
         let placed =
           Array.to_list Core.Facts.(e.facts.tokens)
           |> List.filter_map (fun (token : Core.Token.def) ->
             match token_regex token with
             | None -> None
             | Some regex ->
               (match position_in items regex with
                | None -> None
                | Some index -> Some (token, index)))
         in
         let literals, patterns =
           List.partition
             (fun ((token : Core.Token.def), _) -> Core.Token.text token <> None)
             placed
         in
         List.iter
           (fun ((short : Core.Token.def), at_short) ->
              let text = Option.get (Core.Token.text short) in
              List.iter
                (fun ((long : Core.Token.def), at_long) ->
                   let longer = Option.get (Core.Token.text long) in
                   if
                     String.length longer > String.length text
                     && String.sub longer 0 (String.length text) = text
                   then (
                     incr checked;
                     if at_long > at_short
                     then
                       Law.fail
                         "(l) %s: #tokens tries %S before %S, so %S never matches whole"
                         e.name
                         text
                         longer
                         longer))
                literals)
           literals;
         List.iter
           (fun ((literal : Core.Token.def), at_literal) ->
              List.iter
                (fun ((token : Core.Token.def), at_pattern) ->
                   incr checked;
                   if at_pattern < at_literal
                   then
                     Law.fail
                       "(l) %s: #tokens tries the pattern token %S before the literal \
                        %S, which it can swallow"
                       e.name
                       (Core.Grammar.Name.Token.to_string token.name)
                       (Option.value (Core.Token.text literal) ~default:""))
                patterns)
           literals)
    emitted;
  if Law.failures () = before
  then
    Law.pass
      "(l) a pattern list tries the specific before the general, over %d orderings"
      !checked
;;

(* -- a hand-written pattern reaches the document ------------------------- *)

let () =
  let before = Law.failures () in
  let rust = scopes_of (facts_of Lingo_grammars.Rust_grammar.grammar) in
  match
    Textmate.generate
      rust
      ~raw:[ Core.Grammar.Name.Rule.of_string "Block", {|{ "include": "source.js" }|} ]
      ~language:"rust"
      ()
  with
  | Error problems ->
    List.iter
      (fun p -> Law.fail "a hand-written pattern: %a" Textmate.Check.pp_problem p)
      problems
  | Ok text ->
    let document = Yojson.Basic.from_string text in
    let found =
      match field "repository" document with
      | Some (`Assoc fields) ->
        (match List.assoc_opt "block" fields with
         | Some json ->
           List.exists
             (fun pattern ->
                match field "include" pattern with
                | Some (`String "source.js") -> true
                | _ -> false)
             (patterns_in json)
         | None -> false)
      | _ -> false
    in
    if not found then Law.fail "a hand-written pattern did not reach the entry it names";
    if Law.failures () = before
    then Law.pass "a hand-written pattern reaches the entry it names"
;;

(* -- (m) two guards the corpus cannot reach ------------------------------ *)

(* Both of these read zero over the twelve grammars, and were recorded as
   findings rather than dropped. A witness grammar apiece measures them.

   [Emit.bracket_only] keeps a token in the grammar-wide entry where the
   token is half of a matched pair somewhere and an ordinary child somewhere
   else. Without that, the token's own scope reaches nothing outside the
   pairs.

   [Shape.closing_text] gives a region no [end] anchor where the rule's last
   child may be absent. The anchor is a lookbehind past that child's text, so
   on input that leaves the child out the region would run to the end of the
   file. *)
let () =
  let before = Law.failures () in
  (* [lbrack] frames [File] and is also an ordinary child of [Item]. *)
  let both_bracket_and_child =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "item" (Rule "Item") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ; prod
          "Item"
          [ child_alt ~modifier:Exactly_one "one" [ Token "word"; Token "lbrack" ] ]
      ]
  in
  (match
     Textmate.generate
       (scopes_of (facts_of both_bracket_and_child))
       ~language:"witness"
       ()
   with
   | Error problems ->
     List.iter
       (fun p ->
          Law.fail
            "(m) a token that is a bracket and a child: %a"
            Textmate.Check.pp_problem
            p)
       problems
   | Ok text ->
     let document = Yojson.Basic.from_string text in
     (match field "repository" document with
      | Some (`Assoc fields) ->
        (match List.assoc_opt "tokens" fields with
         | None -> Law.fail "(m) the witness emitted no grammar-wide token entry"
         | Some entry ->
           if
             not
               (List.exists
                  (fun pattern ->
                     match field "match" pattern with
                     | Some (`String regex) -> regex = {|\[|}
                     | _ -> false)
                  (patterns_in entry))
           then
             Law.fail
               "(m) [ is a bracket in one rule and a child in another, and the \
                grammar-wide entry leaves it out, so its occurrences outside a pair are \
                unscoped")
      | _ -> Law.fail "(m) the witness emitted no repository"));
  (* [Decl] contains its own errors and opens on [sig]. Its last child may be
     absent, so no lookbehind can end it. *)
  let optional_last_child =
    let open Core.Grammar in
    create
      ~tokens:
        [ kw "sig"
        ; punct ~space_before:false ~name:"semi" ";"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "decl" (Rule "Decl") ]
      ; prod
          "Decl"
          [ child_req "kw" (Token "sig")
          ; child_req "name" (Token "word")
          ; child_opt "semi" (Token "semi")
          ]
        |> with_committed
      ]
  in
  (match
     Textmate.generate (scopes_of (facts_of optional_last_child)) ~language:"witness" ()
   with
   | Error problems ->
     List.iter
       (fun p ->
          Law.fail
            "(m) a committed rule ending on an optional child: %a"
            Textmate.Check.pp_problem
            p)
       problems
   | Ok text ->
     let document = Yojson.Basic.from_string text in
     (match field "repository" document with
      | Some (`Assoc fields) ->
        (match List.assoc_opt "decl" fields with
         | None -> Law.fail "(m) the witness emitted no entry for decl"
         | Some entry ->
           if
             List.exists
               (fun pattern -> field "begin" pattern <> None)
               (patterns_in entry)
           then
             Law.fail
               "(m) decl ends on a child that may be absent and still opened a region, \
                which would run to the end of the file where the child is left out")
      | _ -> Law.fail "(m) the witness emitted no repository"));
  if Law.failures () = before
  then Law.pass "(m) both guards the corpus cannot reach have a witness"
;;

(* -- (n) every field and every reference form ------------------------------ *)

(* Part (j) says the checker rejects. It does not say what the checker
   accepts, and two of its tables were read by nothing but themselves.
   [known_fields] lists the fifteen keys a hand-written pattern may carry, and
   [is_reference] decides which of four forms an include takes. On 2026-09-26
   twenty-three mutations survived the whole suite there: fifteen dropped a
   field from the list and the rest moved a branch of the reference test.

   So both tables are written a second time here, on purpose. Dropping a field
   makes the pattern that uses it fail, and loosening the reference test makes
   a form that should be refused come back accepted. *)

let rust_scopes = lazy (scopes_of (facts_of Lingo_grammars.Rust_grammar.grammar))

let checked (raw : string) : (string, Textmate.Check.problem list) result =
  Textmate.generate
    (Lazy.force rust_scopes)
    ~raw:[ Core.Grammar.Name.Rule.of_string "Block", raw ]
    ~language:"rust"
    ()
;;

let accepts (what : string) (raw : string) : unit =
  match checked raw with
  | Ok _ -> ()
  | Error problems ->
    Law.fail
      "(n) %s was refused: %s"
      what
      (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
;;

let refuses (what : string) (raw : string) : unit =
  match checked raw with
  | Ok _ -> Law.fail "(n) %s was accepted" what
  | Error problems ->
    if
      not
        (List.exists
           (function
             | Textmate.Check.Bad_raw_pattern _ -> true
             | _ -> false)
           problems)
    then
      Law.fail
        "(n) %s: reported %s"
        what
        (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
;;

(* One pattern per key. A key that needs company brings it: [begin] needs an
   [end], and a capture list needs a group to hang off. *)
let fields =
  [ "match", {|{ "match": "a", "name": "keyword.rust" }|}
  ; "name", {|{ "match": "a", "name": "keyword.rust" }|}
  ; "begin", {|{ "begin": "a", "end": "b", "name": "keyword.rust" }|}
  ; "end", {|{ "begin": "a", "end": "b", "name": "keyword.rust" }|}
  ; "include", {|{ "include": "$self" }|}
  ; "patterns", {|{ "patterns": [ { "match": "a", "name": "keyword.rust" } ] }|}
  ; "captures", {|{ "match": "(a)", "captures": { "1": { "name": "keyword.rust" } } }|}
  ; ( "beginCaptures"
    , {|{ "begin": "(a)", "end": "b", "beginCaptures": { "1": { "name": "keyword.rust" } } }|}
    )
  ; ( "endCaptures"
    , {|{ "begin": "a", "end": "(b)", "endCaptures": { "1": { "name": "keyword.rust" } } }|}
    )
  ; "contentName", {|{ "begin": "a", "end": "b", "contentName": "keyword.rust" }|}
  ; "applyEndPatternLast", {|{ "begin": "a", "end": "b", "applyEndPatternLast": true }|}
  ; "while", {|{ "begin": "a", "end": "b", "while": "c" }|}
  ; ( "whileCaptures"
    , {|{ "begin": "a", "end": "b", "while": "(c)", "whileCaptures": { "1": { "name": "keyword.rust" } } }|}
    )
  ; "comment", {|{ "match": "a", "name": "keyword.rust", "comment": "why" }|}
  ; "disabled", {|{ "match": "a", "name": "keyword.rust", "disabled": true }|}
  ]
;;

(* The four forms a reference takes, and the slips that look like one. *)
let references =
  [ "#entry", true
  ; "$self", true
  ; "$base", true
  ; "source.js", true
  ; "source.js#entry", true
  ; "trivia", false
  ; "#", false
  ; "", false
  ; "source.js#a#b", false
  ; "source.js#", false
  ]
;;

(* The settings a caller passes, rather than anything in the grammar. Each is
   refused for being empty and for holding whitespace, and the two halves of
   that test are separate places in the code. *)
let setting (what : string) (result : (string, Textmate.Check.problem list) result) : unit
  =
  match result with
  | Ok _ -> Law.fail "(n) %s was accepted" what
  | Error problems ->
    if
      not
        (List.exists
           (function
             | Textmate.Check.Bad_setting _ -> true
             | _ -> false)
           problems)
    then
      Law.fail
        "(n) %s: reported %s"
        what
        (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
;;

let () =
  let before = Law.failures () in
  let rust = Lazy.force rust_scopes in
  setting "an empty language" (Textmate.generate rust ~language:"" ());
  setting "a language holding a space" (Textmate.generate rust ~language:"r ust" ());
  setting
    "an empty scope prefix"
    (Textmate.generate rust ~language:"rust" ~scope_prefix:"" ());
  setting
    "a scope prefix holding a space"
    (Textmate.generate rust ~language:"rust" ~scope_prefix:"so urce" ());
  setting
    "an empty file type"
    (Textmate.generate rust ~language:"rust" ~file_types:[ "" ] ());
  setting
    "a file type holding a space"
    (Textmate.generate rust ~language:"rust" ~file_types:[ "r s" ] ());
  List.iter (fun (field, raw) -> accepts ("a pattern with " ^ field) raw) fields;
  List.iter
    (fun (reference, good) ->
       let raw = Printf.sprintf {|{ "include": %S }|} reference in
       let what = Printf.sprintf "the reference %S" reference in
       if good then accepts what raw else refuses what raw)
    references;
  if Law.failures () = before
  then
    Law.pass
      "(n) every one of the %d fields, %d reference forms and the settings read as they \
       should"
      (List.length fields)
      (List.length references)
;;

let () = Law.summarise "law_textmate"
