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

      Mechanism. Parts (a) to (h) read the emitted JSON back and walk it. They
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

      Coverage. The twelve grammars in test/editors/editor_corpus.ml, with the
      overrides an author would write on json, rust and effekt. Counts print
      beside each result. Parts (i) to (k) build their own witnesses.

      What this says nothing about. Whether a regex compiles under Oniguruma,
      and whether an editor colours what a reader expects. The first needs the
      engine. The second needs a person, who reads test/editors/*.textmate by
      eye.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        M1  In [Oniguruma.neutralise], return the argument unchanged.
            -> part (k) alone, 1 finding: two parts were spliced and the regex
               has 4 groups.

               It reddens nothing else, so part (k) is written as a witness
               rather than over the corpus. Nothing in the corpus splices a
               capturing group. redfa emits [(?:] throughout and the trivia
               separator is built non-capturing, so nothing reached the
               function until the witness existed.
        M2  In [Emit.rule_include], leave the alias unresolved.
            -> part (a), 24 findings: effekt 13, wide 5, recovery 3, rust 2,
               shapes 1.
               Every reference to a rule that forwards to another names an
               entry no longer emitted. shapes gives the smallest case,
               [let] referencing [#init].
        M3  In [Textmate.prune], keep every entry.
            -> part (b), 1 finding: postfix's [field]. That is the only entry
               in the corpus that nothing reaches, and it has a reason:
               [Field] is the right-hand side of an access operator, and the
               operator folds the token at the end of it into its own regex.
        M4  In [Textmate.break_cycles], drop nothing.
            -> part (c), 60 findings, all effekt. rust and postfix have flat
               entries that reference a block. The way back runs through a
               region's body, and a region compiles when it fires rather than
               when the reference to it is read. Only effekt has blocks and
               statements that reference each other flat all the way round.
        M5  In [Emit.scope_string], leave the language off.
            -> parts (e) and (h), 532 findings: 339 scopes that no longer end
               in the language, and 193 scopes the document no longer carries
               under their rendered name. Every grammar reddens.
        M6  In [Scopes.resolve], read the token before the child override.
            -> part (h), 7 findings: json's [Member.key], rust's
               [Field.name], [Variant.name], [Param.name] and [Type.name],
               and wide's [Alias.name] and [Table.name].
               These are exactly the positions where the same [ident] means
               different things. A per-position scope exists for them.
        M7  In [Shape.of_rule], let a rule framed by a matched pair fall
            through to a flat pattern list.
            -> parts (g) and (h), 47 findings: 27 rules that are framed and
               emit no region, and 20 delimiter scopes that then reach
               nothing. A delimiter's scope lives in the region's
               [beginCaptures].
        M8  In [Shape.region_begin], never fold the identity child.
            -> part (h), 5 findings, all rust: the five [entity.name.*] scopes
               on [Struct], [Enum], [Trait], [Fn] and [MethodSig]. Those
               scopes land only where the identity child is folded. A rule
               that names itself by a child does it once, and a body pattern
               fires everywhere.

      Two mutations redden nothing and move no golden byte.

        N1  In [Shape.closing_text], drop the guard that the last child be
            required.
            -> nothing. No rule in the corpus contains its own errors, opens
               on a literal and ends on a child that may be absent. The guard
               is there so that a region whose anchor the input never reaches
               does not run to the end of the file. That claim is about an
               input, and this law reads no input.
        N2  In [Emit.bracket_only], stop subtracting the tokens that also
            appear as an ordinary child.
            -> nothing. No token in the corpus is both half of a matched pair
               and a child somewhere else. The subtraction keeps such a token
               in the grammar-wide entry. Its occurrences outside any pair are
               scoped there, and part (h) would report those going dark.
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
           (fun p -> fail "%s: %a" entry.name Textmate.Check.pp_problem p)
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
                fail "(a) %s: %s references #%s, which nothing declares" e.name where key)
           (references_of json)
       in
       List.iter (check "the root") (root_patterns e);
       List.iter (fun (key, json) -> check key json) (entries e))
    emitted;
  if !failures = 0
  then pass "(a) every reference names an entry, over %d of them" !checked
;;

(* -- (b) every entry is reachable ---------------------------------------- *)

let () =
  let before = !failures in
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
            then fail "(b) %s: nothing reaches the entry %S" e.name key)
         table)
    emitted;
  if !failures = before
  then pass "(b) every entry is reachable from the root, over %d of them" !total
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
  let before = !failures in
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
             fail "(c) %s: the reference-only entries %s form a cycle" e.name cycle))
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
  if !failures = before then pass "(c) no cycle runs through a reference-only entry"
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
  let before = !failures in
  let checked = ref 0 in
  let groups = ref 0 in
  List.iter
    (fun (e : emitted) ->
       let check (where : string) (json : Yojson.Basic.t) : unit =
         match json with
         | `Assoc fields ->
           let has key = List.mem_assoc key fields in
           (match has "begin", has "end" with
            | true, false -> fail "(d) %s: %s has a begin and no end" e.name where
            | false, true -> fail "(d) %s: %s has an end and no begin" e.name where
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
                         fail
                           "(d) %s: %s scopes capture %d of %S, which has %d groups"
                           e.name
                           where
                           index
                           regex
                           available
                       | None ->
                         fail
                           "(d) %s: %s scopes %S, which is no capture"
                           e.name
                           where
                           index)
                    indexed
                | Some (`Assoc _), _ ->
                  fail "(d) %s: %s has a %s map and no %s" e.name where map_key regex_key
                | _ -> ())
             [ "captures", "match"; "beginCaptures", "begin"; "endCaptures", "end" ]
         | _ -> ()
       in
       List.iter (fun (key, json) -> List.iter (check key) (patterns_in json)) (entries e))
    emitted;
  if !failures = before
  then
    pass
      "(d) every region is paired and every capture exists, over %d captures and %d \
       groups"
      !checked
      !groups
;;

(* -- (e) every scope is a path ------------------------------------------- *)

let () =
  let before = !failures in
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
                   then fail "(e) %s: the scope %S is not a path" e.name scope;
                   if String.split_on_char '.' scope |> List.exists (String.equal "")
                   then fail "(e) %s: the scope %S has an empty segment" e.name scope;
                   if not (Filename.check_suffix scope ("." ^ e.name))
                   then
                     fail "(e) %s: the scope %S does not end in the language" e.name scope
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
  if !failures = before then pass "(e) every scope is a path, over %d of them" !checked
;;

(* -- (f) the same bytes every time --------------------------------------- *)

let () =
  let before = !failures in
  List.iter
    (fun (e : emitted) ->
       match Textmate.generate e.scopes ~language:e.name () with
       | Error _ -> fail "(f) %s: the second run rejected what the first emitted" e.name
       | Ok again ->
         if not (String.equal again e.text)
         then fail "(f) %s: two runs gave different bytes" e.name)
    emitted;
  if !failures = before
  then
    pass "(f) a grammar emits the same bytes twice, over %d of them" (List.length emitted)
;;

(* -- (g) a region is a frame --------------------------------------------- *)

let () =
  let before = !failures in
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
                      fail
                        "(g) %s: %s is framed by a matched pair and emits no region"
                        e.name
                        (Core.Grammar.Name.Rule.to_string rule.name)
                  | Core.Rule.Committed _ -> ()
                  | Core.Rule.Plain | Core.Rule.Separated _ ->
                    if is_region
                    then
                      fail
                        "(g) %s: %s emits a region and its framing contains nothing"
                        e.name
                        (Core.Grammar.Name.Rule.to_string rule.name))))
         Core.Facts.(e.facts.rules))
    emitted;
  if !failures = before
  then
    pass
      "(g) a region is a frame, over %d regions and %d entries beside them"
      !regions
      !plain
;;

(* -- (h) every scope reaches the document -------------------------------- *)

let () =
  let before = !failures in
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
             fail
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
  if !failures = before
  then pass "(h) every scope reaches the document, over %d of them" !carried
;;

(* -- (i) a witness per finding ------------------------------------------- *)

let facts_of (grammar : Core.Grammar.t) : Core.Facts.t =
  match Core.Facts.of_grammar grammar with
  | Ok facts -> facts
  | Error es -> failwith (Format.asprintf "%a" Core.Error.pp_list es)
;;

let () =
  let before = !failures in
  let facts = facts_of Lingo_grammars.Rust_grammar.grammar in
  let one (what : string) (override : Scopes.override) (expected : Scopes.finding) : unit =
    match Scopes.of_facts facts ~overrides:[ override ] with
    | Ok _ -> fail "(i) %s: the override was accepted" what
    | Error [ finding ] ->
      if
        not
          (String.equal
             (Scopes.finding_to_string finding)
             (Scopes.finding_to_string expected))
      then
        fail
          "(i) %s: reported %S where %S was expected"
          what
          (Scopes.finding_to_string finding)
          (Scopes.finding_to_string expected)
    | Error findings ->
      fail
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
   | Ok _ -> fail "(i) a malformed scope: the override was accepted"
   | Error [ Scopes.Malformed_scope _ ] -> ()
   | Error findings ->
     fail
       "(i) a malformed scope: reported %s"
       (String.concat ", " (List.map Scopes.finding_to_string findings)));
  if !failures = before then pass "(i) every finding has a witness override"
;;

(* -- (j) a witness per problem ------------------------------------------- *)

let scopes_of (facts : Core.Facts.t) : Scopes.t =
  match Scopes.of_facts facts ~overrides:[] with
  | Ok scopes -> scopes
  | Error fs -> failwith (String.concat ", " (List.map Scopes.finding_to_string fs))
;;

let () =
  let before = !failures in
  let expect
        (what : string)
        (result : (string, Textmate.Check.problem list) result)
        (expected : Textmate.Check.problem -> bool)
    : unit
    =
    match result with
    | Ok _ -> fail "(j) %s: the grammar was accepted" what
    | Error problems ->
      if not (List.exists expected problems)
      then
        fail
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
   | Error _ -> fail "(j) a scope with nowhere to go: the override was rejected"
   | Ok scopes ->
     expect
       "a scope on a rule that emits a list"
       (Textmate.generate scopes ~language:"rust" ())
       (function
       | Textmate.Check.Unattachable_scope _ -> true
       | _ -> false));
  if !failures = before then pass "(j) every reachable problem has a witness"
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
  let before = !failures in
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
  | Error fs -> List.iter (fun f -> fail "(k) %s" (Scopes.finding_to_string f)) fs
  | Ok scopes ->
    (match Textmate.generate scopes ~language:"witness" () with
     | Error problems ->
       List.iter (fun p -> fail "(k) %a" Textmate.Check.pp_problem p) problems
     | Ok text ->
       let document = Yojson.Basic.from_string text in
       (match
          Option.bind (field "repository" document) (fun repository ->
            field "pair" repository)
        with
        | None -> fail "(k) the witness emitted no entry for its one production"
        | Some entry ->
          (match field "match" entry, field "captures" entry with
           | Some (`String regex), Some (`Assoc indexed) ->
             let groups = capture_groups regex in
             if groups <> 2
             then
               fail
                 "(k) two parts were spliced and the regex %S has %d groups"
                 regex
                 groups;
             let named = List.sort compare (List.map fst indexed) in
             if named <> [ "1"; "2" ]
             then fail "(k) the capture map names %s" (String.concat ", " named)
           | _ -> fail "(k) the witness did not emit a single match with captures")));
    if !failures = before
    then pass "(k) a spliced part contributes exactly one capture group"
;;

(* -- a hand-written pattern reaches the document ------------------------- *)

let () =
  let before = !failures in
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
      (fun p -> fail "a hand-written pattern: %a" Textmate.Check.pp_problem p)
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
    if not found then fail "a hand-written pattern did not reach the entry it names";
    if !failures = before then pass "a hand-written pattern reaches the entry it names"
;;

let () =
  if !failures = 0
  then print_endline "law_textmate: 0 failures"
  else (
    Printf.printf "law_textmate: %d failures\n" !failures;
    exit 1)
;;
