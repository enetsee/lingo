(* -- the TextMate backend -----------------------------------------------------

      (a) Every [#key] in an emitted grammar names a repository entry.
      (b) Every repository entry is reachable from the root patterns.
      (c) No cycle runs through entries that are nothing but reference lists.
      (d) Every [begin] has an [end], at every depth, and every capture index
          a pattern scopes is a group its regex actually has.
      (e) Every scope is a well formed path ending in the language.
      (f) A grammar emits the same bytes every time.
      (g) A [begin] appears wherever a matched pair frames a rule, and never
          on a separated list.
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
      (o) A declaration's name is scoped once, in a region's [begin], and
          never by a bare pattern in a list.
      (p) Every grammar in the corpus is accepted.
      (q) A clash is found wherever the engine meets one: behind an
          expression block, ahead of one, against an operator its pattern
          also matches, and in a rule nothing literal leads into.
      (r) A region whose list cannot tell two children apart is cut in two
          where a cut parts them, and refused where none does.
      (s) Each token of a list has one thing that opens on it: a rule is a
          region wherever its opening and its end can be told, a region is
          cut wherever two takers of one token stand on two sides, and an
          operand opens a region its operators are read in.

      Mechanism. Parts (a) to (h) and (l) read the emitted JSON back and walk
      it. They
      share no code with the emitter. The reference walk, the capture counter
      and the scope-path check are written here a second time. A law that
      calls the function it is checking proves only that the function agrees
      with itself.

      Part (g) is the claim {!Core.Rule.frame} is for. In the predecessor the
      author wrote each region by hand, once per production, and named the
      anchor regexes. Here the region comes out of the frame, so the law is
      that the two agree: a rule framed by a matched pair is a region, and a
      separated list never is. Any other rule may be one, where it opens on a
      literal and ends on one.

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

      Part (r) came from the engine all the same. On 2026-11-20 a grammar with
      [do (head) \{ \} while (cond)], [while (head) (cond) \{ \}] and
      [try \{ \} catch (e) \{ \}] went through vscode-textmate 9 as the backend
      would have emitted it. The [do] region ended behind the head's bracket
      and its [while] opened a loop of its own that ran to the next brace. The
      [try] region ended behind its first block, and the handler's parameter
      came out as an expression. Cut as part (r) describes, all three colour
      as written, on one line and across several, and nested in each other.
      No grammar in the corpus has the shape, so the part carries witnesses,
      one for each condition a cut has to meet.

      Part (s) came from the engine as well. On 2026-11-20 every token of 39
      Cupcake files and of the good inputs of this corpus went through
      vscode-textmate 9, and the regions around it were compared with the
      rules the parser puts around it. The faults that showed, each of which
      part (s) holds a witness against:

      - A call and a parenthesised expression in one list, told apart by the
        order of the list.
      - A rule with no fixed end left as a list, so that a pattern's bracket
        stood in the block its statement stands in.
      - A rule that opens on a name and holds a bracket left as a list, so
        that a constructor's fields were read where a tuple pattern stood.
      - A wrapper forwarding to what it holds and losing the literal it opens
        on.
      - An expression's list ending in the grammar-wide token list, which took
        the keyword of every region listed after the expression.
      - The first part of a cut region opening a second time on its own
        keyword, so that [if ... else if ...] was one region.
      - A single match reading an expression block as one token.

      With these fixed every token of every one of those inputs stands in the
      regions the parser gives it, and the same holds with a space put before
      every bracket and a line break before every brace. That check needs the
      engine, so no test here makes it. What a test here can say is the last
      line of part (s): no list of any corpus grammar gives one token to two
      regions, but for the one it names.

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

(* -- mutation testing, generated by assay on 2026-10-15 ---------------------
      editors/textmate/textmate.ml, 71 mutants, 54 killed, 16 survived, 1 timed out.
        extreme     12  11 killed, law_textmate (a): 5, law_textmate (c): 4, dump_textmate: 1, law_textmate a: 1; 1 survived
        sbr         29  24 killed, dump_textmate: 8, law_textmate (a): 7, law_textmate (c): 6, law_textmate (b): 3; 4 survived; 1 timed out
        ror          9  6 killed, law_textmate (a): 3, law_textmate (c): 2, dump_textmate: 1; 3 survived
        lcr          8  2 killed, dump_textmate: 2; 6 survived
        aor          3  all killed, law_textmate: 3
        uoi         10  8 killed, law_textmate (c): 4, law_textmate (a): 2, dump_textmate: 1, law_textmate (h): 1; 2 survived
      survived in aliases (sbr), is_flat (lcr 2), references (ror, lcr),
        break_cycles (sbr, ror, lcr 2, uoi), all_references (ror, lcr),
        generate (extreme, sbr 2, uoi)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-15 ---------------------
      editors/textmate/emit.ml, 86 mutants, 75 killed, 11 survived.
        extreme     21  20 killed, law_textmate (h): 15, dump_textmate: 2, law_textmate (a): 1, law_textmate (e): 1, law_textmate (l): 1; 1 survived
        sbr         45  40 killed, law_textmate (h): 15, dump_textmate: 13, law_textmate (d): 9, law_textmate (m): 3; 5 survived
        ror          4  2 killed, (h) 1 (l) 1; 2 survived
        lcr          3  all killed, dump_textmate: 2, law_textmate: 1
        aor          7  4 killed, law_textmate: 3, law_textmate (k): 1; 3 survived
        uoi          6  all killed, law_textmate (h): 3, dump_textmate: 2, law_textmate: 1
      survived in wrap_scope (ror 2, aor 3),
        separator_pattern (extreme, sbr 2), content_includes (sbr),
        bracket_only (sbr 2)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-15 ---------------------
      editors/textmate/shape.ml, 65 mutants, 57 killed, 8 survived.
        extreme     21  all killed, law_textmate (p): 15, dump_textmate: 4, law_textmate (h): 1, law_textmate (j): 1
        sbr          9  8 killed, dump_textmate: 4, law_textmate (h): 4; 1 survived
        ror          8  5 killed, law_textmate (p): 2, dump_textmate: 1, law_textmate (h): 1, law_textmate (k): 1; 3 survived
        lcr          6  3 killed, (h) 3; 3 survived
        aor          5  4 killed, law_textmate: 4; 1 survived
        uoi         16  all killed, law_textmate (p): 8, law_textmate (h): 3, dump_textmate: 2, law_textmate (j): 1, law_textmate (k): 1, law_textmate (q): 1
      survived in leaf_token (ror, lcr 2, aor), region_begin (sbr, ror),
        is_covered_elsewhere (ror), forwards_to (lcr)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-15 ---------------------
      editors/textmate/check.ml, 92 mutants, 74 killed, 18 survived.
        extreme     20  13 killed, (j) 10 a 2 (n) 1; 7 survived
        sbr         39  32 killed, (n) 20 (j) 6 a 6; 7 survived
        ror          9  7 killed, law_textmate (n): 2, law_textmate a: 2, law_textmate: 1, law_textmate (j): 1, law_textmate (p): 1; 2 survived
        lcr          7  6 killed, law_textmate (n): 5, law_textmate: 1; 1 survived
        aor          1  all killed, law_textmate: 1
        uoi         16  15 killed, (p) 5 a 5 (n) 3 (j) 2; 1 survived
      survived in pp_problem (extreme), compare_problem (extreme),
        json_kind (extreme), is_reference (sbr 2), check_keys (sbr),
        check_openers (sbr), check_postfix_leads (extreme, sbr),
        check_forwarding (extreme 2, sbr, uoi),
        carries_a_scope (sbr, ror, lcr), run (extreme, ror)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-15 ---------------------
      editors/textmate/oniguruma.ml, 54 mutants, 36 killed, 17 survived, 1 timed out.
        extreme      8  7 killed, law_textmate (o): 5, dump_textmate: 1, law_textmate (m): 1; 1 survived
        sbr          9  7 killed, (o) 4 (m) 2 (d) 1; 2 survived
        ror         11  6 killed, law_textmate (k): 3, dump_textmate: 1, law_textmate: 1, law_textmate (d): 1; 5 survived
        lcr         10  7 killed, dump_textmate: 3, law_textmate (k): 3, law_textmate (d): 1; 3 survived
        aor          9  3 killed, law_textmate: 3; 5 survived; 1 timed out
        uoi          7  6 killed, law_textmate (k): 2, dump_textmate: 1, law_textmate: 1, law_textmate (m): 1, law_textmate (o): 1; 1 survived
      survived in escape (aor), is_word_byte (ror),
        is_word_shaped (ror, lcr 2), longer_literals (extreme, aor),
        neutralise (sbr 2, ror 3, lcr, aor 3, uoi)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-15 ---------------------
      editors/textmate/clash.ml, 72 mutants, 67 killed, 4 survived, 1 timed out.
        extreme     19  all killed, law_textmate (p): 7, law_textmate (j): 5, law_textmate (q): 5, dump_textmate: 1, law_textmate (h): 1
        sbr         19  15 killed, (j) 6 (o) 4 (p) 4 (q) 1; 3 survived; 1 timed out
        ror          9  8 killed, law_textmate (p): 5, law_textmate: 1, law_textmate (j): 1, law_textmate (q): 1; 1 survived
        lcr          9  all killed, law_textmate (p): 4, law_textmate: 2, law_textmate (q): 2, law_textmate (j): 1
        aor          3  all killed, law_textmate: 2, law_textmate (p): 1
        uoi         13  all killed, law_textmate (p): 8, law_textmate: 2, law_textmate (j): 1, law_textmate (o): 1, law_textmate (q): 1
      survived in places (sbr), of_scopes (sbr 2, ror)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-15 ---------------------
      editors/textmate/key.ml, 2 mutants, 2 killed.
        extreme      2  all killed, (e) 1 (p) 1
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/scopes/scope.ml, 14 mutants, 13 killed, 1 survived.
        extreme      9  8 killed, law_textmate (e): 3, law_textmate (i): 2, dump_scopes: 1, dump_textmate: 1, law_textmate: 1; 1 survived
        sbr          2  all killed, (i) 2
        ror          1  all killed, (i) 1
        lcr          1  all killed, law_textmate: 1
        uoi          1  all killed, law_textmate: 1
      survived in compare (extreme)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/scopes/scopes.ml, 86 mutants, 78 killed, 8 survived.
        extreme     30  27 killed, dump_docs: 7, dump_scopes: 5, law_textmate: 4, law_textmate (i): 4, dump_treesitter: 2, law_textmate (e): 2, dump_textmate: 1, law_textmate (h): 1, law_textmate (j): 1; 3 survived
        sbr         30  27 killed, dump_scopes: 14, dump_docs: 10, law_textmate: 2, law_textmate (i): 1; 3 survived
        ror          6  5 killed, law_textmate: 2, dump_docs: 1, dump_scopes: 1, dump_textmate: 1; 1 survived
        lcr          7  all killed, dump_scopes: 5, law_textmate: 1, law_textmate (i): 1
        uoi         13  12 killed, dump_docs: 9, dump_scopes: 1, dump_textmate: 1, law_textmate: 1; 1 survived
      survived in pp_finding (extreme), finding_rank (extreme),
        compare_finding (extreme, ror, uoi), token_roles (sbr 3)
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
           (fun p -> Law.fail "(p) %s: %a" entry.name Textmate.Check.pp_problem p)
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
                  | Core.Rule.Committed _ | Core.Rule.Plain -> ()
                  | Core.Rule.Separated _ ->
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
  (* Three children whose matched pairs open on the same token. Two are told
     apart by a cut behind the first. With three, one side of any cut still
     holds two. *)
  let shared =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; punct_tight ~name:"rbrace" "}"
        ; punct_tight ~name:"rparen" ")"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "File" ]
      [ prod
          "File"
          [ child_req "one" (Rule "A")
          ; child_req "two" (Rule "B")
          ; child_req "three" (Rule "C")
          ]
      ; prod "A" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ; prod "B" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrace"
      ; prod "C" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rparen"
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
       "three children opening on one token"
       (Textmate.generate (scopes_of facts) ~language:"witness" ())
       (function
         | Textmate.Check.Shared_opener _ -> true
         | _ -> false));
  (* Two children that hold one matched pair. Both reach the one region, so
     the backend takes the grammar. *)
  let twice =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_req "one" (Rule "A"); child_req "two" (Rule "A") ]
      ; prod "A" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ]
  in
  (match Textmate.generate (scopes_of (facts_of twice)) ~language:"witness" () with
   | Ok _ -> ()
   | Error problems ->
     Law.fail
       "(j) two children holding one pair: reported %s"
       (String.concat ", " (List.map Textmate.Check.problem_to_string problems)));
  (* One list that gives a token to two things. A statement reads an
     expression and then a body, and a set among the expression's atoms
     opens on the body's brace. Then an angle bracket that is both an
     operator of the expression and the opener of one of its atoms. *)
  let either_of (problems : Textmate.Check.problem list) ~(loose : bool) : bool =
    List.exists
      (function
        | Textmate.Check.Undecided { second; _ } -> second = None = loose
        | _ -> false)
      problems
  in
  let undecided ~(loose : bool) (what : string) (grammar : Core.Grammar.t) : unit =
    match Textmate.generate (scopes_of (facts_of grammar)) ~language:"witness" () with
    | Ok _ -> Law.fail "(j) %s: the grammar was accepted" what
    | Error problems ->
      if not (either_of problems ~loose)
      then
        Law.fail
          "(j) %s: reported %s"
          what
          (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
  in
  undecided
    ~loose:false
    "a bracket that opens an atom and the body behind the expression"
    (let open Core.Grammar in
     create
       ~expr:[ expr_block ~rule_name:"Expr" ~atoms:[ Token "word"; Rule "Set" ] () ]
       ~tokens:
         [ kw "go"
         ; punct_tight ~name:"lbrace" "{"
         ; punct_tight ~name:"rbrace" "}"
         ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
         ]
       ~roots:[ "File" ]
       [ prod "File" [ child_rep "stmt" (Rule "Stmt") ]
       ; prod
           "Stmt"
           [ child_req "kw" (Token "go")
           ; child_req "value" (Rule "Expr")
           ; child_req "body" (Rule "Body")
           ]
       ; prod "Set" [ child_rep "word" (Token "word") ]
         |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
       ; prod "Body" [ child_rep "word" (Token "word") ]
         |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
       ]);
  undecided
    ~loose:true
    "a bracket that is an operator and opens an atom"
    (let open Core.Grammar in
     create
       ~expr:
         [ expr_block
             ~rule_name:"Expr"
             ~atoms:[ Token "word"; Rule "Angle" ]
             ~infix_ops:[ infix ~token:"langle" ~bp:10 () ]
             ()
         ]
       ~tokens:
         [ punct ~name:"langle" "<"
         ; punct ~name:"rangle" ">"
         ; punct ~name:"semi" ";"
         ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
         ]
       ~roots:[ "File" ]
       [ prod "File" [ child_rep "stmt" (Rule "Stmt") ]
       ; prod "Stmt" [ child_req "value" (Rule "Expr"); child_req "semi" (Token "semi") ]
       ; prod "Angle" [ child_rep "word" (Token "word") ]
         |> with_delimited ~open_tok:"langle" ~close_tok:"rangle"
       ]);
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
  (* [Entry] is a word scoped as a key, then a [Value] that is a word or a
     number. A child with two alternatives makes both lists and not single
     matches. Both words are tried in the brackets' body, nothing literal
     sits beside the key, and nothing literal sits before [Entry] where the
     body includes it. *)
  let unanchored =
    let open Core.Grammar in
    create
      ~tokens:
        [ punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ; pat "num" Redfa.Regex.(plus (range_char ~lo:'0' ~hi:'9'))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "entry" (Rule "Entry") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ; prod "Entry" [ child_req "key" (Token "word"); child_req "value" (Rule "Value") ]
      ; prod "Value" [ child_alt ~modifier:Exactly_one "v" [ Token "word"; Token "num" ] ]
      ]
  in
  (match
     Scopes.of_facts
       (facts_of unanchored)
       ~overrides:
         [ Scopes.Child
             { rule = Core.Grammar.Name.Rule.of_string "Entry"
             ; child = Core.Grammar.Name.Child.of_string "key"
             ; scope = Scopes.Scope.Variable_other_member
             }
         ]
   with
   | Error _ -> Law.fail "(j) a scope with no anchor: the override was rejected"
   | Ok scopes ->
     expect
       "a scoped child with nothing literal beside it"
       (Textmate.generate scopes ~language:"witness" ())
       (function
       | Textmate.Check.Unanchored_scope _ -> true
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
        ; punct ~space_before:Hug ~name:"semi" ";"
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

(* -- (o) a declaration names itself once ---------------------------------- *)

(* A plain rule that opens on a keyword and ends on a closer is a region, and
   its name is a group in the [begin]. A plain rule that cannot be one is a
   pattern list. Its name is scoped where a required literal sits right before
   it, as one match with that literal, and takes the token's own scope
   anywhere else. The pair sits at the front of the place that tries the
   list, here the document root, so nothing spliced ahead of it takes its
   text first.

   Nothing in the corpus has any of these shapes with an identity scope on it,
   so a witness carries all three. [FnDef] is the region. [Ty] opens on an
   optional token and names itself after [type], so it is a list with a pair.
   [Let] has nothing before its name but an optional token, so it is a list
   with no pair. [Decl] above them is a list too. Every pattern in a list is
   tried wherever the list is included. Before the fix, [Let]'s bare [word]
   pattern named every [pub], [def] and lowercase word in the file a
   function.

   A [word] pattern left in a list still comes ahead of [#tokens], so it is
   fenced off every keyword. Without the fence it takes [pub] before the
   keyword scope can. *)
let () =
  let before = Law.failures () in
  let witness =
    let open Core.Grammar in
    create
      ~tokens:
        [ kw "def"
        ; kw "pub"
        ; kw "static"
        ; kw "open"
        ; kw "type"
        ; punct_tight ~name:"lbrace" "{"
        ; punct_tight ~name:"rbrace" "}"
        ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
        ]
      ~roots:[ "File" ]
      [ prod "File" [ child_rep "decl" (Rule "Decl") ]
      ; prod
          "Decl"
          [ child_opt "vis" (Token "pub")
          ; child_alt ~modifier:Exactly_one "kind" [ Rule "FnDef"; Rule "Let"; Rule "Ty" ]
          ]
      ; prod
          "FnDef"
          [ child_req "kw" (Token "def")
          ; child_req "name" (Token "word")
          ; child_req "body" (Rule "Body")
          ]
        |> with_identity "name"
      ; prod "Let" [ child_opt "st" (Token "static"); child_req "name" (Token "word") ]
        |> with_identity "name"
      ; prod
          "Ty"
          [ child_opt "o" (Token "open")
          ; child_req "kw" (Token "type")
          ; child_req "name" (Token "word")
          ]
        |> with_identity "name"
      ; prod "Body" [ child_rep "stmt" (Token "word") ]
        |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace"
      ]
  in
  let identity (name : string) : Scopes.override =
    Scopes.Identity
      { rule = Core.Grammar.Name.Rule.of_string name
      ; scope = Scopes.Scope.Entity_name_function
      }
  in
  let function_name = "entity.name.function.witness" in
  let named_function (json : Yojson.Basic.t) : bool =
    field "name" json = Some (`String function_name)
  in
  (match
     Scopes.of_facts
       (facts_of witness)
       ~overrides:[ identity "FnDef"; identity "Let"; identity "Ty" ]
   with
   | Error findings ->
     Law.fail
       "(o) the witness overrides were rejected: %s"
       (String.concat ", " (List.map Scopes.finding_to_string findings))
   | Ok scopes ->
     (match Textmate.generate scopes ~language:"witness" () with
      | Error problems ->
        List.iter
          (fun p -> Law.fail "(o) a declaration: %a" Textmate.Check.pp_problem p)
          problems
      | Ok text ->
        let document = Yojson.Basic.from_string text in
        (match field "repository" document with
         | Some (`Assoc fields) ->
           (match List.assoc_opt "fn_def" fields with
            | None -> Law.fail "(o) the witness emitted no entry for fn_def"
            | Some entry ->
              if field "begin" entry = None
              then Law.fail "(o) fn_def opens on def and ends on }, and emitted no region";
              let in_begin =
                match field "beginCaptures" entry with
                | Some (`Assoc captures) ->
                  List.exists (fun (_, capture) -> named_function capture) captures
                | _ -> false
              in
              if not in_begin then Law.fail "(o) fn_def does not name itself in its begin");
           (match field "patterns" document with
            | None -> Law.fail "(o) the witness emitted no root patterns"
            | Some (`List root) ->
              let paired =
                List.exists
                  (fun pattern ->
                     match field "match" pattern, field "captures" pattern with
                     | Some (`String regex), Some (`Assoc captures) ->
                       let lead = {|(\btype\b)|} in
                       String.length regex > String.length lead
                       && String.sub regex 0 (String.length lead) = lead
                       &&
                         (match List.assoc_opt "2" captures with
                         | Some capture -> named_function capture
                         | None -> false)
                     | _ -> false)
                  root
              in
              if not paired
              then Law.fail "(o) ty does not name itself in a pair with type at the root"
            | Some _ -> Law.fail "(o) the root patterns are not a list");
           let fence = {|(?!(?:def|open|pub|static|type)\b)|} in
           List.iter
             (fun (key, entry) ->
                List.iter
                  (fun pattern ->
                     (match field "match" pattern with
                      | Some (`String regex)
                        when key <> "tokens"
                             && String.ends_with ~suffix:"[a-z]+" regex
                             && not (String.equal regex (fence ^ "[a-z]+")) ->
                        Law.fail
                          "(o) %s matches %s, which is not fenced off the keywords"
                          key
                          regex
                      | _ -> ());
                     if field "match" pattern <> None && named_function pattern
                     then
                       Law.fail
                         "(o) %s carries a bare pattern scoped %s, which every lowercase \
                          word it reaches would take"
                         key
                         function_name)
                  (patterns_in entry))
             fields
         | _ -> Law.fail "(o) the witness emitted no repository")));
  if Law.failures () = before
  then
    Law.pass
      "(o) a declaration names itself once: in a region's begin, in a pair, or not at all"
;;

(* -- (q) a clash is found wherever the engine meets one ------------------ *)

(* The corpus reaches two clashes, json's key and wide's table name, and both
   are found by more than one path. Each witness here has one path to its
   clash, so a part of {!Textmate.Clash} that stops seeing it changes what is
   emitted.

   A block ahead of the claim: the body is an expression block and then
   [Tail]. Nothing in the block is a [word], and the grammar-wide token list
   is tried behind everything, so there is no clash and [Tail.name] needs no
   tie. The block's list once ended in that token list, which took every
   [word] in the body first.

   A block behind the claim: [Tail] first, then a block with [word] as an
   atom. The clash is with the atom.

   A symbol that holds an operator: [sym]'s language holds [+], and [+] is an
   infix operator of the block after it. Neither side has a literal beside
   it.

   A production that is also an atom: [Pair] is reached after [nest] in
   [Group], and as an atom of the block, where nothing literal leads into
   it. A pair after [nest] would leave the atom's occurrences to the bare
   pattern. [Pair] may start with a [!], so it opens on no name of its own
   and stays a list. *)
let () =
  let before = Law.failures () in
  let open Core.Grammar in
  let word = pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z')) in
  let int = pat "int" Redfa.Regex.(plus (range_char ~lo:'0' ~hi:'9')) in
  let child (rule : string) (child : string) (scope : Scopes.Scope.t) : Scopes.override =
    Scopes.Child
      { rule = Core.Grammar.Name.Rule.of_string rule
      ; child = Core.Grammar.Name.Child.of_string child
      ; scope
      }
  in
  let token (name : string) (scope : Scopes.Scope.t) : Scopes.override =
    Scopes.Token { token = Core.Grammar.Name.Token.of_string name; scope }
  in
  let generate (grammar : Core.Grammar.t) (overrides : Scopes.override list)
    : (string, Textmate.Check.problem list) result option
    =
    match Scopes.of_facts (facts_of grammar) ~overrides with
    | Error _ -> None
    | Ok scopes -> Some (Textmate.generate scopes ~language:"witness" ())
  in
  let brackets = [ punct_tight ~name:"lbrack" "["; punct_tight ~name:"rbrack" "]" ] in
  let tail =
    prod
      "Tail"
      [ child_req "kw" (Token "as")
      ; child_req "name" (Token "word")
      ; child_opt "bang" (Token "bang")
      ]
  in
  let tail_tokens = [ kw "as"; punct ~name:"bang" "!" ] in
  let tail_scopes =
    [ token "word" Scopes.Scope.Variable_other
    ; child "Tail" "name" (Scopes.Scope.Entity_name_type None)
    ]
  in
  (* The body's first pattern is the pair, scoped as a type after [as]. *)
  let paired
        (what : string)
        (result : (string, Textmate.Check.problem list) result option)
    : unit
    =
    match result with
    | None -> Law.fail "(q) %s: the overrides were rejected" what
    | Some (Error problems) ->
      List.iter (fun p -> Law.fail "(q) %s: %a" what Textmate.Check.pp_problem p) problems
    | Some (Ok text) ->
      let document = Yojson.Basic.from_string text in
      (* [Body] is a region of its own where it opens on [as], and its list
         is then where the tie stands. *)
      let first =
        match field "repository" document with
        | Some (`Assoc fields) ->
          (match List.assoc_opt "body" fields, List.assoc_opt "file" fields with
           | Some entry, _ | None, Some entry ->
             (match field "patterns" entry with
              | Some (`List (first :: _)) -> Some first
              | _ -> None)
           | None, None -> None)
        | _ -> None
      in
      let is_pair =
        match first with
        | Some pattern ->
          (match field "match" pattern, field "captures" pattern with
           | Some (`String regex), Some (`Assoc captures) ->
             let lead = {|(\bas\b)|} in
             String.length regex > String.length lead
             && String.sub regex 0 (String.length lead) = lead
             && List.assoc_opt "2" captures
                = Some (`Assoc [ "name", `String "entity.name.type.witness" ])
           | _ -> false)
        | None -> false
      in
      if not is_pair
      then
        Law.fail
          "(q) %s: the body does not open with the pair that scopes Tail.name after as"
          what
  in
  (* The claim is scoped by a pattern of its own, and no tie is emitted. *)
  let untied
        (what : string)
        (result : (string, Textmate.Check.problem list) result option)
    : unit
    =
    match result with
    | None -> Law.fail "(q) %s: the overrides were rejected" what
    | Some (Error problems) ->
      List.iter (fun p -> Law.fail "(q) %s: %a" what Textmate.Check.pp_problem p) problems
    | Some (Ok text) ->
      let scoped : Yojson.Basic.t =
        `Assoc [ "name", `String "entity.name.type.witness" ]
      in
      let rec count (json : Yojson.Basic.t) : int * int =
        match json with
        | `Assoc fields ->
          let own =
            if List.assoc_opt "name" fields = Some (`String "entity.name.type.witness")
            then 1
            else 0
          in
          List.fold_left
            (fun (bare, tied) (key, value) ->
               if key = "captures"
               then (
                 match value with
                 | `Assoc groups ->
                   ( bare
                   , tied
                     + List.length (List.filter (fun (_, group) -> group = scoped) groups)
                   )
                 | _ -> bare, tied)
               else (
                 let more_bare, more_tied = count value in
                 bare + more_bare, tied + more_tied))
            (own, 0)
            fields
        | `List items ->
          List.fold_left
            (fun (bare, tied) item ->
               let more_bare, more_tied = count item in
               bare + more_bare, tied + more_tied)
            (0, 0)
            items
        | _ -> 0, 0
      in
      let bare, tied = count (Yojson.Basic.from_string text) in
      if bare <> 1 || tied <> 0
      then
        Law.fail
          "(q) %s: Tail.name is scoped by %d patterns of its own and %d ties"
          what
          bare
          tied
  in
  let rejected
        (what : string)
        (result : (string, Textmate.Check.problem list) result option)
    : unit
    =
    match result with
    | None -> Law.fail "(q) %s: the overrides were rejected" what
    | Some (Ok _) -> Law.fail "(q) %s: the clash was not reported" what
    | Some (Error problems) ->
      if
        not
          (List.exists
             (function
               | Textmate.Check.Unanchored_scope _ -> true
               | _ -> false)
             problems)
      then
        Law.fail
          "(q) %s: reported %s"
          what
          (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
  in
  untied
    "a block ahead of the claim"
    (generate
       (create
          ~expr:
            [ expr_block
                ~rule_name:"Expr"
                ~atoms:[ Token "int" ]
                ~infix_ops:[ infix ~token:"plus" ~bp:10 () ]
                ()
            ]
          ~tokens:(brackets @ tail_tokens @ [ punct ~name:"plus" "+"; word; int ])
          ~roots:[ "File" ]
          [ prod "File" [ child_req "b" (Rule "Body") ]
            |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
          ; prod "Body" [ child_req "e" (Rule "Expr"); child_req "t" (Rule "Tail") ]
          ; tail
          ])
       tail_scopes);
  paired
    "a block behind the claim"
    (generate
       (create
          ~expr:[ expr_block ~rule_name:"Expr" ~atoms:[ Token "word" ] () ]
          ~tokens:(brackets @ tail_tokens @ [ word ])
          ~roots:[ "File" ]
          [ prod "File" [ child_req "b" (Rule "Body") ]
            |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
          ; prod "Body" [ child_req "t" (Rule "Tail"); child_req "e" (Rule "Expr") ]
          ; tail
          ])
       tail_scopes);
  rejected
    "a symbol that holds an operator"
    (generate
       (create
          ~expr:
            [ expr_block
                ~rule_name:"Expr"
                ~atoms:[ Token "int" ]
                ~infix_ops:[ infix ~token:"plus" ~bp:10 () ]
                ()
            ]
          ~tokens:
            (brackets
             @ [ punct ~name:"plus" "+"
               ; punct ~name:"bang" "!"
               ; pat "sym" Redfa.Regex.(plus (chars_of_char_list [ 'a'; 'b'; '+' ]))
               ; int
               ])
          ~roots:[ "File" ]
          [ prod "File" [ child_req "b" (Rule "Body") ]
            |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
          ; prod "Body" [ child_req "i" (Rule "Item"); child_req "e" (Rule "Expr") ]
          ; prod "Item" [ child_req "s" (Token "sym"); child_opt "bang" (Token "bang") ]
          ])
       [ token "plus" Scopes.Scope.Keyword_operator_arithmetic
       ; child "Item" "s" Scopes.Scope.Variable_other_member
       ]);
  rejected
    "a production that is also an atom"
    (generate
       (create
          ~expr:[ expr_block ~rule_name:"Expr" ~atoms:[ Rule "Pair"; Token "int" ] () ]
          ~tokens:(brackets @ [ kw "nest"; punct ~name:"bang" "!"; word; int ])
          ~roots:[ "File" ]
          [ prod "File" [ child_req "e" (Rule "Expr") ]
            |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
          ; prod
              "Pair"
              [ child_opt "bang" (Token "bang")
              ; child_req "key" (Token "word")
              ; child_req "value" (Rule "Value")
              ]
          ; prod
              "Value"
              [ child_alt ~modifier:Exactly_one "v" [ Token "word"; Rule "Group" ] ]
          ; prod
              "Group"
              [ child_req "kw" (Token "nest"); child_req "inner" (Rule "Pair") ]
          ])
       [ child "Pair" "key" Scopes.Scope.Variable_other_member ]);
  if Law.failures () = before
  then
    Law.pass
      "(q) a clash is found behind a block, ahead of one, against an operator, and in an \
       atom"
;;

(* -- (r) a region that cannot tell two children apart is cut -------------- *)

(* What the witnesses share. [Head], [Cond] and [Last] are three matched pairs
   on one bracket, and [Body] is a pair on another. Each witness is one
   committed production over them, which is a region: it opens on a keyword
   and ends behind its last child. *)
type cut =
  | Cut of
      { first_end : string (** The [end] of the first part. *)
      ; first : string list (** What the first part includes. *)
      ; rest : string list (** What the outer region includes after it. *)
      ; named : bool * bool
        (** Whether the first part's list scopes a type, and whether the
            outer one does. *)
      }
  | Refused of string * (Textmate.Check.problem -> bool)

let () =
  let before = Law.failures () in
  let open Core.Grammar in
  let tokens =
    [ kw "go"
    ; kw "do"
    ; kw "while"
    ; kw "try"
    ; kw "catch"
    ; kw "then"
    ; kw "sep"
    ; kw "end"
    ; punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct_tight ~name:"lbrace" "{"
    ; punct_tight ~name:"rbrace" "}"
    ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
    ; pat ~trivia:Reformat "ws" Redfa.Regex.(plus (singleton_char ' '))
    ]
  in
  let pairs =
    [ ( "Head"
      , prod "Head" [ child_req "word" (Token "word") ]
        |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen" )
    ; ( "Cond"
      , prod "Cond" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen" )
    ; ( "Last"
      , prod "Last" [ child_opt "word" (Token "word") ]
        |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen" )
    ; ( "Body"
      , prod "Body" [ child_rep "word" (Token "word") ]
        |> with_delimited ~open_tok:"lbrace" ~close_tok:"rbrace" )
    ; "Word", prod "Word" [ child_req "word" (Token "word") ]
    ]
  in
  let keyword (text : string) : string * symbol =
    let width = String.length text in
    if width > 0 && text.[width - 1] = '?'
    then (
      let text = String.sub text 0 (width - 1) in
      text ^ "_kw?", Token text)
    else text ^ "_kw", Token text
  in
  let pair (name : string) (rule : string) : string * symbol = name, Rule rule in
  let includes (patterns : Yojson.Basic.t list) : string list =
    List.filter_map
      (fun pattern ->
         match field "include" pattern with
         | Some (`String key) when key <> "#trivia" && key <> "#tokens" -> Some key
         | _ -> None)
      patterns
  in
  let patterns_of (json : Yojson.Basic.t) : Yojson.Basic.t list =
    match field "patterns" json with
    | Some (`List patterns) -> patterns
    | _ -> []
  in
  let text_of (key : string) (json : Yojson.Basic.t) : string =
    match field key json with
    | Some (`String text) -> text
    | _ -> ""
  in
  (* A child whose name ends in [?] may be absent. *)
  let child_of (index : int) ((name, symbol) : string * symbol) : child =
    let width = String.length name in
    if width > 0 && name.[width - 1] = '?'
    then child_opt (Printf.sprintf "%s_%d" (String.sub name 0 (width - 1)) index) symbol
    else child_req (Printf.sprintf "%s_%d" name index) symbol
  in
  let type_name : Yojson.Basic.t = `String "entity.name.type.witness" in
  let ties (patterns : Yojson.Basic.t list) : bool =
    List.exists (fun pattern -> field "name" pattern = Some type_name) patterns
  in
  let check
        ?(overrides = [])
        (what : string)
        (children : (string * symbol) list)
        (expected : cut)
    : unit
    =
    let used (rule : string) : bool =
      List.exists
        (fun (_, symbol) ->
           match symbol with
           | Rule name -> String.equal name rule
           | Token _ -> false)
        children
    in
    let rule = prod "Stmt" (List.mapi child_of children) |> with_committed in
    let grammar =
      create
        ~tokens
        ~roots:[ "File" ]
        (prod "File" [ child_rep "stmt" (Rule "Stmt") ]
         :: rule
         :: List.filter_map
              (fun (name, pair) -> if used name then Some pair else None)
              pairs)
    in
    let scopes =
      match Scopes.of_facts (facts_of grammar) ~overrides with
      | Ok scopes -> scopes
      | Error findings ->
        failwith (String.concat ", " (List.map Scopes.finding_to_string findings))
    in
    match Textmate.generate scopes ~language:"witness" (), expected with
    | Error problems, Refused (_, owed) ->
      if not (List.exists owed problems)
      then
        Law.fail
          "(r) %s: refused, and for %s"
          what
          (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
    | Error problems, Cut _ ->
      Law.fail
        "(r) %s: refused for %s"
        what
        (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
    | Ok _, Refused (why, _) -> Law.fail "(r) %s: accepted, and %s" what why
    | Ok text, Cut { first_end; first; rest; named } ->
      let entry =
        match field "repository" (Yojson.Basic.from_string text) with
        | Some repository -> Option.value (field "stmt" repository) ~default:`Null
        | None -> `Null
      in
      (match patterns_of entry with
       | part :: outer ->
         (* The first part opens once, where the region does. Anywhere later
            it would open again on its own keyword. *)
         let opens = text_of "begin" part in
         let once = {|\G|} in
         let opens_once =
           String.length opens >= String.length once
           && String.equal (String.sub opens 0 (String.length once)) once
         in
         let keyword =
           if opens_once
           then
             String.sub
               opens
               (String.length once)
               (String.length opens - String.length once)
           else opens
         in
         if
           not (opens_once && String.equal (text_of "begin" entry) ("(?=" ^ keyword ^ ")"))
         then
           Law.fail
             "(r) %s: the region opens on %S and its first part on %S"
             what
             (text_of "begin" entry)
             opens;
         let guard = {|(?!\G)(?<=|} in
         let ends = text_of "end" entry in
         if
           not
             (String.length ends >= String.length guard
              && String.equal (String.sub ends 0 (String.length guard)) guard)
         then
           Law.fail
             "(r) %s: the region ends on %S, which it can do where it opened"
             what
             ends;
         if not (String.equal (text_of "end" part) first_end)
         then
           Law.fail
             "(r) %s: the first part ends on %S and not %S"
             what
             (text_of "end" part)
             first_end;
         if includes (patterns_of part) <> first
         then
           Law.fail
             "(r) %s: the first part holds %s"
             what
             (String.concat " " (includes (patterns_of part)));
         if includes outer <> rest
         then
           Law.fail
             "(r) %s: what follows holds %s"
             what
             (String.concat " " (includes outer));
         if (ties (patterns_of part), ties outer) <> named
         then
           Law.fail
             "(r) %s: a type is scoped in the first part's list (%b) and in what follows \
              (%b)"
             what
             (ties (patterns_of part))
             (ties outer);
         (* A literal the first part ends by taking keeps the scope it has. *)
         if
           String.length first_end > 0
           && first_end.[0] <> '('
           && field "endCaptures" part = None
         then Law.fail "(r) %s: the literal that ends the first part has no scope" what
       | [] -> Law.fail "(r) %s: the region has no patterns" what)
  in
  let closes_early : string * (Textmate.Check.problem -> bool) =
    ( "its region ends behind an earlier child"
    , function
      | Textmate.Check.Early_close _ -> true
      | _ -> false )
  in
  let shares : string * (Textmate.Check.problem -> bool) =
    ( "two of its pairs share a list"
    , function
      | Textmate.Check.Shared_opener _ -> true
      | _ -> false )
  in
  let refused ((why, owed) : string * (Textmate.Check.problem -> bool)) : cut =
    Refused (why, owed)
  in
  (* The three shapes the engine was run on. A cut is taken at the first child
     that can carry one, so [do] is cut behind its body and not at [while]. *)
  check
    "do (head) { } while (cond)"
    [ keyword "do"
    ; pair "head" "Head"
    ; pair "body" "Body"
    ; keyword "while"
    ; pair "cond" "Cond"
    ]
    (Cut
       { first_end = {|(?<=\})|}
       ; first = [ "#head"; "#body" ]
       ; rest = [ "#cond" ]
       ; named = false, false
       });
  check
    "while (head) (cond) { }"
    [ keyword "while"; pair "head" "Head"; pair "cond" "Cond"; pair "body" "Body" ]
    (Cut
       { first_end = {|(?<=\))|}
       ; first = [ "#head" ]
       ; rest = [ "#cond"; "#body" ]
       ; named = false, false
       });
  check
    "try { } catch (e) { }"
    [ keyword "try"
    ; pair "body" "Body"
    ; keyword "catch"
    ; pair "param" "Head"
    ; pair "handler" "Body"
    ]
    (Cut
       { first_end = {|\bcatch\b|}
       ; first = [ "#body" ]
       ; rest = [ "#head"; "#body" ]
       ; named = false, false
       });
  (* Behind a pair: the outer region must not end where the first part does. *)
  check
    "two of one pair with nothing between"
    [ keyword "go"; pair "one" "Head"; pair "two" "Head" ]
    (refused closes_early);
  (* Behind a pair: the first part must have nothing to tell apart itself,
     and neither must what follows. Three pairs on one bracket take two
     cuts. *)
  check
    "three pairs on one bracket"
    [ keyword "go"
    ; pair "one" "Head"
    ; pair "two" "Cond"
    ; pair "three" "Last"
    ; pair "body" "Body"
    ]
    (refused shares);
  (* At a literal: no cut in front of a token. A token beside it may be tied
     to it, and the first part's end would take the literal first. The token
     may be absent here, so it carries no cut of its own. *)
  check
    "a literal with a token after it"
    [ keyword "go"
    ; pair "body" "Body"
    ; keyword "then"
    ; "name?", Token "word"
    ; pair "last" "Body"
    ]
    (refused closes_early);
  (* A name that is always there carries the cut itself: the first part ends
     by taking it. *)
  check
    "a name behind a literal"
    [ keyword "go"
    ; pair "body" "Body"
    ; keyword "then"
    ; "name", Token "word"
    ; pair "last" "Body"
    ]
    (Cut
       { first_end = {|(?!(?:catch|do|end|go|sep|then|try|while)\b)[a-z]+|}
       ; first = [ "#body" ]
       ; rest = [ "#body" ]
       ; named = false, false
       });
  (* At a literal: the first part ends at the first place the literal stands,
     so a second one is no cut of its own. *)
  check
    "a literal that stands twice"
    [ keyword "go"
    ; pair "body" "Body"
    ; keyword "sep"
    ; "name?", Token "word"
    ; keyword "sep"
    ; pair "last" "Body"
    ]
    (refused closes_early);
  (* At a literal: one the region's own end looks behind for would end the
     region with the first part. *)
  check
    "a literal the region ends behind"
    [ keyword "go"; pair "body" "Body"; keyword "end"; pair "more" "Body"; keyword "end" ]
    (refused closes_early);
  (* At a literal: the first part must have nothing to tell apart itself. *)
  check
    "two pairs on one bracket in front of a literal"
    [ keyword "go"
    ; pair "one" "Head"
    ; pair "two" "Cond"
    ; keyword "then"
    ; pair "body" "Body"
    ; pair "last" "Last"
    ]
    (refused shares);
  (* At a literal: what follows must have nothing to tell apart, so this one
     is passed over for the pair after it. *)
  check
    "a literal with two pairs on one bracket after it"
    [ keyword "go"
    ; pair "body" "Body"
    ; keyword "then"
    ; pair "one" "Head"
    ; pair "two" "Cond"
    ; pair "last" "Body"
    ]
    (Cut
       { first_end = {|(?<=\))|}
       ; first = [ "#body"; "#head" ]
       ; rest = [ "#cond"; "#body" ]
       ; named = false, false
       });
  (* A child that may be absent carries no cut: with it absent the first part
     would run on to whatever closes next. So the cut is passed on to the pair
     after it. *)
  check
    "a literal that may be absent"
    [ keyword "go"
    ; pair "body" "Body"
    ; keyword "sep?"
    ; pair "head" "Head"
    ; pair "last" "Body"
    ]
    (Cut
       { first_end = {|(?<=\))|}
       ; first = [ "#body"; "#head" ]
       ; rest = [ "#body" ]
       ; named = false, false
       });
  (* A token scoped where it stands is tried on its own side of the cut and
     nowhere else. [name] is a type here and a variable in the token list. *)
  let type_scopes (name : string) : Scopes.override list =
    [ Scopes.Token
        { token = Core.Grammar.Name.Token.of_string "word"
        ; scope = Scopes.Scope.Variable_other
        }
    ; Scopes.Child
        { rule = Core.Grammar.Name.Rule.of_string "Stmt"
        ; child = Core.Grammar.Name.Child.of_string name
        ; scope = Scopes.Scope.Entity_name_type None
        }
    ]
  in
  check
    ~overrides:(type_scopes "name_2")
    "a scoped token in the first part"
    [ keyword "go"
    ; pair "head" "Head"
    ; "name?", Token "word"
    ; keyword "then"
    ; pair "cond" "Cond"
    ]
    (Cut
       { first_end = {|\bthen\b|}
       ; first = [ "#head" ]
       ; rest = [ "#cond" ]
       ; named = true, false
       });
  check
    ~overrides:(type_scopes "name_4")
    "a scoped token in what follows"
    [ keyword "go"
    ; pair "head" "Head"
    ; keyword "then"
    ; pair "cond" "Cond"
    ; "name", Token "word"
    ; keyword "end"
    ]
    (Cut
       { first_end = {|(?<=\))|}
       ; first = [ "#head" ]
       ; rest = [ "#cond" ]
       ; named = false, true
       });
  (* A position tied to the literal beside it goes at the front of both lists.
     [name] and [other] hold the same token in two scopes, so [name] is tied to
     the [sep] after it, and a pair is tried wherever the region is. *)
  check
    ~overrides:(type_scopes "name_1")
    "a position tied to a literal"
    [ keyword "go"
    ; "name", Token "word"
    ; keyword "sep"
    ; "other", Token "word"
    ; pair "head" "Head"
    ; keyword "then"
    ; pair "cond" "Cond"
    ]
    (Cut
       { first_end = {|\bthen\b|}
       ; first = [ "#head" ]
       ; rest = [ "#cond" ]
       ; named = true, true
       });
  (* The same behind the cut. Each side of a cut is a list of its own, and a
     tie found in either is tried in both. [Word] holds the other name here,
     so that [name] carries no cut of its own. *)
  check
    ~overrides:(type_scopes "name_5")
    "a position tied to a literal behind the cut"
    [ keyword "go"
    ; pair "head" "Head"
    ; keyword "then"
    ; pair "cond" "Cond"
    ; pair "other" "Word"
    ; "name", Token "word"
    ; keyword "sep"
    ; keyword "end"
    ]
    (Cut
       { first_end = {|(?<=\))|}
       ; first = [ "#head" ]
       ; rest = [ "#cond"; "#word" ]
       ; named = true, true
       });
  if Law.failures () = before
  then
    Law.pass
      "(r) a region is cut where a cut parts its children, and refused where none does"
;;

(* -- (s) each token of a list has one thing that opens on it -------------- *)

(* What the witnesses share. [Tuple], [Group] and [Trio] are three matched
   pairs on a round bracket, [Set] and [Body] two on a brace. [Left] reaches
   the first of each and [Right] the second, and neither holds one as a child
   of its own. So no rule here has two pairs of its own on one opener, and
   part (r) has nothing to say about any of them.

   Each witness names the entry it reads and writes it out whole: what the
   region opens on, what it ends on, and what its list holds, with each part
   of a cut in angle brackets. A plain list is in square brackets alone. *)
let () =
  let before = Law.failures () in
  let open Core.Grammar in
  let tokens =
    [ kw "go"
    ; kw "val"
    ; kw "if"
    ; kw "else"
    ; kw "with"
    ; kw "at"
    ; kw "end"
    ; kw "fn"
    ; kw "self"
    ; punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct_tight ~name:"lbrace" "{"
    ; punct_tight ~name:"rbrace" "}"
    ; punct ~space_before:Space ~name:"lbrack" "["
    ; punct_tight ~name:"rbrack" "]"
    ; punct ~name:"eq" "="
    ; punct ~name:"arrow" "=>"
    ; punct ~name:"semi" ";"
    ; punct ~name:"comma" ","
    ; punct ~name:"bang" "!"
    ; pat "word" Redfa.Regex.(plus (range_char ~lo:'a' ~hi:'z'))
    ; pat ~trivia:Reformat "ws" Redfa.Regex.(plus (singleton_char ' '))
    ]
  in
  let name = {|(?!(?:at|else|end|fn|go|if|self|val|with)\b)[a-z]+|} in
  let pair (name : string) ~(open_tok : string) ~(close_tok : string) : production =
    prod name [ child_rep "word" (Token "word") ] |> with_delimited ~open_tok ~close_tok
  in
  let tuple = pair "Tuple" ~open_tok:"lparen" ~close_tok:"rparen" in
  let group = pair "Group" ~open_tok:"lparen" ~close_tok:"rparen" in
  let trio = pair "Trio" ~open_tok:"lparen" ~close_tok:"rparen" in
  let set = pair "Set" ~open_tok:"lbrace" ~close_tok:"rbrace" in
  let body = pair "Body" ~open_tok:"lbrace" ~close_tok:"rbrace" in
  let either (name : string) (rules : string list) : production =
    prod name [ child_alt_rules ~modifier:Exactly_one "kind" rules ]
  in
  let left = either "Left" [ "Tuple"; "Set" ] in
  let right = either "Right" [ "Group"; "Body" ] in
  let pairs = [ tuple; group; set; body ] in
  (* A list that leads with a name and can be no region: it holds one
     child. *)
  let lead = prod "Lead" [ child_req "name" (Token "word") ] in
  let file (rule : string) : production = prod "File" [ child_rep "stmt" (Rule rule) ] in
  let document
        ?(expr = [])
        ?(roots = [ "File" ])
        ?(overrides = [])
        (what : string)
        (rules : production list)
    : Yojson.Basic.t
    =
    let scopes =
      match Scopes.of_facts (facts_of (create ~expr ~tokens ~roots rules)) ~overrides with
      | Ok scopes -> scopes
      | Error findings ->
        failwith (String.concat ", " (List.map Scopes.finding_to_string findings))
    in
    match Textmate.generate scopes ~language:"witness" () with
    | Error problems ->
      Law.fail
        "(s) %s: refused for %s"
        what
        (String.concat ", " (List.map Textmate.Check.problem_to_string problems));
      `Null
    | Ok text -> Yojson.Basic.from_string text
  in
  let entries_of (json : Yojson.Basic.t) : (string * Yojson.Basic.t) list =
    match field "repository" json with
    | Some (`Assoc entries) -> entries
    | _ -> []
  in
  let repository ?expr ?roots (what : string) (rules : production list)
    : (string * Yojson.Basic.t) list
    =
    entries_of (document ?expr ?roots what rules)
  in
  let text_of (key : string) (json : Yojson.Basic.t) : string =
    match field key json with
    | Some (`String text) -> text
    | _ -> ""
  in
  let patterns_of (json : Yojson.Basic.t) : Yojson.Basic.t list =
    match field "patterns" json with
    | Some (`List patterns) -> patterns
    | _ -> []
  in
  let holds (part : string) (text : string) : bool =
    let width = String.length part in
    let rec at (i : int) : bool =
      i + width <= String.length text
      && (String.equal (String.sub text i width) part || at (i + 1))
    in
    at 0
  in
  let entry (what : string) (key : string) (entries : (string * Yojson.Basic.t) list)
    : Yojson.Basic.t
    =
    match List.assoc_opt key entries with
    | Some json -> json
    | None ->
      if entries <> [] then Law.fail "(s) %s: no entry %S" what key;
      `Null
  in
  let rec describe (json : Yojson.Basic.t) : string =
    let inner =
      List.map
        (fun pattern ->
           match
             field "include" pattern, field "begin" pattern, field "match" pattern
           with
           | Some (`String key), _, _ -> key
           | _, Some _, _ -> "<" ^ describe pattern ^ ">"
           | _, None, Some _ -> "/" ^ text_of "match" pattern ^ "/"
           | _, None, None -> "?")
        (patterns_of json)
      |> List.filter (fun key -> key <> "#trivia" && key <> "#tokens")
      |> String.concat " "
    in
    match field "begin" json with
    | Some _ ->
      Printf.sprintf "%s..%s[%s]" (text_of "begin" json) (text_of "end" json) inner
    | None -> Printf.sprintf "[%s]" inner
  in
  let expect (what : string) (json : Yojson.Basic.t) (wanted : string) : unit =
    if json <> `Null && not (String.equal (describe json) wanted)
    then Law.fail "(s) %s:\n  wanted %s\n  got    %s" what wanted (describe json)
  in
  (* The backend refuses the grammar, for a list that gives one token to two
     things. *)
  let refuses ?(roots = [ "File" ]) (what : string) (rules : production list) : unit =
    match
      Textmate.generate
        (scopes_of (facts_of (create ~tokens ~roots rules)))
        ~language:"witness"
        ()
    with
    | Ok _ -> Law.fail "(s) %s: accepted" what
    | Error problems ->
      if
        not
          (List.exists
             (function
               | Textmate.Check.Undecided _ -> true
               | _ -> false)
             problems)
      then
        Law.fail
          "(s) %s: refused for %s"
          what
          (String.concat ", " (List.map Textmate.Check.problem_to_string problems))
  in
  let refused = "refused" in
  let reads (what : string) (key : string) (rules : production list) (wanted : string)
    : unit
    =
    if String.equal wanted refused
    then refuses what rules
    else expect what (entry what key (repository what rules)) wanted
  in
  (* -- a region ends behind what its last child always ends on --

     [Rest] is another [Stmt] or a [Body], and both end on a brace. So [Stmt]
     ends behind one, and is a region. Its [Body] ends on a brace too, which
     takes the cut part (r) describes. *)
  let chain (rest : string list) : production list =
    [ file "Stmt"
    ; prod
        "Stmt"
        [ child_req "kw" (Token "if")
        ; child_req "cond" (Rule "Tuple")
        ; child_req "then" (Rule "Body")
        ; child_req "else" (Token "else")
        ; child_req "rest" (Rule "Rest")
        ]
    ; either "Rest" rest
    ; tuple
    ; body
    ]
  in
  reads
    "a last child that ends on one text whichever it is"
    "stmt"
    (chain [ "Stmt"; "Body" ])
    {|(?=\bif\b)..(?!\G)(?<=\})[<\G\bif\b..\belse\b[#tuple #body]> #rest]|};
  (* An alternative that is the rule again ends as the rule does, so it agrees
     with whatever the others end on. Here that is a round bracket, and the
     cut moves to the body. *)
  reads
    "a last child that is the rule again or ends on one text"
    "stmt"
    (chain [ "Stmt"; "Tuple" ])
    {|(?=\bif\b)..(?!\G)(?<=\))[<\G\bif\b..(?<=\})[#tuple #body]> #rest]|};
  (* Two alternatives that end on two texts, and there is nothing to look
     behind for. The rule still ends in front of what follows it, which here
     is the next [if]. *)
  reads
    "a last child that ends on one of two texts"
    "stmt"
    (chain [ "Tuple"; "Body" ])
    {|\bif\b..(?=\bif\b)[#tuple #body #rest]|};
  (* -- a region that ends behind a word ends behind the whole of it -- *)
  reads
    "a region that ends behind a keyword"
    "stmt"
    [ file "Stmt"
    ; prod
        "Stmt"
        [ child_req "kw" (Token "go")
        ; child_req "body" (Rule "Body")
        ; child_req "end" (Token "end")
        ]
      |> with_committed
    ; body
    ]
    {|\bgo\b..(?<=\bend)\b[#body]|};
  (* -- a region is cut where two takers on one token stand on two sides --

     One list over [Left] and [Right] reads every round bracket as [Left]'s
     pair. Cut at the literal between them, each side reads its own. *)
  let assign (children : child list) (rules : production list) : production list =
    [ file "Stmt"
    ; prod
        "Stmt"
        ((child_req "kw" (Token "go") :: children) @ [ child_req "semi" (Token "semi") ])
      |> with_committed
    ]
    @ rules
  in
  let sides (left : production) (right : production) : production list =
    assign
      [ child_req "left" (Rule "Left")
      ; child_req "eq" (Token "eq")
      ; child_req "right" (Rule "Right")
      ]
      ([ left; right ] @ pairs)
  in
  reads
    "two children that reach different pairs on one bracket"
    "stmt"
    (sides left right)
    {|(?=\bgo\b)..(?!\G)(?<=;)[<\G\bgo\b..=(?!>)[#left]> #right]|};
  (* Both sides reach the same pairs, so a cut would change nothing. *)
  reads
    "two children that reach the same pairs"
    "stmt"
    (sides left (either "Right" [ "Tuple"; "Set" ]))
    {|\bgo\b..(?<=;)[#left #right]|};
  (* The literal also stands loose in what the first part would hold, and the
     part would end there. So there is no cut, the two brackets stay in one
     list, and the grammar is refused. *)
  reads
    "a literal that also stands loose before the cut"
    "stmt"
    (sides
       (prod
          "Left"
          [ child_opt "bang" (Token "bang")
          ; child_req "one" (Rule "Tuple")
          ; child_req "eq" (Token "eq")
          ; child_req "name" (Token "word")
          ])
       right)
    refused;
  (* A child that holds a pair of its own is tried ahead of the rest, so here
     it is [Left]'s bracket that one list reads as the other's. *)
  reads
    "a later child that the list tries first"
    "stmt"
    (assign
       [ child_req "left" (Rule "Left")
       ; child_req "eq" (Token "eq")
       ; child_req "group" (Rule "Group")
       ]
       (left :: pairs))
    {|(?=\bgo\b)..(?!\G)(?<=;)[<\G\bgo\b..=(?!>)[#left]> #group]|};
  (* A literal in front of a list that leads with a token may be tied to that
     token, so it carries no cut, and the grammar is refused. *)
  reads
    "a literal in front of a list that leads with a token"
    "stmt"
    (assign
       [ child_req "left" (Rule "Left")
       ; child_req "eq" (Token "eq")
       ; child_req "lead" (Rule "Lead")
       ; child_req "group" (Rule "Group")
       ]
       ([ left; lead ] @ pairs))
    refused;
  (* A cut behind a child that ends on one text, which need be no pair. *)
  reads
    "a child that ends on a fixed text"
    "stmt"
    (assign
       [ child_req "head" (Rule "Head"); child_req "right" (Rule "Right") ]
       ([ prod
            "Head"
            [ child_opt "bang" (Token "bang")
            ; child_req "tuple" (Rule "Tuple")
            ; child_req "arrow" (Token "arrow")
            ]
        ; right
        ]
        @ pairs))
    {|(?=\bgo\b)..(?!\G)(?<=;)[<\G\bgo\b..(?<==>)[#head]> #right]|};
  (* Every cut worth taking is taken, each in what the one before it left:
     three pairs on one bracket are three lists. *)
  reads
    "three children that reach three pairs on one bracket"
    "stmt"
    (assign
       [ child_req "left" (Rule "Left")
       ; child_req "eq" (Token "eq")
       ; child_req "right" (Rule "Right")
       ; child_req "arrow" (Token "arrow")
       ; child_req "last" (Rule "Last")
       ]
       ([ left; right; either "Last" [ "Trio" ]; trio ] @ pairs))
    {|(?=\bgo\b)..(?!\G)(?<=;)[<\G..=>[<\G\bgo\b..=(?!>)[#left]> #right]> #trio]|};
  (* A name carries a cut as a literal does, where what follows takes the
     same name. The label is no operand, and the expression behind it starts
     with one. *)
  let words : expr_def list =
    [ expr_block
        ~rule_name:"Expr"
        ~atoms:[ Token "word"; Token "self"; Rule "Paren" ]
        ~postfix:
          [ postfix_call
              ~kind_suffix:"call"
              ~open_tok:"lparen"
              ~close_tok:"rparen"
              ~elem:(Rule "Expr")
              ~bp:90
              ()
          ; postfix_simple ~kind_suffix:"bang" ~token:"bang" ~bp:90 ()
          ]
        ()
    ]
  in
  let paren : production =
    prod "Paren" [ child_req "inner" (Rule "Expr") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  expect
    "a name in front of an expression"
    (entry
       "a name in front of an expression"
       "stmt"
       (repository
          ~expr:words
          "a name in front of an expression"
          (assign
             [ child_req "label" (Token "word"); child_req "value" (Rule "Expr") ]
             [ paren ])))
    (Printf.sprintf {|(?=\bgo\b)..(?!\G)(?<=;)[<\G\bgo\b..%s[/%s/]> #expr]|} name name);
  (* -- a rule that is a region is no forwarder --

     [Hold] forwarded to [Set] would put that brace beside [Body]'s in the
     list of [Stmt]. It holds the pair through [Wrap], which has no token of
     its own and forwards. [Name] is two tokens, which is one match and no
     region, so it forwards as well. *)
  let what = "a pair behind a literal" in
  let entries =
    repository
      what
      [ file "Stmt"
      ; prod
          "Stmt"
          [ child_req "kw" (Token "go")
          ; child_req "body" (Rule "Body")
          ; child_req "hold" (Rule "Hold")
          ; child_req "name" (Rule "Name")
          ; child_req "semi" (Token "semi")
          ]
        |> with_committed
      ; prod "Hold" [ child_req "kw" (Token "with"); child_req "wrap" (Rule "Wrap") ]
      ; prod "Wrap" [ child_req "set" (Rule "Set") ]
      ; prod "Name" [ child_req "kw" (Token "at"); child_req "leaf" (Rule "Leaf") ]
      ; prod "Leaf" [ child_req "word" (Token "word") ]
      ; body
      ; set
      ]
  in
  expect what (entry what "hold" entries) {|\bwith\b..(?<=\})[#set]|};
  expect what (entry what "stmt" entries) {|\bgo\b..(?<=;)[#body #hold #leaf]|};
  if List.mem_assoc "wrap" entries || List.mem_assoc "name" entries
  then Law.fail "(s) %s: a rule that is no region has an entry of its own" what;
  (* -- a rule with no fixed end ends in front of what follows it --

     [Val] ends on no text. What follows one is the next [val], so its
     region ends in front of that. *)
  let binding (children : child list) (rules : production list) : production list =
    [ file "Val"; prod "Val" (child_req "kw" (Token "val") :: children) ] @ rules
  in
  let bound (right : production) : production list =
    binding
      [ child_req "left" (Rule "Left")
      ; child_req "eq" (Token "eq")
      ; child_req "right" (Rule "Right")
      ]
      ([ left; right ] @ pairs)
  in
  reads
    "a rule that ends on nothing fixed"
    "val"
    (bound right)
    {|(?=\bval\b)..(?!\G)(?=\bval\b)[<\G\bval\b..=(?!>)[#left]> #right]|};
  reads
    "a rule whose two sides reach the same pairs"
    "val"
    (bound (either "Right" [ "Tuple"; "Set" ]))
    {|\bval\b..(?=\bval\b)[#left #right]|};
  (* What follows is a name, and there is no looking ahead for one: an
     expression inside the rule starts with one too. *)
  let what = "a rule that a name may follow" in
  let entries =
    repository
      what
      [ prod "File" [ child_rep "stmt" (Rule "Stmt") ]
      ; either "Stmt" [ "Val"; "Call" ]
      ; prod "Val" [ child_req "kw" (Token "val"); child_req "left" (Rule "Left") ]
      ; prod "Call" [ child_req "name" (Token "word"); child_req "set" (Rule "Set") ]
      ; left
      ; tuple
      ; set
      ]
  in
  if List.mem_assoc "val" entries && field "begin" (entry what "val" entries) <> None
  then Law.fail "(s) %s: the rule is a region" what;
  (* A token that follows the rule is one the rule holds. Cut at it, the end
     is tried only in the list of what comes after. *)
  reads
    "a rule that holds a token which also follows it"
    "fn"
    [ file "Stmt"
    ; prod
        "Stmt"
        [ child_req "kw" (Token "go")
        ; child_req "fn" (Rule "Fn")
        ; child_req "end" (Token "end")
        ]
    ; prod
        "Fn"
        [ child_req "kw" (Token "fn")
        ; child_req "tuple" (Rule "Tuple")
        ; child_req "end" (Token "end")
        ; child_req "right" (Rule "Right")
        ]
    ; either "Right" [ "Tuple"; "Body" ]
    ; tuple
    ; body
    ]
    {|(?=\bfn\b)..(?!\G)(?=\bend\b)[<\G\bfn\b..\bend\b[#tuple]> #right]|};
  (* A bracket follows the rule and the rule's own list takes that bracket,
     with nothing to cut at. It stays a list, and here that is a forwarder. *)
  let what = "a rule whose list takes what follows it" in
  let entries =
    repository
      what
      [ file "Stmt"
      ; prod
          "Stmt"
          [ child_req "kw" (Token "go")
          ; child_req "fn" (Rule "Fn")
          ; child_req "tuple" (Rule "Tuple")
          ; child_req "semi" (Token "semi")
          ]
      ; prod "Fn" [ child_req "kw" (Token "fn"); child_req "left" (Rule "Left") ]
      ; left
      ; tuple
      ; set
      ]
  in
  if List.mem_assoc "fn" entries
  then
    Law.fail "(s) %s: the rule has an entry, %s" what (describe (entry what "fn" entries));
  (* -- no grammar is refused for a region it did not have before --

     A name scoped for its position and tied to the literal in front of it
     needs that literal in its own list. A region would open on it, so the
     rule stays the list it was, and the grammar is still accepted. [name] is
     a type here and a variable in the token list, and [other] holds the same
     token. *)
  let type_scopes (rule : string) : Scopes.override list =
    [ Scopes.Token
        { token = Core.Grammar.Name.Token.of_string "word"
        ; scope = Scopes.Scope.Variable_other
        }
    ; Scopes.Child
        { rule = Core.Grammar.Name.Rule.of_string rule
        ; child = Core.Grammar.Name.Child.of_string "name"
        ; scope = Scopes.Scope.Entity_name_type None
        }
    ]
  in
  let what = "a rule whose opening literal a name is tied to" in
  let json =
    document
      ~overrides:(type_scopes "Lead")
      what
      (binding
         [ child_req "lead" (Rule "Lead")
         ; child_opt "other" (Token "word")
         ; child_req "eq" (Token "eq")
         ; child_req "right" (Rule "Right")
         ]
         ([ lead; right ] @ pairs))
  in
  expect what (entry what "val" (entries_of json)) "[#lead #right]";
  let what =
    "a rule that would end on a fixed text, whose opening literal a name is tied to"
  in
  let json =
    document
      ~overrides:(type_scopes "Lead")
      what
      [ file "Stmt"
      ; prod
          "Stmt"
          [ child_req "kw" (Token "if")
          ; child_req "lead" (Rule "Lead")
          ; child_opt "other" (Token "word")
          ; child_req "then" (Rule "Body")
          ; child_req "else" (Token "else")
          ; child_req "rest" (Rule "Rest")
          ]
      ; either "Rest" [ "Stmt"; "Body" ]
      ; lead
      ; body
      ]
  in
  expect what (entry what "stmt" (entries_of json)) "[#body #lead #rest]";
  (* Two children that hold one pair, in a rule that opens on that pair. As a
     region it would end behind the first, and there is no cut. *)
  let what = "a rule that opens on a pair and holds it twice" in
  let entries =
    repository
      what
      [ prod "File" [ child_req "one" (Rule "Tuple"); child_req "two" (Rule "Tuple") ]
      ; tuple
      ]
  in
  expect what (entry what "file" entries) "[#tuple #tuple]";
  (* -- a rule opens on its name where nothing else takes that name --

     [Named] starts with a name and holds a bracket behind it. A list would
     put that bracket beside [Tuple]'s. *)
  let named : production =
    prod "Named" [ child_req "name" (Token "word"); child_opt "group" (Rule "Group") ]
  in
  reads
    "a rule that starts with a name"
    "named"
    (assign
       [ child_alt_rules ~modifier:Exactly_one "kind" [ "Named"; "Tuple" ] ]
       [ named; tuple; group ])
    (name ^ {|..(?=;)[#group]|});
  (* The list that holds it takes the same name somewhere else, with nothing
     between the two to cut at, and every such name would open the rule. *)
  reads
    "a rule whose name something else takes"
    "named"
    (assign
       [ child_req "lead" (Rule "Lead"); child_req "named" (Rule "Named") ]
       [ prod
           "Named"
           [ child_req "name" (Token "word"); child_req "group" (Rule "Group") ]
       ; lead
       ; group
       ])
    ("[#group /" ^ name ^ "/]");
  (* -- a rule opens on what it starts with where every such token is a
        literal --

     [Head] starts with a pair and ends on an arrow. *)
  let head : production =
    prod "Head" [ child_req "tuple" (Rule "Tuple"); child_req "arrow" (Token "arrow") ]
  in
  reads
    "a rule that starts with a pair"
    "head"
    (assign
       [ child_req "head" (Rule "Head"); child_req "body" (Rule "Body") ]
       [ head; tuple; body ])
    {|(?=\()..(?!\G)(?<==>)[#tuple]|};
  (* A rule that opens on a lookahead and is cut behind a child. Its first
     part takes nothing either, so it stands where it opened, and an end that
     looks behind is kept off that place. *)
  reads
    "a rule that starts with a pair and is cut behind a child"
    "outer"
    (assign
       [ child_req "outer" (Rule "Outer") ]
       ([ prod
            "Outer"
            [ child_req "head" (Rule "Head"); child_req "right" (Rule "Right") ]
        ; head
        ; either "Right" [ "Group" ]
        ]
        @ pairs))
    {|(?=(?=\())..(?!\G)(?<=\))[<\G(?=\()..(?!\G)(?<==>)[#head]> #group]|};
  (* Something else in the list opens on the same bracket, with nothing
     between the two to cut at. [Head] cannot be a region, so its pair stands
     beside the other, and the grammar is refused. *)
  refuses
    "a rule that starts with a pair which something else opens on"
    (assign
       [ child_req "right" (Rule "Right"); child_req "head" (Rule "Head") ]
       ([ head; right ] @ pairs));
  (* -- the tail of a list element is a region --

     [Arm] may start with a name, so it opens on nothing. It is an element of
     a separated list in a pair, so what follows its arrow ends in front of
     the separator or the closer. *)
  let wide_left : production =
    prod
      "Left"
      [ child_alt ~modifier:Exactly_one "kind" [ Rule "Tuple"; Rule "Set"; Token "word" ]
      ]
  in
  let arm (children : child list) : production =
    prod
      "Arm"
      ([ child_req "left" (Rule "Left"); child_req "arrow" (Token "arrow") ] @ children)
  in
  let arms : production =
    prod "Arms" [ child_rep "arm" (Rule "Arm") ]
    |> with_delimited_sep ~open_tok:"lbrack" ~close_tok:"rbrack" ~sep:"semi"
  in
  let one_right : child list = [ child_req "right" (Rule "Right") ] in
  let tail = {|[#left <=>..(?=;|\])[#right]>]|} in
  (* An element with no tail keeps both sides in the list of arms. Where the
     two sides reach different pairs on one bracket, that list cannot tell
     them apart and the grammar is refused. *)
  let whole = "[#left #right]" in
  reads
    "an element of a separated list in a pair"
    "arm"
    ([ file "Arms"; arms; arm one_right; wide_left; right ] @ pairs)
    tail;
  reads
    "an element of a list with no separator"
    "arm"
    ([ file "Arms"
     ; prod "Arms" [ child_rep "arm" (Rule "Arm") ]
       |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
     ; arm one_right
     ; wide_left
     ; right
     ]
     @ pairs)
    refused;
  (* With one list that has no separator, an element there is followed by the
     next one and by nothing the tail could look ahead for. *)
  reads
    "an element of two lists, one with no separator"
    "arm"
    ([ file "Stmt"
     ; either "Stmt" [ "Arms"; "Others" ]
     ; arms
     ; prod "Others" [ child_rep "arm" (Rule "Arm") ]
       |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
     ; arm one_right
     ; wide_left
     ; right
     ]
     @ pairs)
    refused;
  reads
    "an element that something else holds too"
    "arm"
    ([ file "Stmt"
     ; either "Stmt" [ "Arms"; "Lone" ]
     ; arms
     ; prod "Lone" [ child_req "kw" (Token "go"); child_req "arm" (Rule "Arm") ]
     ; arm one_right
     ; wide_left
     ; right
     ]
     @ pairs)
    refused;
  reads
    "an element of two lists with two separators"
    "arm"
    ([ file "Stmt"
     ; either "Stmt" [ "Arms"; "Others" ]
     ; arms
     ; prod "Others" [ child_rep "arm" (Rule "Arm") ]
       |> with_delimited_sep ~open_tok:"lparen" ~close_tok:"rparen" ~sep:"comma"
     ; arm one_right
     ; wide_left
     ; right
     ]
     @ pairs)
    refused;
  refuses
    ~roots:[ "File"; "Arm" ]
    "an element that is a root as well"
    ([ file "Arms"; arms; arm one_right; wide_left; right ] @ pairs);
  reads
    "an element whose two sides reach the same pairs"
    "arm"
    ([ file "Arms"; arms; arm one_right; wide_left; either "Right" [ "Tuple"; "Set" ] ]
     @ pairs)
    whole;
  (* The tail is a list of its own. A rule in front of the arrow that opens
     on a name is alone on that name, whatever the tail takes. One in the
     tail, beside a loose name, is not. *)
  let what = "an element with a name in front of its arrow and names behind it" in
  let entries =
    repository
      what
      ([ file "Arms"
       ; arms
       ; arm one_right
       ; either "Left" [ "Front"; "Tuple" ]
       ; prod "Front" [ child_req "name" (Token "word"); child_req "set" (Rule "Set") ]
       ; prod
           "Right"
           [ child_opt "group" (Rule "Group")
           ; child_req "back" (Rule "Back")
           ; child_req "other" (Token "word")
           ]
       ; prod "Back" [ child_req "name" (Token "word"); child_req "body" (Rule "Body") ]
       ]
       @ pairs)
  in
  expect what (entry what "arm" entries) tail;
  expect what (entry what "front" entries) (name ^ {|..(?<=\})[#set]|});
  expect what (entry what "back" entries) ("[#body /" ^ name ^ "/]");
  (* A token after the arrow may be tied to it, and the tail's own opening
     would take the arrow first. *)
  reads
    "an element whose tail leads with a token"
    "arm"
    ([ file "Arms"
     ; arms
     ; arm [ child_req "lead" (Rule "Lead"); child_req "group" (Rule "Group") ]
     ; wide_left
     ; lead
     ]
     @ pairs)
    refused;
  (* The separator stands loose in the tail, which would end there. *)
  let loose (token : string) : production =
    prod
      "Right"
      [ child_opt "bang" (Token "bang")
      ; child_req "one" (Rule "Group")
      ; child_req "loose" (Token token)
      ; child_req "two" (Rule "Body")
      ]
  in
  reads
    "an element with the separator loose in its tail"
    "arm"
    ([ file "Arms"; arms; arm one_right; wide_left; loose "semi" ] @ pairs)
    refused;
  reads
    "an element with the closer loose in its tail"
    "arm"
    ([ file "Arms"; arms; arm one_right; wide_left; loose "rbrack" ] @ pairs)
    refused;
  (* The separator inside a pair the tail reaches is taken there, and ends
     nothing. *)
  reads
    "an element with the separator inside a pair of its tail"
    "arm"
    [ file "Arms"
    ; arms
    ; arm one_right
    ; wide_left
    ; right
    ; tuple
    ; set
    ; body
    ; prod "Group" [ child_req "inner" (Rule "Inner") ]
      |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
    ; prod
        "Inner"
        [ child_opt "bang" (Token "bang")
        ; child_req "one" (Token "word")
        ; child_req "semi" (Token "semi")
        ; child_req "two" (Token "word")
        ]
    ]
    tail;
  (* -- an operand opens a region, and its operators are read inside it --

     [Paren] is an atom on the bracket the call opens on. The block's own
     list holds no pattern for the call. A token atom opens a region by
     being taken, and the call and the one-token operator stand in it. An
     atom that is a pair opens one around itself, where the call refuses the
     place the region opened at. Both end in front of whatever is none of
     these. *)
  let what = "a call behind its operand" in
  let block =
    entry
      what
      "expr"
      (repository
         ~expr:words
         what
         [ file "Stmt"
         ; prod "Stmt" [ child_req "e" (Rule "Expr"); child_req "semi" (Token "semi") ]
         ; paren
         ])
  in
  let call (opens : string) : string = opens ^ {|..\)[#expr]|} in
  expect
    what
    block
    (Printf.sprintf
       {|[<(?=(?:\())..(?=\S)[<%s> /!/ #paren]> <(%s)|(\bself\b)..(?=\S)[<%s> /!/]>]|}
       (call {|(?!\G)\(|})
       name
       (call {|\(|}));
  List.iter
    (fun (region : Yojson.Basic.t) ->
       if
         field "begin" region <> None
         && field "applyEndPatternLast" region <> Some (`Int 1)
       then
         Law.fail "(s) %s: an operand's region ends before its operators are tried" what)
    (patterns_of block);
  (* A block with no operator that encloses its operand is the list it was. *)
  let what = "a block with no call" in
  expect
    what
    (entry
       what
       "expr"
       (repository
          ~expr:[ expr_block ~rule_name:"Expr" ~atoms:[ Token "word"; Rule "Paren" ] () ]
          what
          [ file "Stmt"
          ; prod "Stmt" [ child_req "e" (Rule "Expr"); child_req "semi" (Token "semi") ]
          ; paren
          ]))
    "[#paren]";
  (* -- every list of the corpus gives each token to one thing --

     The backend refuses a grammar where one does not, so part (p) says this
     already. It is said here by the function that finds them, which part
     (p) reaches only through the refusal. *)
  List.iter
    (fun (entry : Editor_corpus.entry) ->
       let scopes = Editor_corpus.scopes entry in
       let facts = Scopes.facts scopes in
       let rule (id : Core.Rule.id) : string =
         Core.Grammar.Name.Rule.to_string (Core.Facts.rule facts id).name
       in
       List.iter
         (fun (found : Textmate.Shape.undecided) ->
            Law.fail
              "(s) %s: a list gives %s to %s and %s"
              entry.name
              (Core.Kind.Name.to_string (Core.Facts.kind_name facts found.token))
              (rule found.first)
              (match found.second with
               | Some second -> rule second
               | None -> "a token of its own"))
         (Textmate.Shape.undecided facts scopes))
    Editor_corpus.all;
  ignore holds;
  if Law.failures () = before
  then Law.pass "(s) each token of a list has one thing that opens on it"
;;

let () = Law.summarise "law_textmate"
