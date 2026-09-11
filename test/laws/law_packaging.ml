(* -- packaging and encapsulation ----------------------------------------------

      (a) lingo_runtime never depends on lingo. Generated code links the
          runtime, which keeps ppxlib and yojson out of a consumer's build
          graph.
      (b) No module outside lingo.core computes a FIRST set, a kind integer or
          an emitted name.
      (c) Grammar holds no queries: no value in its signature takes a
          [Grammar.t]. It is its own library, [lingo.grammar], depending on
          redfa alone, so the compiler refuses the edge that would let a
          query in. This check reads the signature anyway, since a query
          over a [Grammar.t] needs nothing from core to write.
      (d) [Facts.t] has one constructor. Every value in facts.mli whose result
          mentions [t] is [of_grammar].

      Mechanism. (a) reads the dune-project stanza, so it checks the
      declaration. That is what there is to check today: the runtime holds no
      code, so the edge cannot exist. The check is written now so it is in
      place when the port lands.

      (b) is structural in four places and a grep for the rest.
        - a kind integer is minted in [Kind] and an emitted name in
          [Manifest], since [Kind.t] and [Manifest.name] are abstract and
          neither module hands out a constructor;
        - a [Facts.t] is built in [Facts], since it is a private record whose
          one constructor is [of_grammar];
        - the staged derivation and the check groups are reached through
          [Core.Internal], since core.mli is the library's main module and
          re-exports what is API. Dune's [private_modules] leaves
          the wrapper alias in place, and a module in another library reaches
          through it; the main module interface is what closes that.
      [Internal] is open for one caller: law_validate runs the stages one at a
      time to check each rejection is reported as early as it can be. The grep
      looks for a second.

      (c) and (d) read the interfaces as written.

      Falsification. Every mutation below was applied, run and reverted.

        M1  Add [lingo] to lingo_runtime's depends in dune-project.
            -> this law, part (a).
        M2  Add the text [Core.Internal] to grammars/sexp_grammar.ml, in a
            comment, so the file still compiles.
            -> this law, part (b), naming the file and the needle.

            The mutation that reads better, [let _leak n =
            Core.Internal.Stage.shape n], no longer compiles: grammars/ links
            lingo.grammar and redfa, so [Core] is unbound there. That is a
            stronger guarantee than the grep, and the dune stanza is what
            provides it. Part (b) will earn a real leak to catch when
            runtime/ and editors/ hold code, since both link lingo.core.
            The comment is what keeps the mechanism falsifiable until then.
        M3  Add [primary_root : t -> string] to Grammar, with its
            implementation.
            -> this law, part (c).
        M4  Add [unchecked : Grammar.t -> t] to Facts, with an implementation.
            -> this law, part (d).

      M2 needs an explanation. The first run reddened nothing, because of a
      defect in this test rather than in the code: dune runs a test in a
      sandbox holding its declared deps, grammars/ was not among them, the
      scan read zero files and the law passed without looking. The deps stanza
      declares the tree now and the test fails where it scanned nothing. A
      grep over an empty file list gives the same output as a grep that found
      nothing wrong.

      M3 and M4 have a caveat. As a change to the .mli alone, both fail to
      compile before this law runs, so the typechecker is the enforcement and
      these two catch someone who supplies the implementation as well. Both
      were re-run that way.

      Coverage. (a) covers the declaration. (b) covers the .ml and .mli files
      outside lib/core, of which there is one: the sexp grammar. runtime/ and
      editors/ hold no code and this suite scans itself out. (c) and (d) read
      the interfaces, and would miss a module handing out what they forbid by
      another route; core.mli decides that, and these two do not read
      it.
   -------------------------------------------------------------------------- *)

let failures = ref 0

let fail fmt =
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt

let read path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

(* The nearest enclosing directory holding a dune-project. Found by walking
   up rather than by counting levels, because [dune exec] and [dune runtest]
   start the test in different directories. *)
let root =
  let rec up d =
    if Sys.file_exists (Filename.concat d "dune-project")
    then Some d
    else (
      let parent = Filename.dirname d in
      if parent = d then None else up parent)
  in
  match up (Sys.getcwd ()) with
  | Some d -> d
  | None -> Sys.getcwd ()
;;

let contains hay needle =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* (a) *)
let () =
  let path = Filename.concat root "dune-project" in
  if not (Sys.file_exists path)
  then fail "dune-project not found at %s" path
  else (
    let s = read path in
    (* The runtime's package stanza runs from its (name lingo_runtime) to the
       start of the next (package. *)
    let after =
      match String.index_opt s 'l' with
      | _ ->
        let marker = "(name lingo_runtime)" in
        let rec find i =
          if i + String.length marker > String.length s
          then None
          else if String.sub s i (String.length marker) = marker
          then Some (i + String.length marker)
          else find (i + 1)
        in
        find 0
    in
    match after with
    | None -> fail "dune-project declares no lingo_runtime package"
    | Some start ->
      let rest = String.sub s start (String.length s - start) in
      let stanza =
        match String.index_opt rest '\000' with
        | _ ->
          let marker = "(package" in
          let rec find i =
            if i + String.length marker > String.length rest
            then String.length rest
            else if String.sub rest i (String.length marker) = marker
            then i
            else find (i + 1)
          in
          String.sub rest 0 (find 0)
      in
      (* "lingo_runtime" and "lingo_editors" both contain "lingo", so look
         for the package name as a standalone word. *)
      let words =
        String.split_on_char '\n' stanza
        |> List.concat_map (String.split_on_char ' ')
        |> List.map (fun w -> String.concat "" (String.split_on_char '(' w))
        |> List.map (fun w -> String.concat "" (String.split_on_char ')' w))
      in
      if List.mem "lingo" words
      then
        fail
          "lingo_runtime depends on lingo, which is the one rule a stanza must not break"
      else
        pass
          "lingo_runtime does not depend on lingo (declaration checked; no runtime code \
           yet)")
;;

(* (b) *)
let () =
  (* [Internal] is the only route to the staged derivation now, so naming it
     is the whole test: everything it can reach derives facts. *)
  let minting =
    [ "Core.Internal", "opens the staged derivation"
    ; "Kind.Table.of_names", "assigns kind integers"
    ; "Manifest.of_grammar", "derives emitted names"
    ]
  in
  let rec files dir acc =
    if not (Sys.file_exists dir)
    then acc
    else
      Array.fold_left
        (fun acc entry ->
           let p = Filename.concat dir entry in
           if entry = "_build" || entry = "_opam" || entry.[0] = '.'
           then acc
           else if Sys.is_directory p
           then files p acc
           else if Filename.check_suffix p ".ml" || Filename.check_suffix p ".mli"
           then p :: acc
           else acc)
        acc
        (Sys.readdir dir)
  in
  let scanned =
    files (Filename.concat root "grammars") []
    @ files (Filename.concat root "runtime") []
    @ files (Filename.concat root "editors") []
  in
  let bad = ref 0 in
  List.iter
    (fun p ->
       let s = read p in
       List.iter
         (fun (needle, why) ->
            if contains s needle
            then (
              incr bad;
              fail "%s uses %s, which %s, outside lingo.core" p needle why))
         minting)
    scanned;
  (* Instrument the instrument. A grep over an empty file list is a test that
     passes by not looking, and this one runs in a dune sandbox where an
     undeclared directory is absent. *)
  if scanned = []
  then
    fail
      "the scan found no files to read under %s, so it proved nothing; check the deps \
       stanza in test/laws/dune"
      root
  else if !bad = 0
  then
    pass
      "no module outside lingo.core derives facts (%d files scanned; runtime/ and \
       editors/ hold no code yet, so the scan is over grammars/ alone)"
      (List.length scanned)
;;

(* (c) and (d) — read off the interfaces. Both are properties OCaml enforces
   once the .mli says what it says; what a test can add is noticing when the
   .mli stops saying it, which is the failure mode that actually happens. *)
let read_mli dir name =
  let p = Filename.concat root (Filename.concat dir name) in
  if Sys.file_exists p then Some (read p) else None
;;

(* [val name : sig] blocks, with doc comments and inline comments stripped. *)
let val_blocks src =
  let strip s =
    let b = Buffer.create (String.length s) in
    let depth = ref 0 in
    let i = ref 0 in
    let n = String.length s in
    while !i < n do
      if !i + 1 < n && s.[!i] = '(' && s.[!i + 1] = '*'
      then (
        incr depth;
        i := !i + 2)
      else if !i + 1 < n && s.[!i] = '*' && s.[!i + 1] = ')' && !depth > 0
      then (
        decr depth;
        i := !i + 2)
      else (
        if !depth = 0 then Buffer.add_char b s.[!i];
        incr i)
    done;
    Buffer.contents b
  in
  let src = strip src in
  let parts = String.split_on_char '\n' src in
  let blocks = ref []
  and cur = ref None in
  List.iter
    (fun line ->
       let starts_val = String.length line >= 4 && String.sub line 0 4 = "val " in
       let starts_other = line <> "" && line.[0] <> ' ' && not starts_val in
       if starts_val
       then (
         (match !cur with
          | Some b -> blocks := b :: !blocks
          | None -> ());
         cur := Some line)
       else if starts_other
       then (
         (match !cur with
          | Some b -> blocks := b :: !blocks
          | None -> ());
         cur := None)
       else (
         match !cur with
         | Some b -> cur := Some (b ^ " " ^ String.trim line)
         | None -> ()))
    parts;
  (match !cur with
   | Some b -> blocks := b :: !blocks
   | None -> ());
  List.rev !blocks
;;

(* [Kind.t], [Redfa.Regex.t], [Rule.def] are not [t]. Collapse any dotted path
   ending in [.t] before tokenising, so only a bare [t] counts. *)
let unqualify s =
  Str.global_replace (Str.regexp "[A-Za-z_][A-Za-z0-9_']*\\.t\\b") "QUALIFIED" s
;;

let tokens s =
  let b = Buffer.create 16 in
  let out = ref [] in
  String.iter
    (fun c ->
       if
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '_'
       then Buffer.add_char b c
       else (
         if Buffer.length b > 0 then out := Buffer.contents b :: !out;
         Buffer.clear b))
    s;
  if Buffer.length b > 0 then out := Buffer.contents b :: !out;
  List.rev !out
;;

let () =
  match read_mli "lib/grammar" "grammar.mli" with
  | None -> fail "lib/grammar/grammar.mli not found"
  | Some src ->
    let blocks = val_blocks src in
    if blocks = []
    then fail "no val blocks parsed out of grammar.mli; the check proved nothing"
    else (
      let bad = ref [] in
      List.iter
        (fun b ->
           (* Everything before the last arrow is the argument side. *)
           let b = unqualify b in
           let arrows = Str.split_delim (Str.regexp_string "->") b in
           match List.rev arrows with
           | [] | [ _ ] -> ()
           | _result :: args ->
             let args = String.concat " " args in
             if List.mem "t" (tokens args)
             then (
               let name =
                 match tokens b with
                 | _ :: n :: _ -> n
                 | _ -> "?"
               in
               bad := name :: !bad))
        blocks;
      match !bad with
      | [] ->
        pass
          "Grammar has no derived queries: none of its %d values takes a Grammar.t"
          (List.length blocks)
      | ns ->
        List.iter
          (fun n -> fail "Grammar.%s takes a Grammar.t, so it is a derived query" n)
          ns)
;;

let () =
  match read_mli "lib/core" "facts.mli" with
  | None -> fail "lib/core/facts.mli not found"
  | Some src ->
    let blocks = val_blocks src in
    if blocks = []
    then fail "no val blocks parsed out of facts.mli; the check proved nothing"
    else (
      (* A value that could hand out a [Facts.t] is one whose result mentions
         [t]. [of_grammar] is allowed to; nothing else is. *)
      let producers =
        List.filter_map
          (fun b ->
             let b = unqualify b in
             let arrows = Str.split_delim (Str.regexp_string "->") b in
             let result =
               match List.rev arrows with
               | r :: _ -> r
               | [] -> b
             in
             let name =
               match tokens b with
               | _ :: n :: _ -> n
               | _ -> "?"
             in
             if List.mem "t" (tokens result) then Some name else None)
          blocks
      in
      match producers with
      | [ "of_grammar" ] ->
        pass "Facts.t has exactly one constructor, and it is of_grammar"
      | ps ->
        fail
          "facts.mli hands out a Facts.t from {%s}; it must be of_grammar alone, or a \
           way to obtain facts without passing the checks"
          (String.concat ", " ps))
;;

let () =
  if !failures = 0
  then print_endline "law_packaging: 0 failures"
  else (
    Printf.printf "law_packaging: %d failures\n" !failures;
    exit 1)
;;
