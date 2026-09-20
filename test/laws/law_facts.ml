(* -- the facts ----------------------------------------------------------------

      (a) [of_grammar] is deterministic: two runs over one grammar give the
          same bytes.
      (b) Name and kind are a bijection.
      (c) [Kind.Set.t] is a set. The algebra holds on sets drawn from real
          grammars, and [elements] is ascending and free of duplicates, so a
          dispatch table follows from the set.
      (d) The recovery set at a position holds the closers of the rule's own
          frame and of every frame the rule sits inside.
      (e) An author's [recover_to] replaces what the position computes. The
          closers of the frames the rule sits inside survive it.
      (f) [local_recovery_set] holds what the position contributes, and
          [recovery_set] adds the enclosing frames to it.
      (g) [delimiter_pairs] holds one pair per framed rule, an enclosed
          postfix's among them, and no duplicate.

      Mechanism: (a) is a golden, and see the note on M9. (b) to (g) are
      oracles over the corpus, and (d), (e), (f) and (g) each add a grammar
      built in the law for a case the corpus does not cover.

      Part (d) states what facts.mli promises. Its oracle works out, for
      every frame in the grammar, which rules sit inside it, by taking the
      transitive closure of the reference graph forward from that frame's
      body. [Fixpoint] arrives at the same set backwards, unioning into
      each rule over the sites that reference it, so the two arrive from
      opposite directions. The law this replaced recomputed [d.frame] and
      checked that, which is how a recovery set missing every enclosing
      closer stayed green.

      Falsification. Every mutation below was applied, run and reverted, and
      the result recorded is the one observed.

        M1  In [Facts.recovery_set], drop the [frame] term from the union.
            -> this law, part (d), on the two corpus grammars whose root
               carries a frame: delimited-with-sep and separated. M2 is the
               stronger of the two.
        M2  In [Facts.recovery_set], drop the [t.enclosing] term from the
            union, which is what the function did before the enclosing table
            existed.
            -> this law, part (d), four times: three positions in
               pratt-postfix, where a role rule sits inside a call's
               parentheses, and the nested-frame grammar built below, where
               the first of two children in a plain rule sits inside a
               delimited parent. That last is the reported defect.
        M3  In [Facts.local_recovery_set], ignore the override and take the
            computed set.
            -> this law, part (e): the computed set is back, [T_TA] with it.
               The override lives in the local half. Mutating
               [Facts.recovery_set] instead reddens nothing, since it takes
               the [None] branch and calls the local half, which still
               honours the override.
        M4  In [Facts.recovery_set], drop the enclosing closers from the
            override branch, so [recover_to] replaces the whole set the way
            it did before 2026-09-07.
            -> this law, part (e). [T_RP] goes missing, leaving a recovery
               inside [Item] free to skip the [)] that [Root] is waiting
               for. That is the failure the enclosing closers exist to
               prevent, reached through the override.
        M5  In [Facts.local_recovery_set], union [t.enclosing] back into the
            result, collapsing the two queries into one.
            -> this law, part (f). The local set carries the two closers the
               two-context grammar's enclosing frames put there. Nothing else
               moves: [recovery_set] is unchanged, since the union is
               idempotent, and that is why (f) looks at the local half.
        M6  In [Facts.delimiter_pairs], take pairs from user productions and
            skip the role rules, which is what the predecessor's own
            [delimiter_pairs] does: it reads its productions and leaves the
            expression blocks alone.
            -> this law, part (g), twice on pratt-postfix. That grammar's one
               pair comes from an enclosed postfix, so under the mutation the
               list is empty and a balanced skip would walk straight through
               a stray opener inside a call's arguments.
        M7  In [Kind.Set.add], write the bit into the argument array where it is
            already wide enough, in place of copying first.
            -> this law, part (c). [add] mutates its argument.
        M8  In [Kind.Set.elements], drop the [List.rev].
            -> this law, part (c). Elements descend.

      One that reddens nothing here, recorded as a finding.

        M9  Make [Kind.Table.of_names] build its index from a hash table's fold
            order, so the numbering stops following the manifest's list.
            -> sexp_facts reddens; part (a) does not. Determinism is checked
               by running the derivation twice in one process, and an
               unordered traversal is stable within one process.
               Chasing that found the cause: every traversal in the
               derivation is over an array or a list, and the two that fold a
               hash table ([Check_shape.view_hazards],
               [Check_full.empty_first_sets]) produce findings, which are
               sorted before they leave. Nothing short of real randomness
               reddens part (a). It stays as a guard on a future change that
               puts a hash-ordered traversal in the path to the output. The
               statement with teeth about the numbering is the literal one in
               test/units/sexp_facts.ml.

      Coverage: the accepted corpus. Part (d) covers every child position in
      it, some thirty of them, plus the nested-frame grammar it builds. Only
      pratt-postfix in the corpus puts a rule inside a frame part-way
      through, which is a thin thread for the statement to hang on. Part (e)
      covers one position, and it sits inside a frame, so it separates the
      half the override replaces from the half it leaves. Part (f) covers one
      grammar and one position. Part (g) counts the enclosed postfixes it
      reads and fails at zero, since only pratt-postfix carries one.
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

let dump f = Format.asprintf "%a" Core.Facts.pp f

let () =
  List.iter
    (fun (name, g) ->
       match Core.Facts.of_grammar g, Core.Facts.of_grammar g with
       | Ok a, Ok b ->
         if dump a = dump b
         then pass "%s: deterministic" name
         else fail "%s: two checks of one grammar gave different facts" name
       | _ -> fail "%s: the corpus grammar was rejected" name)
    Corpus.all
;;

let () =
  List.iter
    (fun (name, g) ->
       match Core.Facts.of_grammar g with
       | Error _ -> ()
       | Ok f ->
         let ok = ref true in
         List.iteri
           (fun i n ->
              let spelled = Core.Kind.Name.to_string n in
              match Core.Facts.find_kind f n with
              | None ->
                ok := false;
                fail "%s: kind %S is not findable by name" name spelled
              | Some k when Core.Kind.to_int k <> i ->
                ok := false;
                fail
                  "%s: kind %S numbers %d but finds %d"
                  name
                  spelled
                  i
                  (Core.Kind.to_int k)
              | Some k ->
                if not (Core.Kind.Name.equal (Core.Facts.kind_name f k) n)
                then (
                  ok := false;
                  fail
                    "%s: kind %d round-trips to %S, not %S"
                    name
                    i
                    (Core.Kind.Name.to_string (Core.Facts.kind_name f k))
                    spelled))
           (Core.Kind.Table.names f.kinds);
         if !ok
         then
           pass
             "%s: name and kind are a bijection over %d kinds"
             name
             (Core.Facts.kind_count f))
    Corpus.all
;;

(* (c). The algebra, over sets that came out of real grammars. *)
let () =
  match Core.Facts.of_grammar Lingo_grammars.Sexp_grammar.grammar with
  | Error _ -> fail "sexp was rejected"
  | Ok f ->
    let sets = Array.to_list f.first @ Array.to_list f.follow in
    let all_kinds = Core.Kind.Set.of_list (Core.Kind.Table.kinds f.kinds) in
    let sets = all_kinds :: Core.Kind.Set.empty :: sets in
    let ok = ref true in
    List.iter
      (fun a ->
         let els = Core.Kind.Set.elements a in
         if List.sort_uniq Core.Kind.compare els <> els
         then (
           ok := false;
           fail "elements is not ascending-and-distinct");
         if Core.Kind.Set.cardinal a <> List.length els
         then (
           ok := false;
           fail "cardinal disagrees with elements");
         if not (Core.Kind.Set.equal (Core.Kind.Set.of_list els) a)
         then (
           ok := false;
           fail "of_list . elements is not the identity");
         List.iter
           (fun b ->
              if
                not
                  (Core.Kind.Set.subset (Core.Kind.Set.inter a b) a
                   && Core.Kind.Set.subset (Core.Kind.Set.inter a b) b)
              then (
                ok := false;
                fail "inter is not a lower bound");
              if
                not
                  (Core.Kind.Set.subset a (Core.Kind.Set.union a b)
                   && Core.Kind.Set.subset b (Core.Kind.Set.union a b))
              then (
                ok := false;
                fail "union is not an upper bound");
              if
                not
                  (Core.Kind.Set.is_empty
                     (Core.Kind.Set.inter (Core.Kind.Set.diff a b) b))
              then (
                ok := false;
                fail "diff leaves something of b behind");
              if
                not
                  (Core.Kind.Set.equal
                     (Core.Kind.Set.union a b)
                     (Core.Kind.Set.union
                        (Core.Kind.Set.diff a b)
                        (Core.Kind.Set.union
                           (Core.Kind.Set.inter a b)
                           (Core.Kind.Set.diff b a))))
              then (
                ok := false;
                fail "union is not the disjoint sum of the three parts"))
           sets;
         (* [add] must not disturb the set it was given. *)
         List.iter
           (fun k ->
              let before = Core.Kind.Set.elements a in
              let a' = Core.Kind.Set.add k a in
              if Core.Kind.Set.elements a <> before
              then (
                ok := false;
                fail "add mutated its argument");
              if not (Core.Kind.Set.mem a' k)
              then (
                ok := false;
                fail "add did not add");
              if
                not
                  (Core.Kind.Set.equal
                     (Core.Kind.Set.remove k a')
                     (Core.Kind.Set.remove k a))
              then (
                ok := false;
                fail "remove does not undo add"))
           (Core.Kind.Table.kinds f.kinds))
      sets;
    if !ok then pass "Kind.Set.t algebra holds over %d sets" (List.length sets)
;;

let frame_closers (fr : Core.Rule.frame) =
  match fr with
  | Core.Rule.Delimited { close; sep; _ } ->
    let s = Core.Kind.Set.singleton close in
    (match sep with
     | Some { sep_tok; _ } -> Core.Kind.Set.add sep_tok s
     | None -> s)
  | Core.Rule.Separated { sep_tok; _ } -> Core.Kind.Set.singleton sep_tok
  | Core.Rule.Plain | Core.Rule.Committed _ -> Core.Kind.Set.empty
;;

(* Which rules sit inside a given frame, by walking forward from it.
   [Fixpoint] arrives at the same set backwards, unioning into each rule
   over the sites that reference it, and going the other way keeps the
   oracle independent of it. Once a rule sits inside a frame, so does every
   rule it names, so this is the transitive closure of the reference graph
   from the frame's body. *)
let inside (f : Core.Facts.t) (owner : Core.Rule.def) =
  let n = Array.length f.rules in
  let block_at = Array.make n (-1) in
  Array.iteri (fun j (b : Core.Block.def) -> block_at.(b.rule_id) <- j) f.blocks;
  let targets ks =
    Array.fold_left
      (fun acc k ->
         match Core.Facts.rule_of_kind f k with
         | Some d -> d.Core.Rule.id :: acc
         | None -> acc)
      []
      ks
  in
  let seen = Array.make n false in
  let rec go r =
    if not seen.(r)
    then (
      seen.(r) <- true;
      let d = f.rules.(r) in
      Array.iter (fun (c : Core.Rule.child) -> List.iter go (targets c.alts)) d.children;
      if block_at.(r) >= 0
      then (
        let b = f.blocks.(block_at.(r)) in
        List.iter go (targets b.atoms);
        Array.iter
          (fun (p : Core.Block.postfix) ->
             (* A role node sits where the block sits. *)
             go p.p_rule;
             match p.p_body with
             | Core.Block.Nothing -> ()
             | Core.Block.Then rhs -> List.iter go (targets rhs)
             | Core.Block.Enclosed { content; _ } ->
               List.iter
                 go
                 (targets
                    (match content with
                     | Core.Block.One s -> s
                     | Core.Block.Many { elem; _ } -> elem)))
          b.postfix))
  in
  (* The body is what the frame wraps. A child before [body_from] is a
     postfix operand and sits to the left of the opener. *)
  Array.iteri
    (fun idx (c : Core.Rule.child) ->
       if idx >= owner.Core.Rule.body_from then List.iter go (targets c.alts))
    owner.Core.Rule.children;
  seen
;;

(* (d) *)
let () =
  List.iter
    (fun (name, g) ->
       match Core.Facts.of_grammar g with
       | Error _ -> ()
       | Ok f ->
         let ok = ref true in
         (* What every frame in the grammar demands of the rules under it. *)
         let owed = Array.make (Array.length f.rules) Core.Kind.Set.empty in
         Array.iter
           (fun (owner : Core.Rule.def) ->
              let closers = frame_closers owner.frame in
              if not (Core.Kind.Set.is_empty closers)
              then (
                let within = inside f owner in
                Array.iteri
                  (fun r yes ->
                     if yes then owed.(r) <- Core.Kind.Set.union owed.(r) closers)
                  within))
           f.rules;
         Array.iter
           (fun (d : Core.Rule.def) ->
              Array.iteri
                (fun i (c : Core.Rule.child) ->
                   let r = Core.Facts.recovery_set f d.id ~child:i in
                   match c.recover_to with
                   | Some _ -> ()
                   | None ->
                     (* Its own frame bounds a position in its body. *)
                     if not (Core.Kind.Set.subset (frame_closers d.frame) r)
                     then (
                       ok := false;
                       fail
                         "%s/%s.%s: the recovery set omits a closer of its own frame"
                         name
                         (Grammar.Name.Rule.to_string d.name)
                         (Grammar.Name.Child.to_string c.child_name));
                     (* Every frame it sits inside stays open around it. *)
                     if not (Core.Kind.Set.subset owed.(d.id) r)
                     then (
                       ok := false;
                       fail
                         "%s/%s.%s: the recovery set omits %a, a closer of an enclosing \
                          frame, so a recovery here walks off with it"
                         name
                         (Grammar.Name.Rule.to_string d.name)
                         (Grammar.Name.Child.to_string c.child_name)
                         (Core.Kind.Table.pp_set f.kinds)
                         (Core.Kind.Set.diff owed.(d.id) r)))
                d.children)
           f.rules;
         if !ok
         then
           pass
             "%s: every recovery set contains the closers of its own frame and of every \
              frame it sits inside"
             name)
    Corpus.all
;;

(* (d), on the shape the defect was reported with: a rule with two children
   sitting inside a delimited parent, where the first position is part-way
   through the rule and so its FOLLOW is left out. The corpus reaches this
   only through [pratt-postfix]'s role rules, which is a thin thread to hang
   it on. Built here for the reason part (e) builds its own. *)
let () =
  let open Core.Grammar in
  let g =
    create
      ~tokens:
        [ punct_tight ~name:"lparen" "("
        ; punct_tight ~name:"rparen" ")"
        ; punct_tight ~name:"ta" "a"
        ; punct_tight ~name:"tb" "b"
        ]
      ~roots:[ "Root" ]
      [ prod "Root" [ child_req "o" (Rule "Outer") ]
      ; prod "Outer" [ child_req "i" (Rule "Inner") ]
        |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
      ; prod "Inner" [ child_req "x" (Token "ta"); child_req "y" (Token "tb") ]
      ]
  in
  match Core.Facts.of_grammar g with
  | Error es -> fail "the nested-frame grammar was rejected:@\n%a" Core.Error.pp_list es
  | Ok f ->
    let rule_named nm =
      (Option.get (Core.Facts.rule_of_kind f (Option.get (Core.Facts.find_kind f nm))))
        .Core.Rule.id
    in
    let inner = rule_named (Core.Kind.Name.node "Inner") in
    let rparen = Option.get (Core.Facts.find_kind f (Core.Kind.Name.token "rparen")) in
    let at i = Core.Facts.recovery_set f inner ~child:i in
    if not (Core.Kind.Set.mem (at 0) rparen)
    then
      fail
        "a recovery at the first of two children inside a delimited parent may skip the \
         closer that parent is waiting for"
    else if not (Core.Kind.Set.mem (at 1) rparen)
    then fail "the trailing position lost the closer"
    else pass "a non-trailing position inside a delimited parent keeps the closer"
;;

(* (f). The local half holds what the position contributes and stops there.
   A parser carrying its own open frames relies on that: were the enclosing
   closers in the local half too, it would union them back in and get the
   over-approximation anyway. [Expr] below sits inside two differently
   framed rules, so its enclosing set holds two closers and its local set
   holds neither. *)
let () =
  let open Core.Grammar in
  let g =
    create
      ~tokens:
        [ punct_tight ~name:"lparen" "("
        ; punct_tight ~name:"rparen" ")"
        ; punct_tight ~name:"lbrack" "["
        ; punct_tight ~name:"rbrack" "]"
        ; punct_tight ~name:"a" "a"
        ; punct_tight ~name:"b" "b"
        ]
      ~roots:[ "Root" ]
      [ prod "Root" [ child_req "p" (Rule "Paren"); child_req "i" (Rule "Index") ]
      ; prod "Paren" [ child_req "e" (Rule "Expr") ]
        |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
      ; prod "Index" [ child_req "e" (Rule "Expr") ]
        |> with_delimited ~open_tok:"lbrack" ~close_tok:"rbrack"
      ; prod "Expr" [ child_req "l" (Token "a"); child_req "r" (Token "b") ]
      ]
  in
  match Core.Facts.of_grammar g with
  | Error es -> fail "the two-context grammar was rejected:@\n%a" Core.Error.pp_list es
  | Ok f ->
    let expr =
      (Option.get
         (Core.Facts.rule_of_kind
            f
            (Option.get (Core.Facts.find_kind f (Core.Kind.Name.node "Expr")))))
        .Core.Rule.id
    in
    let local = Core.Facts.local_recovery_set f expr ~child:0 in
    let whole = Core.Facts.recovery_set f expr ~child:0 in
    let encl = f.enclosing.(expr) in
    if Core.Kind.Set.cardinal encl <> 2
    then
      fail
        "a rule used inside two differently framed parents should carry both closers, \
         not %a"
        (Core.Kind.Table.pp_set f.kinds)
        encl
    else if not (Core.Kind.Set.is_empty (Core.Kind.Set.inter local encl))
    then
      fail
        "the local recovery set carries %a, which came from an enclosing frame"
        (Core.Kind.Table.pp_set f.kinds)
        (Core.Kind.Set.inter local encl)
    else if not (Core.Kind.Set.equal whole (Core.Kind.Set.union local encl))
    then fail "the whole recovery set is not the local one plus the enclosing frames"
    else pass "the local recovery set holds nothing an enclosing frame put there"
;;

(* (g). Every delimiter pair, and in particular the ones an expression
   block contributes. Completeness is the statement that matters: a balanced
   skip walks straight through a pair the list left out. [pratt-postfix]
   carries the case, since its one pair comes from an enclosed postfix
   operator. *)
let () =
  List.iter
    (fun (name, g) ->
       match Core.Facts.of_grammar g with
       | Error _ -> ()
       | Ok f ->
         let got = Core.Facts.delimiter_pairs f in
         let expected =
           Array.fold_left
             (fun acc (d : Core.Rule.def) ->
                match d.frame with
                | Core.Rule.Delimited { open_; close; _ } -> (open_, close) :: acc
                | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> acc)
             []
             f.rules
         in
         let norm l =
           List.sort_uniq
             compare
             (List.map (fun (a, b) -> Core.Kind.to_int a, Core.Kind.to_int b) l)
         in
         if norm got <> norm expected
         then fail "%s: delimiter_pairs is not the set of framed rules' pairs" name
         else if List.length got <> List.length (List.sort_uniq compare (norm got))
         then fail "%s: delimiter_pairs repeats a pair" name
         else pass "%s: %d delimiter pairs, one per framed rule" name (List.length got))
    Corpus.all
;;

(* An enclosed postfix is a frame like a production's, so its pair is in the
   list. Pigeon collected production framings alone, which left a stray
   opener inside an index body unbalanced. Here the two shapes are one value,
   so the list covers both by construction.

   Quantified over the corpus, with a count at the end. A law that finds no
   enclosed postfix to look at proves nothing, and dropping [pratt-postfix]
   would put it in that state. *)
let () =
  let checked = ref 0 in
  List.iter
    (fun (name, g) ->
       match Core.Facts.of_grammar g with
       | Error _ -> ()
       | Ok f ->
         let pairs = Core.Facts.delimiter_pairs f in
         Array.iter
           (fun (d : Core.Rule.def) ->
              match d.frame with
              | Core.Rule.Delimited { open_; close; _ } when Core.Rule.is_synthetic d ->
                incr checked;
                if
                  not
                    (List.exists
                       (fun (o, c) -> Core.Kind.equal o open_ && Core.Kind.equal c close)
                       pairs)
                then
                  fail
                    "%s/%s: an enclosed postfix's delimiters are missing from \
                     delimiter_pairs"
                    name
                    (Grammar.Name.Rule.to_string d.name)
              | _ -> ())
           f.rules)
    Corpus.all;
  if !checked = 0
  then fail "no corpus grammar has an enclosed postfix, so this law read nothing"
  else if !failures = 0
  then pass "every enclosed postfix's delimiters are in the pair list (%d)" !checked
;;

(* (e). Built here, so the computed set and the override are known to
   differ. [Item] sits inside [Root]'s parentheses, so the two halves can be
   told apart: the override replaces what the position computes for itself,
   and the closer of the frame around it survives that. *)
let () =
  let open Core.Grammar in
  let g =
    create
      ~tokens:
        [ punct_tight ~name:"lp" "("
        ; punct_tight ~name:"rp" ")"
        ; punct_tight ~name:"ta" "a"
        ; punct_tight ~name:"tb" "b"
        ]
      ~roots:[ "Root" ]
      [ prod "Root" [ child_rep "e" (Rule "Item") ]
        |> with_delimited ~open_tok:"lp" ~close_tok:"rp"
      ; prod "Item" [ child_req ~recover_to:[ "tb" ] "x" (Token "ta") ]
      ]
  in
  match Core.Facts.of_grammar g with
  | Error es -> fail "the override grammar was rejected:@\n%a" Core.Error.pp_list es
  | Ok f ->
    let item = Option.get (Core.Facts.find_kind f (Core.Kind.Name.node "Item")) in
    let d = Option.get (Core.Facts.rule_of_kind f item) in
    let kind nm = Option.get (Core.Facts.find_kind f (Core.Kind.Name.token nm)) in
    let show = Core.Kind.Table.pp_set f.kinds in
    let local = Core.Facts.local_recovery_set f d.id ~child:0 in
    let whole = Core.Facts.recovery_set f d.id ~child:0 in
    if not (Core.Kind.Set.equal local (Core.Kind.Set.singleton (kind "tb")))
    then fail "recover_to did not replace what the position computes: got %a" show local
    else if Core.Kind.Set.mem whole (kind "ta")
    then
      (* [T_TA] is FIRST(Item), which the computed set would have held
         through Root's repeated body. Its absence is what says the override
         replaced something. *)
      fail "the override left the computed set behind: got %a" show whole
    else if not (Core.Kind.Set.mem whole (kind "rp"))
    then fail "the override lost the closer of Item's enclosing frame: got %a" show whole
    else pass "recover_to replaces the computed set and keeps the enclosing closers"
;;

let () =
  if !failures = 0
  then print_endline "law_facts: 0 failures"
  else (
    Printf.printf "law_facts: %d failures\n" !failures;
    exit 1)
;;
