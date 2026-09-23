(* -- first, follow and nullability --------------------------------------------

      FIRST, FOLLOW and nullability over kind bitsets agree, rule for rule and
      token for token, with a naive recomputation over the grammar using string
      sets.

      And the two one-sided laws the sets owe, checked against the structure:
      FOLLOW holds everything the naive walk finds, and nullability holds every
      rule the naive walk finds nullable.

      Mechanism: a cross-check. [Core.Internal.Fixpoint] walks resolved
      rules with bitsets over kind integers; [Naive_first] walks the grammar
      with [Set.Make(String)] over token names and shares no code with it.

      Falsification. Every mutation below was applied, run and reverted, and
      the result recorded is the one observed.

        M1  In [Fixpoint.compute]'s FOLLOW walk, replace the [Zero_or_more] arm's
            separator case with [Kind.Set.union rest_first (alts_first c)].
            -> this law. FOLLOW disagrees on the separated and
               delimited-with-sep grammars.
        M2  In [Fixpoint.compute]'s FIRST, make the delimited arm return the
            body's FIRST in place of the open token.
            -> this law, and law_validate, law_manifest and law_facts, since
               the corpus grammars stop being accepted.
        M3  Make a block's FOLLOW omit [block_own_ops].
            -> this law. FOLLOW(E) disagrees on both pratt grammars.
        M4  Make [trailing_follow] the parent's FOLLOW for a delimited frame
            in place of its close token.
            -> this law and sexp_facts. FOLLOW reaches through every frame.
        M5  Force an expression block nullable.
            -> this law.

      One that reddens nothing, recorded as a finding.

        M6  In [block_own_ops], drop the separator of an enclosed postfix with
            [Many] content, so a call's comma leaves FOLLOW.
            -> nothing reddens, for two reasons. [Naive_first] carries the
               same clause written the same way, so the omission would have to
               be made twice for the cross-check to see it, which is a limit
               of the mechanism rather than a gap in the corpus. And the
               corpus has one grammar with a separated postfix call, whose
               element is the block itself, so no check that runs consults the
               FOLLOW that changed. The sampler law would catch it: a FOLLOW
               missing a separator makes recovery skip a comma a generated
               input holds.

      Coverage. The accepted corpus, and rules whose origin is [User] or
      [Pratt_block]. The naive side does not desugar, so it has no counterpart
      for a role rule. A role rule's FIRST is covered here by nothing; what
      covers it is that the pratt grammars are accepted, which requires only that
      it is non-empty.
   -------------------------------------------------------------------------- *)

module String_set = Set.Make (String)

(* The fast side's sets are kinds; the naive side's are token names. Compare
   in the naive side's alphabet: a kind that is a token becomes the token's
   name, and a kind that is not is a defect in itself. FIRST and FOLLOW hold
   terminals. *)
let to_token_names (f : Core.Facts.t) (set : Core.Kind.Set.t) : String_set.t =
  Core.Kind.Set.fold
    (fun k acc ->
       match Core.Facts.token_of_kind f k with
       | Some t -> String_set.add (Grammar.Name.Token.to_string t.Core.Token.name) acc
       | None ->
         Law.fail
           "a FIRST/FOLLOW set holds %s, which is not a token kind"
           (Core.Kind.Name.to_string (Core.Facts.kind_name f k));
         acc)
    set
    String_set.empty
;;

let show s = "{" ^ String.concat ", " (String_set.elements s) ^ "}"

let check_grammar ((name : string), (g : Core.Grammar.t)) : unit =
  match Core.Facts.of_grammar g with
  | Error _ -> Law.fail "%s: the corpus grammar was rejected" name
  | Ok f ->
    let naive = Naive_first.compute g in
    let get tbl r = Option.value ~default:String_set.empty (Hashtbl.find_opt tbl r) in
    Array.iter
      (fun (d : Core.Rule.def) ->
         match d.origin with
         | Core.Rule.Pratt_role _ -> ()
         | Core.Rule.User | Core.Rule.Pratt_block ->
           let fast_first = to_token_names f (Core.Facts.first_of f d.id)
           and fast_follow = to_token_names f (Core.Facts.follow_of f d.id)
           and fast_null = Core.Facts.is_nullable f d.id in
           let slow_first = get naive.first (Grammar.Name.Rule.to_string d.name)
           and slow_follow = get naive.follow (Grammar.Name.Rule.to_string d.name)
           and slow_null =
             Option.value
               ~default:false
               (Hashtbl.find_opt naive.nullable (Grammar.Name.Rule.to_string d.name))
           in
           if not (String_set.equal fast_first slow_first)
           then
             Law.fail
               "%s/%s FIRST: bitset %s vs naive %s"
               name
               (Grammar.Name.Rule.to_string d.name)
               (show fast_first)
               (show slow_first);
           if not (String_set.equal fast_follow slow_follow)
           then
             Law.fail
               "%s/%s FOLLOW: bitset %s vs naive %s"
               name
               (Grammar.Name.Rule.to_string d.name)
               (show fast_follow)
               (show slow_follow);
           if fast_null <> slow_null
           then
             Law.fail
               "%s/%s nullable: bitset %b vs naive %b"
               name
               (Grammar.Name.Rule.to_string d.name)
               fast_null
               slow_null;
           (* B3 / B4, stated as one-sided containments so the direction is
              on the record even where equality happens to hold. *)
           if not (String_set.subset slow_follow fast_follow)
           then
             Law.fail
               "%s/%s FOLLOW is too small"
               name
               (Grammar.Name.Rule.to_string d.name);
           if slow_null && not fast_null
           then
             Law.fail
               "%s/%s nullability is under-declared"
               name
               (Grammar.Name.Rule.to_string d.name))
      f.rules;
    Law.pass "%s: %d rules agree" name (Array.length f.rules)
;;

let () = List.iter check_grammar Corpus.all
let () = Law.summarise "law_first_follow"
