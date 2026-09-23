(* -- the manifest -------------------------------------------------------------

      (a) The kind numbering follows the documented order: production kinds
          first, hole kinds after token kinds, the four built-ins last. Every
          emitted kind integer follows from that order.
      (b) Every emitted name is an OCaml identifier, and one in a module scope
          starts with a capital.
      (c) An accepted grammar has no collisions left in any scope where a
          duplicate would stop the emitted code compiling.
      (d) The three names a backend emits for every grammar --
          [parse_tokens], [format_node], [format_generic] -- are manifest
          entries, so a user name reaching one collides.

      Mechanism: an oracle over the corpus for (a) to (c); a literal for (d).

      The law that matters most is not here: manifest = what a backend emits,
      cross-checked by parsing the emitted text back and classifying every
      name in it. It needs an emitter to read back and there is none, so the
      manifest's fidelity to emission is a claim. It is the claim here most
      likely to be wrong.

      Falsification. Every mutation below was applied, run and reverted, and
      the result recorded is the one observed.

        M1  In [Manifest.of_grammar], drop [builtin_entries].
            -> this law, part (d); and law_validate, since reserved-name's
               witness is then accepted.
        M2  In [Manifest.raw_kind_sources], move the hole kinds ahead of the
            token kinds.
            -> this law, part (a); and sexp_facts, which states the same fact
               as a literal.
        M3  In [Manifest.view_module], use [Mangle.snake_case] in place of
            [upper_first].
            -> this law, part (b). A view module name without a capital.

      One that was expected to redden this law and does not.

        M4  In [Check_names.collisions], skip the [View_accessor] scope.
            -> law_validate reddens; this law does not. The checker stops
               reporting the collision and [Manifest.collisions] still sees
               it, and part (c) reads the manifest. law_validate is what says
               the checker acts on what the manifest reports.

      An earlier part (a) compared [Manifest.kind_names] against [Kind.Table.names].
      The table is built from the list, so M2 reddened nothing against it.

      Coverage. The accepted corpus, and the witness grammars for (c).
   -------------------------------------------------------------------------- *)

let starts_upper s = s <> "" && s.[0] >= 'A' && s.[0] <= 'Z'

let () =
  List.iter
    (fun (name, g) ->
       match Core.Facts.of_grammar g with
       | Error _ -> Law.fail "%s: the corpus grammar was rejected" name
       | Ok f ->
         (* (a) is the numbering: the manifest's list, in the documented
            order. Checking [Manifest.kind_names = Kind.Table.names] would be
            checking nothing, since the table is built from the list and they
            are equal by construction. The order itself is what every emitted
            kind integer is a function of, so the order is what this reads. *)
         let listed = Core.Manifest.kind_names f.names in
         let prods = List.length g.Grammar.productions in
         let listed = List.map Core.Kind.Name.to_string listed in
         let index_of pred =
           List.filteri (fun _ n -> pred n) listed
           |> fun _ ->
           List.mapi (fun i n -> i, n) listed
           |> List.filter (fun (_, n) -> pred n)
           |> List.map fst
         in
         let ends_hole n =
           String.length n > 5 && String.sub n (String.length n - 5) 5 = "_HOLE"
         in
         let starts_t n = String.length n > 2 && String.sub n 0 2 = "T_" in
         (* The four built-ins sit at the end and two of them are token
            kinds, so they are excluded from the token-before-hole
            comparison and checked separately below. *)
         let n_listed = List.length listed in
         let user_only i = i < n_listed - 4 in
         let holes = List.filter user_only (index_of ends_hole)
         and tokens =
           List.filter user_only (index_of (fun n -> starts_t n && not (ends_hole n)))
         in
         let last_token = List.fold_left max (-1) tokens in
         let first_hole = List.fold_left min max_int holes in
         if first_hole < last_token
         then Law.fail "%s: a hole kind is numbered before a token kind" name
         else if
           List.filteri (fun i _ -> i < prods) listed
           <> List.map
                (fun (p : Grammar.production) ->
                   "N_"
                   ^ Core.Mangle.screaming_snake (Grammar.Name.Rule.to_string p.kind_name))
                g.Grammar.productions
         then
           Law.fail
             "%s: the production kinds are not the first block of the numbering"
             name
         else if
           List.filteri (fun i _ -> i >= List.length listed - 4) listed
           <> [ "T_ERROR"; "N_ERROR"; "N_MISSING"; "T_UNTERMINATED" ]
         then Law.fail "%s: the four built-in kinds are not last" name
         else
           Law.pass
             "%s: %d kinds, numbered in the documented order"
             name
             (List.length listed);
         (* (b) *)
         List.iter
           (fun (e : Core.Manifest.entry) ->
              let s = Core.Manifest.to_string e.emitted in
              if not (Core.Mangle.is_ident s)
              then Law.fail "%s: emitted name %S is not an identifier" name s
              else (
                match e.scope with
                | Core.Manifest.Scope.View_module when not (starts_upper s) ->
                  Law.fail "%s: view module %S does not start with a capital" name s
                | Core.Manifest.Scope.Kind_enum when not (starts_upper s) ->
                  Law.fail "%s: kind constructor %S does not start with a capital" name s
                | _ -> ()))
           (Core.Manifest.entries f.names);
         (* (c) *)
         let real =
           List.filter
             (fun (c : Core.Manifest.collision) ->
                Core.Manifest.Scope.fails_to_compile c.c_scope)
             (Core.Manifest.collisions f.names)
         in
         (match real with
          | [] ->
            Law.pass "%s: no collisions in any scope that would fail to compile" name
          | cs ->
            List.iter
              (fun (c : Core.Manifest.collision) ->
                 Law.fail
                   "%s: accepted grammar still collides on %s %S"
                   name
                   (Core.Manifest.Scope.name c.c_scope)
                   c.c_emitted)
              cs);
         (* (d) *)
         let emitted =
           List.map
             (fun (e : Core.Manifest.entry) -> Core.Manifest.to_string e.emitted)
             (List.filter
                (fun (e : Core.Manifest.entry) -> e.base = Core.Manifest.builtin)
                (Core.Manifest.entries f.names))
         in
         List.iter
           (fun expected ->
              if not (List.mem expected emitted)
              then
                Law.fail
                  "%s: %S is emitted for every grammar but is not a manifest entry, so a \
                   user name reaching it would not collide"
                  name
                  expected)
           [ "parse_tokens"; "format_node"; "format_generic" ])
    Corpus.all
;;

(* (c), the other direction: the collision machinery is what rejects the
   name-hygiene witnesses, so those grammars must have a collision. *)
let () =
  List.iter
    (fun (code, g) ->
       if
         List.mem
           code
           [ "dup-kind-name"; "reserved-name"; "name-collision"; "dup-child-name" ]
       then (
         let m = Core.Manifest.of_grammar g in
         match Core.Manifest.collisions m with
         | [] -> Law.fail "%s: the witness has no manifest collision to report" code
         | _ -> Law.pass "%s: the manifest sees the collision" code))
    Lingo_witness.Witnesses.all
;;

let () = Law.summarise "law_manifest"
