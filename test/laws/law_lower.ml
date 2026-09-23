(* -- the lowering -------------------------------------------------------------

      (a) [Check.run] accepts every plan [of_facts] builds.
      (b) Every message id in a plan indexes the catalogue beside it.
      (c) Every catalogue entry is named by at least one id in the plan.
      (d) Every constructor of [Ir.Plan.instr], and every form beside it, is
          one the corpus lowers to.

      Mechanism. Part (a) makes every invariant [Check] holds a post-condition
      of the lowering, rather than a fact about the plans that happen to be in
      test/expect. There are twelve of them and this law restates none.

      Parts (b) and (c) are one walk over the plan, collecting the ids it
      names, against the catalogue's length. The walk is this law's own: the
      lowering interns an id where it needs one, and this counts them off the
      plan afterwards.

      Part (c) is the one that bites. [Messages.Builder.intern] dedupes on the
      text, so an id no instruction uses disappears wherever its
      wording matches a live entry. It only shows up on wording that is
      unique, which is rare enough to survive a reading of the catalogue and
      not a law.

      Part (d) is the claim that a plan constructor nobody can build is one
      nobody should have written. test/laws/law_plan.ml counts forms too, and
      it counts them over a plan built by hand, so a constructor reaches its
      tally because someone typed it. This one counts over the grammars, so a
      form reads zero until a grammar exists that needs it.

      Coverage. Every grammar in lingo_grammars. A count prints beside the
      result, part (c) fails on the first entry nothing names, and part (d)
      names every form that reads zero.

      What this says nothing about. Whether the plan describes the grammar.
      That needs something that runs one, and test/laws/law_interp.ml is where
      that starts.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        M1  In [Lower.instr_of_child], intern a message for every child rather
            than for the ones that report.
            -> part (c), shapes: "nothing in the plan names the message 4".

            The first run of this reddened nothing. [intern] dedupes, and
            every wording the corpus leaked matched an entry something else
            named, so the leak disappeared into a live entry. shapes gives its
            optional child a wording of its own for this reason and for no
            other, and the mutation reddens now.
        M2  In [Lower.kset], give the kinds in descending order.
            -> part (a), 55 findings across the eight grammars: every set in
               a plan is read as ascending and distinct.

               This read 59 against six grammars once. It does not reproduce:
               the same mutation reads 49 over the seven this corpus held
               before [recovery] and 55 over the eight it holds now. A number
               carried over a corpus that grew, which is what re-running a
               record is for.
        M3  In [Lower.delimited_tail], intern the close's wording as before
            and name [Message.of_int 999] instead of the id that comes back.
            -> part (b), six grammars, and part (c) on the wording that is no
               longer named: 6 and 9 findings. rassoc and recovery have no
               delimited production and are untouched.
        M4  Add a form to part (d)'s list that no grammar lowers to.
            -> part (d), naming it. This is the mutation the part exists for:
               [Ir.Plan.Cannot_exit] sat in the plan for a while with no
               grammar able to build one and no parse able to act on one, and
               test/laws/law_plan.ml counted it as reached because its fixture
               is written by hand.

               Part (d) reads zero or not zero. A lowering that emits a form
               less often still passes, so it is a claim about the plan's
               surface rather than about how much of it the corpus uses.
   -------------------------------------------------------------------------- *)

let corpus =
  [ "sexp", Lingo_grammars.Sexp_grammar.grammar
  ; "json", Lingo_grammars.Json_grammar.grammar
  ; "calc", Lingo_grammars.Calc_grammar.grammar
  ; "rassoc", Lingo_grammars.Rassoc_grammar.grammar
  ; "postfix", Lingo_grammars.Postfix_grammar.grammar
  ; "shapes", Lingo_grammars.Shapes_grammar.grammar
  ; "unicode", Lingo_grammars.Unicode_grammar.grammar
  ; "recovery", Lingo_grammars.Recovery_grammar.grammar
  ]
;;

(* Every message id the plan names, wherever it names one. *)
let ids_in (p : Ir.Plan.t) =
  let seen = Hashtbl.create 32 in
  let note id = Hashtbl.replace seen (Ir.Message.to_int id) () in
  let rec instr (i : Ir.Plan.instr) =
    match i with
    | Open _ | Close | Trivia | Bump | Call _ | Pratt _ -> ()
    | Drain id -> note id
    | Expect e -> note e.message
    | Seq xs -> Array.iter instr xs
    | Alt a -> Array.iter (fun (_, body) -> instr body) a.arms
    | Commit c ->
      note c.message;
      instr c.body
    | Loop l ->
      Array.iter
        (fun (s : Ir.Plan.loop_state) ->
           (match s.exit with
            | Ir.Plan.May_exit -> ()
            | Ir.Plan.May_exit_reporting id -> note id);
           Option.iter (fun (m : Ir.Plan.missing) -> note m.message) s.when_missing;
           instr s.emits)
        l.states
  in
  Array.iter (fun (r : Ir.Plan.rule) -> instr r.body) p.rules;
  Array.iter
    (fun (b : Ir.Plan.block) ->
       note b.message;
       Array.iter (fun (q : Ir.Plan.postfix) -> instr q.body) b.postfix)
    p.blocks;
  seen
;;

(* One tally per plan form. A form the corpus never lowers to is a form no
   grammar reaches. *)
let built : (string, int) Hashtbl.t = Hashtbl.create 32

let saw (name : string) : unit =
  Hashtbl.replace built name (1 + Option.value (Hashtbl.find_opt built name) ~default:0)
;;

let forms =
  [ "seq"
  ; "open"
  ; "close"
  ; "trivia"
  ; "bump"
  ; "drain"
  ; "expect"
  ; "call"
  ; "pratt"
  ; "alt"
  ; "commit"
  ; "loop"
  ; "may-exit"
  ; "may-exit-reporting"
  ; "when-missing"
  ; "resume"
  ; "no-resume"
  ; "boundary"
  ; "postfix"
  ; "prefix"
  ; "infix"
  ; "atom-token"
  ; "atom-rule"
  ; "prefix-kind"
  ; "infix-kind"
  ]
;;

let rec tally (i : Ir.Plan.instr) =
  match i with
  | Open _ -> saw "open"
  | Close -> saw "close"
  | Trivia -> saw "trivia"
  | Bump -> saw "bump"
  | Call _ -> saw "call"
  | Pratt _ -> saw "pratt"
  | Drain _ -> saw "drain"
  | Expect _ -> saw "expect"
  | Seq xs ->
    saw "seq";
    Array.iter tally xs
  | Alt a ->
    saw "alt";
    Array.iter (fun (_, body) -> tally body) a.arms
  | Commit c ->
    saw "commit";
    saw (if c.resume = None then "no-resume" else "resume");
    tally c.body
  | Loop l ->
    saw "loop";
    Array.iter
      (fun (s : Ir.Plan.loop_state) ->
         (match s.exit with
          | Ir.Plan.May_exit -> saw "may-exit"
          | Ir.Plan.May_exit_reporting _ -> saw "may-exit-reporting");
         if s.when_missing <> None then saw "when-missing";
         tally s.emits)
      l.states
;;

let tally_plan (p : Ir.Plan.t) =
  Array.iter
    (fun (r : Ir.Plan.rule) ->
       if r.boundary then saw "boundary";
       tally r.body)
    p.rules;
  Array.iter
    (fun (b : Ir.Plan.block) ->
       if b.prefix_kind <> None then saw "prefix-kind";
       if b.infix_kind <> None then saw "infix-kind";
       if Array.length b.prefix > 0 then saw "prefix";
       if Array.length b.infix > 0 then saw "infix";
       Array.iter
         (fun (q : Ir.Plan.postfix) ->
            saw "postfix";
            tally q.body)
         b.postfix;
       Array.iter
         (fun (_, atom) ->
            match atom with
            | Ir.Plan.Atom_token -> saw "atom-token"
            | Ir.Plan.Atom_rule _ -> saw "atom-rule")
         b.atoms)
    p.blocks
;;

let () =
  let checked = ref 0 in
  let entries = ref 0 in
  List.iter
    (fun (name, g) ->
       match Core.Facts.of_grammar g with
       | Error _ -> Law.fail "%s: the grammar does not check" name
       | Ok f ->
         let plan, msgs = Plan.Lower.of_facts f in
         incr checked;
         tally_plan plan;
         entries := !entries + Plan.Messages.count msgs;
         (match Plan.Check.run plan with
          | Ok () -> ()
          | Error problems ->
            List.iter
              (fun p -> Law.fail "(a) %s: %a" name Plan.Check.pp_problem p)
              problems);
         let named = ids_in plan in
         let n = Plan.Messages.count msgs in
         Hashtbl.iter
           (fun id () ->
              if id < 0 || id >= n
              then
                Law.fail
                  "(b) %s names the message %d, and the catalogue holds %d"
                  name
                  id
                  n)
           named;
         for id = 0 to n - 1 do
           if not (Hashtbl.mem named id)
           then
             Law.fail
               "(c) %s: nothing in the plan names the message %d, %S"
               name
               id
               (Plan.Messages.text msgs (Ir.Message.of_int id))
         done)
    corpus;
  if Law.failures () = 0
  then
    Law.pass
      "every lowering checks, and its plan and catalogue agree, over %d grammars and %d \
       entries"
      !checked
      !entries
;;

let () =
  match List.filter (fun n -> not (Hashtbl.mem built n)) forms with
  | [] ->
    Law.pass
      "every plan form is one a grammar reaches (%s)"
      (String.concat
         " "
         (List.map (fun n -> Printf.sprintf "%s %d" n (Hashtbl.find built n)) forms))
  | missing ->
    Law.fail
      "(d) no grammar lowers to %s, so nothing in the corpus needs it"
      (String.concat ", " missing)
;;

let () = Law.summarise "law_lower"
