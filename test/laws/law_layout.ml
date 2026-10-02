(* -- the layout, and the fold over it ------------------------------------------

      (a) [Check.run] accepts every layout [of_facts] builds.
      (b) Law C: the output holds the tree's tokens, in order, and no others.
      (c) Law B: [format] is idempotent. Its own output formats to itself.
      (d) Law W: which tokens come out does not depend on the width.
      (e) Law F: every break the renderer declined sits on a line inside the
          ruler.
      (f) No text node holds a newline, so the column model is right about
          every byte.
      (g) Every step the fold can take is one the corpus reaches.
      (h) A corpus twice as deep reaches pairs of steps the first did not.

      Mechanism. Nine grammars. Two corpora for each: the inputs written in
      test/inputs, at four widths, and a corpus generated from those by editing
      their tokens, at two. Parts (b) to (f) run on every format. (h) needs a
      second sweep, so it runs under [LINGO_COVER=1] and nowhere else.

      The argument, which the corpus only checks. [format t] is [render (doc t)], and [doc] is a function of [t] alone. So
      idempotence is this: does [parse (format t)] give [t] back?

      The fold writes three things. The tokens of [t], in order, byte for byte,
      bar one; whitespace between them; and, at the end of a body whose frame
      the parse closed, one separator the layout names. The one it may leave
      out is that same separator, where the tree already holds it and the body
      lies flat.

      The first is not a claim about the code, it is its signature.
      [Layout.Written] is the only way bytes reach the document, and it has two
      constructors: a token the tree carries, and that separator. A fold that
      wanted to write anything else would not compile, and the one exception is
      spelled out so a reader looking for bytes the source did not have finds it
      by name. The whitespace is the glue, whose whole alphabet is a space and a
      line break.

      So [format t] holds the tokens of [t], one trailing separator either way.
      Every boundary's glue is checked against the grammar's own lexer, so the
      lexer takes those tokens back. [parse] then runs on that run and gives [t]
      back, with the separator where the next parse also puts one.

      Four things can break that, and they are what the parts watch:

      - the fold dropping a token. Part (b). Dropping changes the token run,
        which changes which frame the next parse gives the closer to, so a
        body's tail loses one separator per pass and never settles. The fold
        did that until 2026-09-19, on the grammar's [trailing_sep].

        The one exception is the trailing separator, and it is guarded by the
        same question the fold failed to ask. A body may drop it only where the parse
        closed the frame and the body up to its last element does not end inside
        a frame the parse left open. Without that second half [\[\[j,,\]]
        loses a comma a pass: the two commas sit in different frames, the outer
        closed and the inner not, and dropping the outer's hands its [\]] to
        the inner frame, which closes it and reveals the next.
      - the fold writing one it may not. The type holds the general case. What
        the type cannot say is *where*: a separator written into a frame the
        parse did not close lets the next parse take a token from outside the
        frame as an element, so the body grows by one on every pass. It also
        wrote the closing delimiter of such a frame until 2026-09-18, which made
        its output repaired input.
      - a boundary that does not read back. Part (b) again, through the join.
      - a break outliving the child that asked for it. A boundary in front of a
        child that is not there is not a boundary, so the request goes back where
        that child writes nothing. Carried on, it reaches a token in some other
        frame.

        A closer the parse never found is the same thing at a frame's own edge,
        and it needs the same treatment or the first half makes matters worse. It
        writes nothing, so its break cuts a run for no reason and the group that
        settles the line stops short.

      The argument is what to reason from. The corpus is what says the code does
      what the argument says it does.

      A separator the source already has. [On_break] writes one where the body
      breaks and takes it back where the body lies flat, so the separator the
      tree holds goes under the same conditional as the one the fold would have
      written. The source's is the fold's, and neither says anything about how
      the body was laid out.

      It has to be that way under [Handsome.Line]. Black's magic trailing comma
      is the other reading: a separator the source has is a request for a broken
      body. The fold wrote that separator itself the pass before, so under a fit
      rule that measures a node by a later one on its line, reading it back
      moves a decision that nothing moved on the pass that wrote it. A body
      forced broken measures its opener; one that chose to break measures its
      full width. The formatter then settles on the second pass rather than the
      first.

      Every other mark the fold makes only adds bytes, so every measurement
      grows and a group that broke goes on breaking. The request was the one
      thing that made a measurement shrink.

      [Always] is a terminator rather than a separator. It is the [;] after every
      statement in a block, the last one included, so a separator the source has
      carries no request and the body still lays out by width.

      One shape either way. Splitting the last run only when the fold adds a
      separator makes the document's shape depend on where the separator came
      from, and the glue then lands somewhere else on the pass after the one that
      added it. The two shapes are one document rather than two kept in step:
      [`Present] is [rest ~sep:false] and a flat [`Maybe] is the same call, so
      the property holds by construction rather than by agreement.

      The parse and its trivia. Six formats of 255,756 could not settle until 2026-09-18, alternating
      between two outputs holding the same tokens and parsing to two different
      shapes.

      The body loop's progress guard read [Cursor.position], the raw index with
      trivia counted. [Recover.expect] emits the trivia in front of the token it
      looks for, so a step that took no token still moved that index and the loop
      carried on. With no space there it stopped. Two spacings, two trees, on 382
      of 115,729 inputs.

      The guard now reads [Cursor.meaningful_position], and a step that takes no
      token but moves the loop's state counts as progress in its own right. That
      second half is what [loop-missing] needs: the position alone ends the body
      at a missing separator and json's ["\[1 2\]"] stops keeping the [2]. The
      number of states bounds how many such steps can run before one takes a
      token. Measured after: 0 of 462,529 inputs parse differently when
      respaced.

      What the corpus cannot reach. Three things the parts here read zero on,
      and test/laws/law_fuzz.ml is what falsifies all of them.

      A run that lands on a group's line and sits outside it. The head of a
      frame is empty unless something sits before the opener, and the only
      production shape that has one is an enclosed postfix operator whose
      operand is a group. Nothing written by hand in test/inputs puts a long
      enough postfix chain at a narrow enough width, and a swept input mangles
      the chain before it gets there.

      A separator coming back somewhere the fold cannot see it. Recovery
      sweeps one into the error node at a body's end, and a frame the parse
      left open takes one as its own trailing separator. Only law_fuzz's
      fragments reach either, and only at depth.

      The traces are part of part (g) rather than beside it. [say] reads as
      logging and is not: the step counts are what say the corpus reached a
      case, so a dropped trace goes red. They are the instrument, and an
      instrument nothing watches says less than it reads.

      What the parts say. That the fold is correct on this corpus. Not that it
      is good, and not that the corpus is the whole of what the fold has to be
      right about. No mutation of the fold reddens (h): that is a claim about
      the corpus, and what would falsify it is a generator that stops
      exploring.

   -------------------------------------------------------------------------- *)

(* The four blocks below are generated, and they are the evidence. assay
   derives a mutation from the code rather than from a sentence beside it,
   applies every one, and records what went red. Regenerate them with

     assay -config assay.conf -only layout,lingo_runtime

   and take the counts as they come: they move whenever the corpus grows, and
   asserting them exactly would train everyone to ignore a red suite. What
   they assert is that every mutant dies. A survivor is the finding, and the
   lines it names are where to look. *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/runtime/layout.ml, 304 mutants, 233 killed, 67 survived, 4 timed out.
        extreme     12  11 killed, dump_format: 4, width 11: 4, law_layout (g): 2, law_layout (c): 1; 1 survived
        sbr         27  25 killed, law_layout (g): 15, width 11: 4, dump_format: 3, width 23: 2, width: 1; 2 survived
        ror         97  58 killed, dump_format: 34, width 11: 9, width: 5, law_layout (c): 4, width 23: 4, law_format: 1, law_layout (g): 1; 39 survived
        lcr         42  28 killed, dump_format: 16, law_layout (c): 4, width: 3, width 23: 3, width 11: 2; 14 survived
        aor         46  38 killed, dump_format: 23, width: 12, width 11: 2, law_layout (b): 1; 5 survived; 3 timed out
        uoi         80  73 killed, dump_format: 42, width 23: 14, width 11: 10, width: 4, law_layout (c): 2, law_layout (g): 1; 6 survived; 1 timed out
      survived in stronger (ror), join (lcr), glue (aor 2), all_space (ror 2),
        delimiters (ror 3, aor), entries (sbr, ror 16, lcr 9, aor, uoi 3),
        silent (extreme, ror), nest_of (ror),
        node (sbr, ror 15, lcr 4, aor, uoi 3)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/layout/check.ml, 58 mutants, 57 killed, 1 survived.
        sbr         20  all killed, (i) 20
        ror         17  16 killed, dump_layout: 8, law_layout (i): 5, law_layout: 2, law_layout (a): 1; 1 survived
        lcr         10  all killed, dump_layout: 8, law_layout (i): 2
        aor          3  all killed, dump_layout: 3
        uoi          8  all killed, dump_layout: 8
      survived in run (ror)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/layout/lower.ml, 25 mutants, 23 killed, 2 survived.
        extreme      2  all killed, dump_format: 2
        sbr         13  12 killed, dump_format: 9, dump_layout: 2, width 11: 1; 1 survived
        ror          5  4 killed, dump_format: 4; 1 survived
        uoi          5  all killed, dump_format: 5
      survived in operator_slots (sbr, ror)
   ---------------------------------------------------------------------- *)

(* -- mutation testing, generated by assay on 2026-10-02 ---------------------
      lib/layout/text.ml, 23 mutants, 21 killed, 2 survived.
        extreme      6  all killed, dump_layout: 6
        sbr         15  13 killed, dump_layout: 13; 2 survived
        uoi          2  all killed, dump_layout: 2
      survived in frame (sbr 2)
   ---------------------------------------------------------------------- *)

(* -- the corpus ------------------------------------------------------------ *)

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; inputs : Inputs.t
  }

let corpus =
  [ { name = "sexp"; grammar = Lingo_grammars.Sexp_grammar.grammar; inputs = Inputs.sexp }
  ; { name = "json"; grammar = Lingo_grammars.Json_grammar.grammar; inputs = Inputs.json }
  ; { name = "calc"; grammar = Lingo_grammars.Calc_grammar.grammar; inputs = Inputs.calc }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; inputs = Inputs.rassoc
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; inputs = Inputs.postfix
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; inputs = Inputs.unicode
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; inputs = Inputs.recovery
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; inputs = Inputs.shapes
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; inputs = Inputs.comments
    }
  ; { name = "rust"; grammar = Lingo_grammars.Rust_grammar.grammar; inputs = Inputs.rust }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    }
  ; { name = "wide"; grammar = Lingo_grammars.Wide_grammar.grammar; inputs = Inputs.wide }
  ; { name = "ml"; grammar = Lingo_grammars.Ml_grammar.grammar; inputs = Inputs.ml }
  ]
;;

(* -- what the corpus reaches ----------------------------------------------- *)

let reach : (string, int) Hashtbl.t = Hashtbl.create 32

let ran ~(step : string) ~kind:(_ : Ir.Kind.t) : unit =
  Hashtbl.replace reach step (1 + Option.value (Hashtbl.find_opt reach step) ~default:0)
;;

(* Whether the generated corpus is explored or only resampled.

   An edge is a transition rather than a single step. The number of steps the
   fold has is a property of the layout, so counting those saturates at once
   and shows nothing about depth.

   Counting pairs of them is not enough either, and that was this law's own
   reading until 2026-09-19: 97 pairs at depth 1, 99 at 4 and 100 at 16,
   with the ceiling at fifteen steps squared. Depth 8 found two defects and
   depth 64 a third while the count stood still. So the step is paired with
   the rule it was taken in, which is where the grammar enters. A point is a
   grammar, a step and a rule kind, and an edge is two consecutive points.
   That reads 1,544 at depth 1, 1,639 at 4, 1,702 at 16, 1,748 at 64 and 1,772
   at 128.

   The role of the child was tried as a step of its own and made the instrument
   worse: 1,574 at depth 1 and 1,684 at 128. A step between every pair collapses
   the pairs that used to be adjacent, so a role belongs in the point or nowhere.

   What remains is a property of having nine grammars. The reachable set over a
   fixed set of them is finite, and what widens it is a sampler over grammars.

   The predecessor ran ten sweeps of a million before anyone noticed its own
   instrument held constant. Off by default, because it costs a hashtable write
   per step; [LINGO_COVER=1] turns it on. *)
let covering = Sys.getenv_opt "LINGO_COVER" = Some "1"

type point = string * string * Ir.Kind.t

let edges : (point * point, int) Hashtbl.t = Hashtbl.create 1024
let nowhere : point = "", "", -1
let previous = ref nowhere
let grammar = ref ""

let stepped ~(step : string) ~(kind : Ir.Kind.t) : unit =
  let p = !grammar, step, kind in
  let e = !previous, p in
  Hashtbl.replace edges e (1 + Option.value (Hashtbl.find_opt edges e) ~default:0);
  previous := p
;;

let steps =
  [ "join-touching"
  ; "join-blank"
  ; "join-newline"
  ; "break-flat"
  ; "break-fit"
  ; "break-hard"
  ; "held-same"
  ; "held-line"
  ; "stray"
  ; "repeat"
  ; "sep-on-break"
  ; "sep-always"
  ; "unruled"
  ; "absent"
  ; "break-back"
  ; "split"
  ; "new-line"
  ; "blank-kept"
  ; "lead-sep"
  ; "lead-sep-owned"
  ]
;;

(* -- reading a tree -------------------------------------------------------- *)

(* The non-trivia token texts, left to right. Read off the tree rather than by
   lexing a string, so this needs nothing from the lexer that part (b) is
   about. *)
let meaningful (l : Ir.Layout.t) (n : Siesta.Green.node) =
  let acc = ref [] in
  let rec go (n : Siesta.Green.node) =
    Array.iter
      (function
        | Siesta.Green.Node m -> go m
        | Siesta.Green.Token t ->
          let k = Siesta.Green.Token.kind t in
          let trivia =
            match l.tokens.(k) with
            | Some t -> t.trivia
            | None -> None
          in
          if trivia = None then acc := Siesta.Green.Token.text t :: !acc)
      (Siesta.Green.children_array n)
  in
  go n;
  List.rev !acc
;;

(* The separator texts a body's policy may add. There is one per frame that has
   one, and it is the only byte string the fold writes of its own accord. *)
let separators (l : Ir.Layout.t) =
  Array.fold_left
    (fun acc (r : Ir.Layout.rule) ->
       match r.frame with
       | Delimited { sep = Some s; _ } | Separated s -> s.text :: acc
       | Delimited { sep = None; _ } | Plain -> acc)
    []
    l.rules
;;

(* Every token of [before] is in [after], in the same order, but for the
   separator a body's policy governs. In a frame the parse closed, [after] may
   hold one the tree does not and may drop one the tree holds. Nothing else
   moves.

   Dropping is the half that is new, and it is what this law gives up. A
   separator at either end of a body under [On_break] is written where the
   body breaks and taken back where it does not, so that the separator says
   nothing about the layout. A formatter that reads back a mark it wrote itself cannot settle
   under a fit rule that measures one node by a later one. So the claim weakens
   from "no token is lost" to "no token but a separator is lost". *)
let rec keeps ~(seps : string list) (before : string list) (after : string list) : bool =
  match before, after with
  | [], [] -> true
  | x :: b, y :: a when String.equal x y -> keeps ~seps b a
  | b, y :: a when List.mem y seps -> keeps ~seps b a
  | x :: b, a when List.mem x seps -> keeps ~seps b a
  | _ -> false
;;

let first_difference ~(seps : string list) (before : string list) (after : string list)
  : string
  =
  let rec go i b a =
    match b, a with
    | [], [] -> "?"
    | x :: b, y :: a when String.equal x y -> go (i + 1) b a
    | b, y :: a when List.mem y seps -> go i b a
    | x :: b, a when List.mem x seps -> go (i + 1) b a
    | x :: _, y :: _ -> Printf.sprintf "token %d is %S and came back %S" i x y
    | x :: _, [] -> Printf.sprintf "token %d is %S and did not come back" i x
    | [], y :: _ -> Printf.sprintf "%S came back and was never written" y
  in
  go 0 before after
;;

(* The same tokens, in the same order, the separators a policy may add aside. *)
let agree ~(seps : string list) (before : string list) (after : string list) : bool =
  let without = List.filter (fun t -> not (List.mem t seps)) in
  List.equal String.equal (without before) (without after)
;;

(* The tree's shape, with trivia left out. Two strings holding the same
   meaningful tokens have to give the same shape, or a formatter cannot settle:
   it writes the same tokens every time and the spacing is all it may change. *)
let shape (l : Ir.Layout.t) (n : Siesta.Green.node) =
  let b = Buffer.create 64 in
  let rec go (n : Siesta.Green.node) =
    Buffer.add_string b ("(" ^ string_of_int (Siesta.Green.kind n));
    Array.iter
      (function
        | Siesta.Green.Node m -> go m
        | Siesta.Green.Token t ->
          let k = Siesta.Green.Token.kind t in
          let trivia =
            match l.tokens.(k) with
            | Some t -> t.trivia <> None
            | None -> false
          in
          if not trivia then Buffer.add_string b (" " ^ Siesta.Green.Token.text t))
      (Siesta.Green.children_array n);
    Buffer.add_string b ")"
  in
  go n;
  Buffer.contents b
;;

(* -- the runs -------------------------------------------------------------- *)

(* How deep the generated half runs. One is what the suite runs, and it is not
   deep enough to mean anything on its own: the predecessor's idempotence
   defect read zero at 100,000 inputs for months and only appeared once its
   sweep went to a million. So the depth is a parameter, the suite runs it at
   1, and the record below says what a deep run found. *)
let depth =
  match Sys.getenv_opt "LINGO_SWEEP" with
  | None -> 1
  | Some s ->
    (try int_of_string s with
     | _ -> 1)
;;

(* A format that has not settled after this many passes is one that never
   will. The predecessor's never did: it added two columns per pass. *)
let passes_allowed = 8
let hand_widths = [ 80; 40; 20; 8 ]
let swept_widths = [ 80; 20 ]

let () =
  let hand = ref 0 in
  let swept = ref 0 in
  let recovered = ref 0 in
  let lossy = ref [] in
  let unstable = ref [] in
  let variant = ref [] in
  let newlines = ref [] in
  let past_ruler = ref [] in
  List.iter
    (fun c ->
       match Core.Facts.of_grammar c.grammar with
       | Error _ -> Law.fail "%s: the grammar does not check" c.name
       | Ok f ->
         grammar := c.name;
         let l = Layout.Lower.of_facts f in
         (match Layout.Check.run l with
          | Ok () -> ()
          | Error ps ->
            List.iter (fun p -> Law.fail "(a) %s: %a" c.name Layout.Check.pp_problem p) ps);
         let plan, _ = Plan.Lower.of_facts f in
         let entry = plan.Ir.Plan.roots.(0) in
         let seps = separators l in
         let boundary = Lex.boundary f in
         let parse src = Interp.run plan entry (Lex.run f src) in
         (* [trace] costs a hashtable write per step, so only the hand corpus
            pays it. The coverage claim is about that corpus; the generated one is
            about volume. *)
         let check ~widths ~trace ~count src =
           let tree, diags = parse src in
           let clean = diags = [] in
           if not clean then incr recovered;
           let before = meaningful l tree in
           let at_width width =
             let d =
               if trace
               then Lingo_runtime.Layout.doc ~trace:ran l ~boundary tree
               else if covering
               then (
                 previous := nowhere;
                 Lingo_runtime.Layout.doc ~trace:stepped l ~boundary tree)
               else Lingo_runtime.Layout.doc l ~boundary tree
             in
             incr count;
             (match Handsome.Utf8.check d with
              | Ok () -> ()
              | Error es -> newlines := (c.name, src, List.length es) :: !newlines);
             let stream, res =
               Handsome.Utf8.render ~fit:Lingo_runtime.Layout.fit ~width d
             in
             let lines = Handsome.Utf8.lines stream in
             List.iter
               (fun (ln, _) ->
                  if ln < Array.length lines && lines.(ln) > width
                  then past_ruler := (c.name, src, width, ln, lines.(ln)) :: !past_ruler)
               res.declined;
             let once = Handsome.Utf8.to_string stream in
             let again = fst (parse once) in
             (* [format] is idempotent: its own output formats to itself.
                [once] is already [format] of something, so anything but an
                equality here is the fold changing what it wrote the first
                time. *)
             let twice = Lingo_runtime.Layout.format l ~boundary ~width again in
             if not (String.equal once twice)
             then unstable := (c.name, src, width, once, twice) :: !unstable;
             let after = meaningful l again in
             if not (keeps ~seps before after)
             then
               lossy
               := (c.name, src, width, first_difference ~seps before after) :: !lossy;
             after
           in
           match List.map at_width widths with
           | [] -> ()
           | first :: rest ->
             List.iter2
               (fun w toks ->
                  if not (agree ~seps first toks)
                  then variant := (c.name, src, w) :: !variant)
               (List.tl widths)
               rest
         in
         let seeds = Inputs.all c.inputs in
         List.iter (check ~widths:hand_widths ~trace:true ~count:hand) seeds;
         List.iter
           (check ~widths:swept_widths ~trace:false ~count:swept)
           (Sweep.inputs ~depth f seeds))
    corpus;
  let formats = !hand + !swept in
  if Law.failures () = 0
  then Law.pass "every layout the lowering builds passes its own check";
  (match !newlines with
   | [] -> Law.pass "(f) no text node holds a newline, over %d documents" formats
   | bad ->
     List.iter
       (fun (g, src, n) -> Law.fail "(f) %s on %S: %d text nodes hold a newline" g src n)
       (List.filteri (fun i _ -> i < 10) bad);
     Law.fail "(f) %d documents hold a newline in a text node" (List.length bad));
  (match !lossy with
   | [] -> Law.pass "(b) every token reaches the output, over %d formats" formats
   | bad ->
     List.iter
       (fun (g, src, w, d) -> Law.fail "(b) %s on %S at %d: %s" g src w d)
       (List.filteri (fun i _ -> i < 10) bad);
     Law.fail "(b) %d formats lost or gained a token" (List.length bad));
  (match !unstable with
   | [] ->
     Law.pass
       "(c) format is idempotent, over %d formats: %d written here and %d generated at \
        depth %d, from %d trees that recovered"
       formats
       !hand
       !swept
       depth
       !recovered
   | bad ->
     List.iter
       (fun (g, src, w, once, twice) ->
          Law.fail "(c) %s on %S at %d:\n  once  %S\n  twice %S" g src w once twice)
       (List.filteri (fun i _ -> i < 10) bad);
     Law.fail "(c) %d formats are not idempotent" (List.length bad));
  (match !variant with
   | [] -> Law.pass "(d) the tokens do not depend on the width"
   | bad ->
     List.iter
       (fun (g, src, w) -> Law.fail "(d) %s on %S differs at width %d" g src w)
       (List.filteri (fun i _ -> i < 10) bad);
     Law.fail "(d) %d inputs vary with the width" (List.length bad));
  match !past_ruler with
  | [] -> Law.pass "(e) every declined break sits inside the ruler"
  | bad ->
    List.iter
      (fun (g, src, w, ln, got) ->
         Law.fail "(e) %s on %S at %d: line %d is %d wide" g src w ln got)
      (List.filteri (fun i _ -> i < 10) bad);
    Law.fail
      "(e) %d lines past the ruler had a break the printer declined"
      (List.length bad)
;;

(* -- (h) the search is searching ------------------------------------------- *)

(* The coverage count's other half. A count is only evidence while it is still
   moving, so the
   corpus is taken to twice the depth and has to reach pairs the first pass did
   not. A count that stops is measuring the fold's vocabulary.

   Only the folding runs here. The laws above have already read this corpus at
   [depth], and reading it again at [depth * 2] would double the suite for a
   question about the instrument. *)
let () =
  if covering
  then (
    let shallow = Hashtbl.length edges in
    List.iter
      (fun c ->
         match Core.Facts.of_grammar c.grammar with
         | Error _ -> ()
         | Ok f ->
           grammar := c.name;
           let l = Layout.Lower.of_facts f in
           let plan, _ = Plan.Lower.of_facts f in
           let entry = plan.Ir.Plan.roots.(0) in
           let boundary = Lex.boundary f in
           let seeds = Inputs.all c.inputs in
           List.iter
             (fun src ->
                let tree, _ = Interp.run plan entry (Lex.run f src) in
                previous := nowhere;
                ignore (Lingo_runtime.Layout.doc ~trace:stepped l ~boundary tree))
             (Sweep.inputs ~depth:(depth * 2) f seeds))
      corpus;
    let deep = Hashtbl.length edges in
    Printf.printf
      "COVER depth %d: %d edges; and %d after depth %d\n"
      depth
      shallow
      deep
      (depth * 2);
    if deep <= shallow
    then
      Law.fail
        "(h) depth %d reached no edge depth %d had not, over %d"
        (depth * 2)
        depth
        shallow
    else
      Law.pass
        "(h) depth %d adds %d edges to depth %d's %d"
        (depth * 2)
        (deep - shallow)
        depth
        shallow)
;;

let () =
  let missing = List.filter (fun n -> not (Hashtbl.mem reach n)) steps in
  if missing <> []
  then
    Law.fail
      "(g) the fold never took the step %s, so this law says nothing about it"
      (String.concat ", " missing)
  else
    Law.pass
      "every step the fold takes was taken (%s)"
      (String.concat
         " "
         (List.map (fun n -> Printf.sprintf "%s %d" n (Hashtbl.find reach n)) steps))
;;

(* -- (i) every check is checked where it stands ---------------------------- *)

(* Part (a) says [Check.run] accepts every layout the lowering builds. That is
   half a claim: a checker that accepted everything would pass it too.

   The other half is that it rejects, and one broken layout per problem it can
   name is still not enough. It raises each of those from several places, so a
   layout aimed at one of them proves only that the checker can say the word.
   Thirty-six mutations to lib/layout/check.ml survived the whole suite before
   this part existed, twenty-five of them a [report] call deleted outright.

   So the broken layouts here are derived. The walk breaks one thing at a time
   in a real layout and the law asserts the checker names it. A field added to
   [Ir.Layout] is covered the day it appears. *)
let map_rule (l : Ir.Layout.t) (i : int) ~(f : Ir.Layout.rule -> Ir.Layout.rule)
  : Ir.Layout.t
  =
  { l with rules = Array.mapi (fun j r -> if j = i then f r else r) l.rules }
;;

let map_slot (r : Ir.Layout.rule) (j : int) ~(f : Ir.Layout.slot -> Ir.Layout.slot)
  : Ir.Layout.rule
  =
  { r with slots = Array.mapi (fun k s -> if k = j then f s else s) r.slots }
;;

let is_kind_out_of_range (p : Layout.Check.problem) : bool =
  match p with
  | Layout.Check.Kind_out_of_range _ -> true
  | _ -> false
;;

let is_empty_break (p : Layout.Check.problem) : bool =
  match p with
  | Layout.Check.Empty_break _ -> true
  | _ -> false
;;

(* One broken layout per place the checker looks, with the problem it owes. *)
let variants (l : Ir.Layout.t)
  : (string * Ir.Layout.t * (Layout.Check.problem -> bool)) list
  =
  let out = ref [] in
  let add name broken owed = out := (name, broken, owed) :: !out in
  let negative = -1 in
  (* One past the end as well as below it. A bounds test is two comparisons
     and only one of them is reached from below. *)
  let past_end = Array.length l.tokens in
  let a_token =
    let found = ref None in
    Array.iteri (fun k t -> if !found = None && t <> None then found := Some k) l.tokens;
    !found
  in
  Array.iteri
    (fun k _ ->
       let of_kind = Array.copy l.of_kind in
       of_kind.(k) <- Array.length l.rules;
       add (Printf.sprintf "of_kind %d out of range" k) { l with of_kind } (fun p ->
         match p with
         | Layout.Check.Rule_out_of_range _ -> true
         | _ -> false))
    l.of_kind;
  Array.iteri
    (fun i (r : Ir.Layout.rule) ->
       let at = Printf.sprintf "rule %d %s" i r.name in
       add
         (at ^ " kind")
         (map_rule l i ~f:(fun r -> { r with kind = negative }))
         is_kind_out_of_range;
       add
         (at ^ " kind past the end")
         (map_rule l i ~f:(fun r -> { r with kind = past_end }))
         is_kind_out_of_range;
       (* A rule the fold cannot be reached by is a rule that never runs, so
          the kind has to index back to this rule and not to another one. *)
       if Array.length l.rules > 1
       then (
         let other = (i + 1) mod Array.length l.rules in
         let theirs = l.rules.(other).Ir.Layout.kind in
         if theirs <> r.kind && theirs >= 0 && theirs < Array.length l.of_kind
         then
           add
             (at ^ " kind is another rule's")
             (map_rule l i ~f:(fun r -> { r with kind = theirs }))
             (fun p ->
                match p with
                | Layout.Check.Kind_not_its_rule _ -> true
                | _ -> false));
       (match a_token with
        | None -> ()
        | Some k ->
          add
            (at ^ " kind is a token")
            (map_rule l i ~f:(fun r -> { r with kind = k }))
            (fun p ->
               match p with
               | Layout.Check.Rule_kind_is_a_token _ -> true
               | _ -> false));
       add
         (at ^ " indent")
         (map_rule l i ~f:(fun r -> { r with indent = -1 }))
         (fun p ->
            match p with
            | Layout.Check.Negative_indent _ -> true
            | _ -> false);
       add
         (at ^ " body break")
         (map_rule l i ~f:(fun r -> { r with body = Ir.Layout.Hard 0 }))
         is_empty_break;
       add
         (at ^ " inner break")
         (map_rule l i ~f:(fun r -> { r with inner = Ir.Layout.Hard 0 }))
         is_empty_break;
       (match r.frame with
        | Ir.Layout.Plain -> ()
        | Ir.Layout.Delimited d ->
          add
            (at ^ " open")
            (map_rule l i ~f:(fun r ->
               { r with frame = Ir.Layout.Delimited { d with open_ = negative } }))
            is_kind_out_of_range;
          add
            (at ^ " close")
            (map_rule l i ~f:(fun r ->
               { r with frame = Ir.Layout.Delimited { d with close = negative } }))
            is_kind_out_of_range;
          (match d.sep with
           | None -> ()
           | Some s ->
             add
               (at ^ " delimited sep")
               (map_rule l i ~f:(fun r ->
                  { r with
                    frame =
                      Ir.Layout.Delimited
                        { d with sep = Some { s with sep_kind = negative } }
                  }))
               is_kind_out_of_range)
        | Ir.Layout.Separated s ->
          add
            (at ^ " separated sep")
            (map_rule l i ~f:(fun r ->
               { r with frame = Ir.Layout.Separated { s with sep_kind = negative } }))
            is_kind_out_of_range);
       Array.iteri
         (fun j (s : Ir.Layout.slot) ->
            let at = Printf.sprintf "%s slot %d" at j in
            if Array.length s.kinds > 0
            then (
              add
                (at ^ " kind")
                (map_rule l i ~f:(fun r ->
                   map_slot r j ~f:(fun s ->
                     let kinds = Array.copy s.kinds in
                     kinds.(0) <- negative;
                     { s with kinds })))
                is_kind_out_of_range;
              add
                (at ^ " kind past the end")
                (map_rule l i ~f:(fun r ->
                   map_slot r j ~f:(fun s ->
                     let kinds = Array.copy s.kinds in
                     kinds.(0) <- past_end;
                     { s with kinds })))
                is_kind_out_of_range;
              if Array.length s.kinds > 1
              then
                add
                  (at ^ " kinds ascending")
                  (map_rule l i ~f:(fun r ->
                     map_slot r j ~f:(fun s ->
                       let kinds = Array.copy s.kinds in
                       kinds.(1) <- kinds.(0);
                       { s with kinds })))
                  (fun p ->
                     match p with
                     | Layout.Check.Kinds_unordered _ -> true
                     | _ -> false));
            add
              (at ^ " before break")
              (map_rule l i ~f:(fun r ->
                 map_slot r j ~f:(fun s -> { s with before = Ir.Layout.Hard 0 })))
              is_empty_break;
            add
              (at ^ " between break")
              (map_rule l i ~f:(fun r ->
                 map_slot r j ~f:(fun s -> { s with between = Ir.Layout.Hard 0 })))
              is_empty_break;
            if j = 0 && s.before = Ir.Layout.Flat
            then
              add
                (at ^ " lead break")
                (map_rule l i ~f:(fun r ->
                   map_slot r j ~f:(fun s -> { s with before = Ir.Layout.Fit })))
                (fun p ->
                   match p with
                   | Layout.Check.Lead_slot_breaks _ -> true
                   | _ -> false);
            if (not s.repeats) && s.between = s.before && s.before <> Ir.Layout.Fit
            then
              add
                (at ^ " single slot between")
                (map_rule l i ~f:(fun r ->
                   map_slot r j ~f:(fun s -> { s with between = Ir.Layout.Fit })))
                (fun p ->
                   match p with
                   | Layout.Check.Single_slot_between _ -> true
                   | _ -> false))
         r.slots)
    l.rules;
  List.rev !out
;;

let () =
  let places = ref 0 in
  let missed = ref [] in
  List.iter
    (fun c ->
       match Core.Facts.of_grammar c.grammar with
       | Error _ -> ()
       | Ok f ->
         let l = Layout.Lower.of_facts f in
         List.iter
           (fun (where, broken, owed) ->
              incr places;
              let named =
                match Layout.Check.run broken with
                | Ok () -> false
                | Error ps -> List.exists owed ps
              in
              if not named then missed := Printf.sprintf "%s %s" c.name where :: !missed)
           (variants l))
    corpus;
  match List.rev !missed with
  | [] ->
    Law.pass
      "(i) every invariant is reported wherever it can be broken, over %d places"
      !places
  | wheres -> List.iter (fun w -> Law.fail "(i) breaking %s is not reported" w) wheres
;;

let () = Law.summarise "law_layout"
