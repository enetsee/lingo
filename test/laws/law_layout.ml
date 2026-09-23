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

      The fold writes three things. The tokens of [t], in order, byte for byte;
      whitespace between them; and, at the end of a body whose frame the parse
      closed, one separator the layout names.

      The first is not a claim about the code, it is its signature.
      [Layout.Written] is the only way bytes reach the document, and it has two
      constructors: a token the tree carries, and that separator. A fold that
      wanted to write anything else would not compile, and the one exception is
      spelled out so a reader looking for bytes the source did not have finds it
      by name. The whitespace is the glue, whose whole alphabet is a space and a
      line break.

      So [format t] holds the tokens of [t] and at most one separator more. Every
      boundary's glue is checked against the grammar's own lexer, so the lexer
      takes those tokens back. [parse] then runs on that run and gives [t] back,
      with the separator where the next parse also puts one.

      Four things can break that, and they are what the parts watch:

      - the fold dropping a token. Part (b), and there is no exception: dropping
        changes the token run, which changes which frame the next parse gives the
        closer to, so a body's tail loses one separator per pass and never
        settles. The fold did that until 2026-09-19, on the grammar's
        [trailing_sep]. M6.
      - the fold writing one it may not. The type holds the general case. What
        the type cannot say is *where*: a separator written into a frame the
        parse did not close lets the next parse take a token from outside the
        frame as an element, so the body grows by one on every pass. M12. It also
        wrote the closing delimiter of such a frame until 2026-09-18, which made
        its output repaired input. M5.
      - a boundary that does not read back. Part (b) again, through the join.
      - a break outliving the child that asked for it. A boundary in front of a
        child that is not there is not a boundary, so the request goes back where
        that child writes nothing. Carried on, it reaches a token in some other
        frame. M17.

        A closer the parse never found is the same thing at a frame's own edge,
        and it needs the same treatment or the first half makes matters worse. It
        writes nothing, so its break cuts a run for no reason and the group that
        settles the line stops short. M18.

      The argument is what to reason from. The corpus is what says the code does
      what the argument says it does.

      A separator the source already has. [On_break] writes a separator where the body breaks. So a separator the
      source has there is a request for a broken body, and the fold breaks it.
      That is the only way a grammar's user can make one directly, and it is
      Black's magic trailing comma.

      It also has to be the case. The fold cannot drop the separator, so the
      alternative is a flat body carrying a trailing one, which reads badly.

      [Always] is a terminator rather than a separator. It is the [;] after every
      statement in a block, the last one included, so a separator the source has
      carries no request and the body still lays out by width.

      One shape either way. Splitting the last run only when the fold adds a
      separator makes the document's shape depend on where the separator came
      from, and the glue then lands somewhere else on the pass after the one that
      added it. M15.

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

      Falsification. Every mutation was applied, built, run and reverted, and
      the result recorded is the one observed. A count of formats counts
      distinct (input, width) pairs. Depth is 1 unless the entry says
      otherwise, and where it says 8 the mutation reads zero at 1.

        M1  In [Layout.join], give [Touching] at every boundary.
            -> (b) 877 formats, (c) 34, (d) 22 inputs, (g) two joins read zero.
               comments' ["1 . 1"] comes out ["1.1"] and reads back as one
               number. This is the law for the whole of D9.
        M2  In [Layout.glue], take the join over the last byte written rather
            than over the run.
            -> (b) 877 formats, (c) 256, (d) 22 inputs, (g) one join reads zero.
               No pair of ["1"], ["."] and ["5"] joins and the three of them are
               one number, so a boundary stated over the previous token alone
               reads safe.
        M3  In [Layout.entries], take a held child's break from the layout.
            -> (g) both holds read zero, and nothing else. recovery.format moves
               by 46 lines, comments.format by 128 and shapes.format by 2. A comment is held to the
               line the source put it on; taking the break from the layout is
               legible and idempotent, so only the goldens carry it.
        M4  In [Layout.walk], hand the last child of a run [nothing] as its tail.
            -> (b) 91,082 formats, (c) 33,719, (g) both separators read zero.
               This is D8, and it reddens more than anything else here: a group
               that does not measure what follows it on its line renders flat,
               and what follows is then placed by a fit it was left out of.
        M5  In [Layout.node], write something for a childless node.
            -> (b) 36,824 formats, (c) 38,836, (e) 8 lines. A childless node is
               the parse saying it wanted a token and never found one. Its bytes
               are not in the tree, and writing any is repair.
        M6  In [Layout.entries], drop the separators at the end of a body.
            -> (b) 23,330 formats, (c) 4,176, (g) both separators read zero.
               Dropping is as much a change to the token run as writing, and each
               drop reshapes the parse and reveals the next.
        M7  In [Layout.entries], write whitespace trivia out rather than letting
            the boundaries re-emit it.
            -> (c) 190,949 formats, (e) 2 lines, (g) three read zero. The
               source's indentation is written, then indented again, on every
               pass.
        M8  In [Layout.node], nest the body of every rule, whether or not it has
            a boundary of its own.
            -> nothing here. json.format moves by 86 lines, comments.format by
               39 and shapes.format by 1. Every rule that wraps a single child
               adds its indent again, so json's ["{\"a\": 1}"] indents six where
               the object's own indent is two. It stays idempotent and inside the
               ruler, so only the goldens carry it.
        M9  In [Layout.Written.doc], leave a token's own newlines inside its text
            node.
            -> (f) 296 documents. An unterminated block comment holds them, and
               so does any comment that spans lines. This is D6, and the engine's
               own [check] is what states it.
       M10  In [Layout.node], end the body segment at the closer always.
            [depth 8]
            -> (e) 675 lines. A recovery node beside the closer holds it to the
               body's last line, and the group that settles that line then has
               not measured it.
       M11  In [Lower.slots], give slot zero the rule's break rather than [Flat].
            -> (a) 39 findings across the nine grammars, and nothing else.
               Nothing downstream reads that break, so the check is what says the
               layout means one thing.
       M12  In [Layout.node], write the separator into a frame the parse never
            closed.
            -> (b) 11 formats, (c) 17, (d) 1 input. The body grows one element
               per pass: the separator lets the next parse take a token that was
               outside the frame, which gives the body a new last element, which
               earns another separator. This is pigeon's [format] exactly.
       M13  In [Layout.node], stop reading a separator the source has as a
            request to break.
            -> nothing here, and comments.format moves by 53 lines. A body whose
               user requested broken lays out by width instead, and since the
               fold cannot drop the separator, a flat body then carries a trailing
               one. It is legal and idempotent and reads badly, so only the
               goldens carry it.
       M14  In [Layout.body], carry the state the folding *with* the separator
            left, rather than the one without it.
            -> (b) 7 formats, (d) 1 input, and law_fuzz by 231 over its good
               corpus, 543 over its mutated one and 185 over its fragments.
               The flat branch writes no separator, so its state is the one the
               closer glues against. Carrying the other one puts the
               separator's bytes in the run twice over.

               Re-measured after the fold moved this from [Layout.node] to
               [Layout.body]. It read 5 formats and 3 inputs before, which is
               the corpus reaching the same defect through a different
               document.

               The first version of this was a shadowed name that made the
               second folding start where the first one left off. It read one
               format, and the corpus was the only thing that saw it.
       M15  In [Layout.body], fold a body whose separator is already there in a
            different shape from one that gains it: let [`Present] skip the
            split at the last element that [`Add] and [`Maybe] take.
            -> nothing, here or in law_fuzz, where it read 209 formats before
               the separator was folded once.

               The two shapes are now the same document rather than two
               documents kept in step: [`Present] is [rest ~sep:false] and a
               flat [`Maybe] is the same call. So the property holds by
               construction, and what this mutation now says is that no input
               in either corpus reaches a case where the difference shows. A
               shape that cannot be told apart is not the same as one that is
               the same, and this is the entry that would notice if the
               construction stopped holding.
       M16  In [Layout.body], put the [On_break] separator in the flat branch.
            -> (c) 2,576 formats, and law_fuzz by 970, 1,875 and 2,338 over
               its three corpora, every one of them Law B. comments.format
               moves by 43 lines as well. A flat body
               then carries a trailing separator, the next parse reads that as
               a request to break, and the pass after that takes it out of the
               flat branch again.

       M17  In [Layout.node], carry a break on out of the child that asked for
            it, rather than putting it back where the child writes nothing.
            [depth 64]
            -> nothing here. recovery.format moves by 55 lines and
               comments.format by 2. A boundary in front of a child that is not
               there is not a boundary. Carried on, the request reaches a token
               in some other frame: the trailing separator of a list lands on a
               line of its own, taking the break meant for the name a [Field]
               never got. It is idempotent and inside the ruler, so only the
               goldens carry it.

               Re-measured on 2026-09-19 against the [break-back] step, which
               marks this situation. Still nothing, and that is the honest
               reading: the step fires on a request outstanding where a child
               wrote nothing, before anything is done about it. A step on the
               restoring would have reddened (g) for having been deleted rather
               than for the layout being wrong.
       M18  In [Layout.entries], leave the rule's break on a closer the parse
            never found. [depth 8]
            -> (e) 584 lines. Such a closer writes nothing, so there is nothing
               to break in front of. Leaving the break there cuts the run, and
               the group that settles the line then stops short of the tokens
               the caller writes on it.

       M21  In [Lower.rule], write [edge_before] and [edge_after] as [None].
            -> (g), both of its steps, and the two sexp goldens move by 14
               lines. The override is what puts a space between a group and what
               sits beside it. Neither parenthesis can carry that, because two
               parentheses still touch.

       M19  In [Layout.node], drop the [absent] trace.
       M20  In [Layout.node], replace [undo] with the identity, which drops the
            restoring and the [break-back] trace together.
            -> (g), each naming its own step. Both are mutations of the
               instrument rather than of the layout, and they are here because
               a step the corpus cannot reach makes (g) and the coverage count
               both say less than they read.

       M22  In [Layout.flat_end], let a child that writes nothing end the run.
       M23  In [Layout.node], start the body segment at the opener rather than
            after its leading run.
            -> nothing here, and no golden moves. Both redden
               test/laws/law_fuzz.ml, which reads zero, by 7 and by 1,196,
               every one of them Law F. They are two halves of one defect: a run
               that lands on a group's line and sits outside it. This corpus
               cannot reach it. The head of a frame is empty unless something
               sits before the opener, and the only production shape that has
               one is an enclosed postfix operator, whose operand is a group.
               Nothing anyone wrote by hand in test/inputs puts a long enough
               postfix chain at a narrow enough width, and a swept input
               mangles the chain before it gets there.
       M24  In [Layout.node], read [flat_through] as [false].
            -> (e), 70 lines past the ruler, and law_fuzz by 143. The third
               half of the same defect, and the one this corpus does reach:
               it did not before the separator was folded once, and what
               changed is that the body's document no longer splits around the
               last run.
       M25  In [Layout.body], split the walk at the last element under
            [`Plain] as well.
            -> (c), 11 formats, and law_fuzz by 60. A policy that adds nothing
               has nothing to insert there, and cutting the walk truncates
               every run that crosses the cut.
       M26  In [Layout.node], take a plain group and a conditional that always
            answers flat, so an [On_break] separator is never written.
            -> nothing here, nothing in law_fuzz, and comments.format moves by
               16 lines. No law says a body that broke carries its separator.
               Law C allows one and does not require it, and a fold that never
               writes one is stably idempotent. The golden is the whole of what
               covers it.

       M27  In [Lower.expansion], leave a block's rule atoms out of the kinds a
            slot admits.
            -> nothing here. law_fuzz reads 4 over its mutated corpus at depth
               1 and 10 at depth 8, and 1 over its fragments and 11.
               calc.layout loses kind 1 from four slots, which is [Parens], its
               only rule atom.

               A slot whose symbol is an expression block admits the block, its
               hole and its roles. A rule atom is the fourth thing that parse
               builds, and it stands unwrapped, so without it the fold does not
               count one as an element: the separator a body's policy adds goes
               in front of the atom rather than after it, and the next parse
               reads it as a real separator. rust's [f(0, i, {})] is the shape.
       M28  In [Layout.node], write the policy separator after bytes an error
            node swept up.
       M29  In [Layout.node], read an inner frame the parse never closed as
            closed, so the policy separator is written after it.
            -> nothing here, and nothing over law_fuzz's mutated corpus
               either, at any depth. Its fragments read 27 and 14 at depth 1
               and 195 and 147 at depth 8, on one witness between them, and
               they are the whole of what falsifies these two.

               Two ways for a separator to come back somewhere this fold cannot
               see it. Under M28 recovery sweeps it into the error node at the
               body's end, and [fn{match]{e(}] goes [e(] to [e(,] to [e(,,],
               gaining one on every pass without ever settling. Under M29 a
               frame still open there takes it as its own trailing separator,
               and [fn{match]{e\te(!}] settles on the second pass rather than
               the first: the call [e(] has no [)], and the comma this body
               wrote becomes that call's.

      All three read zero at depth 1, which is why the law takes a depth and
      this one does not.

      Five of the twenty-nine redden nothing here and move the goldens instead,
      and six more redden nothing here at all: three of those redden law_fuzz,
      two move no golden either, and M15 is the one whose property became
      structural. That
      is the honest state of the parts: they say the fold is correct on this
      corpus, they do not say it is good, and law_fuzz is what says this corpus
      is not the whole of what the fold has to be right about.

      No mutation of the fold reddens (h). It is a claim about the corpus, and
      what would falsify it is a generator that stops exploring.
   -------------------------------------------------------------------------- *)

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
   with the ceiling at fifteen steps squared. Depth 8 found M10 and M18 and
   depth 64 found M17 while the count stood still. So the step is paired with
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
  ; "edge-before"
  ; "edge-after"
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

(* Every token of [before] is in [after], in the same order. [after] may hold
   one extra token where a body's policy added a separator, and nothing else: the
   fold writes the tree's tokens and, in a frame the parse closed, one separator.
   It may never lose one. *)
let rec keeps ~(seps : string list) (before : string list) (after : string list) : bool =
  match before, after with
  | [], [] -> true
  | b, y :: a
    when match b with
         | x :: _ -> not (String.equal x y)
         | [] -> true -> List.mem y seps && keeps ~seps b a
  | x :: b, y :: a when String.equal x y -> keeps ~seps b a
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
             let stream, res = Handsome.Utf8.render ~width d in
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

(* M1's other half. A count is only evidence while it is still moving, so the
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

let () = Law.summarise "law_layout"
