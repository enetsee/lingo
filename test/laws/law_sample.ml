(* -- the sampler --------------------------------------------------------------

      (a) Every sampled input is the sample it was drawn from, and parses
          with no diagnostics.
      (b) Law A over the corpus: the tree rebuilds the input, byte for byte.
      (c) The number of structures of size n is the number of token sequences
          of length n the parser accepts.
      (d) The system's nullability is the grammar's.
      (e) The system's minimum size is the grammar's.
      (f) Every active role of every expression block appears in the corpus.
      (g) Size is tokens, and the measured mean tracks the size asked for.

      Mechanism. Fourteen grammars: nine from grammars/, and the witness
      grammars beside them for the framings and operator tables those nine do
      not carry. For each one the translation builds a system, bolts solves it,
      and the corpus is what the sampler draws. Every input goes through parts
      (a) and (b), and part (f) counts what the trees held.

      The size asked for is 40 tokens, or the largest the species has where
      that is smaller: one witness root is two required tokens and is asked for
      two. Each grammar's target and window print with part (g).

      Parts (a) and (b) read a second corpus as well, drawn from a system that
      also draws comments. Only a grammar with [Preserve] trivia has one:
      elsewhere it would be the same system drawn from twice. The parts that
      count structures read the first corpus alone, where a size is a count of
      meaningful tokens and a comment would be a node of size zero carrying
      bytes past the count.

      The engine is fixed here rather than chosen by [Strategy.auto], and the
      seed with it, so the corpus is the same on every run and a count in the
      record below is reproducible. Choosing per grammar is the fuzz sweep's
      job, and it measures to choose, which is a different corpus each time.

      Part (a) has two halves and the first is the one the rest rests on: the
      decoded string, lexed back with the whitespace [decode] wrote taken out
      again, is the token list the sampler drew. Without it "the corpus parses"
      is a claim about some other corpus. It is worth its own line: a lexeme
      that spells another token corrupts 1,953 of 20,000 samples and only 103
      of them report anything, so the second half reads a twentieth of it.

      The second half reports classes rather than a rate. A class is the first
      diagnostic's own words and the chain of node kinds it landed in, and the
      shortest input that reached it is kept beside the count. The predecessor
      read 0.1% here and that one number covered 3,984 samples that were
      legitimately empty and 17 that were not.

      Part (c) is what says the system is the language rather than something
      near it. Its oracle is the parser: for each length n, every sequence of
      meaningful token kinds of that length is offered to the interpreter, and
      the ones it takes without complaint are counted. That number is compared
      with [Exact.count], which counts structures. The two agree only where
      the system generates every sequence the parser takes, no others, and one
      structure per sequence -- which is uniformity over strings, and what the
      Pratt encoding is for. The interpreter reads kinds, so the sequences go
      to it as tokens rather than as bytes and no lexeme is needed.

      Lengths run from one up to where the alphabet's own size stops them: a
      budget of 200,000 sequences takes a grammar with four meaningful tokens
      to eight and one with eleven to five, and the length reached prints
      beside the result. That is what the suite runs. Taken to 30,000,000 by
      hand it reaches sexp 12, rassoc 12, unicode 12, comments 9, and 7 or 8
      everywhere else, in 111 s, and still agrees at every length. rassoc is
      the one to watch there: its operators are right-associative, which is
      where a level reads its own threshold.

      Parts (d) and (e) are two derivations of one fact meeting. bolts reads
      the species and [Core.Fixpoint] reads the rules, neither of them reads
      the other, and a disagreement says the system is not the grammar.

      Part (e) is the weaker of the two and it is worth saying what it covers.
      A minimum moves where a required child is translated optional, where a
      zero-or-more is translated one-or-more, where a child is dropped or
      doubled, where a frame loses a delimiter, and where the Pratt
      stratification loses a level. A minimum does not move where a token is
      missing from a sum, where alternatives are in the wrong order, or where
      a binding power is wrong, so it says nothing about any of those. Part
      (c) is what reads those.

      Neither part quantifies over an expression block's roles. A role
      describes the shape of a node the block's parse builds, nothing
      references it, and the system holds no species for it.

      Part (g) is the phase's own claim: a size asked for is a size in
      tokens. Its first half reads every draw and says bolts counted exactly
      the tokens the draw held, so a terminal translated uncounted, or
      translated to two tokens, moves it. Its second half is the oracle, and
      the tolerance is four standard errors of the spread the sampler itself
      reports. A sampler reporting no finite spread fails outright, because the
      comparison would otherwise hold of any mean at all. grammars/rust is such
      a grammar, and it is drawn in test/laws/law_fuzz.ml for that reason.

      What this says nothing about. Whether the distribution inside a size is
      uniform, which part (c) establishes only through the counting. Nor where
      a comment lands, as against whether it parses: Law B's fixed point is
      what reads placement, and it is Phase 5's.

      Coverage. Fourteen grammars and fifteen corpora, 30,000 sampled inputs,
      and the lengths part (c) reached print beside the result. 2.0 s of
      processor time for the whole of it, on the machine this record was
      written on.

      Depth 1 is what the suite runs, and it says nothing about a defect
      waiting at a million. [LINGO_SWEEP=40] is 1,200,000 inputs carrying
      432,041 comments and 67 MB of decoded source, in 52 s, and every part
      still reads zero. Part (g) is the one that sharpens with depth rather
      than only repeating: at depth 1 the means sit within 2.6 of the target
      and at depth 40 within 0.8, which is the oracle being exact rather than
      close.

      What the witnesses add, and what they do not. They add a delimited body
      with a separator, a separated body, two operators at one binding power,
      a block with three postfix shapes, and a root of two required tokens.
      Every token they declare is a single character of punctuation, so they
      hold no pattern token for a lexeme walk to get wrong and no whitespace
      token for {!Sample.decode} to write. No sample of theirs ever needs a
      joiner, which is why M8, M9 and M15 read the same on fifteen corpora as
      they did on ten.

      Falsification. Every mutation was applied, built, run and reverted, and
      the result recorded is the one observed. A count of lengths counts the
      lengths that disagreed, and the grammars beside it are where they came
      from.

        M1  In [Sample.repeat], treat every separator policy as [Never], so no
            body carries a trailing separator.
            -> part (c), 4 lengths: shapes 5, comments 4, 5 and 6. The parser
               takes a trailing separator wherever the policy is not [Never],
               so a system without one holds fewer strings than the language.
               The twelve grammars that declare no policy but [Never] do not
               move.
        M2  In [Sample.child], translate [Zero_or_one] as a required child.
            -> part (c), 4 lengths, shapes 2 to 5. Part (e), 1 rule: shapes
               [Let] reads 5 against the grammar's 2.
        M3  In [Sample.child], translate every repeated child as one or more.
            -> part (c), 10 lengths: sexp 6, comments 4. Part (d), 2 rules:
               recovery [File] and shapes [Program], each a root whose body is
               a repeated child and so nullable. Part (e), 4 rules: sexp
               [Group], recovery [File], shapes [Program], comments [Item].
        M4  In [Sample.production], leave the closer out of a [Delimited]
            frame.
            -> part (a) second half, 18,000 of 30,000 inputs in 2,497 classes.
               Part (c), 46 lengths over eight grammars. Part (e), 12 rules,
               every delimited production in the corpus.

               The first half does not move. The system holds a shorter token
               list and [decode] renders that list faithfully, which is the
               distinction the two halves are there to draw.
        M5  In the block's [loop], let an infix step leave no ceiling behind:
            [step k (nt (expr right)) no_ceiling].
            -> part (c), 9 lengths: rassoc 4, calc 2, pratt 2, postfix 1.
               Every one reads more structures than the parser takes strings,
               which is the ambiguity the ceiling is there to remove: calc at
               length 5 holds 49 structures for 45 strings.
        M6  In the block's [expr], leave the first prefix operator out.
            -> part (c), 17 lengths: rassoc 7, calc 5, pratt 5. Part (f), 3
               roles: calc and rassoc [ExprPrefix] and pratt [EPrefix], which
               no sample then holds. postfix and pratt-postfix declare no
               prefix operator and do not move.
        M7  In the block's [loop], make the threshold strict: [bp > m] for
            [bp >= m].
            -> part (c), 6 lengths: rassoc 4, pratt 2. A right-associative
               operator reads its own level and is the only one that does, so
               calc's four left-associative operators do not move.
        M8  In [Sample.system_of]'s [draw_for], let the lexeme walk stop at
            any accepting state rather than one accepting this token.
            -> part (a) first half, 1,953 inputs; second half, 103 in 94
               classes. A pattern lexeme then spells a keyword. This is the
               pair that says why the first half is there: nineteen out of
               twenty corrupted samples parse clean, and the predecessor's own
               version of this defect hid in exactly that gap.
        M9  In [Sample.decode], join with nothing and never a space or a line
            break.
            -> part (a) first half, 8,301 inputs; second half, 7,591 in 1,578
               classes. 710 samples merge two tokens into one and still parse.

               Leaving out the space alone, and keeping the line break behind
               it, moves nothing. No part here reads which of the three
               joiners was taken, and the formatter's laws are what do.
       M10  In [Core.Fixpoint.min_size], charge an absent child for its
            element.
            -> part (e), 15 rules over eight grammars. The grammar's side
               alone moves, which is the shape a cross-check is supposed to
               have.
       M11  In [Core.Fixpoint.min_size], add one to a block's minimum.
            -> part (e), 12 rules: every rule on a path to an expression
               block, across all five grammars that have one.
       M12  In [Sample.system_of]'s [terminal], translate a keyword or a
            punctuation token uncounted: [gen0] for [atom].
            -> part (g), both halves: 4,000 draws held a number of tokens the
               sampler did not count, and unicode's mean reads 82.9 tokens and
               recovery's 198.5 against a target of 40. Part (c), 9 lengths.
               Part (d), 2 rules. Part (e), 8 rules.

               Only the grammars whose smallest shapes are literal tokens
               move. A grammar whose size still comes from its pattern tokens
               is tuned to the same mean and reads a mean of tokens near it.
       M13  In [Interp.drain], drop the trailing [Cursor.skip_trivia]. This is
            law_interp's own M1 and it reddens there.
            -> nothing. [decode] writes its joiner in front of each token, so
               no sampled input ends in trivia and there is none to lose.
       M14  In [Sample.decode], take the run from the last blank in the bytes
            written, wherever that blank came from, rather than from the last
            joiner [decode] itself wrote.
            -> part (a) first half, 374 inputs; second half, 347 in 267
               classes, and all 267 are in the corpus drawn with comments.

               This was the defect. A line comment runs to the end of its
               line, and its own text may hold a tab. Clearing the run there
               loses the [//] in front of it, the next token then finds a
               boundary where there is none, and the comment swallows the rest
               of the line. Setting the run to the last token alone leaves the
               [//] in place and moves nothing, which is why the mutation is
               the blank and not the length.

               json's strings hold blanks too -- 258 of 2,000 draws -- and
               none of them reaches a class. Losing the run mid-string makes
               the next test an unterminated string, and a space goes in that
               was not needed, which parses. The error runs the safe way for
               every lexeme but one that runs to the end of a line.
       M15  In [Sample.live_suffix], drop a token whose scan was still running
            when the run ran out, so the run is always empty.
            -> part (a) first half, 8,301 inputs; second half, 7,591 in 1,578
               classes, which is M9 exactly. An empty run tests only whether
               the next token lexes on its own, which it always does, so no
               joiner is ever written.

               Keeping the prefix a later byte cannot change is what makes
               [decode] linear rather than quadratic, and this is the mutation
               that says the shortening is not free to go further.
       M16  In [Sample.system_of]'s comment draw, draw no comment ever.
            -> part (a), the corpus drawn with comments holds none. Nothing
               else moves: that corpus is then the first corpus over again,
               and every part would read exactly what it read without it.

      Part (b) reads zero under every mutation above, M13 included. A parse
      rebuilding its input is the parse's property rather than the corpus's,
      and every byte [decode] writes becomes a token. It is here because the
      corpus is new: this is where an input the parse could not give back
      would show, and nowhere else.
   -------------------------------------------------------------------------- *)

let pass fmt = Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt

(* -- the corpus ------------------------------------------------------------ *)

(* The size the corpus is asked for, where the grammar has structures that
   large. A witness grammar whose root is two required tokens has one size and
   is asked for that; the target and the window each grammar settled on print
   with part (g).

   One pointing level. It flattens the size distribution, which takes sexp's
   standard deviation from 525 to 60 at a mean of 40 and makes the window
   reject far less. Part (g) is what reads the difference. *)
let mean = 40.
let seed = [| 0x5EED |]

(* How many inputs each grammar contributes. The suite runs at 1, and
   [LINGO_SWEEP] multiplies it, which is the knob law_layout and law_format
   already take. A depth of 1 is 30,000 inputs and reads zero; that says
   nothing about a defect waiting at a million, so the record below says what
   a deep run found. *)
let depth =
  match Sys.getenv_opt "LINGO_SWEEP" with
  | None -> 1
  | Some s ->
    (try int_of_string s with
     | _ -> 1)
;;

let draws = 2000 * depth

(* A species with a largest size cannot be asked for more than it has, and both
   bounds are reachable. The window is half the target to one and a half times
   it, clipped to the sizes the species admits. *)
let target_of (smallest : int) (largest : int option) : float * (int * int) =
  let target =
    match largest with
    | Some most when float_of_int most < mean -> float_of_int most
    | Some _ | None -> Float.max mean (float_of_int smallest)
  in
  let lo = max smallest (int_of_float (target /. 2.)) in
  let hi =
    let wanted = int_of_float (target *. 1.5) in
    match largest with
    | Some most -> min most wanted
    | None -> wanted
  in
  target, (lo, max lo hi)
;;

(* Everything a part below reads, derived once. Deriving it per sample would
   pay the automaton and the oracle for every draw. *)
type case =
  { name : string
  ; facts : Core.Facts.t
  ; sys : Bolts.system
  ; map : Sample.map
  ; root : Core.Rule.id
  ; nt : Sample.token list Bolts.nonterminal
  ; plan : Ir.Plan.t
  ; sampler : Sample.token list Bolts.Sampler.t
  ; target : float (** What the oracle was tuned for. *)
  ; window : int * int (** What the corpus is kept inside. *)
  ; kind_names : string array
  ; whitespace : int list
    (** The [Reformat] trivia kinds. [Sample.decode] writes those, so they
          are not part of what the sampler drew. *)
  ; comment_kinds : int list (** The [Preserve] trivia kinds. *)
  }

let trivia_kinds (f : Core.Facts.t) (trivia : Core.Grammar.trivia_class) : int list =
  List.filter_map
    (fun (tok : Core.Token.def) ->
       if tok.trivia = Some trivia then Some (Core.Kind.to_int tok.kind) else None)
    (Array.to_list f.tokens)
;;

let kind_names (f : Core.Facts.t) : string array =
  let names = Array.make (Core.Facts.kind_count f) "?" in
  List.iter
    (fun k ->
       names.(Core.Kind.to_int k) <- Core.Kind.Name.to_string (Core.Facts.kind_name f k))
    (Core.Kind.Table.kinds f.kinds);
  names
;;

let prepare ((name : string), (grammar : Core.Grammar.t)) : case option =
  match Core.Facts.of_grammar grammar with
  | Error _ ->
    Law.fail "%s: the corpus grammar was rejected" name;
    None
  | Ok facts ->
    let sys, map = Sample.system_of facts in
    let root = List.hd facts.roots in
    (match Sample.nonterminal map root with
     | None ->
       Law.fail "%s: the root rule has no species" name;
       None
     | Some root_nt ->
       let plan, _ = Plan.Lower.of_facts facts in
       (try
          Bolts.Analysis.check sys root_nt;
          let target, window =
            target_of
              (Bolts.Analysis.min_size sys root_nt)
              (Bolts.Analysis.max_size sys root_nt)
          in
          Some
            { name
            ; facts
            ; sys
            ; map
            ; root
            ; nt = root_nt
            ; plan
            ; sampler =
                Bolts.Sampler.compile ~points:1 sys root_nt (Bolts.Sampler.Mean target)
            ; target
            ; window
            ; kind_names = kind_names facts
            ; whitespace = trivia_kinds facts Core.Grammar.Reformat
            ; comment_kinds = trivia_kinds facts Core.Grammar.Preserve
            }
        with
        | e ->
          Law.fail "%s: the system has no sampler (%s)" name (Printexc.to_string e);
          None))
;;

let cases : case list = List.filter_map prepare Sample_corpus.all

(* -- the corpus, drawn once and read by four parts ------------------------- *)

type sample =
  { tokens : Sample.token list (** What the sampler drew. *)
  ; src : string (** What [Sample.decode] made of it. *)
  ; lexed : Lingo_runtime.Token.t array
  ; tree : Siesta.Green.node
  ; diags : Lingo_runtime.Diagnostic.t list
  }

let drawn (case : case) (sampler : Sample.token list Bolts.Sampler.t) (map : Sample.map)
  : sample list
  =
  let rng = Random.State.make seed in
  List.init draws (fun _ ->
    let tokens = Bolts.Sampler.sample sampler ~window:case.window rng in
    let src = Sample.decode case.facts map tokens in
    let lexed = Lex.run case.facts src in
    let tree, diags = Interp.run case.plan case.root lexed in
    { tokens; src; lexed; tree; diags })
;;

(* bolts raises where a window holds no attainable size and where the exact
   tables overflow. Neither can happen on a grammar here, and a law that dies
   says less than one that reports, so every call that can raise is caught. *)
let guarded (name : string) (what : string) (attempt : unit -> 'a) : 'a option =
  try Some (attempt ()) with
  | e ->
    Law.fail "%s: %s raised (%s)" name what (Printexc.to_string e);
    None
;;

let corpus : (case * sample list) list =
  List.filter_map
    (fun (case : case) ->
       Option.map
         (fun samples -> case, samples)
         (guarded case.name "the draw" (fun () -> drawn case case.sampler case.map)))
    cases
;;

(* A second corpus, over a system that also draws comments. Only a grammar
   with [Preserve] trivia has one: elsewhere the system is the same system and
   drawing from it twice would say nothing.

   Parts (a) and (b) read both corpora. The parts that count structures read
   the first alone, where a size is a count of meaningful tokens and a comment
   would be a node of size zero carrying bytes past the count. *)
let comment_density = 0.15
let has_comments (case : case) : bool = case.comment_kinds <> []

let commented : (case * sample list) list =
  List.filter_map
    (fun (case : case) ->
       if not (has_comments case)
       then None
       else (
         let sys, map = Sample.system_of ~comments:comment_density case.facts in
         match Sample.nonterminal map case.root with
         | None -> None
         | Some root_nt ->
           guarded case.name "the commented draw" (fun () ->
             let sampler =
               Bolts.Sampler.compile
                 ~points:1
                 sys
                 root_nt
                 (Bolts.Sampler.Mean case.target)
             in
             case, drawn case sampler map)))
    cases
;;

(* What parts (a) and (b) quantify over, with the label a class is reported
   under. *)
let everything : (string * case * sample list) list =
  List.map (fun ((case : case), samples) -> case.name, case, samples) corpus
  @ List.map
      (fun ((case : case), samples) -> case.name ^ "+comments", case, samples)
      commented
;;

let inputs = List.fold_left (fun acc (_, _, s) -> acc + List.length s) 0 everything

(* -- (a) the input is the sample, and it parses clean ---------------------- *)

(* The chain of node kinds from the root down to the deepest node holding this
   byte. Two samples that broke in the same construct then share a class, and
   a count over classes says which construct rather than how often. *)
let chain (case : case) (root : Siesta.Green.node) (at : int) : string =
  let rec down (node : Siesta.Green.node) (starts : int) (above : string list)
    : string list
    =
    let above = case.kind_names.(Siesta.Green.kind node) :: above in
    let children = Siesta.Green.children_array node in
    let rec across (index : int) (offset : int) : string list =
      if index >= Array.length children
      then above
      else (
        let len = Siesta.Green.child_text_len children.(index) in
        if at >= offset && at < offset + len
        then (
          match children.(index) with
          | Siesta.Green.Node child -> down child offset above
          | Siesta.Green.Token _ -> above)
        else across (index + 1) (offset + len))
    in
    across 0 starts
  in
  String.concat "/" (List.rev (down root 0 []))
;;

let classes = Hashtbl.create 16

let note
      (label : string)
      (case : case)
      (src : string)
      (diag : Lingo_runtime.Diagnostic.t)
      (tree : Siesta.Green.node)
  : unit
  =
  let key =
    Printf.sprintf
      "%s: %s in %s"
      label
      (Format.asprintf "%a" Lingo_runtime.Diagnostic.pp diag)
      (chain case tree (fst diag.range))
  in
  match Hashtbl.find_opt classes key with
  | Some (n, witness) ->
    Hashtbl.replace
      classes
      key
      (n + 1, if String.length src < String.length witness then src else witness)
  | None -> Hashtbl.add classes key (1, src)
;;

(* The decoded string has to be the sample, or "the corpus parses" is a claim
   about some other corpus. Lexing it back gives the tokens the sampler drew,
   with the whitespace [decode] wrote taken out again. *)
let is_the_sample (case : case) (input : sample) : bool =
  let lexed_back =
    List.filter_map
      (fun (tok : Lingo_runtime.Token.t) ->
         if List.mem tok.kind case.whitespace then None else Some (tok.kind, tok.text))
      (Array.to_list input.lexed)
  in
  let drawn =
    List.map
      (fun (tok : Sample.token) -> Core.Kind.to_int tok.kind, tok.text)
      input.tokens
  in
  lexed_back = drawn
;;

let () =
  let failures_before = Law.failures () in
  let strayed = ref 0 in
  let drew_comments = ref 0 in
  List.iter
    (fun (label, (case : case), samples) ->
       List.iter
         (fun (input : sample) ->
            if not (is_the_sample case input) then incr strayed;
            List.iter
              (fun (tok : Sample.token) ->
                 if List.mem (Core.Kind.to_int tok.kind) case.comment_kinds
                 then incr drew_comments)
              input.tokens;
            match input.diags with
            | [] -> ()
            | first :: _ -> note label case input.src first input.tree)
         samples)
    everything;
  if !strayed > 0
  then Law.fail "(a) %d decoded inputs do not lex back to the tokens drawn" !strayed;
  (* A second corpus that held no comment would be the first corpus over again,
     and every reading below it would be one grammar short without saying so. *)
  if commented <> [] && !drew_comments = 0
  then Law.fail "(a) the corpus drawn with comments holds none";
  let table =
    List.sort
      (fun (_, (a, _)) (_, (b, _)) -> compare b a)
      (Hashtbl.fold (fun k v acc -> (k, v) :: acc) classes [])
  in
  (match table with
   | [] -> ()
   | _ ->
     let unclean = List.fold_left (fun acc (_, (n, _)) -> acc + n) 0 table in
     Law.fail
       "(a) %d of %d inputs did not parse clean, in %d classes"
       unclean
       inputs
       (List.length table);
     (* The commonest twenty, with the shortest input that reached each. A rate
        says how often and a class says which construct, and the second is what
        a fix starts from. *)
     List.iteri
       (fun i (key, (n, witness)) ->
          if i < 20 then Printf.printf "     %d x %s -- %S\n" n key witness)
       table);
  if Law.failures () = failures_before
  then
    Law.pass
      "(a) every sampled input is its sample and parses clean, over %d inputs and %d \
       corpora holding %d comments"
      inputs
      (List.length everything)
      !drew_comments
;;

(* -- (b) the tree rebuilds the input --------------------------------------- *)

let () =
  let lossy = ref 0 in
  let bytes = ref 0 in
  List.iter
    (fun (_, _, samples) ->
       List.iter
         (fun (input : sample) ->
            bytes := !bytes + String.length input.src;
            if Siesta.Green.to_source input.tree <> input.src then incr lossy)
         samples)
    everything;
  if !lossy = 0
  then
    Law.pass
      "(b) every parse rebuilds its input, over %d inputs and %d bytes"
      inputs
      !bytes
  else Law.fail "(b) %d parses did not rebuild their input" !lossy
;;

(* -- (c) the structures of a size are the sequences of that length --------- *)

(* How many sequences a length is allowed to cost. A grammar with a wide
   alphabet stops sooner, and the length it reached prints with the result.
   Raise it to take the law deeper by hand; the header records a run at
   30,000,000. *)
let sequences_per_length = 200_000

let meaningful_kinds (f : Core.Facts.t) : int list =
  Array.to_list f.tokens
  |> List.filter (fun (tok : Core.Token.def) -> not (Core.Token.is_trivia tok))
  |> List.map (fun (tok : Core.Token.def) -> Core.Kind.to_int tok.kind)
;;

(* The parser settles it. A sequence of kinds it takes with nothing to report
   is in the language, and the interpreter reads kinds, so the text is a
   placeholder. *)
let accepted (case : case) (kinds : int list) : bool =
  let tokens =
    Array.of_list (List.map (fun kind -> { Lingo_runtime.Token.kind; text = "x" }) kinds)
  in
  snd (Interp.run case.plan case.root tokens) = []
;;

let () =
  let failures_before = Law.failures () in
  let reached = ref [] in
  List.iter
    (fun ((case : case), _) ->
       let alphabet = meaningful_kinds case.facts in
       let width = List.length alphabet in
       (* The longest sequence the budget reaches with this alphabet. *)
       let rec longest (length : int) (sequences : int) : int =
         if sequences > sequences_per_length / width
         then length
         else longest (length + 1) (sequences * width)
       in
       let last = longest 1 width in
       match
         guarded case.name "the exact tables" (fun () ->
           Bolts.Exact.make case.sys case.nt ~max:last)
       with
       | None -> ()
       | Some exact ->
         for length = 1 to last do
           let taken = ref 0 in
           (* Every sequence of that length, offered to the parser one at a
              time. [seen] is the sequence so far, reversed. *)
           let rec walk (seen : int list) (left : int) : unit =
             if left = 0
             then (if accepted case (List.rev seen) then incr taken)
             else List.iter (fun kind -> walk (kind :: seen) (left - 1)) alphabet
           in
           walk [] length;
           let structures = Bolts.Exact.count exact length in
           if
             abs_float (float_of_int !taken -. structures)
             > 1e-6 *. (1. +. abs_float structures)
           then
             Law.fail
               "(c) %s at length %d: the parser takes %d sequences and the system holds \
                %.0f structures"
               case.name
               length
               !taken
               structures
         done;
         reached := Printf.sprintf "%s %d" case.name last :: !reached)
    corpus;
  if Law.failures () = failures_before
  then
    Law.pass
      "(c) the structures of a size are the sequences of that length, up to (%s)"
      (String.concat " " (List.rev !reached))
;;

(* -- (d) and (e) two derivations of one fact ------------------------------- *)

let in_system (case : case) : (Core.Rule.def * Sample.token list Bolts.nonterminal) list =
  Array.to_list case.facts.rules
  |> List.filter_map (fun (d : Core.Rule.def) ->
    Option.map (fun nt -> d, nt) (Sample.nonterminal case.map d.id))
;;

let () =
  let failures_before = Law.failures () in
  let checked = ref 0 in
  List.iter
    (fun ((case : case), _) ->
       List.iter
         (fun ((d : Core.Rule.def), nt) ->
            incr checked;
            let theirs = Bolts.Analysis.nullable case.sys nt in
            let ours = Core.Facts.is_nullable case.facts d.id in
            if theirs <> ours
            then
              Law.fail
                "(d) %s %s: the system says nullable=%b and the grammar says %b"
                case.name
                (Core.Grammar.Name.Rule.to_string d.name)
                theirs
                ours)
         (in_system case))
    corpus;
  if Law.failures () = failures_before
  then Law.pass "(d) nullability agrees, over %d rules" !checked
;;

let () =
  let failures_before = Law.failures () in
  let checked = ref 0 in
  List.iter
    (fun ((case : case), _) ->
       List.iter
         (fun ((d : Core.Rule.def), nt) ->
            incr checked;
            let theirs = Bolts.Analysis.min_size case.sys nt in
            let ours = Core.Facts.min_size case.facts d.id in
            if theirs <> ours
            then
              Law.fail
                "(e) %s %s: the system says min=%d and the grammar says %d"
                case.name
                (Core.Grammar.Name.Rule.to_string d.name)
                theirs
                ours)
         (in_system case))
    corpus;
  if Law.failures () = failures_before
  then Law.pass "(e) the minimum size agrees, over %d rules" !checked
;;

(* -- (f) every active role appears ----------------------------------------- *)

let rec kinds_in (node : Siesta.Green.node) (seen : (int, unit) Hashtbl.t) : unit =
  Hashtbl.replace seen (Siesta.Green.kind node) ();
  Array.iter
    (function
      | Siesta.Green.Node c -> kinds_in c seen
      | Siesta.Green.Token _ -> ())
    (Siesta.Green.children_array node)
;;

let () =
  let failures_before = Law.failures () in
  let counted = ref [] in
  List.iter
    (fun ((case : case), samples) ->
       let seen = Hashtbl.create 64 in
       List.iter (fun (input : sample) -> kinds_in input.tree seen) samples;
       Array.iter
         (fun (d : Core.Rule.def) ->
            match d.origin with
            | Core.Rule.User | Core.Rule.Pratt_block -> ()
            | Core.Rule.Pratt_role _ ->
              let name = Core.Grammar.Name.Rule.to_string d.name in
              if Hashtbl.mem seen (Core.Kind.to_int d.kind)
              then counted := Printf.sprintf "%s.%s" case.name name :: !counted
              else Law.fail "(f) %s %s: no sample in the corpus holds one" case.name name)
         case.facts.rules)
    corpus;
  if Law.failures () = failures_before
  then
    Law.pass "(f) every active role appears (%s)" (String.concat " " (List.rev !counted))
;;

(* -- (g) the measured mean tracks the target ------------------------------- *)

(* Without a window, so the mean measured is the mean the oracle was tuned for
   rather than that mean conditioned on a window. *)
let () =
  let failures_before = Law.failures () in
  let report = ref [] in
  let miscounted = ref 0 in
  List.iter
    (fun ((case : case), _) ->
       let rng = Random.State.make seed in
       let total = ref 0 in
       for _ = 1 to draws do
         let ts, size = Bolts.Sampler.sample_with_size case.sampler rng in
         if List.length ts <> size then incr miscounted;
         total := !total + List.length ts
       done;
       let measured = float_of_int !total /. float_of_int draws in
       let expected = Bolts.Sampler.expected_size case.sampler in
       (* Four standard errors of the spread the sampler reports, and a floor
          under it so a species with one size is not held to exact equality in
          floating point. *)
       let error =
         Float.max
           1e-6
           (4. *. sqrt (Bolts.Sampler.size_variance case.sampler /. float_of_int draws))
       in
       report
       := Printf.sprintf "%s %.0f:%.1f+-%.1f" case.name case.target measured error
          :: !report;
       (* A sampler reporting no finite spread leaves the comparison below true
          of every mean there is. That is the finding, rather than a tolerance
          of infinity printed beside a PASS. *)
       if not (Float.is_finite error)
       then
         Law.fail
           "(g) %s: the sampler reports no finite spread, so the mean has no tolerance"
           case.name
       else if abs_float (measured -. expected) > error
       then
         Law.fail
           "(g) %s: %d draws mean %.1f tokens, and the oracle was tuned for %.1f (+- \
            %.1f)"
           case.name
           draws
           measured
           expected
           error)
    corpus;
  if !miscounted > 0
  then
    Law.fail "(g) %d draws held a number of tokens the sampler did not count" !miscounted;
  if Law.failures () = failures_before
  then
    Law.pass
      "(g) size is tokens and each mean tracks its target (%s)"
      (String.concat " " (List.rev !report))
;;

let () =
  Printf.printf "law_sample: %.1f s of processor time\n" (Sys.time ());
  Law.summarise "law_sample"
;;
