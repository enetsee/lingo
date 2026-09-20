(* -- the runtime --------------------------------------------------------------

      (a) A tree built through the runtime rebuilds its input, byte for byte.
      (b) The cursor moves forward only, it never goes past the last token,
          and a drain reaches the end of the input in one step per token.
      (c) [Build.finish] refuses a tree with a frame still open.
      (d) [Recover.expect] takes the token where the cursor is on one. Where
          it is not, it reports one diagnostic and leaves the cursor where it
          was. A placeholder puts a childless node in the tree carrying that
          diagnostic's id.
      (e) [Cursor.offset] is the byte length of everything the parse has
          taken, and [Cursor.report_at] records the range it is given. So a
          parse can read the offset before and after taking input, and report
          over exactly the bytes it read.

      Mechanism. (a) and (b) are oracles over random token streams driven by a
      random walk over the builder. The walk is not a parse of anything. It
      reaches every event the runtime offers, and the tree still has to hold
      every byte. (c) is an oracle with an exact count. (d) reaches for a token at
      every position of a random stream, half the time for one that is there
      and half the time for one that is not, and checks the tree and the
      diagnostics against each other afterwards. Its last part is one literal
      case for the same-offset fold.

      A repetition's progress guard used to be here, as [while_progress] and a
      part of its own. The runtime holds no loop now: a loop is parse control
      flow, so the emitter writes one and the interpreter brings its own, and
      what each of them writes is read where that code is tested.
      test/parse_emit/law_parse.ml M4 is the emitter's, and
      test/laws/law_interp.ml part (b) is the interpreter's.

      Part (a)'s oracle does not recompute what the code computes. The runtime
      builds the tree through siesta from a stream of events, and the oracle
      joins the token texts in the array. The two agree only if every byte the
      cursor read reached the tree in order.

      Every loop the law owns carries a ceiling of one step per token, so a
      cursor that stops advancing is reported rather than hung.

      This law links lingo_runtime and nothing else. The runtime holds no
      grammars, so the kinds below are the law's own. It holds no balanced
      skip and no set of kinds, because both read the grammar: the emitter
      writes them with the kinds as constants and the interpreter brings its
      own.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed.

        M1  In [drain], drop the trailing [Cursor.skip_trivia].
            -> part (a), on 62 of 600 streams, and part (d), 60 of 600.
               Trivia after the last meaningful token never reaches the tree,
               because [Cursor.eof] looks past trivia and the loop stops on
               it. This is the predecessor's own rule: trivia past the last
               child otherwise falls off the end of the input.
        M2  In [Cursor.bump], emit the token and leave [pos] where it is.
            -> part (a), 570 of 600 streams; part (b), 570 drains hit the
               ceiling; part (d), 3760 of 3792 tries.
        M2b In [Cursor.emit_token], advance [pos] by two.
            -> the suite dies on [Invalid_argument "index out of bounds"],
               from [next_meaningful] indexed past its last entry. Red, and
               not a report.
        M3  In [Cursor.skip_trivia], advance [pos] without emitting the token.
            -> part (a), 393 of 600 streams, and part (d), 395 of 600.
        M4  In [Recover.expect], consume a token after reporting one missing.
            -> part (d), 1881 of 2111 misses, and the same-offset case, which
               reports two diagnostics because the second [expect] is no
               longer at the first one's position.
        M5  In [Recover.expect], never take the token.
            -> part (d), all 3714 tries that asked for a token that was
               there.
        M6  In [Build.missing_node], drop the payload.
            -> part (d), 1533 holes carry a payload that indexes no
               diagnostic, and the same-offset case reads ids 0 and 0. This
               is what makes tree-to-diagnostic one step rather than a
               search.
        M7  In [Cursor.report_id], append a [Missing] over a range that
            already carries one instead of folding.
            -> part (d), the same-offset case: two diagnostics where a
               committed production's two failing children should share one.
        M8  is gone. It mutated [Cursor.while_progress], and the runtime no
            longer holds a loop for it to mutate.
        M9  In [Cursor.report_at], record [Cursor.range] instead of the range
            it was given.
            -> part (e), all 288 spans. The cursor has moved past what the
               span covers by the time the report happens, which is why the
               function exists.
        M10 In [Cursor.emit_token], advance the offset by one byte rather
            than by the token's length.
            -> part (e), both halves: the offset disagreed at 4271 of 8135
               steps, and 156 of 288 spans named the wrong bytes. The spans
               that still passed are the ones whose tokens are all one byte
               long.

      Part (b) holds by construction in two of its three halves. Nothing in
      the runtime moves the cursor backwards, and both sites that advance it
      are guarded, so no single-site mutation reports those. M2b is the
      attempt, and it dies on an index rather than reporting. What M2 shows
      is the half this check earns: a cursor that stops advancing.

      Coverage. Two runs of 600 random streams, one seed for parts (a) and
      (b) and another for (d), plus 400 for part (e), over 9 token kinds with
      one of them trivia.
      Counts print beside each result. Every builder event has a counter, and
      the law fails where one reads zero. The balanced skip is not covered
      here, because the runtime no longer holds one.

      What this says nothing about. There is no lexer and no parser yet, so
      the token arrays are the law's own rather than lexed, and the event
      sequences are not parses. Trivia attachment is exercised and not fixed:
      the law checks that trivia reaches the tree, and which frame it lands
      in is a question about a parser.
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

(* -- the law's own alphabet ------------------------------------------------ *)

(* Parentheses, brackets, braces, a word, a separator and whitespace. A
   witness reads as source, so every kind has a spelling. *)
let k_error = 0
let k_lparen = 1
let k_rparen = 2
let k_lbrack = 3
let k_rbrack = 4
let k_lbrace = 5
let k_rbrace = 6
let k_word = 7
let k_semi = 8
let k_ws = 9
let k_file = 10
let k_node = 11
let k_missing = 12
let k_message = Lingo_runtime.Message.of_int 1
let trivia_kinds = [ k_ws ]

let punctuation =
  [ '(', k_lparen
  ; ')', k_rparen
  ; '[', k_lbrack
  ; ']', k_rbrack
  ; '{', k_lbrace
  ; '}', k_rbrace
  ; ';', k_semi
  ]
;;

(* A run of spaces makes one trivia token and a run of letters makes one word,
   so a witness written as source lexes to the tokens it looks like. *)
let lex (s : string) : Lingo_runtime.Token.t array =
  let out = ref [] in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    match List.assoc_opt c punctuation with
    | Some kind ->
      out := { Lingo_runtime.Token.kind; text = String.make 1 c } :: !out;
      incr i
    | None ->
      let kind = if c = ' ' then k_ws else k_word in
      let start = !i in
      let more j =
        if kind = k_ws
        then s.[j] = ' '
        else s.[j] <> ' ' && not (List.mem_assoc s.[j] punctuation)
      in
      while !i < n && more !i do
        incr i
      done;
      out := { Lingo_runtime.Token.kind; text = String.sub s start (!i - start) } :: !out
  done;
  Array.of_list (List.rev !out)
;;

let source_of (tokens : Lingo_runtime.Token.t array) =
  String.concat
    ""
    (Array.to_list (Array.map (fun (t : Lingo_runtime.Token.t) -> t.text) tokens))
;;

let new_cursor (tokens : Lingo_runtime.Token.t array) : Lingo_runtime.Cursor.t =
  Lingo_runtime.Cursor.create ~cache:(Siesta.Cache.create_plain ()) ~trivia_kinds tokens
;;

(* Reads to the end of the input, then takes the trailing trivia. [Cursor.eof]
   looks past trivia, so the loop stops with that trivia unread, and the skip
   takes it.

   Answers false where the ceiling ran out. A drain takes one step per token,
   so a cursor that has stopped advancing is reported here instead of hanging
   the suite. *)
let drain ?(watch = fun _ -> ()) (c : Lingo_runtime.Cursor.t) ~(n : int) : bool =
  let steps = ref 0 in
  while (not (Lingo_runtime.Cursor.eof c)) && !steps <= n do
    Lingo_runtime.Cursor.bump c;
    watch c;
    incr steps
  done;
  Lingo_runtime.Cursor.skip_trivia c;
  watch c;
  !steps <= n
;;

(* -- (a) and (b): a random walk over the builder --------------------------- *)

let spellings =
  [| k_lparen, "("
   ; k_rparen, ")"
   ; k_lbrack, "["
   ; k_rbrack, "]"
   ; k_lbrace, "{"
   ; k_rbrace, "}"
   ; k_word, "ab"
   ; k_semi, ";"
   ; k_ws, " "
  |]
;;

let random_stream (rng : Random.State.t) : Lingo_runtime.Token.t array =
  let n = Random.State.int rng 24 in
  Array.init n (fun _ ->
    let kind, text = spellings.(Random.State.int rng (Array.length spellings)) in
    { Lingo_runtime.Token.kind; text })
;;

(* How often each builder event was reached. An event the walk never takes is
   a line in the coverage claim that is not true. *)
type reach =
  { mutable opened : int
  ; mutable wrapped : int
  ; mutable missing : int
  ; mutable skipped : int
  ; mutable bumped : int
  }

(* The walk has a parser's shape and none of a parser's rules. It opens a
   node, puts tokens and nodes in it, and closes it. *)
let rec drive
          (rng : Random.State.t)
          (c : Lingo_runtime.Cursor.t)
          ~(depth : int)
          ~(reach : reach)
          ~(watch : Lingo_runtime.Cursor.t -> unit)
  : unit
  =
  for _ = 1 to Random.State.int rng 4 do
    (match Random.State.int rng 6 with
     | 0 when depth < 4 ->
       reach.opened <- reach.opened + 1;
       Lingo_runtime.Build.start_node c k_node;
       drive rng c ~depth:(depth + 1) ~reach ~watch;
       Lingo_runtime.Build.finish_node c
     | 1 when depth < 4 ->
       reach.wrapped <- reach.wrapped + 1;
       let cp = Lingo_runtime.Build.mark c in
       drive rng c ~depth:(depth + 1) ~reach ~watch;
       Lingo_runtime.Build.start_node_at c cp k_node;
       Lingo_runtime.Build.finish_node c
     | 2 ->
       reach.missing <- reach.missing + 1;
       Lingo_runtime.Build.missing_node c k_missing
     | 3 ->
       reach.skipped <- reach.skipped + 1;
       Lingo_runtime.Cursor.skip_trivia c
     | _ ->
       reach.bumped <- reach.bumped + 1;
       Lingo_runtime.Cursor.bump c);
    watch c
  done
;;

let () =
  let rng = Random.State.make [| 20260911 |] in
  let streams = 600 in
  let reach = { opened = 0; wrapped = 0; missing = 0; skipped = 0; bumped = 0 } in
  let lossless = ref 0 in
  let steps = ref 0 in
  let backwards = ref 0 in
  let overrun = ref 0 in
  let stuck = ref 0 in
  let off_wrong = ref 0 in
  let tokens_seen = ref 0 in
  for _ = 1 to streams do
    let tokens = random_stream rng in
    let n = Array.length tokens in
    tokens_seen := !tokens_seen + n;
    let c = new_cursor tokens in
    (* The byte length of the first [i] tokens. [Cursor.offset] is maintained
       a token at a time as the parse takes them, and this reads it off the
       array instead. *)
    let prefix = Array.make (n + 1) 0 in
    for i = 0 to n - 1 do
      prefix.(i + 1) <- prefix.(i) + String.length tokens.(i).Lingo_runtime.Token.text
    done;
    let last = ref 0 in
    let watch c =
      let p = Lingo_runtime.Cursor.position c in
      incr steps;
      if p < !last then incr backwards;
      if p > n then incr overrun;
      if p <= n && Lingo_runtime.Cursor.offset c <> prefix.(p) then incr off_wrong;
      last := p
    in
    Lingo_runtime.Build.start_node c k_file;
    drive rng c ~depth:0 ~reach ~watch;
    if not (drain ~watch c ~n) then incr stuck;
    Lingo_runtime.Build.finish_node c;
    let root, _ = Lingo_runtime.Build.finish c in
    if String.equal (Siesta.Green.to_source root) (source_of tokens) then incr lossless
  done;
  if !lossless <> streams
  then
    fail "(a) %d of %d streams did not rebuild their input" (streams - !lossless) streams
  else if
    reach.opened = 0
    || reach.wrapped = 0
    || reach.missing = 0
    || reach.skipped = 0
    || reach.bumped = 0
  then
    fail
      "(a) the walk never reached an event: open %d, wrap %d, missing %d, skip %d, bump \
       %d"
      reach.opened
      reach.wrapped
      reach.missing
      reach.skipped
      reach.bumped
  else
    pass
      "a tree rebuilds its input on %d streams, %d tokens (open %d, wrap %d, missing %d, \
       skip %d, bump %d)"
      streams
      !tokens_seen
      reach.opened
      reach.wrapped
      reach.missing
      reach.skipped
      reach.bumped;
  if !backwards > 0
  then fail "(b) the cursor went backwards at %d of %d steps" !backwards !steps
  else if !overrun > 0
  then fail "(b) the cursor went past the last token at %d of %d steps" !overrun !steps
  else if !stuck > 0
  then
    fail
      "(b) the drain hit its ceiling on %d of %d streams, so the cursor stopped advancing"
      !stuck
      streams
  else if !off_wrong > 0
  then
    fail
      "(e) the offset disagreed with the tokens taken at %d of %d steps"
      !off_wrong
      !steps
  else
    pass
      "the cursor moves forward only over %d steps, every drain reached the end, and the \
       offset tracked the tokens taken"
      !steps
;;

(* -- (e) report_at ---------------------------------------------------------- *)

(* A parse that reports on input it has already taken reads the offset before
   and after, and reports over the two. So the diagnostic has to come back
   carrying exactly those bytes.

   The oracle joins the texts of the tokens the walk consumed between the two
   reads. That is a different route to the same span: the cursor counts bytes
   as it emits them, and this counts them off the array afterwards. Leading
   trivia is left out, because the start comes from [Cursor.range], which
   looks past it. *)
let () =
  let rng = Random.State.make [| 20260915 |] in
  let streams = 400 in
  let spans = ref 0 in
  let wrong = ref 0 in
  for _ = 1 to streams do
    let tokens = random_stream rng in
    let n = Array.length tokens in
    let c = new_cursor tokens in
    Lingo_runtime.Build.start_node c k_file;
    for _ = 1 to Random.State.int rng (n + 1) do
      Lingo_runtime.Cursor.bump c
    done;
    let expected =
      if Lingo_runtime.Cursor.eof c
      then None
      else (
        let lo = fst (Lingo_runtime.Cursor.range c) in
        (* The first meaningful token at or after the cursor. [lo] starts
           there, so the oracle has to as well. *)
        let from = ref (Lingo_runtime.Cursor.position c) in
        while
          !from < n
          && Lingo_runtime.Cursor.is_trivia c tokens.(!from).Lingo_runtime.Token.kind
        do
          incr from
        done;
        let first = !from in
        for _ = 1 to 1 + Random.State.int rng 4 do
          Lingo_runtime.Cursor.bump c
        done;
        let hi = Lingo_runtime.Cursor.offset c in
        Lingo_runtime.Cursor.report_at c (lo, hi) Lingo_runtime.Diagnostic.Unexpected;
        incr spans;
        let stop = Lingo_runtime.Cursor.position c in
        let texts =
          List.init (stop - first) (fun i -> tokens.(first + i).Lingo_runtime.Token.text)
        in
        Some ((lo, hi), String.concat "" texts))
    in
    ignore (drain c ~n);
    Lingo_runtime.Build.finish_node c;
    let root, diags = Lingo_runtime.Build.finish c in
    let source = Siesta.Green.to_source root in
    match expected, List.rev diags with
    | None, _ -> ()
    | Some ((lo, hi), text), last :: _ ->
      if last.Lingo_runtime.Diagnostic.range <> (lo, hi)
      then incr wrong
      else if not (String.equal (String.sub source lo (hi - lo)) text)
      then incr wrong
    | Some _, [] -> incr wrong
  done;
  if !spans = 0
  then fail "(e) no stream reported a span, so this says nothing"
  else if !wrong > 0
  then fail "(e) %d of %d spans did not name the bytes the parse took" !wrong !spans
  else pass "report_at names the bytes the parse took, over %d spans" !spans
;;

(* -- (c) finish refuses an open frame -------------------------------------- *)

let () =
  let c = new_cursor (lex "a") in
  Lingo_runtime.Build.start_node c k_file;
  Lingo_runtime.Build.start_node c k_node;
  match Lingo_runtime.Build.finish c with
  | _ -> fail "(c) finish returned a tree with a frame still open"
  | exception Failure _ -> pass "finish refuses a tree with a frame still open"
;;

(* -- (d) expect ----------------------------------------------------------- *)

(* Every node in the tree, with its kind, payload and child count. *)
let rec nodes (n : Siesta.Green.node) (acc : (int * int * int) list)
  : (int * int * int) list
  =
  let acc =
    (Siesta.Green.kind n, Siesta.Green.payload n, Siesta.Green.num_children n) :: acc
  in
  Array.fold_left
    (fun acc child ->
       match child with
       | Siesta.Green.Node m -> nodes m acc
       | Siesta.Green.Token _ -> acc)
    acc
    (Siesta.Green.children_array n)
;;

(* Asks for a token at every position, half the time for the one that is there
   and half the time for one that is not. The two halves are the whole of
   [expect]: it consumes, or it reports and stands still. *)
let () =
  let rng = Random.State.make [| 5391 |] in
  let streams = 600 in
  let hits = ref 0 in
  let misses = ref 0 in
  let placeholders = ref 0 in
  let bad_hit = ref 0 in
  let bad_miss = ref 0 in
  let bad_holes = ref 0 in
  let bad_payload = ref 0 in
  let lost_bytes = ref 0 in
  for _ = 1 to streams do
    let tokens = random_stream rng in
    let n = Array.length tokens in
    let c = new_cursor tokens in
    Lingo_runtime.Build.start_node c k_file;
    let want_holes = ref 0 in
    let steps = ref 0 in
    while (not (Lingo_runtime.Cursor.eof c)) && !steps <= n do
      incr steps;
      let here = Lingo_runtime.Cursor.current c in
      let before_pos = Lingo_runtime.Cursor.position c in
      let before_diags = List.length (Lingo_runtime.Cursor.diagnostics c) in
      if Random.State.bool rng
      then (
        incr hits;
        Lingo_runtime.Recover.expect c here k_message;
        if
          Lingo_runtime.Cursor.position c <= before_pos
          || List.length (Lingo_runtime.Cursor.diagnostics c) <> before_diags
        then incr bad_hit)
      else (
        incr misses;
        let absent = if here = k_semi then k_word else k_semi in
        let placeholder = Random.State.bool rng in
        if placeholder
        then (
          incr placeholders;
          incr want_holes;
          Lingo_runtime.Recover.expect ~placeholder:k_missing c absent k_message)
        else Lingo_runtime.Recover.expect c absent k_message;
        if
          Lingo_runtime.Cursor.current c <> here
          || List.length (Lingo_runtime.Cursor.diagnostics c) <> before_diags + 1
        then incr bad_miss;
        (* A miss leaves the cursor, so take the token to reach the next
           position. *)
        Lingo_runtime.Cursor.bump c)
    done;
    let _ : bool = drain c ~n in
    Lingo_runtime.Build.finish_node c;
    let root, diags = Lingo_runtime.Build.finish c in
    if not (String.equal (Siesta.Green.to_source root) (source_of tokens))
    then incr lost_bytes;
    let holes = List.filter (fun (k, _, _) -> k = k_missing) (nodes root []) in
    if List.length holes <> !want_holes then incr bad_holes;
    List.iter
      (fun (_, payload, children) ->
         let ok =
           children = 0
           && payload >= 1
           && payload <= List.length diags
           &&
           match (List.nth diags (payload - 1)).Lingo_runtime.Diagnostic.kind with
           | Lingo_runtime.Diagnostic.Missing _ -> true
           | _ -> false
         in
         if not ok then incr bad_payload)
      holes
  done;
  if !bad_hit > 0
  then
    fail
      "(d) expect did not take the token it asked for, on %d of %d tries"
      !bad_hit
      !hits
  else if !bad_miss > 0
  then
    fail
      "(d) a failed expect moved the cursor or reported the wrong number of diagnostics, \
       on %d of %d tries"
      !bad_miss
      !misses
  else if !bad_holes > 0
  then
    fail
      "(d) the tree held the wrong number of holes on %d of %d streams"
      !bad_holes
      streams
  else if !bad_payload > 0
  then
    fail
      "(d) %d holes carried a payload that does not index a Missing diagnostic"
      !bad_payload
  else if !lost_bytes > 0
  then
    fail "(d) a parse with holes in it lost bytes on %d of %d streams" !lost_bytes streams
  else if !hits = 0 || !misses = 0 || !placeholders = 0
  then
    fail
      "(d) a branch was never taken: %d hits, %d misses, %d placeholders"
      !hits
      !misses
      !placeholders
  else
    pass
      "expect takes it or reports it: %d hits, %d misses, %d placeholders over %d streams"
      !hits
      !misses
      !placeholders
      streams
;;

(* The same-offset fold. A committed production whose leading required
   children all fail at one cursor reports once per child. The tree
   keeps a hole per child and the list keeps one entry, so both holes carry
   the same id. *)
let () =
  let c = new_cursor (lex "a") in
  Lingo_runtime.Build.start_node c k_file;
  Lingo_runtime.Recover.expect ~placeholder:k_missing c k_semi k_message;
  Lingo_runtime.Recover.expect ~placeholder:k_missing c k_lparen k_message;
  let _ : bool = drain c ~n:1 in
  Lingo_runtime.Build.finish_node c;
  let root, diags = Lingo_runtime.Build.finish c in
  let holes = List.filter (fun (k, _, _) -> k = k_missing) (nodes root []) in
  let payloads = List.map (fun (_, p, _) -> p) holes in
  if List.length holes <> 2
  then fail "(d) two failed expects built %d holes rather than two" (List.length holes)
  else if List.length diags <> 1
  then
    fail
      "(d) two failed expects at one position reported %d diagnostics rather than one"
      (List.length diags)
  else if payloads <> [ 1; 1 ]
  then
    fail
      "(d) the two holes carry ids %s, and both should be 1"
      (String.concat "," (List.map string_of_int payloads))
  else pass "two failed expects at one position give two holes and one diagnostic"
;;

let () =
  if !failures = 0
  then print_endline "law_runtime: 0 failures"
  else (
    Printf.printf "law_runtime: %d failures\n" !failures;
    exit 1)
;;
