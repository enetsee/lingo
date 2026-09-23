open StdLabels

type edit =
  | Trace of Harness.trace
  | Tokens

type reduced =
  { tokens : Sample.token list
  ; src : string
  ; before : int
  ; after : int
  ; moves : int
  ; tried : int
  ; capped : bool
  ; reproduces : bool
  }

(* The most candidates any class has taken is 385, over the mutations law_fuzz
   reduces under, at both depths. Three times that leaves room for a longer
   witness and costs a red run about two seconds a class. *)
let default_limit = 1200

(* The seed a replacement draw comes from. Fixed, so a state gives the same
   candidates on every run and a reduction in a record is reproducible. *)
let seed = [| 0x5A17 |]

(* Bytes first, because that is what the report prints, and the token count
   after it so that two spellings of one length still order. Every move has to
   make this strictly smaller, so the loop settles. *)
let rank (tokens : Sample.token list) (src : string) : int * int =
  String.length src, List.length tokens
;;

(* Take the first candidate that is smaller and still reproduces, then start
   again from it. The candidates arrive largest reduction first, so the one
   that sticks is the biggest cut available at that step.

   [Bolts.Sampler.minimize] is this loop over a [Sampler.t]. The engine a law
   fixes is [Bolts.Exact], which records and replays a trace and has no
   [minimize], so the loop is written here and the candidates come from
   [Bolts.Sampler.shrink_candidates], which edits a trace and holds no engine. *)
let greedy
      (type state)
      ~(render : state -> (Sample.token list * string) option)
      ~(candidates : state -> state Seq.t)
      ~(holds : Sample.token list -> string -> bool)
      ~(limit : int)
      (start : state)
      (tokens : Sample.token list)
      (src : string)
  : state * Sample.token list * string * int * int * bool
  =
  let tried = ref 0
  and moves = ref 0
  and capped = ref false in
  let best = ref (start, tokens, src) in
  let smallest = ref (rank tokens src) in
  let rec scan (seq : state Seq.t)
    : (state * Sample.token list * string * (int * int)) option
    =
    match seq () with
    | Seq.Nil -> None
    | Seq.Cons (candidate, rest) ->
      if !tried >= limit
      then (
        capped := true;
        None)
      else (
        match render candidate with
        | None -> scan rest
        | Some (tokens, src) ->
          let size = rank tokens src in
          if size >= !smallest
          then scan rest
          else (
            incr tried;
            if holds tokens src then Some (candidate, tokens, src, size) else scan rest))
  in
  let rec settle () : unit =
    let state, _, _ = !best in
    match scan (candidates state) with
    | None -> ()
    | Some (candidate, tokens, src, size) ->
      best := candidate, tokens, src;
      smallest := size;
      incr moves;
      settle ()
  in
  settle ();
  let state, tokens, src = !best in
  state, tokens, src, !moves, !tried, !capped
;;

(* -- editing a trace ------------------------------------------------------- *)

(* Bolts' own candidate traces at the size the draw was made at, then the same
   trace at every smaller size. Editing a trace never shortens the input on its
   own, because [Bolts.Exact] draws a structure of the size it is given. *)
let trace_candidates (trace : Harness.trace) : Harness.trace Seq.t =
  Seq.append
    (Seq.map
       (fun (events : Bolts.Source.trace) -> { trace with Harness.events })
       (Bolts.Sampler.shrink_candidates trace.events))
    (Seq.init trace.size (fun (size : int) -> { trace with Harness.size }))
;;

(* -- editing a token list -------------------------------------------------- *)

(* The moves come largest reduction first. A node's whole span goes, then a
   node's span is replaced by the smallest structure its rule has, then one
   token goes, then one token's bytes are cut back.

   The spans come from a parse of the candidate in hand rather than of the
   witness, so a move that changed the tree is followed by moves against the
   tree it left.

   The root is left out of the first two. Cutting its span leaves nothing, and
   replacing it draws a fresh input.

   A sampler spells a pattern token by walking the automaton, so the
   identifiers in a draw run to five and six characters. Cutting them back is
   the difference between M17's witness stopping at 18 bytes and at 12. Each
   token is tried at one character, at half its length, and one character
   short. *)
let token_candidates
      (h : Harness.t)
      ?(at : Core.Rule.id option)
      (tokens : Sample.token list)
  : Sample.token list Seq.t
  =
  let tree, _ = Harness.parse ?at h (Harness.decode h tokens) in
  let root = Harness.spans h tree in
  let nodes =
    List.filter (Harness.Span.every root) ~f:(fun (s : Harness.Span.t) ->
      (not (s == root)) && s.upto > s.from)
    |> List.sort ~cmp:(fun (a : Harness.Span.t) (b : Harness.Span.t) ->
      compare (b.upto - b.from) (a.upto - a.from))
  in
  let cut =
    Seq.map
      (fun (s : Harness.Span.t) -> Harness.splice tokens ~from:s.from ~upto:s.upto [])
      (List.to_seq nodes)
  in
  let smaller =
    Seq.filter_map
      (fun (s : Harness.Span.t) ->
         match Harness.rule_of h s.kind with
         | None -> None
         | Some rule ->
           (match Harness.sizes_at h rule with
            | None -> None
            | Some (lo, _) when lo >= s.upto - s.from -> None
            | Some (lo, _) ->
              Option.map
                (fun (drawn : Sample.token list) ->
                   Harness.splice tokens ~from:s.from ~upto:s.upto drawn)
                (Harness.draw_at h rule ~size:lo (Random.State.make seed))))
      (List.to_seq nodes)
  in
  let numbered = List.mapi tokens ~f:(fun (i : int) (tok : Sample.token) -> i, tok) in
  let drop =
    Seq.map
      (fun ((i, _) : int * Sample.token) ->
         Harness.splice tokens ~from:i ~upto:(i + 1) [])
      (List.to_seq numbered)
  in
  let cut_back =
    Seq.concat_map
      (fun ((i, tok) : int * Sample.token) ->
         let n = String.length tok.text in
         List.to_seq
           (List.filter_map
              [ 1; n / 2; n - 1 ]
              ~f:(fun (len : int) ->
                if len <= 0 || len >= n
                then None
                else
                  Some
                    (Harness.splice
                       tokens
                       ~from:i
                       ~upto:(i + 1)
                       [ { tok with Sample.text = String.sub tok.text ~pos:0 ~len } ]))))
      (List.to_seq numbered)
  in
  Seq.append cut (Seq.append smaller (Seq.append drop cut_back))
;;

(* A candidate has to lex back to the token list it was made from. The next
   round takes spans off a parse of its bytes, and an index from that parse
   lands in the right place only while the two agree. Cutting a lexeme back is
   the move that can break it, because a prefix of a lexeme need not lex as the
   token it came from. *)
let reads_back (t : Harness.t) (tokens : Sample.token list)
  : (Sample.token list * string) option
  =
  let src = Harness.decode t tokens in
  if Harness.relex t src = Harness.of_tokens tokens then Some (tokens, src) else None
;;

(* -- one witness ----------------------------------------------------------- *)

let reduce
      (t : Harness.t)
      ?(at : Core.Rule.id option)
      ?(limit : int = default_limit)
      (edit : edit)
      ~(holds : Sample.token list -> string -> bool)
      (src : string)
  : reduced
  =
  let before = String.length src in
  let decoded (tokens : Sample.token list) : Sample.token list * string =
    tokens, Harness.decode t tokens
  in
  (* Without a reading of the witness there is no move to make.
     [Harness.tokens_of] gives none where a mutation put in a byte the grammar
     does not declare. The witness still goes back out and [holds] is still
     asked about it, because a class whose witness does not reproduce is worth
     a failure either way. *)
  let nothing : reduced =
    { tokens = []
    ; src
    ; before
    ; after = before
    ; moves = 0
    ; tried = 1
    ; capped = false
    ; reproduces = holds [] src
    }
  in
  let out
        ((tokens, src, moves, tried, capped) :
          Sample.token list * string * int * int * bool)
    : reduced
    =
    { tokens
    ; src
    ; before
    ; after = String.length src
    ; moves
    ; tried
    ; capped
    ; reproduces = holds tokens src
    }
  in
  match edit with
  | Trace trace ->
    (match Option.map decoded (Harness.replay t trace) with
     | None -> nothing
     | Some (tokens, src) ->
       let _, tokens, src, moves, tried, capped =
         greedy
           ~render:(fun (trace : Harness.trace) ->
             Option.map decoded (Harness.replay t trace))
           ~candidates:trace_candidates
           ~holds
           ~limit
           trace
           tokens
           src
       in
       out (tokens, src, moves, tried, capped))
  | Tokens ->
    (match Option.map decoded (Harness.tokens_of t src) with
     | None -> nothing
     | Some (tokens, src) ->
       let _, tokens, src, moves, tried, capped =
         greedy
           ~render:(reads_back t)
           ~candidates:(token_candidates t ?at)
           ~holds
           ~limit
           tokens
           tokens
           src
       in
       out (tokens, src, moves, tried, capped))
;;
