open StdLabels

type subject =
  { tokens : Sample.token list
  ; tree : Siesta.Green.node
  }

type outcome =
  | Fired of Sample.token list
  | Declined of string

type reach =
  { mutator : string
  ; attempts : int
  ; fired : int
  ; declines : (string * int) list
  }

type row =
  { mutable attempts : int
  ; mutable fired : int
  ; why : (string, int) Hashtbl.t
  }

type t =
  { h : Harness.t
  ; rows : (string, row) Hashtbl.t
  ; donors : (Core.Rule.id, Sample.token list list) Hashtbl.t
  ; lexemes : (int, string list) Hashtbl.t
    (* Texts seen for a pattern token's kind. A [Token] arm of a pattern token
       has to be spelled, and the automaton belongs to the sampler's own draw:
       every lexeme here came out of one. *)
  ; blocks : (Core.Rule.id, Core.Block.def) Hashtbl.t
  ; arms : (string, int) Hashtbl.t
  ; sources : (string, int) Hashtbl.t
  }

let names = [ "regenerate"; "repeat"; "graft"; "swap-arm"; "corrupt"; "drop" ]

let shapes =
  [ "node at a child slot"
  ; "token at a child slot"
  ; "node at an atom"
  ; "token at an atom"
  ]
;;

let create (h : Harness.t) : t =
  let blocks = Hashtbl.create 8 in
  Array.iter h.facts.blocks ~f:(fun (b : Core.Block.def) ->
    Hashtbl.replace blocks b.rule_id b);
  let rows = Hashtbl.create 8 in
  List.iter names ~f:(fun name ->
    Hashtbl.replace rows name { attempts = 0; fired = 0; why = Hashtbl.create 8 });
  { h
  ; rows
  ; donors = Hashtbl.create 32
  ; lexemes = Hashtbl.create 32
  ; blocks
  ; arms = Hashtbl.create 16
  ; sources = Hashtbl.create 8
  }
;;

(* -- the record ------------------------------------------------------------ *)

let bump (table : (string, int) Hashtbl.t) (key : string) : unit =
  Hashtbl.replace table key (1 + Option.value (Hashtbl.find_opt table key) ~default:0)
;;

(* The one site that writes a row. Every mutator's every exit is its own return
   value, so [attempts = fired + sum declines] is a property of this function
   rather than of the six below it. *)
let tally (t : t) (name : string) (body : unit -> outcome) : outcome =
  let row = Hashtbl.find t.rows name in
  row.attempts <- row.attempts + 1;
  let out = body () in
  (match out with
   | Fired _ -> row.fired <- row.fired + 1
   | Declined why -> bump row.why why);
  out
;;

let commonest (table : (string, int) Hashtbl.t) : (string * int) list =
  Hashtbl.fold (fun key count acc -> (key, count) :: acc) table []
  |> List.sort ~cmp:(fun (a, x) (b, y) -> if x = y then compare a b else compare y x)
;;

let reach (t : t) : reach list =
  List.map names ~f:(fun mutator ->
    let row = Hashtbl.find t.rows mutator in
    { mutator; attempts = row.attempts; fired = row.fired; declines = commonest row.why })
;;

let arms (t : t) : (string * int) list = commonest t.arms
let sources (t : t) : (string * int) list = commonest t.sources

(* -- editing the list ------------------------------------------------------ *)

let same (a : Sample.token list) (b : Sample.token list) : bool =
  List.length a = List.length b
  && List.for_all2 a b ~f:(fun (x : Sample.token) (y : Sample.token) ->
    Core.Kind.equal x.kind y.kind && String.equal x.text y.text)
;;

let pick (rng : Random.State.t) (xs : 'a list) : 'a option =
  match List.length xs with
  | 0 -> None
  | n -> Some (List.nth xs (Random.State.int rng n))
;;

(* -- drawing at a rule near a size ----------------------------------------- *)

(* A species has structures at some sizes and not others: one whose every
   shape adds two tokens has none at an odd size at all. So the sizes in the
   band are tried nearest the target first, rather than the target alone. *)
let draw_near (t : t) (rng : Random.State.t) (rule : Core.Rule.id) ~(target : int)
  : Sample.token list option
  =
  match Harness.sizes_at t.h rule with
  | None -> None
  | Some (lo, hi) ->
    let want = max lo (min hi target) in
    List.init ~len:(hi - lo + 1) ~f:(fun i -> lo + i)
    |> List.sort ~cmp:(fun a b -> compare (abs (a - want)) (abs (b - want)))
    |> List.find_map ~f:(fun size -> Harness.draw_at t.h rule ~size rng)
;;

(* -- what the grammar says about a position -------------------------------- *)

let rule_at (t : t) (kind : int) : Core.Rule.def option =
  if kind < 0 || kind >= Core.Facts.kind_count t.h.facts
  then None
  else (
    let id = t.h.facts.kind_rule.(kind) in
    if id < 0 then None else Some t.h.facts.rules.(id))
;;

let rule_name (d : Core.Rule.def) : string = Core.Grammar.Name.Rule.to_string d.name

let is_base_role (t : t) (kind : int) : bool =
  match rule_at t kind with
  | Some { origin = Core.Rule.Pratt_role { role = Core.Role.Base; _ }; _ } -> true
  | Some _ | None -> false
;;

(* The child slots of [parent] whose alternatives admit [kind]. A slot with one
   alternative is not an alternation and is left out. *)
let slots_admitting (parent : Core.Rule.def) (kind : int) : Core.Rule.child list =
  Array.to_list parent.children
  |> List.filter ~f:(fun (c : Core.Rule.child) ->
    Array.length c.alts > 1
    && Array.exists c.alts ~f:(fun alt -> Core.Kind.to_int alt = kind))
;;

(* Where a child sits, when it sits at an alternation. Two shapes, because a
   grammar has two ways to write one: a child slot with several symbols, and an
   expression block's atom set, which is written on the block and named by no
   slot. *)
type alternation =
  | At_slot of Core.Rule.child
  | At_atoms of Core.Block.def

let alternation (t : t) (parent : Harness.Span.t) (child_kind : int) : alternation option =
  match rule_at t parent.kind with
  | None -> None
  | Some d ->
    (match slots_admitting d child_kind with
     | [ slot ] -> Some (At_slot slot)
     | _ :: _ :: _ | [] ->
       (* An operand of an expression: the alternation is the block's atoms,
          and a role node above says which block. The [Base] role is the
          wrapper a token atom gets, and a rule atom stands unwrapped. *)
       (match d.origin with
        | Core.Rule.User | Core.Rule.Pratt_block -> None
        | Core.Rule.Pratt_role { block; _ } ->
          (match Hashtbl.find_opt t.blocks block with
           | None -> None
           | Some b ->
             let is_atom =
               Array.exists b.atoms ~f:(fun a -> Core.Kind.to_int a = child_kind)
             in
             if is_base_role t child_kind || is_atom then Some (At_atoms b) else None)))
;;

(* -- spelling one token ---------------------------------------------------- *)

let note_lexeme (t : t) (kind : int) (text : string) : unit =
  let seen = Option.value (Hashtbl.find_opt t.lexemes kind) ~default:[] in
  if not (List.mem text ~set:seen)
  then Hashtbl.replace t.lexemes kind (text :: (if List.length seen > 8 then [] else seen))
;;

let observe (t : t) (subject : subject) : unit =
  List.iter subject.tokens ~f:(fun (tok : Sample.token) ->
    note_lexeme t (Core.Kind.to_int tok.kind) tok.text)
;;

(* A keyword and a punctuation token spell themselves. A pattern token spells
   whatever it matched, and the walk that draws one belongs to the sampler, so
   what is offered here is a lexeme the corpus has already produced for that
   kind. A kind the corpus has never held cannot be spelled, and that is a
   decline rather than a guess. *)
let spell (t : t) (rng : Random.State.t) (tok : Core.Token.def) : string option =
  match Core.Token.text tok with
  | Some text -> Some text
  | None ->
    (match Hashtbl.find_opt t.lexemes (Core.Kind.to_int tok.kind) with
     | None | Some [] -> None
     | Some texts -> pick rng texts)
;;

(* The tokens an arm stands for, and the arm's name for the reach table. *)
let build_arm (t : t) (rng : Random.State.t) ~(target : int) (arm : Core.Kind.t)
  : (Sample.token list * string) option
  =
  match Core.Facts.token_of_kind t.h.facts arm with
  | Some tok ->
    Option.map
      (fun text ->
         ( [ { Sample.kind = tok.kind; text } ]
         , "Token " ^ Core.Grammar.Name.Token.to_string tok.name ))
      (spell t rng tok)
  | None ->
    (match Core.Facts.rule_of_kind t.h.facts arm with
     | None -> None
     | Some d ->
       Option.map
         (fun tokens -> tokens, "Rule " ^ rule_name d)
         (draw_near t rng d.id ~target))
;;

(* -- the six --------------------------------------------------------------- *)

(* A node at a rule with a species, other than the root: replacing the root's
   whole span is a fresh draw rather than an edit. *)
let targets (t : t) (root : Harness.Span.t) : Harness.Span.t list =
  List.filter (Harness.Span.every root) ~f:(fun s ->
    (not (s == root))
    &&
    match Harness.rule_of t.h s.kind with
    | None -> false
    | Some rule -> Harness.sizes_at t.h rule <> None)
;;

let regenerate (t : t) (rng : Random.State.t) (subject : subject) : outcome =
  tally t "regenerate" (fun () ->
    let root = Harness.spans t.h subject.tree in
    match pick rng (targets t root) with
    | None -> Declined "no node sits at a rule with a species"
    | Some target ->
      let rule = Option.get (Harness.rule_of t.h target.kind) in
      (match draw_near t rng rule ~target:(target.upto - target.from) with
       | None -> Declined "no size near the span has a structure"
       | Some drawn ->
         let edited =
           Harness.splice subject.tokens ~from:target.from ~upto:target.upto drawn
         in
         if same edited subject.tokens
         then Declined "the draw is the span it replaces"
         else Fired edited))
;;

let repeat (t : t) (rng : Random.State.t) (subject : subject) : outcome =
  tally t "repeat" (fun () ->
    let root = Harness.spans t.h subject.tree in
    (* A pair of adjacent children of one kind, in a slot the rule repeats.
       Two children of the same kind at fixed positions are not a repetition,
       and duplicating one adds a slot the grammar does not admit. *)
    let pairs =
      List.concat_map (Harness.Span.every root) ~f:(fun (parent : Harness.Span.t) ->
        match rule_at t parent.kind with
        | None -> []
        | Some d ->
          let repeats (kind : int) =
            Array.exists d.children ~f:(fun (c : Core.Rule.child) ->
              (match c.modifier with
               | Core.Grammar.Zero_or_more | Core.Grammar.One_or_more -> true
               | Core.Grammar.Exactly_one | Core.Grammar.Zero_or_one -> false)
              && Array.exists c.alts ~f:(fun alt -> Core.Kind.to_int alt = kind))
          in
          let rec adjacent (kids : Harness.Span.t list) =
            match kids with
            | first :: (second :: _ as rest) ->
              (if first.kind = second.kind && repeats second.kind
               then [ first, second ]
               else [])
              @ adjacent rest
            | [ _ ] | [] -> []
          in
          adjacent (Harness.Span.kids parent))
    in
    match pick rng pairs with
    | None -> Declined "no parent repeats a child of one kind twice"
    | Some (first, second) ->
      (* From the end of the first element rather than the start of the
         second, so the copy carries the separator between them and the next
         parse reads a repetition rather than two elements with nothing
         between. *)
      let slice =
        List.filteri subject.tokens ~f:(fun i _ -> i >= first.upto && i < second.upto)
      in
      if slice = []
      then Declined "the element between them holds no token"
      else Fired (Harness.splice subject.tokens ~from:second.upto ~upto:second.upto slice))
;;

let admit (t : t) (subject : subject) : unit =
  observe t subject;
  let root = Harness.spans t.h subject.tree in
  List.iter (targets t root) ~f:(fun (s : Harness.Span.t) ->
    let rule = Option.get (Harness.rule_of t.h s.kind) in
    let slice = List.filteri subject.tokens ~f:(fun i _ -> i >= s.from && i < s.upto) in
    if slice <> []
    then (
      let held = Option.value (Hashtbl.find_opt t.donors rule) ~default:[] in
      (* Eight per rule. A donor pool that grows with the corpus is the corpus
         held twice over, and what a graft wants is a shape from somewhere
         else rather than every shape there has been. *)
      if not (List.exists held ~f:(fun other -> same other slice))
      then Hashtbl.replace t.donors rule (slice :: List.filteri held ~f:(fun i _ -> i < 7))))
;;

let graft (t : t) (rng : Random.State.t) (subject : subject) : outcome =
  tally t "graft" (fun () ->
    let root = Harness.spans t.h subject.tree in
    let offered =
      List.filter_map (targets t root) ~f:(fun (s : Harness.Span.t) ->
        let rule = Option.get (Harness.rule_of t.h s.kind) in
        match Hashtbl.find_opt t.donors rule with
        | None | Some [] -> None
        | Some pool -> Some (s, pool))
    in
    match pick rng offered with
    | None -> Declined "no donor matches a node in the tree"
    | Some (target, pool) ->
      (match pick rng pool with
       | None -> Declined "no donor matches a node in the tree"
       | Some donor ->
         let edited =
           Harness.splice subject.tokens ~from:target.from ~upto:target.upto donor
         in
         if same edited subject.tokens
         then Declined "the donor is the span it replaces"
         else Fired edited))
;;

let swap_arm (t : t) (rng : Random.State.t) (subject : subject) : outcome =
  tally t "swap-arm" (fun () ->
    observe t subject;
    let root = Harness.spans t.h subject.tree in
    (* Both ends of both alternations. A slot holds whichever shape the arm it
       matched has, so a slot whose every arm is a token is one no node can
       stand in, and a walk that saw only nodes would never reach it. *)
    let sites =
      List.concat_map (Harness.Span.every root) ~f:(fun (parent : Harness.Span.t) ->
        List.filter_map parent.items ~f:(fun (item : Harness.Span.item) ->
          let kind, from, upto, shape =
            match item with
            | Harness.Span.Kid kid -> kid.kind, kid.from, kid.upto, "node"
            | Harness.Span.Leaf leaf -> leaf.kind, leaf.at, leaf.at + 1, "token"
          in
          Option.map
            (fun where -> where, kind, from, upto, shape)
            (alternation t parent kind)))
    in
    match pick rng sites with
    | None -> Declined "nothing in the tree sits at an alternation"
    | Some (where, kind, from, upto, shape) ->
      let offered, at =
        match where with
        | At_slot slot -> Array.to_list slot.alts, "a child slot"
        | At_atoms b -> Array.to_list b.atoms, "an atom"
      in
      (* The arm the child already matches, dropped. A rule arm is matched
         through its rule rather than its kind, because an expression's node
         carries the role's kind and the arm names the block. *)
      let mine (alt : Core.Kind.t) : bool =
        Core.Kind.to_int alt = kind
        ||
        match Harness.rule_of t.h kind, Harness.rule_of t.h (Core.Kind.to_int alt) with
        | Some a, Some b -> a = b
        | Some _, None | None, Some _ | None, None -> false
      in
      (match List.filter offered ~f:(fun alt -> not (mine alt)) with
       | [] -> Declined "the alternation offers no arm but the one there"
       | others ->
         let target = upto - from in
         (match
            List.filter_map (List.map others ~f:(build_arm t rng ~target)) ~f:Fun.id
          with
          | [] -> Declined "no other arm could be built"
          | built ->
            let tokens, arm = List.nth built (Random.State.int rng (List.length built)) in
            let edited = Harness.splice subject.tokens ~from ~upto tokens in
            if same edited subject.tokens
            then Declined "the arm built is the arm that was there"
            else (
              let label =
                match where with
                | At_slot _ -> arm
                | At_atoms b ->
                  Printf.sprintf
                    "%s atom %s"
                    (Core.Grammar.Name.Rule.to_string b.name)
                    arm
              in
              bump t.arms label;
              bump t.sources (Printf.sprintf "%s at %s" shape at);
              Fired edited))))
;;

(* The bytes a corruption may put in. Every byte of every literal the grammar
   declares, and every byte this input already holds: the first reaches the
   keywords and the delimiters, and the second reaches what a pattern token
   matched -- a quote, a backslash, a digit inside a string. *)
let alphabet (t : t) (subject : subject) : string =
  let seen = Bytes.make 256 '\000' in
  let take (text : string) =
    String.iter text ~f:(fun c -> Bytes.set seen (Char.code c) '\001')
  in
  Array.iter t.h.facts.tokens ~f:(fun (tok : Core.Token.def) ->
    Option.iter take (Core.Token.text tok));
  List.iter subject.tokens ~f:(fun (tok : Sample.token) -> take tok.text);
  String.init 256 ~f:Char.chr
  |> String.to_seq
  |> Seq.filter (fun c -> Bytes.get seen (Char.code c) = '\001')
  |> String.of_seq
;;

let corrupt (t : t) (rng : Random.State.t) (subject : subject) : outcome =
  tally t "corrupt" (fun () ->
    if not (List.exists subject.tokens ~f:(fun (tok : Sample.token) -> tok.text <> ""))
    then Declined "no token has bytes to change"
    else (
      let pool = alphabet t subject in
      if String.length pool = 0
      then Declined "the grammar declares no literal byte"
      else (
        let at = Random.State.int rng (List.length subject.tokens) in
        let chosen : Sample.token = List.nth subject.tokens at in
        if chosen.text = ""
        then Declined "the token drawn has no bytes"
        else (
          let byte = Random.State.int rng (String.length chosen.text) in
          let put = pool.[Random.State.int rng (String.length pool)] in
          if chosen.text.[byte] = put
          then Declined "the byte drawn is the byte there"
          else (
            let text = Bytes.of_string chosen.text in
            Bytes.set text byte put;
            Fired
              (List.mapi subject.tokens ~f:(fun i (tok : Sample.token) ->
                 if i = at then { tok with text = Bytes.to_string text } else tok)))))))
;;

(* [repeat]'s inverse, and the one that reaches recovery hardest. A token gone
   is a frame left open, a separator missing between two elements, a delimiter
   whose partner is still there -- the shapes a parse has to recover from and a
   draw from the language never holds. Corrupting a token's bytes reaches some
   of those; taking one out reaches them all. *)
let drop (t : t) (rng : Random.State.t) (subject : subject) : outcome =
  tally t "drop" (fun () ->
    match List.length subject.tokens with
    | 0 -> Declined "the input holds no token"
    | 1 -> Declined "the input holds one token, and dropping it leaves nothing"
    | n ->
      let at = Random.State.int rng n in
      Fired (List.filteri subject.tokens ~f:(fun i _ -> i <> at)))
;;

(* -- one iteration --------------------------------------------------------- *)

let six =
  [| "regenerate", regenerate
   ; "repeat", repeat
   ; "graft", graft
   ; "swap-arm", swap_arm
   ; "corrupt", corrupt
   ; "drop", drop
  |]
;;

let apply (t : t) (rng : Random.State.t) (subject : subject)
  : (string * Sample.token list) option
  =
  observe t subject;
  (* From a random start rather than always the first, so a mutator's share of
     the iterations does not depend on how often the one in front of it
     declines. *)
  let start = Random.State.int rng (Array.length six) in
  let rec go (tried : int) : (string * Sample.token list) option =
    if tried >= Array.length six
    then None
    else (
      let name, mutator = six.((start + tried) mod Array.length six) in
      match mutator t rng subject with
      | Fired tokens -> Some (name, tokens)
      | Declined _ -> go (tried + 1))
  in
  go 0
;;
