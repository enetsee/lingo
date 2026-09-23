open StdLabels

type engine =
  | Tables
  | Measured

(* The largest subtree a mutation splices. The counting tables are quadratic in
   this, and a splice larger than the input it goes into is a replacement
   rather than a mutation. *)
let subtree_cap = 32

type draws =
  { root : Random.State.t -> Sample.token list
  ; traced : (Sample.token list Bolts.Exact.t * int array) option
    (* The root's tables and the sizes it draws at, held so that a draw can be
       recorded and a recording replayed. [None] under [Measured], which hands
       back a closure and exposes no engine. *)
  ; how : string
  ; subtrees : Sample.token list Bolts.Exact.t option option array
    (* Indexed by rule id. The outer option is whether the tables have been
       asked for, the inner whether bolts built them. *)
  ; bands : (int * int) option option array (* The same, for the sizes. *)
  }

type t =
  { name : string
  ; facts : Core.Facts.t
  ; plan : Ir.Plan.t
  ; entry : int
  ; root : Core.Rule.id
  ; layout : Ir.Layout.t
  ; system : Bolts.system
  ; map : Sample.map
  ; lex : string -> Lingo_runtime.Token.t array
  ; boundary : string -> int -> bool
  ; window : int * int
  ; separators : string list
  ; respelt : bool array
  ; draws : draws
  }

(* -- the window ------------------------------------------------------------ *)

(* Half the mean to one and a half times it, clipped to the sizes the species
   admits. A finite species asked for more than it has draws nothing, and a
   species whose smallest structure is above the mean has to be asked for
   that. *)
let window_of (smallest : int) (largest : int option) (mean : int) : int * int =
  let target =
    match largest with
    | Some most when most < mean -> most
    | Some _ | None -> max mean smallest
  in
  let lo = max smallest (target / 2) in
  let hi =
    match largest with
    | Some most -> min most (target * 3 / 2)
    | None -> target * 3 / 2
  in
  lo, max lo hi
;;

(* The sizes in [lo, hi] a species has a structure at. *)
let attainable (exact : 'a Bolts.Exact.t) ((lo, hi) : int * int) : int array =
  let rec go (n : int) (acc : int list) : int list =
    if n < lo
    then acc
    else go (n - 1) (if Bolts.Exact.count exact n > 0. then n :: acc else acc)
  in
  Array.of_list (go hi [])
;;

(* Uniform over the sizes, then uniform over the structures of the size drawn.

   Not uniform over the structures in the window, which is what weighting the
   sizes by their counts would give. Counts grow exponentially in the size, so
   that puts very nearly every input at the top of the window, and a corpus
   wants the short ones: a recovery path reached by a two-token input is
   reached by nothing else. *)
let uniform (exact : 'a Bolts.Exact.t) (sizes : int array) (rng : Random.State.t) : 'a =
  Bolts.Exact.sample exact sizes.(Random.State.int rng (Array.length sizes)) rng
;;

(* -- what the formatter does with a token ---------------------------------- *)

(* Whether the fold drops this token and writes the spacing again. That is
   [Reformat] trivia and nothing else: a comment is [Preserve] and its bytes
   reach the output as they stand. *)
let respelt_of (f : Core.Facts.t) : bool array =
  let out = Array.make (Core.Facts.kind_count f) false in
  Array.iter f.tokens ~f:(fun (tok : Core.Token.def) ->
    if tok.trivia = Some Core.Grammar.Reformat
    then out.(Core.Kind.to_int tok.kind) <- true);
  out
;;

(* The separator texts a body's policy may add to a format. One per frame that
   has one, and the only bytes the fold writes of its own accord. *)
let separators_of (l : Ir.Layout.t) : string list =
  Array.fold_left l.rules ~init:[] ~f:(fun acc (r : Ir.Layout.rule) ->
    match r.frame with
    | Delimited { sep = Some s; _ } | Separated s ->
      if List.mem s.text ~set:acc then acc else s.text :: acc
    | Delimited { sep = None; _ } | Plain -> acc)
;;

(* -- building one ---------------------------------------------------------- *)

let make
      ~(lex : string -> Lingo_runtime.Token.t array)
      ?(comments : float = 0.)
      ?(engine : engine = Tables)
      ?(mean : int = 40)
      ~(name : string)
      (facts : Core.Facts.t)
  : t
  =
  let system, map = Sample.system_of ~comments facts in
  let root =
    match facts.roots with
    | first :: _ -> first
    | [] -> invalid_arg (Printf.sprintf "Harness.make: %s declares no root" name)
  in
  let nt =
    match Sample.nonterminal map root with
    | Some nt -> nt
    | None ->
      invalid_arg (Printf.sprintf "Harness.make: %s's root rule has no species" name)
  in
  Bolts.Analysis.check system nt;
  let window =
    window_of (Bolts.Analysis.min_size system nt) (Bolts.Analysis.max_size system nt) mean
  in
  let root_draw, traced, how =
    match engine with
    | Measured ->
      let draw, choice = Bolts.Strategy.auto system nt window in
      ( draw
      , None
      , (match choice.strategy with
         | Bolts.Strategy.Exact k -> Printf.sprintf "measured, exact %d" k
         | Bolts.Strategy.Boltzmann b ->
           Printf.sprintf
             "measured, boltzmann %d pointings %s"
             b.points
             (match b.target with
              | Bolts.Sampler.Singular -> "singular"
              | Bolts.Sampler.Mean m -> Printf.sprintf "at mean %.0f" m
              | Bolts.Sampler.Profile _ -> "profiled")) )
    | Tables ->
      let exact = Bolts.Exact.make system nt ~max:(snd window) in
      let sizes = attainable exact window in
      if Array.length sizes = 0
      then
        invalid_arg
          (Printf.sprintf
             "Harness.make: %s has no structure of any size in %d to %d"
             name
             (fst window)
             (snd window));
      uniform exact sizes, Some (exact, sizes), Printf.sprintf "tables to %d" (snd window)
  in
  let plan, _ = Plan.Lower.of_facts facts in
  let layout = Layout.Lower.of_facts facts in
  let rules = Array.length facts.rules in
  { name
  ; facts
  ; plan
  ; entry = plan.Ir.Plan.roots.(0)
  ; root
  ; layout
  ; system
  ; map
  ; lex
  ; boundary = Lingo_runtime.Layout.boundary ~lex
  ; window
  ; separators = separators_of layout
  ; respelt = respelt_of facts
  ; draws =
      { root = root_draw
      ; traced
      ; how
      ; subtrees = Array.make rules None
      ; bands = Array.make rules None
      }
  }
;;

let engine_of (t : t) : string = t.draws.how
let draw (t : t) (rng : Random.State.t) : Sample.token list = t.draws.root rng

type trace =
  { size : int
  ; events : Bolts.Source.trace
  }

(* The size is drawn first and the structure after, which is the order
   [uniform] takes them in. Recording adds a write per decision and draws
   nothing of its own, so the generator is left in the same state either way. *)
let draw_traced (t : t) (rng : Random.State.t) : (Sample.token list * trace) option =
  match t.draws.traced with
  | None -> None
  | Some (exact, sizes) ->
    let size = sizes.(Random.State.int rng (Array.length sizes)) in
    let tokens, events = Bolts.Exact.sample_traced exact size rng in
    Some (tokens, { size; events })
;;

let replay (t : t) (trace : trace) : Sample.token list option =
  match t.draws.traced with
  | None -> None
  | Some (exact, _) -> Bolts.Exact.replay exact trace.size trace.events
;;

let decode (t : t) (tokens : Sample.token list) : string =
  Sample.decode t.facts t.map tokens
;;

(* -- drawing at a rule ----------------------------------------------------- *)

(* The tables for one rule, built the first time one is asked for. Sixty rules
   is sixty sets of tables, 3.1 s on grammars/effekt_grammar.ml against 0.16 s
   for twenty thousand draws from them, so a law that never mutates must not
   pay for them. Whether bolts could share the solve across nonterminals is
   open in its own design notes. *)
let tables_at (t : t) (rule : Core.Rule.id) : Sample.token list Bolts.Exact.t option =
  match t.draws.subtrees.(rule) with
  | Some built -> built
  | None ->
    let built =
      match Sample.nonterminal t.map rule with
      | None -> None
      | Some nt ->
        (try Some (Bolts.Exact.make t.system nt ~max:subtree_cap) with
         | Invalid_argument _ -> None)
    in
    t.draws.subtrees.(rule) <- Some built;
    built
;;

let sizes_at (t : t) (rule : Core.Rule.id) : (int * int) option =
  match t.draws.bands.(rule) with
  | Some band -> band
  | None ->
    let band =
      match tables_at t rule with
      | None -> None
      | Some exact ->
        (match attainable exact (0, subtree_cap) with
         | [||] -> None
         | sizes -> Some (sizes.(0), sizes.(Array.length sizes - 1)))
    in
    t.draws.bands.(rule) <- Some band;
    band
;;

let draw_at (t : t) (rule : Core.Rule.id) ~(size : int) (rng : Random.State.t)
  : Sample.token list option
  =
  match tables_at t rule with
  | None -> None
  | Some exact ->
    if size < 0 || size > subtree_cap || Bolts.Exact.count exact size <= 0.
    then None
    else Some (Bolts.Exact.sample exact size rng)
;;

(* -- running one ----------------------------------------------------------- *)

(* [Ir.Plan.t.rules] is built one for one from [Core.Facts.t.rules], so a rule's
   id is the index a parse enters at. *)
let parse ?(cover : Coverage.t option) ?(at : Core.Rule.id option) (t : t) (src : string)
  : Siesta.Green.node * Lingo_runtime.Diagnostic.t list
  =
  let tokens = t.lex src in
  let entry = Option.value at ~default:t.entry in
  match cover with
  | None -> Interp.run t.plan entry tokens
  | Some c ->
    Coverage.walk c;
    Interp.run
      ~at:(fun state ~index:_ ~reported:_ -> Coverage.at c state)
      t.plan
      entry
      tokens
;;

let enterable (t : t) (rule : Core.Rule.id) : bool = Interp.may_enter t.plan rule

let dispatches (t : t) (rule : Core.Rule.id) (src : string) : bool =
  let first =
    List.map
      ~f:Core.Kind.to_int
      (Core.Kind.Set.elements (Core.Facts.first_of t.facts rule))
  in
  match
    Array.find_opt (t.lex src) ~f:(fun (tok : Lingo_runtime.Token.t) ->
      not (Array.exists t.plan.trivia ~f:(fun k -> k = tok.kind)))
  with
  | None -> false
  | Some tok -> List.mem tok.kind ~set:first
;;

let doc (t : t) (tree : Siesta.Green.node) : Ir.Kind.t Handsome.Utf8.t =
  Lingo_runtime.Layout.doc t.layout ~boundary:t.boundary tree
;;

let format (t : t) ~(width : int) (tree : Siesta.Green.node) : string =
  Lingo_runtime.Layout.format t.layout ~boundary:t.boundary ~width tree
;;

let written (t : t) (tree : Siesta.Green.node) : (int * string) list =
  let acc = ref [] in
  let rec go (node : Siesta.Green.node) : unit =
    Array.iter (Siesta.Green.children_array node) ~f:(function
      | Siesta.Green.Node child -> go child
      | Siesta.Green.Token tok ->
        let kind = Siesta.Green.Token.kind tok in
        let text = Siesta.Green.Token.text tok in
        (* A token with no bytes is one recovery inserted. It writes nothing,
           so the lexer never gives it back and it is not in the output. *)
        if (not t.respelt.(kind)) && text <> "" then acc := (kind, text) :: !acc)
  in
  go tree;
  List.rev !acc
;;

let relex (t : t) (src : string) : (int * string) list =
  Array.to_list (t.lex src)
  |> List.filter_map ~f:(fun (tok : Lingo_runtime.Token.t) ->
    if t.respelt.(tok.kind) then None else Some (tok.kind, tok.text))
;;

let of_tokens (tokens : Sample.token list) : (int * string) list =
  List.map tokens ~f:(fun (tok : Sample.token) -> Core.Kind.to_int tok.kind, tok.text)
;;

(* [Core.Kind.t] is abstract, so the kind a token carries has to be the one the
   facts hold rather than the integer siesta records. *)
let tokens_of (t : t) (src : string) : Sample.token list option =
  let kinds = Hashtbl.create 64 in
  Array.iter t.facts.tokens ~f:(fun (tok : Core.Token.def) ->
    Hashtbl.replace kinds (Core.Kind.to_int tok.kind) tok.kind);
  let rec go (acc : Sample.token list) (read : (int * string) list)
    : Sample.token list option
    =
    match read with
    | [] -> Some (List.rev acc)
    | (kind, text) :: rest ->
      (match Hashtbl.find_opt kinds kind with
       | None -> None
       | Some kind -> go ({ Sample.kind; text } :: acc) rest)
  in
  go [] (relex t src)
;;

(* -- the tree as spans of the token list ----------------------------------- *)

module Span = struct
  type t =
    { kind : int
    ; from : int
    ; upto : int
    ; items : item list
    }

  and item =
    | Kid of t
    | Leaf of
        { kind : int
        ; at : int
        }

  let rec under (s : t) (acc : t list) : t list =
    List.fold_left s.items ~init:(s :: acc) ~f:(fun acc item ->
      match item with
      | Kid kid -> under kid acc
      | Leaf _ -> acc)
  ;;

  let every (s : t) : t list = under s []

  let kids (s : t) : t list =
    List.filter_map s.items ~f:(function
      | Kid kid -> Some kid
      | Leaf _ -> None)
  ;;
end

let spans (t : t) (tree : Siesta.Green.node) : Span.t =
  let next = ref 0 in
  let rec go (node : Siesta.Green.node) : Span.t =
    let from = !next in
    let items =
      Array.fold_left (Siesta.Green.children_array node) ~init:[] ~f:(fun acc child ->
        match child with
        | Siesta.Green.Node inner -> Span.Kid (go inner) :: acc
        | Siesta.Green.Token tok ->
          let kind = Siesta.Green.Token.kind tok in
          if t.respelt.(kind) || Siesta.Green.Token.text tok = ""
          then acc
          else (
            let at = !next in
            incr next;
            Span.Leaf { kind; at } :: acc))
    in
    { Span.kind = Siesta.Green.kind node; from; upto = !next; items = List.rev items }
  in
  go tree
;;

let splice
      (tokens : Sample.token list)
      ~(from : int)
      ~(upto : int)
      (insert : Sample.token list)
  : Sample.token list
  =
  let all = Array.of_list tokens in
  let n = Array.length all in
  let from = max 0 (min n from) in
  let upto = max from (min n upto) in
  Array.to_list (Array.sub all ~pos:0 ~len:from)
  @ insert
  @ Array.to_list (Array.sub all ~pos:upto ~len:(n - upto))
;;

(* Every rule a walk from the root can reach: a child slot's symbols, a block's
   atoms, and the roles a block's parse builds. A walk over the grammar rather
   than over the corpus, so the two readings are independent: a rule this
   reaches and no draw ever built is a fact about the draws. *)
let reachable (t : t) : Core.Rule.id list =
  let seen = Hashtbl.create 64 in
  let blocks = Hashtbl.create 8 in
  Array.iter t.facts.blocks ~f:(fun (b : Core.Block.def) ->
    Hashtbl.replace blocks b.rule_id b);
  let rec go (id : Core.Rule.id) : unit =
    if not (Hashtbl.mem seen id)
    then (
      Hashtbl.replace seen id ();
      let d = t.facts.rules.(id) in
      Array.iter d.children ~f:(fun (c : Core.Rule.child) ->
        Array.iter c.alts ~f:(fun k ->
          Option.iter
            (fun (r : Core.Rule.def) -> go r.id)
            (Core.Facts.rule_of_kind t.facts k)));
      Option.iter
        (fun (b : Core.Block.def) ->
           Array.iter b.atoms ~f:(fun a ->
             Option.iter
               (fun (r : Core.Rule.def) -> go r.id)
               (Core.Facts.rule_of_kind t.facts a)))
        (Hashtbl.find_opt blocks id);
      Array.iter t.facts.rules ~f:(fun (r : Core.Rule.def) ->
        match r.origin with
        | Core.Rule.Pratt_role { block; _ } when block = id -> go r.id
        | Core.Rule.User | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> ()))
  in
  go t.root;
  List.sort ~cmp:compare (Hashtbl.fold (fun id () acc -> id :: acc) seen [])
;;

(* The rules a tree holds. *)
let built (t : t) (tree : Siesta.Green.node) : Core.Rule.id list =
  let seen = Hashtbl.create 32 in
  let rec go (n : Siesta.Green.node) : unit =
    let k = Siesta.Green.kind n in
    if k >= 0 && k < Core.Facts.kind_count t.facts
    then (
      let id = t.facts.kind_rule.(k) in
      if id >= 0 then Hashtbl.replace seen id ());
    Array.iter (Siesta.Green.children_array n) ~f:(function
      | Siesta.Green.Node m -> go m
      | Siesta.Green.Token _ -> ())
  in
  go tree;
  Hashtbl.fold (fun id () acc -> id :: acc) seen []
;;

let rule_of (t : t) (kind : Ir.Kind.t) : Core.Rule.id option =
  if kind < 0 || kind >= Core.Facts.kind_count t.facts
  then None
  else (
    let id = t.facts.kind_rule.(kind) in
    if id < 0
    then None
    else (
      match t.facts.rules.(id).origin with
      | Core.Rule.User | Core.Rule.Pratt_block -> Some id
      | Core.Rule.Pratt_role { block; _ } -> Some block))
;;
