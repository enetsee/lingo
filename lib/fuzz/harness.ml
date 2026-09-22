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
  let root_draw, how =
    match engine with
    | Measured ->
      let draw, choice = Bolts.Strategy.auto system nt window in
      ( draw
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
      uniform exact sizes, Printf.sprintf "tables to %d" (snd window)
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
      ; how
      ; subtrees = Array.make rules None
      ; bands = Array.make rules None
      }
  }
;;

let engine_of (t : t) : string = t.draws.how
let draw (t : t) (rng : Random.State.t) : Sample.token list = t.draws.root rng

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

let parse ?(cover : Coverage.t option) (t : t) (src : string)
  : Siesta.Green.node * Lingo_runtime.Diagnostic.t list
  =
  let tokens = t.lex src in
  match cover with
  | None -> Interp.run t.plan t.entry tokens
  | Some c ->
    Coverage.walk c;
    Interp.run
      ~at:(fun state ~index:_ ~reported:_ -> Coverage.at c state)
      t.plan
      t.entry
      tokens
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
