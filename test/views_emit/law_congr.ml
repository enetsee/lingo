(* -- the generated congruences -------------------------------------------------

      (a) [R.congr ()] gives back every node of kind [R] with its own tag.
          So does [R.congr] with the identity given to any one child.
      (b) [R.congr] with a changing rule given to one child changes the
          elements of that child's slot and nothing else.
      (c) [R.congr] fails on a node of any other kind.

      Mechanism. Every input in test/inputs is parsed by the interpreter, and
      every part runs at every node of every tree, recovered ones included.
      The congruences are reached through [probe], which the generated
      signature leaves out. It calls a view's [congr] with a rule given to
      the child at one index, under that child's label.

      Part (b)'s oracle builds the expected node from the views module's
      [Slots.slots] and the facts, without the generated code. It changes each
      element of the slot whose shape the child's rule takes. A child of
      tokens takes a token rule, a child of rules and blocks a node rule, and a
      child with both takes both. That shape is read from the child's symbols
      here, and separately from the emitter's reading of them. So a label
      bound to the wrong index, or a child given the wrong shape of rule,
      changes elements the oracle does not.

      The slot walk itself is not checked here. test/views_emit/law_views.ml
      reads it against the parse.
   -------------------------------------------------------------------------- *)

(* No mutation record has been generated for lib/ocaml/rewrite.ml yet.
   Generate it with

     assay -config assay.conf -only ocaml *)

open StdLabels

type probe =
  int
  -> int option
  -> node:unit Lingo_runtime.Rewrite.t
  -> token:unit Lingo_runtime.Rewrite.Token.t
  -> unit Lingo_runtime.Rewrite.t option

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; inputs : Inputs.t
  ; slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option
  ; probe : probe
  }

let corpus : case list =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; inputs = Inputs.sexp
    ; slots = Emitted_views.Sexp_views.Slots.slots
    ; probe = Emitted_probe.Sexp_probe.probe
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; inputs = Inputs.json
    ; slots = Emitted_views.Json_views.Slots.slots
    ; probe = Emitted_probe.Json_probe.probe
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; inputs = Inputs.calc
    ; slots = Emitted_views.Calc_views.Slots.slots
    ; probe = Emitted_probe.Calc_probe.probe
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; inputs = Inputs.rassoc
    ; slots = Emitted_views.Rassoc_views.Slots.slots
    ; probe = Emitted_probe.Rassoc_probe.probe
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; inputs = Inputs.postfix
    ; slots = Emitted_views.Postfix_views.Slots.slots
    ; probe = Emitted_probe.Postfix_probe.probe
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; inputs = Inputs.shapes
    ; slots = Emitted_views.Shapes_views.Slots.slots
    ; probe = Emitted_probe.Shapes_probe.probe
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; inputs = Inputs.unicode
    ; slots = Emitted_views.Unicode_views.Slots.slots
    ; probe = Emitted_probe.Unicode_probe.probe
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; inputs = Inputs.recovery
    ; slots = Emitted_views.Recovery_views.Slots.slots
    ; probe = Emitted_probe.Recovery_probe.probe
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; inputs = Inputs.comments
    ; slots = Emitted_views.Comments_views.Slots.slots
    ; probe = Emitted_probe.Comments_probe.probe
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; inputs = Inputs.rust
    ; slots = Emitted_views.Rust_views.Slots.slots
    ; probe = Emitted_probe.Rust_probe.probe
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    ; slots = Emitted_views.Effekt_views.Slots.slots
    ; probe = Emitted_probe.Effekt_probe.probe
    }
  ; { name = "wide"
    ; grammar = Lingo_grammars.Wide_grammar.grammar
    ; inputs = Inputs.wide
    ; slots = Emitted_views.Wide_views.Slots.slots
    ; probe = Emitted_probe.Wide_probe.probe
    }
  ; { name = "ml"
    ; grammar = Lingo_grammars.Ml_grammar.grammar
    ; inputs = Inputs.ml
    ; slots = Emitted_views.Ml_views.Slots.slots
    ; probe = Emitted_probe.Ml_probe.probe
    }
  ]
;;

(* -- the law's rules ------------------------------------------------------- *)

let prime_node (cache : Siesta.Cache.t) (green : Siesta.Green.node) : Siesta.Green.node =
  let mark = Siesta.Green.mk_token cache ~kind:Lingo_runtime.Kind.none ~text:"'" in
  Siesta.Green.mk_node
    cache
    ~kind:(Siesta.Green.kind green)
    ~payload:(Siesta.Green.payload green)
    ~children:
      (Array.append (Siesta.Green.children_array green) [| Siesta.Green.Token mark |])
    ()
;;

let prime_token (cache : Siesta.Cache.t) (token : Siesta.Green.token) : Siesta.Green.token
  =
  Siesta.Green.mk_token
    cache
    ~kind:(Siesta.Green.Token.kind token)
    ~text:(Siesta.Green.Token.text token ^ "'")
;;

let node_rule : unit Lingo_runtime.Rewrite.t =
  fun ctx node ->
  Ok (prime_node (Lingo_runtime.Rewrite.Ctx.cache ctx) (Siesta.Syntax.green node))
;;

let token_rule : unit Lingo_runtime.Rewrite.Token.t =
  fun ctx token ->
  Ok (prime_token (Lingo_runtime.Rewrite.Ctx.cache ctx) (Siesta.Syntax.Token.green token))
;;

(* -- comparing results ----------------------------------------------------- *)

let rec same (a : Siesta.Green.node) (b : Siesta.Green.node) : bool =
  Siesta.Green.kind a = Siesta.Green.kind b
  && Siesta.Green.payload a = Siesta.Green.payload b
  && Siesta.Green.num_children a = Siesta.Green.num_children b
  &&
  let rec children (index : int) =
    index = Siesta.Green.num_children a
    || ((match Siesta.Green.nth_child a index, Siesta.Green.nth_child b index with
         | Some (Siesta.Green.Node x), Some (Siesta.Green.Node y) -> same x y
         | Some (Siesta.Green.Token x), Some (Siesta.Green.Token y) ->
           Siesta.Green.Token.kind x = Siesta.Green.Token.kind y
           && String.equal (Siesta.Green.Token.text x) (Siesta.Green.Token.text y)
         | _ -> false)
        && children (index + 1))
  in
  children 0
;;

let pp_result (ppf : Format.formatter) (r : (Siesta.Green.node, string) result) : unit =
  match r with
  | Ok green -> Format.fprintf ppf "Ok %S" (Siesta.Green.to_source green)
  | Error reason -> Format.fprintf ppf "Error %S" reason
;;

(* -- the oracle ------------------------------------------------------------ *)

(* Which elements of a child's slot its rule changes: nodes, tokens, or both. *)
let takes (f : Core.Facts.t) (c : Core.Rule.child) : bool * bool =
  let tokens = Array.exists c.alts ~f:(Core.Facts.is_token_kind f) in
  let nodes = Array.exists c.alts ~f:(fun k -> not (Core.Facts.is_token_kind f k)) in
  nodes || not tokens, tokens
;;

let expected
      (cache : Siesta.Cache.t)
      (node : Siesta.Syntax.t)
      (elems : Siesta.Syntax.elem list)
      ((nodes, tokens) : bool * bool)
  : Siesta.Green.node
  =
  let green = Siesta.Syntax.green node in
  let children = Siesta.Green.children_array green in
  List.iter elems ~f:(fun (elem : Siesta.Syntax.elem) ->
    match elem with
    | Siesta.Syntax.Node child when nodes ->
      children.(Siesta.Syntax.index_in_parent child)
      <- Siesta.Green.Node (prime_node cache (Siesta.Syntax.green child))
    | Siesta.Syntax.Token token when tokens ->
      children.(Siesta.Syntax.Token.index_in_parent token)
      <- Siesta.Green.Token (prime_token cache (Siesta.Syntax.Token.green token))
    | Siesta.Syntax.Node _ | Siesta.Syntax.Token _ -> ());
  Siesta.Green.mk_node
    cache
    ~kind:(Siesta.Green.kind green)
    ~payload:(Siesta.Green.payload green)
    ~children
    ()
;;

(* -- the law --------------------------------------------------------------- *)

let () =
  let before = Law.failures () in
  let nodes = ref 0 in
  let children = ref 0 in
  let trees = ref 0 in
  List.iter corpus ~f:(fun (c : case) ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> Law.fail "%s: the grammar does not check" c.name
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      let rule_of (k : int) : Core.Rule.def option =
        Array.find_opt f.rules ~f:(fun (d : Core.Rule.def) -> Core.Kind.to_int d.kind = k)
      in
      List.iter (Inputs.all c.inputs) ~f:(fun (src : string) ->
        incr trees;
        let tree, _ = Interp.run plan entry (Lex.run f src) in
        let root = Siesta.Syntax.of_root tree in
        let cache = Siesta.Cache.create_plain () in
        let ctx = Lingo_runtime.Rewrite.Ctx.create cache root () in
        let probe (k : int) (slot : int option) ~node ~token =
          c.probe k slot ~node ~token
        in
        let identity (k : int) (slot : int option) =
          probe
            k
            slot
            ~node:Lingo_runtime.Rewrite.id
            ~token:Lingo_runtime.Rewrite.Token.id
        in
        Seq.iter
          (fun (node : Siesta.Syntax.t) ->
             let k = Siesta.Syntax.kind node in
             let green = Siesta.Syntax.green node in
             let unchanged (part : string) (s : unit Lingo_runtime.Rewrite.t) =
               match s ctx node with
               | Ok result when Siesta.Green.equal result green -> ()
               | r ->
                 Law.fail
                   "(a) %s, kind %d: %s gives %a, a new node"
                   c.name
                   k
                   part
                   pp_result
                   r
             in
             match c.slots node, identity k None, rule_of k with
             | Some elems, Some congr, Some d ->
               incr nodes;
               unchanged "congr ()" congr;
               (* (c), against one other kind that has a view. *)
               (match
                  List.find_map
                    (List.of_seq (Seq.take 64 (Siesta.Syntax.descendants root)))
                    ~f:(fun (other : Siesta.Syntax.t) ->
                      if Siesta.Syntax.kind other <> k
                      then identity (Siesta.Syntax.kind other) None
                      else None)
                with
                | Some other ->
                  (match other ctx node with
                   | Error _ -> ()
                   | Ok _ ->
                     Law.fail "(c) %s, kind %d: another kind's congr succeeded" c.name k)
                | None -> ());
               Array.iteri d.children ~f:(fun (i : int) (child : Core.Rule.child) ->
                 incr children;
                 (match identity k (Some i) with
                  | None -> Law.fail "(a) %s, kind %d: no congr for child %d" c.name k i
                  | Some s -> unchanged (Printf.sprintf "child %d given id" i) s);
                 match probe k (Some i) ~node:node_rule ~token:token_rule with
                 | None -> ()
                 | Some s ->
                   let slot = if i < Array.length elems then elems.(i) else [] in
                   let want = expected cache node slot (takes f child) in
                   (match s ctx node with
                    | Ok got when same got want -> ()
                    | r ->
                      Law.fail
                        "(b) %s, kind %d, child %d: gives %a where %S was expected"
                        c.name
                        k
                        i
                        pp_result
                        r
                        (Siesta.Green.to_source want)));
               (match identity k (Some (Array.length d.children)) with
                | None -> ()
                | Some _ ->
                  Law.fail "(a) %s, kind %d: a congr for a child the view lacks" c.name k)
             | None, None, _ -> ()
             | Some _, None, _ | None, Some _, _ | _, _, None ->
               Law.fail "(a) %s, kind %d: the views and the congruences disagree" c.name k)
          (Siesta.Syntax.descendants root)));
  if Law.failures () = before
  then
    Law.pass
      "every congruence holds at %d nodes, over %d children, in %d trees"
      !nodes
      !children
      !trees
;;

let () = Law.summarise "law_congr"
