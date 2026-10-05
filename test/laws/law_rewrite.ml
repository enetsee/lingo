(* -- the rewriting combinators ------------------------------------------------

      (a) Stratego's identities hold at every node of every tree:
          [seq id s] and [seq s id] give what [s] gives, [choice (fail _) s]
          gives what [s] gives, and [try_ s] never fails. [all id],
          [topdown id] and [bottomup id] give the node's own green node, with
          its tag. [one (fail _)] and [some (fail _)] fail.
      (b) Each traversal changes the nodes its definition names, and no
          others. A subtree it did not change keeps its tag.
      (c) After [seq] changes a node, the second rule's cursor has the
          ancestors the node had, and it is in a new tree. Where nothing
          changed, it is the cursor the first rule had.
      (d) [repeat] stops where its rule fails, and fails where the fuel runs
          out. [collect] finds the nodes [alltd] changes, in source order.
      (e) [congruence] over one slot holding every child agrees with the
          one-level traversals. [Elems.all], [Elems.one] and [Elems.some]
          with a node rule succeed where [all], [one] and [some] do, and give
          the same node. [Elems.nth i] changes child node [i] alone. A token
          rule changes the tokens and passes the nodes through.

      Mechanism. Every input in test/inputs is parsed by the interpreter, and
      every part runs over every node of every tree, recovered ones included.
      The rules are the law's own and read no grammar. [prime] appends a
      token, so it always succeeds and always changes the node. [prime_once]
      fails on a node already primed, so a rule set built on it terminates.
      [odd] succeeds without a change on a node of odd kind.

      Results are compared by structure, because the interpreter parses
      through a plain cache. There every node built is a fresh record, so two
      rebuilds of one node carry different tags. For the same reason a tag
      that survives proves the node was not rebuilt. Part (a)'s tag checks and
      part (b)'s sharing check both read that.

      Part (b)'s oracle builds the expected tree without the combinators. It
      walks the original tree and appends the prime token at each node a
      predicate picks: every node of kind [k], the outermost ones, the first in
      preorder, or the first in postorder. Each traversal is run once per kind
      the tree holds, so a kind nested in itself is reached wherever the corpus
      nests one. A traversal that primes the parent where it should prime the
      last child gives the same text, so the comparison is by structure.

      Part (e) gives [congruence] a slot function of its own, which puts
      every child in one repeated slot. The generated congruences, with the
      grammar's real slots, are test/views_emit/law_congr.ml's.

      Part (c) reads the ancestors through [Syntax.ancestors] on both cursors
      and compares their kinds.

      What this says nothing about. The cache a parse used is not shared with
      the rewrite here, so the law cannot see a rebuilt node that is
      structurally the same as the one it replaced. Under a hash-consed cache
      that node would carry the old tag and read as unchanged.
   -------------------------------------------------------------------------- *)

(* No mutation record has been generated for lib/runtime/rewrite.ml yet.
   Generate it with

     assay -config assay.conf -only lingo_runtime *)

open StdLabels

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

(* Every tree in the corpus, with the grammar it came from. *)
let trees : (string * Siesta.Green.node) list =
  List.concat_map corpus ~f:(fun (c : case) ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ ->
      Law.fail "%s: the grammar does not check" c.name;
      []
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      List.map (Inputs.all c.inputs) ~f:(fun (src : string) ->
        let tree, _ = Interp.run plan entry (Lex.run f src) in
        c.name, tree))
;;

(* -- the law's rules ------------------------------------------------------- *)

let prime_kind : Ir.Kind.t = Lingo_runtime.Kind.none

let prime_green (cache : Siesta.Cache.t) (green : Siesta.Green.node) : Siesta.Green.node =
  let mark = Siesta.Green.mk_token cache ~kind:prime_kind ~text:"'" in
  Siesta.Green.mk_node
    cache
    ~kind:(Siesta.Green.kind green)
    ~payload:(Siesta.Green.payload green)
    ~children:
      (Array.append (Siesta.Green.children_array green) [| Siesta.Green.Token mark |])
    ()
;;

let prime : unit Lingo_runtime.Rewrite.t =
  fun ctx node ->
  Ok (prime_green (Lingo_runtime.Rewrite.Ctx.cache ctx) (Siesta.Syntax.green node))
;;

let primed (node : Siesta.Syntax.t) : bool =
  let green = Siesta.Syntax.green node in
  match Siesta.Green.nth_child green (Siesta.Green.num_children green - 1) with
  | Some (Siesta.Green.Token token) -> Siesta.Green.Token.kind token = prime_kind
  | Some (Siesta.Green.Node _) | None -> false
;;

let prime_once : unit Lingo_runtime.Rewrite.t =
  fun ctx node -> if primed node then Error "primed" else prime ctx node
;;

let odd : unit Lingo_runtime.Rewrite.t =
  fun _ node ->
  if Siesta.Syntax.kind node mod 2 = 1
  then Ok (Siesta.Syntax.green node)
  else Error "even"
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

let agree
      (x : (Siesta.Green.node, string) result)
      (y : (Siesta.Green.node, string) result)
  : bool
  =
  match x, y with
  | Ok a, Ok b -> same a b
  | Error a, Error b -> String.equal a b
  | Ok _, Error _ | Error _, Ok _ -> false
;;

let pp_result (ppf : Format.formatter) (r : (Siesta.Green.node, string) result) : unit =
  match r with
  | Ok green -> Format.fprintf ppf "Ok %S" (Siesta.Green.to_source green)
  | Error reason -> Format.fprintf ppf "Error %S" reason
;;

let ctx_of (root : Siesta.Syntax.t) : unit Lingo_runtime.Rewrite.Ctx.t =
  Lingo_runtime.Rewrite.Ctx.create (Siesta.Cache.create_plain ()) root ()
;;

(* -- (a) the identities ---------------------------------------------------- *)

let () =
  let before = Law.failures () in
  let nodes = ref 0 in
  let rules : (string * unit Lingo_runtime.Rewrite.t) list =
    [ "id", Lingo_runtime.Rewrite.id
    ; "fail", Lingo_runtime.Rewrite.fail "r"
    ; "odd", odd
    ; "prime", prime
    ; "prime_once", prime_once
    ; "kind prime", Lingo_runtime.Rewrite.kind 1 prime
    ]
  in
  List.iter trees ~f:(fun ((name : string), (tree : Siesta.Green.node)) ->
    let root = Siesta.Syntax.of_root tree in
    let ctx = ctx_of root in
    Seq.iter
      (fun (node : Siesta.Syntax.t) ->
         incr nodes;
         let kind = Siesta.Syntax.kind node in
         List.iter rules ~f:(fun ((rule : string), (s : unit Lingo_runtime.Rewrite.t)) ->
           let alone = s ctx node in
           let check (law : string) (r : (Siesta.Green.node, string) result) =
             if not (agree r alone)
             then
               Law.fail
                 "(a) %s, kind %d: %s gives %a where %s gives %a"
                 name
                 kind
                 law
                 pp_result
                 r
                 rule
                 pp_result
                 alone
           in
           check
             ("seq id " ^ rule)
             (Lingo_runtime.Rewrite.seq Lingo_runtime.Rewrite.id s ctx node);
           check
             ("seq " ^ rule ^ " id")
             (Lingo_runtime.Rewrite.seq s Lingo_runtime.Rewrite.id ctx node);
           check
             ("choice (fail) " ^ rule)
             (Lingo_runtime.Rewrite.choice (Lingo_runtime.Rewrite.fail "f") s ctx node);
           match Lingo_runtime.Rewrite.try_ s ctx node with
           | Ok _ -> ()
           | Error reason ->
             Law.fail "(a) %s, kind %d: try_ %s failed with %S" name kind rule reason);
         let green = Siesta.Syntax.green node in
         List.iter
           [ "all id", Lingo_runtime.Rewrite.all Lingo_runtime.Rewrite.id
           ; "topdown id", Lingo_runtime.Rewrite.topdown Lingo_runtime.Rewrite.id
           ; "bottomup id", Lingo_runtime.Rewrite.bottomup Lingo_runtime.Rewrite.id
           ]
           ~f:(fun ((law : string), (s : unit Lingo_runtime.Rewrite.t)) ->
             match s ctx node with
             | Ok result when Siesta.Green.equal result green -> ()
             | r ->
               Law.fail
                 "(a) %s, kind %d: %s gives %a, a new node"
                 name
                 kind
                 law
                 pp_result
                 r);
         List.iter
           [ "one (fail)", Lingo_runtime.Rewrite.one (Lingo_runtime.Rewrite.fail "f")
           ; "some (fail)", Lingo_runtime.Rewrite.some (Lingo_runtime.Rewrite.fail "f")
           ]
           ~f:(fun ((law : string), (s : unit Lingo_runtime.Rewrite.t)) ->
             match s ctx node with
             | Error _ -> ()
             | Ok _ -> Law.fail "(a) %s, kind %d: %s succeeded" name kind law))
      (Siesta.Syntax.descendants root));
  if Law.failures () = before
  then
    Law.pass "(a) the identities hold at %d nodes in %d trees" !nodes (List.length trees)
;;

(* -- (b) the traversals against an oracle ---------------------------------- *)

(* The original tree with [prime] applied at each node [pick] chooses, built
   bottom-up without the combinators. *)
let rec expected
          (cache : Siesta.Cache.t)
          (pick : Siesta.Syntax.t -> bool)
          (node : Siesta.Syntax.t)
  : Siesta.Green.node
  =
  let green = Siesta.Syntax.green node in
  let children =
    Array.map (Siesta.Syntax.children_array node) ~f:(fun (elem : Siesta.Syntax.elem) ->
      match elem with
      | Siesta.Syntax.Node child -> Siesta.Green.Node (expected cache pick child)
      | Siesta.Syntax.Token token -> Siesta.Green.Token (Siesta.Syntax.Token.green token))
  in
  let rebuilt =
    Siesta.Green.mk_node
      cache
      ~kind:(Siesta.Green.kind green)
      ~payload:(Siesta.Green.payload green)
      ~children
      ()
  in
  if pick node then prime_green cache rebuilt else rebuilt
;;

let has_kind (k : Ir.Kind.t) (node : Siesta.Syntax.t) : bool = Siesta.Syntax.kind node = k

(* A node of kind [k] with no strict ancestor of kind [k]. *)
let outermost (k : Ir.Kind.t) (node : Siesta.Syntax.t) : bool =
  has_kind k node
  && not (Seq.exists (has_kind k) (Seq.drop 1 (Siesta.Syntax.ancestors node)))
;;

let rec postorder (node : Siesta.Syntax.t) : Siesta.Syntax.t list =
  List.concat_map
    (Array.to_list (Siesta.Syntax.children_array node))
    ~f:(fun (elem : Siesta.Syntax.elem) ->
      match elem with
      | Siesta.Syntax.Node child -> postorder child
      | Siesta.Syntax.Token _ -> [])
  @ [ node ]
;;

let first (k : Ir.Kind.t) (order : Siesta.Syntax.t list) : Siesta.Syntax.t -> bool =
  match List.find_opt order ~f:(has_kind k) with
  | None -> fun _ -> false
  | Some chosen -> fun (node : Siesta.Syntax.t) -> node == chosen
;;

(* Every original subtree holding no node of kind [k] comes back with its tag.
   The prime token is appended, so an original child keeps its index in the
   result. *)
let shared (k : Ir.Kind.t) (original : Siesta.Green.node) (result : Siesta.Green.node)
  : bool
  =
  let ok = ref true in
  let rec walk (o : Siesta.Green.node) (r : Siesta.Green.node) : bool =
    let holds = ref (Siesta.Green.kind o = k) in
    for index = 0 to Siesta.Green.num_children o - 1 do
      match Siesta.Green.nth_child o index, Siesta.Green.nth_child r index with
      | Some (Siesta.Green.Node a), Some (Siesta.Green.Node b) ->
        if walk a b then holds := true
      | _ -> ()
    done;
    if (not !holds) && not (Siesta.Green.equal o r) then ok := false;
    !holds
  in
  let _ : bool = walk original result in
  !ok
;;

let () =
  let before = Law.failures () in
  let runs = ref 0 in
  List.iter trees ~f:(fun ((name : string), (tree : Siesta.Green.node)) ->
    let root = Siesta.Syntax.of_root tree in
    let ctx = ctx_of root in
    let cache = Lingo_runtime.Rewrite.Ctx.cache ctx in
    let preorder = List.of_seq (Siesta.Syntax.descendants root) in
    let postorder = postorder root in
    let kinds =
      List.sort_uniq ~cmp:Int.compare (List.map preorder ~f:Siesta.Syntax.kind)
    in
    List.iter kinds ~f:(fun (k : Ir.Kind.t) ->
      let once = Lingo_runtime.Rewrite.kind k prime_once in
      let stop = Lingo_runtime.Rewrite.kind k Lingo_runtime.Rewrite.id in
      let every = has_kind k in
      let outer = outermost k in
      List.iter
        [ ( "topdown"
          , Lingo_runtime.Rewrite.topdown (Lingo_runtime.Rewrite.try_ once)
          , every )
        ; ( "bottomup"
          , Lingo_runtime.Rewrite.bottomup (Lingo_runtime.Rewrite.try_ once)
          , every )
        ; "downup", Lingo_runtime.Rewrite.downup (Lingo_runtime.Rewrite.try_ once), every
        ; "innermost", Lingo_runtime.Rewrite.innermost once, every
        ; "outermost", Lingo_runtime.Rewrite.outermost once, every
        ; "alltd", Lingo_runtime.Rewrite.alltd once, outer
        ; "sometd", Lingo_runtime.Rewrite.sometd once, outer
        ; ( "topdown_stop"
          , Lingo_runtime.Rewrite.topdown_stop ~stop (Lingo_runtime.Rewrite.try_ once)
          , outer )
        ; ( "bottomup_stop"
          , Lingo_runtime.Rewrite.bottomup_stop ~stop (Lingo_runtime.Rewrite.try_ once)
          , outer )
        ; "oncetd", Lingo_runtime.Rewrite.oncetd once, first k preorder
        ; "oncebu", Lingo_runtime.Rewrite.oncebu once, first k postorder
        ]
        ~f:(fun ((traversal : string), (s : unit Lingo_runtime.Rewrite.t), pick) ->
          incr runs;
          let want = expected cache pick root in
          match s ctx root with
          | Error reason ->
            Law.fail "(b) %s, kind %d: %s failed with %S" name k traversal reason
          | Ok got when not (same got want) ->
            Law.fail
              "(b) %s, kind %d: %s gives %S where %S was expected"
              name
              k
              traversal
              (Siesta.Green.to_source got)
              (Siesta.Green.to_source want)
          | Ok got ->
            if not (shared k tree got)
            then
              Law.fail
                "(b) %s, kind %d: %s rebuilt a subtree it did not change"
                name
                k
                traversal));
    (* A kind no node has. Nothing changes, so nothing is rebuilt. *)
    let absent = Lingo_runtime.Rewrite.kind prime_kind prime in
    List.iter
      [ "topdown", Lingo_runtime.Rewrite.topdown (Lingo_runtime.Rewrite.try_ absent)
      ; "alltd", Lingo_runtime.Rewrite.alltd absent
      ]
      ~f:(fun ((traversal : string), (s : unit Lingo_runtime.Rewrite.t)) ->
        match s ctx root with
        | Ok got when Siesta.Green.equal got tree -> ()
        | r ->
          Law.fail
            "(b) %s: %s with nothing to do gives %a, a new node"
            name
            traversal
            pp_result
            r);
    List.iter
      [ "oncetd", Lingo_runtime.Rewrite.oncetd absent
      ; "oncebu", Lingo_runtime.Rewrite.oncebu absent
      ; "sometd", Lingo_runtime.Rewrite.sometd absent
      ]
      ~f:(fun ((traversal : string), (s : unit Lingo_runtime.Rewrite.t)) ->
        match s ctx root with
        | Error _ -> ()
        | Ok _ -> Law.fail "(b) %s: %s with nothing to do succeeded" name traversal));
  if Law.failures () = before
  then Law.pass "(b) every traversal matches its oracle, over %d runs" !runs
;;

(* -- (c) the cursor after a change ----------------------------------------- *)

let ancestor_kinds (node : Siesta.Syntax.t) : Ir.Kind.t list =
  List.of_seq (Seq.map Siesta.Syntax.kind (Seq.drop 1 (Siesta.Syntax.ancestors node)))
;;

let () =
  let before = Law.failures () in
  let nodes = ref 0 in
  List.iter trees ~f:(fun ((name : string), (tree : Siesta.Green.node)) ->
    let root = Siesta.Syntax.of_root tree in
    let ctx = ctx_of root in
    Seq.iter
      (fun (node : Siesta.Syntax.t) ->
         incr nodes;
         let kind = Siesta.Syntax.kind node in
         let seen = ref None in
         let probe : unit Lingo_runtime.Rewrite.t =
           fun _ cursor ->
           seen := Some cursor;
           Ok (Siesta.Syntax.green cursor)
         in
         let _ = Lingo_runtime.Rewrite.seq prime probe ctx node in
         (match !seen with
          | None -> Law.fail "(c) %s, kind %d: seq never ran its second rule" name kind
          | Some cursor ->
            if Siesta.Syntax.same_tree cursor root
            then
              Law.fail
                "(c) %s, kind %d: a changed node is still in the old tree"
                name
                kind;
            if ancestor_kinds cursor <> ancestor_kinds node
            then Law.fail "(c) %s, kind %d: a changed node lost its ancestors" name kind;
            if not (primed cursor)
            then Law.fail "(c) %s, kind %d: the second rule saw the old node" name kind);
         seen := None;
         let _ = Lingo_runtime.Rewrite.seq Lingo_runtime.Rewrite.id probe ctx node in
         match !seen with
         | Some cursor when cursor == node -> ()
         | Some _ | None ->
           Law.fail "(c) %s, kind %d: an unchanged node got a new cursor" name kind)
      (Siesta.Syntax.descendants root));
  if Law.failures () = before
  then Law.pass "(c) seq keeps the ancestors of %d nodes" !nodes
;;

(* -- (d) repeat and collect ------------------------------------------------ *)

let () =
  let before = Law.failures () in
  let runs = ref 0 in
  List.iter trees ~f:(fun ((name : string), (tree : Siesta.Green.node)) ->
    let root = Siesta.Syntax.of_root tree in
    let ctx = ctx_of root in
    let cache = Lingo_runtime.Rewrite.Ctx.cache ctx in
    (match Lingo_runtime.Rewrite.repeat prime_once ctx root with
     | Ok got when same got (prime_green cache tree) -> ()
     | r -> Law.fail "(d) %s: repeat prime_once gives %a" name pp_result r);
    (match Lingo_runtime.Rewrite.repeat ~fuel:3 prime ctx root with
     | Error "repeat: out of fuel" -> ()
     | r -> Law.fail "(d) %s: repeat prime with fuel 3 gives %a" name pp_result r);
    let kinds =
      List.sort_uniq
        ~cmp:Int.compare
        (List.of_seq (Seq.map Siesta.Syntax.kind (Siesta.Syntax.descendants root)))
    in
    List.iter kinds ~f:(fun (k : Ir.Kind.t) ->
      incr runs;
      let found =
        Lingo_runtime.Rewrite.collect (Lingo_runtime.Rewrite.kind k prime) ctx root
      in
      let wanted =
        List.filter (List.of_seq (Siesta.Syntax.descendants root)) ~f:(outermost k)
      in
      let matches =
        List.length found = List.length wanted
        && List.for_all2
             found
             wanted
             ~f:
               (fun
                 ((cursor : Siesta.Syntax.t), (green : Siesta.Green.node))
                 (node : Siesta.Syntax.t)
               ->
               cursor == node && same green (prime_green cache (Siesta.Syntax.green node)))
      in
      if not matches
      then
        Law.fail
          "(d) %s, kind %d: collect found %d nodes where %d were expected"
          name
          k
          (List.length found)
          (List.length wanted)));
  if Law.failures () = before
  then
    Law.pass "(d) repeat stops and collect finds the outermost nodes, over %d kinds" !runs
;;

(* -- (e) congruences against the one-level traversals ----------------------- *)

let one_slot (node : Siesta.Syntax.t) : Siesta.Syntax.elem list array option =
  Some [| Array.to_list (Siesta.Syntax.children_array node) |]
;;

let succeeds_alike
      (x : (Siesta.Green.node, string) result)
      (y : (Siesta.Green.node, string) result)
  : bool
  =
  match x, y with
  | Ok a, Ok b -> same a b
  | Error _, Error _ -> true
  | Ok _, Error _ | Error _, Ok _ -> false
;;

let prime_token : unit Lingo_runtime.Rewrite.Token.t =
  fun ctx token ->
  Ok
    (Siesta.Green.mk_token
       (Lingo_runtime.Rewrite.Ctx.cache ctx)
       ~kind:(Siesta.Syntax.Token.kind token)
       ~text:(Siesta.Syntax.Token.text token ^ "'"))
;;

let () =
  let before = Law.failures () in
  let nodes = ref 0 in
  List.iter trees ~f:(fun ((name : string), (tree : Siesta.Green.node)) ->
    let root = Siesta.Syntax.of_root tree in
    let ctx = ctx_of root in
    let cache = Lingo_runtime.Rewrite.Ctx.cache ctx in
    Seq.iter
      (fun (node : Siesta.Syntax.t) ->
         incr nodes;
         let kind = Siesta.Syntax.kind node in
         let congr (slot : unit Lingo_runtime.Rewrite.Slot.t)
           : unit Lingo_runtime.Rewrite.t
           =
           Lingo_runtime.Rewrite.congruence kind one_slot [| Some slot |]
         in
         List.iter
           [ "prime", prime; "odd", odd; "fail", Lingo_runtime.Rewrite.fail "f" ]
           ~f:(fun ((rule : string), (s : unit Lingo_runtime.Rewrite.t)) ->
             List.iter
               [ ( "all"
                 , congr
                     (Lingo_runtime.Rewrite.Slot.nodes
                        (Lingo_runtime.Rewrite.Elems.all s))
                 , Lingo_runtime.Rewrite.all s )
               ; ( "one"
                 , congr
                     (Lingo_runtime.Rewrite.Slot.nodes
                        (Lingo_runtime.Rewrite.Elems.one s))
                 , Lingo_runtime.Rewrite.one s )
               ; ( "some"
                 , congr
                     (Lingo_runtime.Rewrite.Slot.nodes
                        (Lingo_runtime.Rewrite.Elems.some s))
                 , Lingo_runtime.Rewrite.some s )
               ]
               ~f:
                 (fun
                   ( (traversal : string)
                   , (via : unit Lingo_runtime.Rewrite.t)
                   , (direct : unit Lingo_runtime.Rewrite.t) ) ->
                 let got = via ctx node in
                 let want = direct ctx node in
                 if not (succeeds_alike got want)
                 then
                   Law.fail
                     "(e) %s, kind %d: Elems.%s %s gives %a where %s gives %a"
                     name
                     kind
                     traversal
                     rule
                     pp_result
                     got
                     traversal
                     pp_result
                     want));
         let elems = Siesta.Syntax.children_array node in
         Array.iteri elems ~f:(fun (i : int) (elem : Siesta.Syntax.elem) ->
           let got =
             congr
               (Lingo_runtime.Rewrite.Slot.nodes
                  (Lingo_runtime.Rewrite.Elems.nth i prime))
               ctx
               node
           in
           match elem, got with
           | Siesta.Syntax.Node child, Ok got ->
             let children = Siesta.Green.children_array (Siesta.Syntax.green node) in
             children.(i)
             <- Siesta.Green.Node (prime_green cache (Siesta.Syntax.green child));
             let green = Siesta.Syntax.green node in
             let want =
               Siesta.Green.mk_node
                 cache
                 ~kind:(Siesta.Green.kind green)
                 ~payload:(Siesta.Green.payload green)
                 ~children
                 ()
             in
             if not (same got want)
             then
               Law.fail "(e) %s, kind %d: Elems.nth %d changed another child" name kind i
           | Siesta.Syntax.Token _, Error _ -> ()
           | Siesta.Syntax.Node _, Error reason ->
             Law.fail "(e) %s, kind %d: Elems.nth %d failed with %S" name kind i reason
           | Siesta.Syntax.Token _, Ok _ ->
             Law.fail
               "(e) %s, kind %d: Elems.nth %d ran a node rule on a token"
               name
               kind
               i);
         (match
            congr
              (Lingo_runtime.Rewrite.Slot.nodes
                 (Lingo_runtime.Rewrite.Elems.nth (Array.length elems) prime))
              ctx
              node
          with
          | Error _ -> ()
          | Ok _ -> Law.fail "(e) %s, kind %d: Elems.nth past the end succeeded" name kind);
         let want =
           Siesta.Green.mk_node
             cache
             ~kind
             ~payload:(Siesta.Green.payload (Siesta.Syntax.green node))
             ~children:
               (Array.map elems ~f:(fun (elem : Siesta.Syntax.elem) ->
                  match elem with
                  | Siesta.Syntax.Node child ->
                    Siesta.Green.Node (Siesta.Syntax.green child)
                  | Siesta.Syntax.Token token ->
                    Siesta.Green.Token
                      (Siesta.Green.mk_token
                         cache
                         ~kind:(Siesta.Syntax.Token.kind token)
                         ~text:(Siesta.Syntax.Token.text token ^ "'"))))
             ()
         in
         match
           congr
             (Lingo_runtime.Rewrite.Slot.tokens
                (Lingo_runtime.Rewrite.Elems.all prime_token))
             ctx
             node
         with
         | Ok got when same got want -> ()
         | r ->
           Law.fail
             "(e) %s, kind %d: a token rule over every child gives %a"
             name
             kind
             pp_result
             r)
      (Siesta.Syntax.descendants root));
  if Law.failures () = before
  then Law.pass "(e) congruence agrees with all, one and some at %d nodes" !nodes
;;

let () = Law.summarise "law_rewrite"
