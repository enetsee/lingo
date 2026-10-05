(* -- the generated constructors -------------------------------------------------

      Every view's [make] builds the node it is given the children of.
      For every node with a view in a clean parse, the node is rebuilt
      with [make] from its own children and put back in the tree. The tree is
      formatted and parsed again. The original tree is formatted and parsed
      the same way. Then:

      (a) the rebuilt tree parses without a diagnostic;
      (b) both trees have the same shape, [Oracles.shape];
      (c) both have the same meaningful tokens, kind and text, in order.

      And (d): where a node is rebuilt with fewer elements than it had, every
      comment it held is still there, in the order it had.

      Mechanism. The children are read through the views, and [rebuild]
      passes the node itself as [~replacing], so [make] keeps the comments
      between its children. [make] writes no whitespace, and the formatter
      writes all of it, so a tree is compared only after both have been
      formatted.

      Part (b) compares kinds and leaves out every token. Part (c) is there
      for what (b) cannot see: a token with the wrong text, a delimiter or a
      separator that is missing or one too many, and a comment lost or moved
      past a token. Comments are compared in (c), and whitespace is left out.

      A separator at either end of a body is left out of (c), on both sides.
      A lone separator in an empty body, as in ml's [{ | }], parses and the
      formatter keeps it. [make] writes the empty body without it.

      Part (d) calls [Construct.finish] directly, on every node of every
      tree, with a slot function of its own that puts every child but trivia
      in one slot. It rebuilds the node without its last element. The
      comments that sat around that element have nowhere to go back to, and
      they go after the last element that is left.

      Roles are rebuilt as well. A role's operands come from a valid parse,
      so its [make] never needs parentheses here. test/views_emit/law_parens.ml
      is about the ones it adds.
   -------------------------------------------------------------------------- *)

(* No mutation record has been generated for lib/ocaml/rewrite.ml yet.
   Generate it with

     assay -config assay.conf -only ocaml *)

open StdLabels

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; inputs : Inputs.t
  ; rebuild :
      Siesta.Cache.t -> Siesta.Syntax.t -> (Siesta.Green.node, string) result option
  }

let corpus : case list =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; inputs = Inputs.sexp
    ; rebuild = Emitted_probe.Sexp_probe.rebuild
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; inputs = Inputs.json
    ; rebuild = Emitted_probe.Json_probe.rebuild
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; inputs = Inputs.calc
    ; rebuild = Emitted_probe.Calc_probe.rebuild
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; inputs = Inputs.rassoc
    ; rebuild = Emitted_probe.Rassoc_probe.rebuild
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; inputs = Inputs.postfix
    ; rebuild = Emitted_probe.Postfix_probe.rebuild
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; inputs = Inputs.shapes
    ; rebuild = Emitted_probe.Shapes_probe.rebuild
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; inputs = Inputs.unicode
    ; rebuild = Emitted_probe.Unicode_probe.rebuild
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; inputs = Inputs.recovery
    ; rebuild = Emitted_probe.Recovery_probe.rebuild
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; inputs = Inputs.comments
    ; rebuild = Emitted_probe.Comments_probe.rebuild
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; inputs = Inputs.rust
    ; rebuild = Emitted_probe.Rust_probe.rebuild
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    ; rebuild = Emitted_probe.Effekt_probe.rebuild
    }
  ; { name = "wide"
    ; grammar = Lingo_grammars.Wide_grammar.grammar
    ; inputs = Inputs.wide
    ; rebuild = Emitted_probe.Wide_probe.rebuild
    }
  ; { name = "ml"
    ; grammar = Lingo_grammars.Ml_grammar.grammar
    ; inputs = Inputs.ml
    ; rebuild = Emitted_probe.Ml_probe.rebuild
    }
  ]
;;

let width = 80

(* The tokens, kind and text, left to right, with whitespace and any
   separator at either end of a body left out. *)
let meaningful (f : Core.Facts.t) (tree : Siesta.Green.node) : (int * string) list =
  let trivia (k : int) : bool =
    Array.exists f.tokens ~f:(fun (t : Core.Token.def) ->
      Core.Kind.to_int t.kind = k && t.trivia = Some Core.Grammar.Reformat)
  in
  let rule (k : int) : Core.Rule.def option =
    Array.find_opt f.rules ~f:(fun (d : Core.Rule.def) -> Core.Kind.to_int d.kind = k)
  in
  let is_sep (sep : int) (item : (int * string) list) : bool =
    match item with
    | [ (k, _) ] -> k = sep
    | _ -> false
  in
  (* The body lies between [first] and [last], items counted from each end. *)
  let trim (sep : int) ~(first : int) ~(last : int) (items : (int * string) list list) =
    let n = List.length items in
    List.filteri items ~f:(fun (i : int) (item : (int * string) list) ->
      not (is_sep sep item && (i = first || i = n - 1 - last)))
  in
  let rec go (node : Siesta.Green.node) : (int * string) list =
    let items =
      List.filter_map
        (Array.to_list (Siesta.Green.children_array node))
        ~f:(fun (child : Siesta.Green.child) ->
          match child with
          | Siesta.Green.Node node -> Some (go node)
          | Siesta.Green.Token token ->
            let k = Siesta.Green.Token.kind token in
            if trivia k then None else Some [ k, Siesta.Green.Token.text token ])
    in
    let items =
      match rule (Siesta.Green.kind node) with
      | Some { frame = Core.Rule.Delimited { sep = Some sep; _ }; body_from; _ } ->
        trim (Core.Kind.to_int sep.sep_tok) ~first:(body_from + 1) ~last:1 items
      | Some { frame = Core.Rule.Separated { sep_tok; _ }; _ } ->
        trim (Core.Kind.to_int sep_tok) ~first:0 ~last:0 items
      | Some _ | None -> items
    in
    List.concat items
  in
  go tree
;;

let () =
  let before = Law.failures () in
  let nodes = ref 0 in
  let trees = ref 0 in
  List.iter corpus ~f:(fun (c : case) ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> Law.fail "%s: the grammar does not check" c.name
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      let layout = Layout.Lower.of_facts f in
      let boundary = Lex.boundary f in
      let parse (src : string) = Interp.run plan entry (Lex.run f src) in
      (* The tree as the formatter writes it, parsed again. *)
      let settle (tree : Siesta.Green.node) =
        parse (Lingo_runtime.Layout.format layout ~boundary ~width tree)
      in
      List.iter (Inputs.all c.inputs) ~f:(fun (src : string) ->
        match parse src with
        | _, _ :: _ -> ()
        | tree, [] ->
          incr trees;
          let original, _ = settle tree in
          let want_shape = Fuzz.Oracles.shape original in
          let want_tokens = meaningful f original in
          let cache = Siesta.Cache.create_plain () in
          Seq.iter
            (fun (node : Siesta.Syntax.t) ->
               let k = Siesta.Syntax.kind node in
               match c.rebuild cache node with
               | None -> ()
               | Some (Error reason) ->
                 Law.fail "(a) %s, kind %d: make failed with %S" c.name k reason
               | Some (Ok green) ->
                 incr nodes;
                 let root = (Siesta.Syntax.replace cache node green).root in
                 let again, diagnostics = settle (Siesta.Syntax.green root) in
                 if diagnostics <> []
                 then
                   Law.fail
                     "(a) %s, kind %d: %S rebuilt as %S does not parse"
                     c.name
                     k
                     (Siesta.Syntax.to_source node)
                     (Siesta.Green.to_source green)
                 else if not (String.equal (Fuzz.Oracles.shape again) want_shape)
                 then
                   Law.fail
                     "(b) %s, kind %d: %S rebuilt as %S changes the shape"
                     c.name
                     k
                     (Siesta.Syntax.to_source node)
                     (Siesta.Green.to_source green)
                 else if meaningful f again <> want_tokens
                 then
                   Law.fail
                     "(c) %s, kind %d: %S rebuilt as %S changes the tokens"
                     c.name
                     k
                     (Siesta.Syntax.to_source node)
                     (Siesta.Green.to_source green))
            (Siesta.Syntax.descendants (Siesta.Syntax.of_root tree))));
  if Law.failures () = before
  then Law.pass "every production rebuilds, at %d nodes in %d clean trees" !nodes !trees
;;

(* -- (d) a shorter list keeps its comments --------------------------------- *)

let () =
  let before = Law.failures () in
  let nodes = ref 0 in
  List.iter corpus ~f:(fun (c : case) ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> ()
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      let has (k : int) (wanted : Core.Grammar.trivia_class option -> bool) : bool =
        Array.exists f.tokens ~f:(fun (t : Core.Token.def) ->
          Core.Kind.to_int t.kind = k && wanted t.trivia)
      in
      let trivia (k : int) : bool = has k Option.is_some in
      let comment (k : int) : bool = has k (fun t -> t = Some Core.Grammar.Preserve) in
      let meaningful (node : Siesta.Syntax.t) : Siesta.Syntax.elem list =
        List.filter
          (Array.to_list (Siesta.Syntax.children_array node))
          ~f:(fun (elem : Siesta.Syntax.elem) ->
            not (trivia (Siesta.Syntax.elem_kind elem)))
      in
      let comments (green : Siesta.Green.node) : string list =
        List.filter_map
          (Array.to_list (Siesta.Green.children_array green))
          ~f:(fun (child : Siesta.Green.child) ->
            match child with
            | Siesta.Green.Token token when comment (Siesta.Green.Token.kind token) ->
              Some (Siesta.Green.Token.text token)
            | Siesta.Green.Token _ | Siesta.Green.Node _ -> None)
      in
      let original (elem : Siesta.Syntax.elem) : Siesta.Green.child =
        match elem with
        | Siesta.Syntax.Node node -> Siesta.Green.Node (Siesta.Syntax.green node)
        | Siesta.Syntax.Token token ->
          Siesta.Green.Token (Siesta.Syntax.Token.green token)
      in
      List.iter (Inputs.all c.inputs) ~f:(fun (src : string) ->
        let tree, _ = Interp.run plan entry (Lex.run f src) in
        let cache = Siesta.Cache.create_plain () in
        Seq.iter
          (fun (node : Siesta.Syntax.t) ->
             let elems = meaningful node in
             let want = comments (Siesta.Syntax.green node) in
             if want <> [] && elems <> []
             then (
               incr nodes;
               let kept = List.filteri elems ~f:(fun i _ -> i < List.length elems - 1) in
               match
                 Lingo_runtime.Rewrite.Construct.finish
                   ~replacing:node
                   ~slots:(fun (node : Siesta.Syntax.t) -> Some [| meaningful node |])
                   ~trivia
                   ~comment
                   ~takes:(fun _ _ -> false)
                   cache
                   (Siesta.Syntax.kind node)
                   (fun (cursor : Siesta.Syntax.t) -> Some cursor)
                   [ Lingo_runtime.Rewrite.Construct.slot 0 (List.map kept ~f:original) ]
               with
               | Error reason -> Law.fail "(d) %s: finish failed with %S" c.name reason
               | Ok built ->
                 let got = comments (Siesta.Syntax.green built) in
                 if got <> want
                 then
                   Law.fail
                     "(d) %s, kind %d: %S without its last element keeps %d of %d \
                      comments"
                     c.name
                     (Siesta.Syntax.kind node)
                     (Siesta.Syntax.to_source node)
                     (List.length got)
                     (List.length want)))
          (Siesta.Syntax.descendants (Siesta.Syntax.of_root tree))));
  if Law.failures () = before
  then Law.pass "(d) every comment survives a shorter list, at %d nodes" !nodes
;;

let () = Law.summarise "law_make"
