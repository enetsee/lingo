(* -- binders --------------------------------------------------------------------

      The generated [visible], [fresh], [resolve], [rename] and [substitute]
      read names the way the grammar declares them. On every clean input of
      every grammar with binders:

      (a) [resolve] takes every reference to the binder the oracle takes it
          to, or to none where the oracle finds none;
      (b) [visible] at every node is the oracle's list there, nearest first;
      (c) [fresh] at every node gives a name no visible binder has, for a
          base nothing uses and for every visible name as a base;
      (d) [rename] of every binder to a fresh name leaves every reference
          resolving to the binder it resolved to before;
      (e) [substitute] at every scope keeps every free name of what it puts
          in free: each one resolves where it resolved at the scope, and
          never to a binder inside the scope. And no free use of the name it
          replaces is left where an expression stands.

      Mechanism. The oracle is tree-sitter's way of reading locals, written
      out apart from the runtime: one walk in source order with a stack of
      scopes. A scope is pushed at a production that opens one, a binder
      goes into the innermost scope when the walk reaches it, and a
      reference takes the latest binder of its text in the innermost scope
      that has one.

      Part (e) puts in a copy of the expression at a use of the name, spelt
      with a name bound inside the scope where there is one. That is the
      name the scope would capture if nothing stopped it.
   -------------------------------------------------------------------------- *)

(* No mutation record has been generated for the runtime's [Binders] yet.
   Generate it with

     assay -config assay.conf -only lingo_runtime *)

open StdLabels

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; inputs : Inputs.t
  ; slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option
  ; visible : Siesta.Syntax.t -> (string * Siesta.Syntax.token_cursor) list
  ; fresh : Siesta.Syntax.t -> base:string -> string
  ; resolve : Siesta.Syntax.token_cursor -> Siesta.Syntax.token_cursor option
  ; rename :
      Siesta.Cache.t
      -> Siesta.Syntax.token_cursor
      -> to_:string
      -> (Siesta.Green.node, string) result
  ; substitute : name:string -> by:Siesta.Green.node -> unit Lingo_runtime.Rewrite.t
  }

let corpus : case list =
  [ { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; inputs = Inputs.rust
    ; slots = Emitted_views.Rust_views.Slots.slots
    ; visible = Emitted_rewrite.Rust_rewrite.visible
    ; fresh = Emitted_rewrite.Rust_rewrite.fresh
    ; resolve = Emitted_rewrite.Rust_rewrite.resolve
    ; rename = Emitted_rewrite.Rust_rewrite.rename
    ; substitute = Emitted_rewrite.Rust_rewrite.substitute
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    ; slots = Emitted_views.Effekt_views.Slots.slots
    ; visible = Emitted_rewrite.Effekt_rewrite.visible
    ; fresh = Emitted_rewrite.Effekt_rewrite.fresh
    ; resolve = Emitted_rewrite.Effekt_rewrite.resolve
    ; rename = Emitted_rewrite.Effekt_rewrite.rename
    ; substitute = Emitted_rewrite.Effekt_rewrite.substitute
    }
  ; { name = "ml"
    ; grammar = Lingo_grammars.Ml_grammar.grammar
    ; inputs = Inputs.ml
    ; slots = Emitted_views.Ml_views.Slots.slots
    ; visible = Emitted_rewrite.Ml_rewrite.visible
    ; fresh = Emitted_rewrite.Ml_rewrite.fresh
    ; resolve = Emitted_rewrite.Ml_rewrite.resolve
    ; rename = Emitted_rewrite.Ml_rewrite.rename
    ; substitute = Emitted_rewrite.Ml_rewrite.substitute
    }
  ]
;;

let start (token : Siesta.Syntax.token_cursor) : int =
  fst (Siesta.Syntax.Token.text_range token)
;;

let () =
  let before = Law.failures () in
  let references = ref 0 in
  let nodes = ref 0 in
  let renamed = ref 0 in
  let substituted = ref 0 in
  List.iter corpus ~f:(fun (c : case) ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> Law.fail "%s: the grammar does not check" c.name
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      let rule_of (k : int) : Core.Rule.def option =
        Array.find_opt f.rules ~f:(fun (d : Core.Rule.def) -> Core.Kind.to_int d.kind = k)
      in
      let scope (k : int) : bool =
        match rule_of k with
        | Some d -> d.opens_scope
        | None -> false
      in
      let reference_kinds =
        Array.to_list f.rules
        |> List.concat_map ~f:(fun (d : Core.Rule.def) ->
          List.concat_map (Array.to_list d.binders) ~f:(fun (i : int) ->
            List.map (Array.to_list d.children.(i).alts) ~f:Core.Kind.to_int))
      in
      let trivia (k : int) : bool =
        Array.exists f.tokens ~f:(fun (t : Core.Token.def) ->
          Core.Kind.to_int t.kind = k && Core.Token.is_trivia t)
      in
      let bases =
        List.map (Array.to_list f.blocks) ~f:(fun (b : Core.Block.def) ->
          Core.Kind.to_int b.kind)
      in
      (* The binder tokens a node holds itself, by their start. *)
      let own (node : Siesta.Syntax.t) : int list =
        match rule_of (Siesta.Syntax.kind node), c.slots node with
        | Some d, Some filled ->
          List.concat_map (Array.to_list d.binders) ~f:(fun (i : int) ->
            if i < Array.length filled
            then
              List.filter_map filled.(i) ~f:(fun (e : Siesta.Syntax.elem) ->
                match e with
                | Siesta.Syntax.Token t -> Some (start t)
                | Siesta.Syntax.Node _ -> None)
            else [])
        | _ -> []
      in
      (* The oracle: each reference's binder by start, and the environment
         at each node's start, nearest first. *)
      let oracle (tree : Siesta.Green.node) =
        let resolved = Hashtbl.create 64 in
        let environment = Hashtbl.create 64 in
        let rec walk (node : Siesta.Syntax.t) (stack : (string * int) list ref list) =
          let flat = List.concat_map stack ~f:(fun s -> !s) in
          Hashtbl.replace
            environment
            (Siesta.Syntax.text_range node, Siesta.Syntax.kind node)
            flat;
          let stack =
            if
              scope (Siesta.Syntax.kind node)
              && Option.is_some (Siesta.Syntax.parent node)
            then ref [] :: stack
            else stack
          in
          let binders = own node in
          Array.iter
            (Siesta.Syntax.children_array node)
            ~f:(fun (e : Siesta.Syntax.elem) ->
              match e with
              | Siesta.Syntax.Node child -> walk child stack
              | Siesta.Syntax.Token t ->
                let k = Siesta.Syntax.Token.kind t in
                let text = Siesta.Syntax.Token.text t in
                if List.mem (start t) ~set:binders
                then (
                  let innermost = List.hd stack in
                  innermost := (text, start t) :: !innermost;
                  Hashtbl.replace resolved (start t) (Some (start t)))
                else if List.mem k ~set:reference_kinds
                then
                  Hashtbl.replace
                    resolved
                    (start t)
                    (List.find_map stack ~f:(fun s -> List.assoc_opt text !s)))
        in
        walk (Siesta.Syntax.of_root tree) [ ref [] ];
        resolved, environment
      in
      let parse (src : string) = Interp.run plan entry (Lex.run f src) in
      let reference_tokens (root : Siesta.Syntax.t) : Siesta.Syntax.token_cursor list =
        let found = ref [] in
        Siesta.Syntax.preorder root ~f:(fun (n : Siesta.Syntax.t) ->
          Array.iter (Siesta.Syntax.children_array n) ~f:(fun (e : Siesta.Syntax.elem) ->
            match e with
            | Siesta.Syntax.Token t
              when List.mem (Siesta.Syntax.Token.kind t) ~set:reference_kinds ->
              found := t :: !found
            | Siesta.Syntax.Token _ | Siesta.Syntax.Node _ -> ());
          Siesta.Syntax.Descend);
        List.rev !found
      in
      List.iter (Inputs.all c.inputs) ~f:(fun (src : string) ->
        match parse src with
        | _, _ :: _ -> ()
        | tree, [] ->
          let root = Siesta.Syntax.of_root tree in
          let resolved, environment = oracle tree in
          let tokens = reference_tokens root in
          (* (a) *)
          List.iter tokens ~f:(fun (t : Siesta.Syntax.token_cursor) ->
            incr references;
            let got = Option.map start (c.resolve t) in
            let want = Option.join (Hashtbl.find_opt resolved (start t)) in
            if got <> want
            then
              Law.fail
                "(a) %s: %S at %d resolves to %s, and the oracle says %s, in %S"
                c.name
                (Siesta.Syntax.Token.text t)
                (start t)
                (match got with
                 | Some s -> string_of_int s
                 | None -> "nothing")
                (match want with
                 | Some s -> string_of_int s
                 | None -> "nothing")
                src);
          (* (b), (c) *)
          Seq.iter
            (fun (node : Siesta.Syntax.t) ->
               incr nodes;
               let got = List.map (c.visible node) ~f:(fun (name, b) -> name, start b) in
               let want =
                 Option.value
                   (Hashtbl.find_opt
                      environment
                      (Siesta.Syntax.text_range node, Siesta.Syntax.kind node))
                   ~default:[]
               in
               if got <> want
               then
                 Law.fail
                   "(b) %s: visible at a node of %S differs from the oracle"
                   c.name
                   src;
               let names = List.map got ~f:fst in
               List.iter ("lingo_fresh" :: names) ~f:(fun (base : string) ->
                 let name = c.fresh node ~base in
                 if List.mem name ~set:names
                 then
                   Law.fail
                     "(c) %s: fresh gives %S, which is visible, in %S"
                     c.name
                     name
                     src))
            (Siesta.Syntax.descendants root);
          (* (d) every binder renamed. Tokens keep their order, so a reference
             is the same one by its index. *)
          let index_of (starts : int list) (s : int) : int option =
            List.find_mapi starts ~f:(fun i x -> if x = s then Some i else None)
          in
          let mapping (tree : Siesta.Green.node) =
            let resolved, _ = oracle tree in
            let starts =
              List.map (reference_tokens (Siesta.Syntax.of_root tree)) ~f:start
            in
            List.map starts ~f:(fun s ->
              Option.bind (Option.join (Hashtbl.find_opt resolved s)) (index_of starts))
          in
          let before_map = mapping tree in
          List.iter tokens ~f:(fun (t : Siesta.Syntax.token_cursor) ->
            if Hashtbl.find_opt resolved (start t) = Some (Some (start t))
            then (
              let to_ =
                c.fresh
                  (Siesta.Syntax.Token.parent t)
                  ~base:(Siesta.Syntax.Token.text t ^ "z")
              in
              match c.rename (Siesta.Cache.create_plain ()) t ~to_ with
              | Error _ -> ()
              | Ok renamed_tree ->
                incr renamed;
                if mapping renamed_tree <> before_map
                then
                  Law.fail
                    "(d) %s: renaming %S to %S in %S moves a reference"
                    c.name
                    (Siesta.Syntax.Token.text t)
                    to_
                    src));
          (* (e) at every scope, for every name used free in it. *)
          Seq.iter
            (fun (n : Siesta.Syntax.t) ->
               if scope (Siesta.Syntax.kind n)
               then (
                 let a, b = Siesta.Syntax.text_range n in
                 let within (s : int) = a <= s && s < b in
                 let inner_binders =
                   List.filter tokens ~f:(fun t ->
                     within (start t)
                     && Hashtbl.find_opt resolved (start t) = Some (Some (start t)))
                 in
                 (* A free use standing as an expression: a base node's only token. *)
                 let sites =
                   List.filter tokens ~f:(fun t ->
                     let parent = Siesta.Syntax.Token.parent t in
                     within (start t)
                     && List.mem (Siesta.Syntax.kind parent) ~set:bases
                     && List.length
                          (List.filter
                             (Array.to_list (Siesta.Syntax.children_array parent))
                             ~f:(fun e -> not (trivia (Siesta.Syntax.elem_kind e))))
                        = 1
                     &&
                     match Option.join (Hashtbl.find_opt resolved (start t)) with
                     | None -> true
                     | Some s -> not (within s))
                 in
                 List.iter sites ~f:(fun (site : Siesta.Syntax.token_cursor) ->
                   let x = Siesta.Syntax.Token.text site in
                   let y =
                     match inner_binders with
                     | binder :: _ -> Siesta.Syntax.Token.text binder
                     | [] -> x
                   in
                   let cache = Siesta.Cache.create_plain () in
                   let parent = Siesta.Syntax.Token.parent site in
                   let token =
                     Siesta.Green.mk_token
                       cache
                       ~kind:(Siesta.Syntax.Token.kind site)
                       ~text:y
                   in
                   let by =
                     Siesta.Green.mk_node
                       cache
                       ~kind:(Siesta.Syntax.kind parent)
                       ~children:[| Siesta.Green.Token token |]
                       ()
                   in
                   let ctx = Lingo_runtime.Rewrite.Ctx.create cache root () in
                   match c.substitute ~name:x ~by ctx n with
                   | Error reason -> Law.fail "(e) %s: substitute fails: %s" c.name reason
                   | Ok green ->
                     incr substituted;
                     let result = (Siesta.Syntax.replace cache n green).root in
                     let after_resolved, _ = oracle (Siesta.Syntax.green result) in
                     let n_end = a + Siesta.Green.text_len green in
                     (* Where [y] resolved at the scope: before it, so the same
                        offset in both trees. *)
                     let outside =
                       List.assoc_opt
                         y
                         (Option.value
                            (Hashtbl.find_opt
                               environment
                               (Siesta.Syntax.text_range n, Siesta.Syntax.kind n))
                            ~default:[])
                     in
                     List.iter
                       (reference_tokens result)
                       ~f:(fun (t : Siesta.Syntax.token_cursor) ->
                         if Siesta.Syntax.Token.green t == token
                         then (
                           let got =
                             Option.join (Hashtbl.find_opt after_resolved (start t))
                           in
                           if got <> outside
                           then
                             Law.fail
                               "(e) %s: putting %S for %S in a scope of %S captures it"
                               c.name
                               y
                               x
                               src)
                         else if
                           String.equal (Siesta.Syntax.Token.text t) x
                           && a <= start t
                           && start t < n_end
                           && (match
                                 Option.join (Hashtbl.find_opt after_resolved (start t))
                               with
                               | None -> true
                               | Some s -> s < a || s >= n_end)
                           && List.mem
                                (Siesta.Syntax.kind (Siesta.Syntax.Token.parent t))
                                ~set:bases
                         then
                           Law.fail
                             "(e) %s: a free use of %S is left in a scope of %S"
                             c.name
                             x
                             src))))
            (Siesta.Syntax.descendants root)));
  if Law.failures () = before
  then
    Law.pass
      "binders hold: %d references, %d nodes, %d renames, %d substitutions"
      !references
      !nodes
      !renamed
      !substituted
;;

let () = Law.summarise "law_binders"
