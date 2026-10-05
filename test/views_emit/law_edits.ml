(* -- list edits -----------------------------------------------------------------

      [insert_<child>] and [delete_<child>] change one repeated child and
      nothing else. For every repeated child of every node in a clean parse:

      (a) deleting each element in turn gives a tree that parses without a
          diagnostic, and whose child holds the other elements, in order;
      (b) inserting an element at each place, from the first to after the
          last, gives a tree that parses, and whose child holds the elements
          with the new one at that place;
      (c) in both, every element that stays keeps the comments directly in
          front of it. A deleted element takes its own with it, and an
          inserted one has none.

      Mechanism. Every tree is formatted and parsed again before an edit,
      and again after, so what is compared is what the parser makes of the
      text. The edited node is found again by its place in preorder. Nodes
      before it in preorder are the same in both trees, since the edit
      changes nothing outside the node.

      An element is compared by its kinds and its tokens, whitespace left
      out. The element inserted is a copy of an element of the same child,
      taken from anywhere in the grammar's corpus. That reaches an empty
      child, which holds none of its own.

      The separator policies are the corpus's own. json and shapes forbid a
      trailing separator, comments writes one where a body breaks, ml writes
      one always and a leading one where a body breaks, and shapes has a
      separated list with no delimiters.
   -------------------------------------------------------------------------- *)

(* No mutation record has been generated for lib/ocaml/rewrite.ml yet.
   Generate it with

     assay -config assay.conf -only ocaml *)

open StdLabels

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; inputs : Inputs.t
  ; insert_at :
      int -> int -> at:int -> Siesta.Green.child -> unit Lingo_runtime.Rewrite.t option
  ; delete_at : int -> int -> at:int -> unit Lingo_runtime.Rewrite.t option
  ; slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option
  }

let corpus : case list =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; inputs = Inputs.sexp
    ; insert_at = Emitted_probe.Sexp_probe.insert_at
    ; delete_at = Emitted_probe.Sexp_probe.delete_at
    ; slots = Emitted_views.Sexp_views.Slots.slots
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; inputs = Inputs.json
    ; insert_at = Emitted_probe.Json_probe.insert_at
    ; delete_at = Emitted_probe.Json_probe.delete_at
    ; slots = Emitted_views.Json_views.Slots.slots
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; inputs = Inputs.calc
    ; insert_at = Emitted_probe.Calc_probe.insert_at
    ; delete_at = Emitted_probe.Calc_probe.delete_at
    ; slots = Emitted_views.Calc_views.Slots.slots
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; inputs = Inputs.rassoc
    ; insert_at = Emitted_probe.Rassoc_probe.insert_at
    ; delete_at = Emitted_probe.Rassoc_probe.delete_at
    ; slots = Emitted_views.Rassoc_views.Slots.slots
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; inputs = Inputs.postfix
    ; insert_at = Emitted_probe.Postfix_probe.insert_at
    ; delete_at = Emitted_probe.Postfix_probe.delete_at
    ; slots = Emitted_views.Postfix_views.Slots.slots
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; inputs = Inputs.shapes
    ; insert_at = Emitted_probe.Shapes_probe.insert_at
    ; delete_at = Emitted_probe.Shapes_probe.delete_at
    ; slots = Emitted_views.Shapes_views.Slots.slots
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; inputs = Inputs.unicode
    ; insert_at = Emitted_probe.Unicode_probe.insert_at
    ; delete_at = Emitted_probe.Unicode_probe.delete_at
    ; slots = Emitted_views.Unicode_views.Slots.slots
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; inputs = Inputs.recovery
    ; insert_at = Emitted_probe.Recovery_probe.insert_at
    ; delete_at = Emitted_probe.Recovery_probe.delete_at
    ; slots = Emitted_views.Recovery_views.Slots.slots
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; inputs = Inputs.comments
    ; insert_at = Emitted_probe.Comments_probe.insert_at
    ; delete_at = Emitted_probe.Comments_probe.delete_at
    ; slots = Emitted_views.Comments_views.Slots.slots
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; inputs = Inputs.rust
    ; insert_at = Emitted_probe.Rust_probe.insert_at
    ; delete_at = Emitted_probe.Rust_probe.delete_at
    ; slots = Emitted_views.Rust_views.Slots.slots
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    ; insert_at = Emitted_probe.Effekt_probe.insert_at
    ; delete_at = Emitted_probe.Effekt_probe.delete_at
    ; slots = Emitted_views.Effekt_views.Slots.slots
    }
  ; { name = "wide"
    ; grammar = Lingo_grammars.Wide_grammar.grammar
    ; inputs = Inputs.wide
    ; insert_at = Emitted_probe.Wide_probe.insert_at
    ; delete_at = Emitted_probe.Wide_probe.delete_at
    ; slots = Emitted_views.Wide_views.Slots.slots
    }
  ; { name = "ml"
    ; grammar = Lingo_grammars.Ml_grammar.grammar
    ; inputs = Inputs.ml
    ; insert_at = Emitted_probe.Ml_probe.insert_at
    ; delete_at = Emitted_probe.Ml_probe.delete_at
    ; slots = Emitted_views.Ml_views.Slots.slots
    }
  ]
;;

let width = 80

(* An element as compared: the comments directly in front of it, its kinds
   and its tokens. *)
type element =
  { comments : string list
  ; shape : string
  ; tokens : string list
  }

let green_of (elem : Siesta.Syntax.elem) : Siesta.Green.child =
  match elem with
  | Siesta.Syntax.Node node -> Siesta.Green.Node (Siesta.Syntax.green node)
  | Siesta.Syntax.Token token -> Siesta.Green.Token (Siesta.Syntax.Token.green token)
;;

let () =
  let before = Law.failures () in
  let deleted = ref 0 in
  let inserted = ref 0 in
  let into_empty = ref 0 in
  List.iter corpus ~f:(fun (c : case) ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> Law.fail "%s: the grammar does not check" c.name
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      let layout = Layout.Lower.of_facts f in
      let boundary = Lex.boundary f in
      let parse (src : string) = Interp.run plan entry (Lex.run f src) in
      let settle (tree : Siesta.Green.node) =
        parse (Lingo_runtime.Layout.format layout ~boundary ~width tree)
      in
      let has (k : int) (wanted : Core.Grammar.trivia_class option -> bool) : bool =
        Array.exists f.tokens ~f:(fun (t : Core.Token.def) ->
          Core.Kind.to_int t.kind = k && wanted t.trivia)
      in
      let trivia (k : int) : bool = has k Option.is_some in
      let comment (k : int) : bool = has k (fun t -> t = Some Core.Grammar.Preserve) in
      let rec tokens (child : Siesta.Green.child) : string list =
        match child with
        | Siesta.Green.Token token ->
          let k = Siesta.Green.Token.kind token in
          if trivia k && not (comment k) then [] else [ Siesta.Green.Token.text token ]
        | Siesta.Green.Node node ->
          List.concat_map (Array.to_list (Siesta.Green.children_array node)) ~f:tokens
      in
      let rec shape (child : Siesta.Green.child) : string =
        match child with
        | Siesta.Green.Token token -> string_of_int (Siesta.Green.Token.kind token)
        | Siesta.Green.Node node ->
          "("
          ^ string_of_int (Siesta.Green.kind node)
          ^ String.concat
              ~sep:""
              (List.filter_map
                 (Array.to_list (Siesta.Green.children_array node))
                 ~f:(fun child ->
                   match child with
                   | Siesta.Green.Node _ -> Some (" " ^ shape child)
                   | Siesta.Green.Token _ -> None))
          ^ ")"
      in
      (* The elements of one slot, each with the comments directly in front
         of it. *)
      let elements (node : Siesta.Syntax.t) (slot : int) : element list =
        match c.slots node with
        | None -> []
        | Some filled when slot >= Array.length filled -> []
        | Some filled ->
          let children = Siesta.Syntax.children_array node in
          List.map filled.(slot) ~f:(fun (elem : Siesta.Syntax.elem) ->
            let here =
              match elem with
              | Siesta.Syntax.Node n -> Siesta.Syntax.index_in_parent n
              | Siesta.Syntax.Token t -> Siesta.Syntax.Token.index_in_parent t
            in
            let rec leading (i : int) (acc : string list) =
              if i < 0
              then acc
              else (
                let k = Siesta.Syntax.elem_kind children.(i) in
                if not (trivia k)
                then acc
                else (
                  match children.(i) with
                  | Siesta.Syntax.Token token when comment k ->
                    leading (i - 1) (Siesta.Syntax.Token.text token :: acc)
                  | Siesta.Syntax.Token _ | Siesta.Syntax.Node _ -> leading (i - 1) acc))
            in
            let green = green_of elem in
            { comments = leading (here - 1) []
            ; shape = shape green
            ; tokens = tokens green
            })
      in
      let trees =
        List.filter_map (Inputs.all c.inputs) ~f:(fun (src : string) ->
          match parse src with
          | _, _ :: _ -> None
          | tree, [] ->
            (match settle tree with
             | settled, [] -> Some settled
             | _, _ :: _ -> None))
      in
      (* An element of each repeated slot, from anywhere in the corpus. *)
      let pool : (int * int, Siesta.Green.child) Hashtbl.t = Hashtbl.create 16 in
      List.iter trees ~f:(fun (tree : Siesta.Green.node) ->
        Seq.iter
          (fun (node : Siesta.Syntax.t) ->
             match c.slots node with
             | None -> ()
             | Some filled ->
               Array.iteri
                 filled
                 ~f:(fun (slot : int) (elems : Siesta.Syntax.elem list) ->
                   match elems with
                   | elem :: _ when not (Hashtbl.mem pool (Siesta.Syntax.kind node, slot))
                     ->
                     Hashtbl.replace pool (Siesta.Syntax.kind node, slot) (green_of elem)
                   | _ -> ()))
          (Siesta.Syntax.descendants (Siesta.Syntax.of_root tree)));
      List.iter trees ~f:(fun (tree : Siesta.Green.node) ->
        let root = Siesta.Syntax.of_root tree in
        let ctx =
          Lingo_runtime.Rewrite.Ctx.create (Siesta.Cache.create_plain ()) root ()
        in
        let cache = Lingo_runtime.Rewrite.Ctx.cache ctx in
        (* The edited tree settled, and the node at [ordinal] in it. *)
        let after (node : Siesta.Syntax.t) (green : Siesta.Green.node) (ordinal : int) =
          let edited = (Siesta.Syntax.replace cache node green).root in
          match settle (Siesta.Syntax.green edited) with
          | _, _ :: _ -> Error (Siesta.Green.to_source (Siesta.Syntax.green edited))
          | again, [] ->
            (match
               List.nth_opt
                 (List.of_seq (Siesta.Syntax.descendants (Siesta.Syntax.of_root again)))
                 ordinal
             with
             | Some found when Siesta.Syntax.kind found = Siesta.Syntax.kind node ->
               Ok found
             | Some _ | None -> Error "the node is not where it was")
        in
        let check
              (part : string)
              (node : Siesta.Syntax.t)
              (slot : int)
              (ordinal : int)
              (what : string)
              (result : (Siesta.Green.node, string) result)
              (want : element list)
          =
          match result with
          | Error reason ->
            Law.fail
              "%s %s: %s in %S failed with %S"
              part
              c.name
              what
              (Siesta.Syntax.to_source node)
              reason
          | Ok green ->
            (match after node green ordinal with
             | Error text ->
               Law.fail
                 "%s %s: %s in %S gives %S"
                 part
                 c.name
                 what
                 (Siesta.Syntax.to_source node)
                 text
             | Ok found ->
               let got = elements found slot in
               if
                 List.map got ~f:(fun e -> e.shape, e.tokens)
                 <> List.map want ~f:(fun e -> e.shape, e.tokens)
               then
                 Law.fail
                   "%s %s: %s in %S changes the other elements"
                   part
                   c.name
                   what
                   (Siesta.Syntax.to_source node)
               else if
                 List.map got ~f:(fun e -> e.comments)
                 <> List.map want ~f:(fun e -> e.comments)
               then
                 Law.fail
                   "(c) %s: %s in %S moves a comment"
                   c.name
                   what
                   (Siesta.Syntax.to_source node))
        in
        List.iteri
          (List.of_seq (Siesta.Syntax.descendants root))
          ~f:(fun (ordinal : int) (node : Siesta.Syntax.t) ->
            let k = Siesta.Syntax.kind node in
            match c.slots node with
            | None -> ()
            | Some filled ->
              Array.iteri filled ~f:(fun (slot : int) (_ : Siesta.Syntax.elem list) ->
                let have = elements node slot in
                let n = List.length have in
                for j = 0 to n - 1 do
                  match c.delete_at k slot ~at:j with
                  | None -> ()
                  | Some s ->
                    (match s ctx node with
                     | Error "at least one is required" when n = 1 -> ()
                     | result ->
                       incr deleted;
                       check
                         "(a)"
                         node
                         slot
                         ordinal
                         (Printf.sprintf "deleting element %d" j)
                         result
                         (List.filteri have ~f:(fun i _ -> i <> j)))
                done;
                match Hashtbl.find_opt pool (k, slot) with
                | None -> ()
                | Some element ->
                  for j = 0 to n do
                    match c.insert_at k slot ~at:j element with
                    | None -> ()
                    | Some s ->
                      incr inserted;
                      if n = 0 then incr into_empty;
                      let fresh =
                        { comments = []; shape = shape element; tokens = tokens element }
                      in
                      check
                        "(b)"
                        node
                        slot
                        ordinal
                        (Printf.sprintf "inserting at %d" j)
                        (s ctx node)
                        (List.filteri have ~f:(fun i _ -> i < j)
                         @ [ fresh ]
                         @ List.filteri have ~f:(fun i _ -> i >= j))
                  done))));
  if !into_empty = 0 then Law.fail "(b) no insertion into an empty child";
  if Law.failures () = before
  then
    Law.pass
      "every list edit holds: %d deletions and %d insertions, %d of them into an empty \
       child"
      !deleted
      !inserted
      !into_empty
;;

let () = Law.summarise "law_edits"
