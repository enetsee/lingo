(* -- applying a result -----------------------------------------------------------

      [apply] gives splices that turn the old source into text whose parse is
      the new tree, one top-level item at a time. For every clean file whose
      root has items, and for each of four edits to those items (deleting
      one, inserting a copy of one, rebuilding one, and moving one to every other place):

      (a) the spliced text parses without a diagnostic;
      (b) its parse has the new tree's shape and meaningful tokens;
      (c) every old item that the new tree keeps is still there byte for
          byte, in its old place or moved;
      (d) formatting the spliced text and formatting that again give the
          same text;
      (e) a comment is lost only with a deleted item: the run of comments
          directly above it, with no blank line in between, and those inside
          it.

      Mechanism. The new trees are made with the generated list edits and
      [rebuild], on the root's first repeated child of rules. That slot is
      found here from the facts. A rebuilt item has a new tag and the same
      text, so it is spliced as a changed item and formatted. A move deletes
      an item and inserts that same node elsewhere, so its tag is still the
      old one.

      Part (b) compares tokens as test/views_emit/body_tokens.ml settles
      them. A changed item is formatted, and the formatter writes or leaves
      out a separator at the end of a body as its policy says.

      Part (e) works out the runs of comments here, apart from the runtime:
      walking back from an item over comments and over whitespace that holds
      at most one line break.

      The roots with no items, such as json's, splice the whole file, and
      they are not drawn from here.
   -------------------------------------------------------------------------- *)

(* No mutation record has been generated for lib/runtime/rewrite.ml's [apply]
   yet. Generate it with

     assay -config assay.conf -only lingo_runtime *)

open StdLabels

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; inputs : Inputs.t
  ; apply :
      format:(Siesta.Green.node -> string)
      -> before:Siesta.Green.node
      -> after:Siesta.Green.node
      -> Lingo_runtime.Rewrite.splice list
  ; insert_at :
      int -> int -> at:int -> Siesta.Green.child -> unit Lingo_runtime.Rewrite.t option
  ; delete_at : int -> int -> at:int -> unit Lingo_runtime.Rewrite.t option
  ; rebuild :
      Siesta.Cache.t -> Siesta.Syntax.t -> (Siesta.Green.node, string) result option
  ; slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option
  }

let corpus : case list =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; inputs = Inputs.sexp
    ; apply = Emitted_rewrite.Sexp_rewrite.apply
    ; insert_at = Emitted_probe.Sexp_probe.insert_at
    ; delete_at = Emitted_probe.Sexp_probe.delete_at
    ; rebuild = Emitted_probe.Sexp_probe.rebuild
    ; slots = Emitted_views.Sexp_views.Slots.slots
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; inputs = Inputs.json
    ; apply = Emitted_rewrite.Json_rewrite.apply
    ; insert_at = Emitted_probe.Json_probe.insert_at
    ; delete_at = Emitted_probe.Json_probe.delete_at
    ; rebuild = Emitted_probe.Json_probe.rebuild
    ; slots = Emitted_views.Json_views.Slots.slots
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; inputs = Inputs.calc
    ; apply = Emitted_rewrite.Calc_rewrite.apply
    ; insert_at = Emitted_probe.Calc_probe.insert_at
    ; delete_at = Emitted_probe.Calc_probe.delete_at
    ; rebuild = Emitted_probe.Calc_probe.rebuild
    ; slots = Emitted_views.Calc_views.Slots.slots
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; inputs = Inputs.rassoc
    ; apply = Emitted_rewrite.Rassoc_rewrite.apply
    ; insert_at = Emitted_probe.Rassoc_probe.insert_at
    ; delete_at = Emitted_probe.Rassoc_probe.delete_at
    ; rebuild = Emitted_probe.Rassoc_probe.rebuild
    ; slots = Emitted_views.Rassoc_views.Slots.slots
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; inputs = Inputs.postfix
    ; apply = Emitted_rewrite.Postfix_rewrite.apply
    ; insert_at = Emitted_probe.Postfix_probe.insert_at
    ; delete_at = Emitted_probe.Postfix_probe.delete_at
    ; rebuild = Emitted_probe.Postfix_probe.rebuild
    ; slots = Emitted_views.Postfix_views.Slots.slots
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; inputs = Inputs.shapes
    ; apply = Emitted_rewrite.Shapes_rewrite.apply
    ; insert_at = Emitted_probe.Shapes_probe.insert_at
    ; delete_at = Emitted_probe.Shapes_probe.delete_at
    ; rebuild = Emitted_probe.Shapes_probe.rebuild
    ; slots = Emitted_views.Shapes_views.Slots.slots
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; inputs = Inputs.unicode
    ; apply = Emitted_rewrite.Unicode_rewrite.apply
    ; insert_at = Emitted_probe.Unicode_probe.insert_at
    ; delete_at = Emitted_probe.Unicode_probe.delete_at
    ; rebuild = Emitted_probe.Unicode_probe.rebuild
    ; slots = Emitted_views.Unicode_views.Slots.slots
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; inputs = Inputs.recovery
    ; apply = Emitted_rewrite.Recovery_rewrite.apply
    ; insert_at = Emitted_probe.Recovery_probe.insert_at
    ; delete_at = Emitted_probe.Recovery_probe.delete_at
    ; rebuild = Emitted_probe.Recovery_probe.rebuild
    ; slots = Emitted_views.Recovery_views.Slots.slots
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; inputs = Inputs.comments
    ; apply = Emitted_rewrite.Comments_rewrite.apply
    ; insert_at = Emitted_probe.Comments_probe.insert_at
    ; delete_at = Emitted_probe.Comments_probe.delete_at
    ; rebuild = Emitted_probe.Comments_probe.rebuild
    ; slots = Emitted_views.Comments_views.Slots.slots
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; inputs = Inputs.rust
    ; apply = Emitted_rewrite.Rust_rewrite.apply
    ; insert_at = Emitted_probe.Rust_probe.insert_at
    ; delete_at = Emitted_probe.Rust_probe.delete_at
    ; rebuild = Emitted_probe.Rust_probe.rebuild
    ; slots = Emitted_views.Rust_views.Slots.slots
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    ; apply = Emitted_rewrite.Effekt_rewrite.apply
    ; insert_at = Emitted_probe.Effekt_probe.insert_at
    ; delete_at = Emitted_probe.Effekt_probe.delete_at
    ; rebuild = Emitted_probe.Effekt_probe.rebuild
    ; slots = Emitted_views.Effekt_views.Slots.slots
    }
  ; { name = "wide"
    ; grammar = Lingo_grammars.Wide_grammar.grammar
    ; inputs = Inputs.wide
    ; apply = Emitted_rewrite.Wide_rewrite.apply
    ; insert_at = Emitted_probe.Wide_probe.insert_at
    ; delete_at = Emitted_probe.Wide_probe.delete_at
    ; rebuild = Emitted_probe.Wide_probe.rebuild
    ; slots = Emitted_views.Wide_views.Slots.slots
    }
  ; { name = "ml"
    ; grammar = Lingo_grammars.Ml_grammar.grammar
    ; inputs = Inputs.ml
    ; apply = Emitted_rewrite.Ml_rewrite.apply
    ; insert_at = Emitted_probe.Ml_probe.insert_at
    ; delete_at = Emitted_probe.Ml_probe.delete_at
    ; rebuild = Emitted_probe.Ml_probe.rebuild
    ; slots = Emitted_views.Ml_views.Slots.slots
    }
  ]
;;

let width = 80

(* The root's slot of items: its first repeated child of rules. *)
let item_slot (f : Core.Facts.t) (root : int) : int option =
  Array.find_map f.rules ~f:(fun (d : Core.Rule.def) ->
    if Core.Kind.to_int d.kind <> root
    then None
    else
      List.find_map
        (List.mapi (Array.to_list d.children) ~f:(fun i c -> i, c))
        ~f:(fun ((i : int), (c : Core.Rule.child)) ->
          match c.modifier with
          | (Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _)
            when Array.for_all c.alts ~f:(fun k -> not (Core.Facts.is_token_kind f k)) ->
            Some i
          | _ -> None))
;;

let () =
  let before = Law.failures () in
  let runs = ref 0 in
  let moved = ref 0 in
  List.iter corpus ~f:(fun (c : case) ->
    match Core.Facts.of_grammar c.grammar with
    | Error _ -> Law.fail "%s: the grammar does not check" c.name
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      let layout = Layout.Lower.of_facts f in
      let boundary = Lex.boundary f in
      let parse (src : string) = Interp.run plan entry (Lex.run f src) in
      let format (tree : Siesta.Green.node) : string =
        Lingo_runtime.Layout.format layout ~boundary ~width tree
      in
      let has (k : int) (wanted : Core.Grammar.trivia_class option -> bool) : bool =
        Array.exists f.tokens ~f:(fun (t : Core.Token.def) ->
          Core.Kind.to_int t.kind = k && wanted t.trivia)
      in
      let trivia (k : int) : bool = has k Option.is_some in
      let comment (k : int) : bool = has k (fun t -> t = Some Core.Grammar.Preserve) in
      let rec tokens ~(comments : bool) (node : Siesta.Green.node) : string list =
        List.concat_map
          (Array.to_list (Siesta.Green.children_array node))
          ~f:(fun child ->
            match child with
            | Siesta.Green.Node node -> tokens ~comments node
            | Siesta.Green.Token token ->
              let k = Siesta.Green.Token.kind token in
              if comment k
              then if comments then [ Siesta.Green.Token.text token ] else []
              else if trivia k || comments
              then []
              else [ Siesta.Green.Token.text token ])
      in
      let sorted (l : string list) = List.sort l ~cmp:String.compare in
      List.iter (Inputs.all c.inputs) ~f:(fun (src : string) ->
        match parse src with
        | _, _ :: _ -> ()
        | tree, [] ->
          let root = Siesta.Syntax.of_root tree in
          let root_kind = Siesta.Syntax.kind root in
          (match item_slot f root_kind, c.slots root with
           | Some slot, Some filled ->
             let items =
               List.filter_map filled.(slot) ~f:(fun (elem : Siesta.Syntax.elem) ->
                 match elem with
                 | Siesta.Syntax.Node node -> Some node
                 | Siesta.Syntax.Token _ -> None)
             in
             let n = List.length items in
             let children = Siesta.Syntax.children_array root in
             (* The comments directly above an item, worked out here. *)
             let run (item : Siesta.Syntax.t) : string list =
               let rec back (i : int) (acc : string list) =
                 if i < 0
                 then acc
                 else (
                   match children.(i) with
                   | Siesta.Syntax.Token token
                     when comment (Siesta.Syntax.Token.kind token) ->
                     back (i - 1) (Siesta.Syntax.Token.text token :: acc)
                   | Siesta.Syntax.Token token
                     when trivia (Siesta.Syntax.Token.kind token) ->
                     let text = Siesta.Syntax.Token.text token in
                     if
                       String.fold_left text ~init:0 ~f:(fun n ch ->
                         if ch = '\n' then n + 1 else n)
                       >= 2
                     then acc
                     else back (i - 1) acc
                   | Siesta.Syntax.Token _ | Siesta.Syntax.Node _ -> acc)
               in
               back (Siesta.Syntax.index_in_parent item - 1) []
             in
             let ctx =
               Lingo_runtime.Rewrite.Ctx.create (Siesta.Cache.create_plain ()) root ()
             in
             let cache = Lingo_runtime.Rewrite.Ctx.cache ctx in
             let edit (s : unit Lingo_runtime.Rewrite.t option) : Siesta.Green.node option
               =
               match s with
               | None -> None
               | Some s ->
                 (match s ctx root with
                  | Ok green -> Some green
                  | Error _ -> None)
             in
             (* Each edit, with the comments it loses and the ones it adds. *)
             let edits =
               List.concat
                 (List.init ~len:n ~f:(fun (j : int) ->
                    let item = List.nth items j in
                    let item_green = Siesta.Syntax.green item in
                    let inner = tokens ~comments:true item_green in
                    let deleted =
                      Option.map
                        (fun after ->
                           "deleting item " ^ string_of_int j, after, run item @ inner, [])
                        (edit (c.delete_at root_kind slot ~at:j))
                    in
                    let inserted =
                      Option.map
                        (fun after ->
                           "inserting a copy of item " ^ string_of_int j, after, [], inner)
                        (edit
                           (c.insert_at
                              root_kind
                              slot
                              ~at:(n - j)
                              (Siesta.Green.Node item_green)))
                    in
                    let rebuilt =
                      match c.rebuild cache item with
                      | Some (Ok green) ->
                        Some
                          ( "rebuilding item " ^ string_of_int j
                          , Siesta.Syntax.green
                              (Siesta.Syntax.replace cache item green).root
                          , []
                          , [] )
                      | Some (Error _) | None -> None
                    in
                    (* Each item to every other place. Which item the match
                       reads as moved depends on the order, so this is how an
                       item with comments above it gets moved at all. *)
                    let moves =
                      List.filter_map
                        (List.init ~len:n ~f:(fun t -> t))
                        ~f:(fun (target : int) ->
                          if target = j
                          then None
                          else (
                            match c.delete_at root_kind slot ~at:j with
                            | None -> None
                            | Some delete ->
                              (match delete ctx root with
                               | Error _ -> None
                               | Ok without ->
                                 let without = Siesta.Syntax.of_root without in
                                 (match
                                    c.insert_at
                                      root_kind
                                      slot
                                      ~at:target
                                      (Siesta.Green.Node item_green)
                                  with
                                  | None -> None
                                  | Some insert ->
                                    (match insert ctx without with
                                     | Error _ -> None
                                     | Ok after ->
                                       incr moved;
                                       Some
                                         ( Printf.sprintf "moving item %d to %d" j target
                                         , after
                                         , []
                                         , [] ))))))
                    in
                    List.filter_map [ deleted; inserted; rebuilt ] ~f:(fun e -> e) @ moves))
             in
             List.iter
               edits
               ~f:
                 (fun
                   ( (what : string)
                   , (after : Siesta.Green.node)
                   , (lost : string list)
                   , (gained : string list) )
                 ->
                 incr runs;
                 let splices = c.apply ~format ~before:tree ~after in
                 let text = Lingo_runtime.Rewrite.splice src splices in
                 match parse text with
                 | _, _ :: _ ->
                   Law.fail
                     "(a) %s: %s of %S gives %S, which does not parse"
                     c.name
                     what
                     src
                     text
                 | spliced, [] ->
                   if
                     (not
                        (String.equal
                           (Fuzz.Oracles.shape spliced)
                           (Fuzz.Oracles.shape after)))
                     || Body_tokens.meaningful ~comments:false f spliced
                        <> Body_tokens.meaningful ~comments:false f after
                   then
                     Law.fail
                       "(b) %s: %s of %S gives %S, which is another tree"
                       c.name
                       what
                       src
                       text;
                   (* (c) every kept item's bytes are still in the text. *)
                   let after_tags =
                     match c.slots (Siesta.Syntax.of_root after) with
                     | Some filled ->
                       List.filter_map
                         filled.(slot)
                         ~f:(fun (elem : Siesta.Syntax.elem) ->
                           match elem with
                           | Siesta.Syntax.Node node ->
                             Some (Siesta.Green.tag (Siesta.Syntax.green node))
                           | Siesta.Syntax.Token _ -> None)
                     | None -> []
                   in
                   List.iter items ~f:(fun (item : Siesta.Syntax.t) ->
                     let tag = Siesta.Green.tag (Siesta.Syntax.green item) in
                     if List.mem tag ~set:after_tags
                     then (
                       let bytes = Siesta.Syntax.to_source item in
                       let rec found (from : int) =
                         from + String.length bytes <= String.length text
                         && (String.equal
                               (String.sub text ~pos:from ~len:(String.length bytes))
                               bytes
                             || found (from + 1))
                       in
                       if not (found 0)
                       then
                         Law.fail
                           "(c) %s: %s of %S loses the bytes of %S"
                           c.name
                           what
                           src
                           bytes));
                   (* (d) *)
                   let once = format spliced in
                   (match parse once with
                    | twice, [] ->
                      if not (String.equal (format twice) once)
                      then
                        Law.fail
                          "(d) %s: %s of %S does not format to a fixed point"
                          c.name
                          what
                          src
                    | _, _ :: _ ->
                      Law.fail
                        "(d) %s: %s of %S formats to text that does not parse"
                        c.name
                        what
                        src);
                   (* (e) *)
                   let had = tokens ~comments:true tree in
                   let want =
                     List.fold_left lost ~init:had ~f:(fun acc gone ->
                       let rec drop l =
                         match l with
                         | [] -> []
                         | x :: rest ->
                           if String.equal x gone then rest else x :: drop rest
                       in
                       drop acc)
                     @ gained
                   in
                   if sorted (tokens ~comments:true spliced) <> sorted want
                   then
                     Law.fail
                       "(e) %s: %s of %S gives %S, which has other comments"
                       c.name
                       what
                       src
                       text)
           | _ -> ())));
  if !moved = 0 then Law.fail "no item was moved";
  if Law.failures () = before
  then Law.pass "every splice holds, over %d edits, %d of them moves" !runs !moved
;;

let () = Law.summarise "law_apply"
