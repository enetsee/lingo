(* -- parentheses ----------------------------------------------------------------

      A block's role constructors put parentheses round an operand where the
      parse needs them, and nowhere else.

      (a) Every bracketing atom that is an operand of a role is taken out of
          a clean parse, and every role node is built again with its [make],
          bottom up. The tree that comes
          out parses without a diagnostic, and with the bracketing atoms left
          out of both, it has the shape the original had.
      (b) Every bracketing atom in that tree is needed. Taking any one of
          them out changes the shape, or leaves a tree that does not parse.
      (c) [needs_parens ~at node] holds for an operand inside a bracketing
          atom exactly where taking the atom out changes the shape.
      (d) An expression built from the constructors alone, at random, parses
          back to the expression that was built. Every bracketing atom the
          constructors put in is needed, as in (b). [needs_parens] holds for
          what each atom holds, and for no operand left bare.

      Mechanism. Every tree is formatted and parsed again before it is
      compared, so a shape here is what the parser makes of the text. The
      shape is the kinds alone, with every token left out and every
      bracketing atom replaced by what it holds.

      The bracketing atoms are found here from the facts, separately from the
      generator: a delimited production whose one required child is the
      block, and one of the block's atoms.

      The corpus holds few operands in parentheses, so (d) builds its own.
      It draws calc expressions, with a prefix operator and two levels of
      left-associative ones; ml expressions, whose [if] ends in an
      expression that takes in any operator after it; and ml types, with a
      right-associative arrow above a left-associative product. Each is
      formatted and parsed again inside its file.

      ml's expressions have no bracketing atom, so a draw that needs one is
      refused with a reason, and counted. Building it anyway would give text
      that parses as another expression.

      rassoc and rust have no bracketing atom. A role constructor there that
      wanted parentheses would fail, so on those two grammars (a) checks that
      none is ever wanted for a tree the parser built. calc carries a prefix
      operator and both associativities, wide and ml carry atoms of their
      own, and ml's [if] atom ends in an expression that takes in whatever
      operator follows it.
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
  ; needs_parens : at:Siesta.Syntax.t -> Siesta.Syntax.t -> bool
  }

let corpus : case list =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; inputs = Inputs.sexp
    ; rebuild = Emitted_probe.Sexp_probe.rebuild
    ; needs_parens = Emitted_probe.Sexp_probe.needs_parens
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; inputs = Inputs.json
    ; rebuild = Emitted_probe.Json_probe.rebuild
    ; needs_parens = Emitted_probe.Json_probe.needs_parens
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; inputs = Inputs.calc
    ; rebuild = Emitted_probe.Calc_probe.rebuild
    ; needs_parens = Emitted_probe.Calc_probe.needs_parens
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; inputs = Inputs.rassoc
    ; rebuild = Emitted_probe.Rassoc_probe.rebuild
    ; needs_parens = Emitted_probe.Rassoc_probe.needs_parens
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; inputs = Inputs.postfix
    ; rebuild = Emitted_probe.Postfix_probe.rebuild
    ; needs_parens = Emitted_probe.Postfix_probe.needs_parens
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; inputs = Inputs.shapes
    ; rebuild = Emitted_probe.Shapes_probe.rebuild
    ; needs_parens = Emitted_probe.Shapes_probe.needs_parens
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; inputs = Inputs.unicode
    ; rebuild = Emitted_probe.Unicode_probe.rebuild
    ; needs_parens = Emitted_probe.Unicode_probe.needs_parens
    }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; inputs = Inputs.recovery
    ; rebuild = Emitted_probe.Recovery_probe.rebuild
    ; needs_parens = Emitted_probe.Recovery_probe.needs_parens
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; inputs = Inputs.comments
    ; rebuild = Emitted_probe.Comments_probe.rebuild
    ; needs_parens = Emitted_probe.Comments_probe.needs_parens
    }
  ; { name = "rust"
    ; grammar = Lingo_grammars.Rust_grammar.grammar
    ; inputs = Inputs.rust
    ; rebuild = Emitted_probe.Rust_probe.rebuild
    ; needs_parens = Emitted_probe.Rust_probe.needs_parens
    }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; inputs = Inputs.effekt
    ; rebuild = Emitted_probe.Effekt_probe.rebuild
    ; needs_parens = Emitted_probe.Effekt_probe.needs_parens
    }
  ; { name = "wide"
    ; grammar = Lingo_grammars.Wide_grammar.grammar
    ; inputs = Inputs.wide
    ; rebuild = Emitted_probe.Wide_probe.rebuild
    ; needs_parens = Emitted_probe.Wide_probe.needs_parens
    }
  ; { name = "ml"
    ; grammar = Lingo_grammars.Ml_grammar.grammar
    ; inputs = Inputs.ml
    ; rebuild = Emitted_probe.Ml_probe.rebuild
    ; needs_parens = Emitted_probe.Ml_probe.needs_parens
    }
  ]
;;

let width = 80

(* The bracketing atoms' kinds. *)
let parens (f : Core.Facts.t) : int list =
  Array.to_list f.blocks
  |> List.filter_map ~f:(fun (b : Core.Block.def) ->
    Array.find_map b.atoms ~f:(fun (k : Core.Kind.t) ->
      match Core.Facts.rule_of_kind f k with
      | Some { origin = Core.Rule.User; frame = Core.Rule.Delimited _; children; _ } ->
        (match children with
         | [| { modifier = Core.Grammar.Exactly_one; alts = [| child |]; _ } |]
           when Core.Kind.equal child b.kind -> Some (Core.Kind.to_int k)
         | _ -> None)
      | Some _ | None -> None))
;;

let roles (f : Core.Facts.t) : int list =
  Array.to_list f.rules
  |> List.filter_map ~f:(fun (d : Core.Rule.def) ->
    match d.origin with
    | Core.Rule.Pratt_role _ -> Some (Core.Kind.to_int d.kind)
    | Core.Rule.User | Core.Rule.Pratt_block -> None)
;;

(* A bracketing atom that is an operand of a role. Elsewhere a production can
   name the atom itself, as effekt's [if (x)] does, and taking it out there
   breaks the syntax rather than the precedence. *)
let operand (parens : int list) (roles : int list) (node : Siesta.Syntax.t) : bool =
  List.mem (Siesta.Syntax.kind node) ~set:parens
  &&
  match Siesta.Syntax.parent node with
  | Some parent -> List.mem (Siesta.Syntax.kind parent) ~set:roles
  | None -> false
;;

(* The one node a bracketing atom holds. *)
let inner (node : Siesta.Syntax.t) : Siesta.Syntax.t option =
  Array.find_map
    (Siesta.Syntax.children_array node)
    ~f:(fun (elem : Siesta.Syntax.elem) ->
      match elem with
      | Siesta.Syntax.Node child -> Some child
      | Siesta.Syntax.Token _ -> None)
;;

(* The kinds, with tokens left out and each bracketing atom replaced by what
   it holds. *)
let shape (parens : int list) (tree : Siesta.Green.node) : string =
  let buffer = Buffer.create 256 in
  let rec go (node : Siesta.Green.node) =
    let children =
      List.filter_map
        (Array.to_list (Siesta.Green.children_array node))
        ~f:(fun (c : Siesta.Green.child) ->
          match c with
          | Siesta.Green.Node n -> Some n
          | Siesta.Green.Token _ -> None)
    in
    match children with
    | [ held ] when List.mem (Siesta.Green.kind node) ~set:parens -> go held
    | _ ->
      Buffer.add_string buffer (Printf.sprintf "(%d" (Siesta.Green.kind node));
      List.iter children ~f:(fun n ->
        Buffer.add_char buffer ' ';
        go n);
      Buffer.add_char buffer ')'
  in
  go tree;
  Buffer.contents buffer
;;

let () =
  let before = Law.failures () in
  let trees = ref 0 in
  let rebuilt = ref 0 in
  let added = ref 0 in
  let asked = ref 0 in
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
      let parens = parens f in
      let roles = roles f in
      let shape = shape parens in
      (* The tree with one bracketing atom replaced by what it holds, settled. *)
      let without (atom : Siesta.Syntax.t) (held : Siesta.Syntax.t) =
        let cache = Siesta.Cache.create_plain () in
        settle
          (Siesta.Syntax.green
             (Siesta.Syntax.replace cache atom (Siesta.Syntax.green held)).root)
      in
      List.iter (Inputs.all c.inputs) ~f:(fun (src : string) ->
        match parse src with
        | _, _ :: _ -> ()
        | tree, [] ->
          incr trees;
          let original, _ = settle tree in
          let want = shape original in
          (* (c) on the original's own bracketing atoms. *)
          Seq.iter
            (fun (atom : Siesta.Syntax.t) ->
               if operand parens roles atom
               then (
                 match inner atom with
                 | None -> ()
                 | Some held ->
                   incr asked;
                   let gone, diagnostics = without atom held in
                   let changes =
                     diagnostics <> [] || not (String.equal (shape gone) want)
                   in
                   let says = c.needs_parens ~at:atom held in
                   if says <> changes
                   then
                     Law.fail
                       "(c) %s: needs_parens gives %b for %S in %S"
                       c.name
                       says
                       (Siesta.Syntax.to_source held)
                       (Siesta.Green.to_source original)))
            (Siesta.Syntax.descendants (Siesta.Syntax.of_root original));
          (* (a): take every atom out, and build every role again. *)
          let strip : unit Lingo_runtime.Rewrite.t =
            fun ctx node ->
            let k = Siesta.Syntax.kind node in
            if operand parens roles node
            then (
              match inner node with
              | Some held -> Ok (Siesta.Syntax.green held)
              | None -> Ok (Siesta.Syntax.green node))
            else if List.mem k ~set:roles
            then (
              incr rebuilt;
              match c.rebuild (Lingo_runtime.Rewrite.Ctx.cache ctx) node with
              | Some result -> result
              | None -> Ok (Siesta.Syntax.green node))
            else Ok (Siesta.Syntax.green node)
          in
          let root = Siesta.Syntax.of_root original in
          let ctx =
            Lingo_runtime.Rewrite.Ctx.create (Siesta.Cache.create_plain ()) root ()
          in
          (match Lingo_runtime.Rewrite.bottomup strip ctx root with
           | Error reason ->
             Law.fail
               "(a) %s: rebuilding %S failed with %S"
               c.name
               (Siesta.Green.to_source original)
               reason
           | Ok stripped ->
             let again, diagnostics = settle stripped in
             if diagnostics <> []
             then
               Law.fail
                 "(a) %s: %S rebuilt as %S does not parse"
                 c.name
                 (Siesta.Green.to_source original)
                 (Siesta.Green.to_source stripped)
             else if not (String.equal (shape again) want)
             then
               Law.fail
                 "(a) %s: %S rebuilt as %S changes the shape"
                 c.name
                 (Siesta.Green.to_source original)
                 (Siesta.Green.to_source again)
             else
               (* (b): every atom the constructors put in is needed. *)
               Seq.iter
                 (fun (atom : Siesta.Syntax.t) ->
                    if operand parens roles atom
                    then (
                      match inner atom with
                      | None -> ()
                      | Some held ->
                        incr added;
                        let gone, diagnostics = without atom held in
                        if diagnostics = [] && String.equal (shape gone) want
                        then
                          Law.fail
                            "(b) %s: the parentheses round %S in %S are not needed"
                            c.name
                            (Siesta.Syntax.to_source held)
                            (Siesta.Green.to_source again)))
                 (Siesta.Syntax.descendants (Siesta.Syntax.of_root again)))));
  if !added = 0 then Law.fail "(b) no constructor put in a bracketing atom";
  if !asked = 0 then Law.fail "(c) no bracketing atom in the corpus";
  if Law.failures () = before
  then
    Law.pass
      "parentheses go where they are needed: %d roles rebuilt in %d trees, %d atoms put \
       in, %d asked about"
      !rebuilt
      !trees
      !added
      !asked
;;

(* -- (d) expressions built from the constructors ---------------------------- *)

let ( let* ) = Result.bind

module Calc = struct
  let rec expr (cache : Siesta.Cache.t) (state : Random.State.t) (depth : int)
    : (Emitted_views.Calc_views.expr_position, string) result
    =
    if depth = 0 || Random.State.int state 4 = 0
    then
      let* atom =
        Emitted_rewrite.Calc_rewrite.Expr.make
          cache
          (string_of_int (Random.State.int state 10))
      in
      Ok (Emitted_views.Calc_views.Expr_position_expr atom)
    else if Random.State.int state 4 = 0
    then
      let* operand = expr cache state (depth - 1) in
      let* prefix = Emitted_rewrite.Calc_rewrite.Expr_prefix.make cache operand in
      Ok (Emitted_views.Calc_views.Expr_position_expr_prefix prefix)
    else (
      let op =
        match Random.State.int state 4 with
        | 0 -> `Plus
        | 1 -> `Minus
        | 2 -> `Star
        | _ -> `Slash
      in
      let* lhs = expr cache state (depth - 1) in
      let* rhs = expr cache state (depth - 1) in
      let* bin = Emitted_rewrite.Calc_rewrite.Expr_bin.make cache ~lhs ~op ~rhs () in
      Ok (Emitted_views.Calc_views.Expr_position_expr_bin bin))
  ;;

  let build (cache : Siesta.Cache.t) (state : Random.State.t)
    : (Siesta.Syntax.t, string) result
    =
    Result.map Emitted_views.Calc_views.Expr_position.syntax (expr cache state 5)
  ;;
end

module Ml_type = struct
  let rec ty (cache : Siesta.Cache.t) (state : Random.State.t) (depth : int)
    : (Emitted_views.Ml_views.type__position, string) result
    =
    if depth = 0 || Random.State.int state 4 = 0
    then
      let* atom =
        Emitted_rewrite.Ml_rewrite.Type.make
          cache
          (if Random.State.bool state then "int" else "bool")
      in
      Ok (Emitted_views.Ml_views.Type__position_type atom)
    else (
      let op = if Random.State.bool state then `Arrow else `Star in
      let* lhs = ty cache state (depth - 1) in
      let* rhs = ty cache state (depth - 1) in
      let* bin = Emitted_rewrite.Ml_rewrite.Type_bin.make cache ~lhs ~op ~rhs () in
      Ok (Emitted_views.Ml_views.Type__position_type_bin bin))
  ;;

  let build (cache : Siesta.Cache.t) (state : Random.State.t)
    : (Siesta.Syntax.t, string) result
    =
    Result.map Emitted_views.Ml_views.Type__position.syntax (ty cache state 5)
  ;;
end

module Ml_expr = struct
  (* [with_else] says whether an [if] may leave its [else] out. *)
  let rec expr
            ~(with_else : bool)
            (cache : Siesta.Cache.t)
            (state : Random.State.t)
            (depth : int)
    : (Emitted_views.Ml_views.expr_position, string) result
    =
    let sub () = expr ~with_else cache state (depth - 1) in
    if depth = 0 || Random.State.int state 4 = 0
    then
      let* atom =
        Emitted_rewrite.Ml_rewrite.Expr.make
          cache
          (if Random.State.bool state then `Int "1" else `Ident "x")
      in
      Ok (Emitted_views.Ml_views.Expr_position_expr atom)
    else (
      match Random.State.int state 5 with
      | 0 ->
        let* operand = sub () in
        let* prefix = Emitted_rewrite.Ml_rewrite.Expr_prefix.make cache operand in
        Ok (Emitted_views.Ml_views.Expr_position_expr_prefix prefix)
      | 1 ->
        let* cond = sub () in
        let* t = sub () in
        let* else_ =
          if with_else || Random.State.bool state
          then
            let* e = sub () in
            Result.map Option.some (Emitted_rewrite.Ml_rewrite.Else.make cache e)
          else Ok None
        in
        let* if_ = Emitted_rewrite.Ml_rewrite.If.make cache ~cond ~t ?else_ () in
        Ok (Emitted_views.Ml_views.Expr_position_if if_)
      | _ ->
        let op =
          match Random.State.int state 6 with
          | 0 -> `Eqeq
          | 1 -> `Lt
          | 2 -> `Plus
          | 3 -> `Minus
          | 4 -> `Star
          | _ -> `Slash
        in
        let* lhs = sub () in
        let* rhs = sub () in
        let* bin = Emitted_rewrite.Ml_rewrite.Expr_bin.make cache ~lhs ~op ~rhs () in
        Ok (Emitted_views.Ml_views.Expr_position_expr_bin bin))
  ;;

  let build ~(with_else : bool) (cache : Siesta.Cache.t) (state : Random.State.t)
    : (Siesta.Syntax.t, string) result
    =
    Result.map Emitted_views.Ml_views.Expr_position.syntax (expr ~with_else cache state 4)
  ;;
end

type drawn =
  { grammar_name : string
  ; draw_grammar : Core.Grammar.t
  ; build : Siesta.Cache.t -> Random.State.t -> (Siesta.Syntax.t, string) result
  ; wrap : string -> string (** A whole file around the expression's text. *)
  ; block : string (** The block the expression belongs to. *)
  ; asks : at:Siesta.Syntax.t -> Siesta.Syntax.t -> bool
  }

let drawn : drawn list =
  [ { grammar_name = "calc"
    ; draw_grammar = Lingo_grammars.Calc_grammar.grammar
    ; build = Calc.build
    ; wrap = (fun text -> text)
    ; block = "Expr"
    ; asks = Emitted_probe.Calc_probe.needs_parens
    }
  ; { grammar_name = "ml"
    ; draw_grammar = Lingo_grammars.Ml_grammar.grammar
    ; build = Ml_expr.build ~with_else:true
    ; wrap = (fun text -> "let x : int = " ^ text ^ ";")
    ; block = "Expr"
    ; asks = Emitted_probe.Ml_probe.needs_parens
    }
  ; { grammar_name = "ml"
    ; draw_grammar = Lingo_grammars.Ml_grammar.grammar
    ; build = Ml_type.build
    ; wrap = (fun text -> "type T = " ^ text ^ ";")
    ; block = "Type"
    ; asks = Emitted_probe.Ml_probe.needs_parens
    }
  ]
;;

let draws = 400

let () =
  let before = Law.failures () in
  let built = ref 0 in
  let added = ref 0 in
  let refused = ref 0 in
  List.iter drawn ~f:(fun (d : drawn) ->
    match Core.Facts.of_grammar d.draw_grammar with
    | Error _ -> Law.fail "%s: the grammar does not check" d.grammar_name
    | Ok f ->
      let plan, _ = Plan.Lower.of_facts f in
      let entry = plan.Ir.Plan.roots.(0) in
      let parens = parens f in
      let roles = roles f in
      let shape = shape parens in
      (* Every kind an expression of the block can have at its top. *)
      let kinds =
        Array.to_list f.blocks
        |> List.concat_map ~f:(fun (b : Core.Block.def) ->
          if String.equal (Core.Grammar.Name.Rule.to_string b.name) d.block
          then
            (Core.Kind.to_int b.kind
             :: List.map (Array.to_list b.atoms) ~f:Core.Kind.to_int)
            @ List.filter_map (Array.to_list f.rules) ~f:(fun (r : Core.Rule.def) ->
              match r.origin with
              | Core.Rule.Pratt_role { block; _ } when block = b.rule_id ->
                Some (Core.Kind.to_int r.kind)
              | Core.Rule.Pratt_role _ | Core.Rule.User | Core.Rule.Pratt_block -> None)
          else [])
      in
      (* The text parsed inside its file, and the expression found again. *)
      let reparse (text : string) : Siesta.Syntax.t option =
        match Interp.run plan entry (Lex.run f (d.wrap text)) with
        | _, _ :: _ -> None
        | tree, [] ->
          Seq.find
            (fun (node : Siesta.Syntax.t) ->
               List.mem (Siesta.Syntax.kind node) ~set:kinds)
            (Siesta.Syntax.descendants (Siesta.Syntax.of_root tree))
      in
      let layout = Layout.Lower.of_facts f in
      let boundary = Lex.boundary f in
      (* The formatter writes the spaces that keep [if] and [x] apart. *)
      let format (node : Siesta.Syntax.t) : string =
        Lingo_runtime.Layout.format layout ~boundary ~width (Siesta.Syntax.green node)
      in
      let state = Random.State.make [| 7 |] in
      for _ = 1 to draws do
        let cache = Siesta.Cache.create_plain () in
        match d.build cache state with
        | Error reason when String.ends_with ~suffix:"has no bracketing atom" reason ->
          incr refused
        | Error reason -> Law.fail "(d) %s: building failed with %S" d.grammar_name reason
        | Ok expression ->
          incr built;
          let text = format expression in
          let want = shape (Siesta.Syntax.green expression) in
          (match reparse text with
           | None -> Law.fail "(d) %s: %S does not parse" d.grammar_name text
           | Some again when not (String.equal (shape (Siesta.Syntax.green again)) want)
             -> Law.fail "(d) %s: %S parses as another expression" d.grammar_name text
           | Some _ ->
             (* [needs_parens] holds inside every atom put in, since each is
                needed, and for no operand left bare. *)
             Seq.iter
               (fun (node : Siesta.Syntax.t) ->
                  match Siesta.Syntax.parent node with
                  | Some parent when List.mem (Siesta.Syntax.kind parent) ~set:roles ->
                    let says, held =
                      if List.mem (Siesta.Syntax.kind node) ~set:parens
                      then (
                        match inner node with
                        | Some held -> d.asks ~at:node held, true
                        | None -> true, true)
                      else d.asks ~at:node node, false
                    in
                    if says <> held
                    then
                      Law.fail
                        "(d) %s: needs_parens gives %b for %S in %S"
                        d.grammar_name
                        says
                        (Siesta.Syntax.to_source node)
                        text
                  | Some _ | None -> ())
               (Siesta.Syntax.descendants expression);
             Seq.iter
               (fun (atom : Siesta.Syntax.t) ->
                  if operand parens roles atom
                  then (
                    match inner atom with
                    | None -> ()
                    | Some held ->
                      incr added;
                      let gone =
                        Siesta.Syntax.replace cache atom (Siesta.Syntax.green held)
                      in
                      let gone_text = format gone.root in
                      (match reparse gone_text with
                       | Some again
                         when String.equal (shape (Siesta.Syntax.green again)) want ->
                         Law.fail
                           "(d) %s: the parentheses round %S in %S are not needed"
                           d.grammar_name
                           (Siesta.Syntax.to_source held)
                           text
                       | Some _ | None -> ())))
               (Siesta.Syntax.descendants expression))
      done);
  if Law.failures () = before
  then
    Law.pass
      "(d) %d expressions built from the constructors parse back, with %d atoms put in, \
       and %d refused for want of one"
      !built
      !added
      !refused
;;

let () = Law.summarise "law_parens"
