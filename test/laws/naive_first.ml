(** A second implementation of FIRST, FOLLOW and nullability.

    It walks the grammar with [Set.Make(String)] over token names, where
    {!Core.Internal.Fixpoint} walks resolved rules with bitsets over
    kind integers. Two implementations of one fact, so a cross-check can
    tell whether they agree.

    It follows the predecessor's algorithm, which has years of grammars run
    through it. Where the two disagree, the argument has to be made about
    which is right. *)

open Core.Grammar
module SS = Set.Make (String)

type tables =
  { nullable : (string, bool) Hashtbl.t
  ; first : (string, SS.t) Hashtbl.t
  ; follow : (string, SS.t) Hashtbl.t
  }

let syms_of = function
  | Single s -> [ s ]
  | Alternatives ss -> ss
;;

let compute (g : t) : tables =
  let null = Hashtbl.create 16 in
  List.iter
    (fun (p : production) -> Hashtbl.replace null (Name.Rule.to_string p.kind_name) false)
    g.productions;
  List.iter
    (fun (e : expr_def) -> Hashtbl.replace null (Name.Rule.to_string e.rule_name) false)
    g.expr;
  let nullable_of r = Option.value ~default:false (Hashtbl.find_opt null r) in
  let sym_nullable = function
    | Token _ -> false
    | Rule r -> nullable_of r
  in
  let csym_nullable cs = List.exists sym_nullable (syms_of cs) in
  let child_nullable (c : child) =
    match c.modifier with
    | Zero_or_one | Zero_or_more -> true
    | Exactly_one | One_or_more -> csym_nullable c.sym
  in
  let prod_nullable (p : production) =
    match p.framing with
    (* A delimited production takes its open token whatever its children do. A
       separated one is its children, and whether a list can be empty is the
       element child's modifier. *)
    | Delimited _ -> false
    | Separated _ | Plain | Committed _ -> List.for_all child_nullable p.children
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (p : production) ->
         let now = prod_nullable p in
         if nullable_of (Name.Rule.to_string p.kind_name) <> now
         then (
           Hashtbl.replace null (Name.Rule.to_string p.kind_name) now;
           changed := true))
      g.productions
  done;
  (* ---- FIRST ---- *)
  let first = Hashtbl.create 16 in
  List.iter
    (fun (p : production) ->
       Hashtbl.replace first (Name.Rule.to_string p.kind_name) SS.empty)
    g.productions;
  List.iter
    (fun (e : expr_def) ->
       Hashtbl.replace first (Name.Rule.to_string e.rule_name) SS.empty)
    g.expr;
  let first_of r = Option.value ~default:SS.empty (Hashtbl.find_opt first r) in
  let sym_first = function
    | Token t -> SS.singleton t
    | Rule r -> first_of r
  in
  let csym_first cs =
    List.fold_left (fun acc s -> SS.union acc (sym_first s)) SS.empty (syms_of cs)
  in
  let rec children_first = function
    | [] -> SS.empty
    | (c : child) :: rest ->
      let mine = csym_first c.sym in
      if child_nullable c then SS.union mine (children_first rest) else mine
  in
  let prod_first (p : production) =
    match p.framing with
    | Delimited { open_tok; _ } -> SS.singleton (Name.Token.to_string open_tok)
    | Separated _ ->
      (match p.children with
       | c :: _ -> csym_first c.sym
       | [] -> SS.empty)
    | Plain | Committed _ -> children_first p.children
  in
  let expr_first (e : expr_def) =
    let atoms =
      List.fold_left (fun acc s -> SS.union acc (sym_first s)) SS.empty e.atoms
    in
    List.fold_left
      (fun acc (o : operator) -> SS.add (Name.Token.to_string o.op_token) acc)
      atoms
      e.prefix_ops
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (p : production) ->
         let now = prod_first p in
         if not (SS.equal (first_of (Name.Rule.to_string p.kind_name)) now)
         then (
           Hashtbl.replace first (Name.Rule.to_string p.kind_name) now;
           changed := true))
      g.productions;
    List.iter
      (fun (e : expr_def) ->
         let now = expr_first e in
         if not (SS.equal (first_of (Name.Rule.to_string e.rule_name)) now)
         then (
           Hashtbl.replace first (Name.Rule.to_string e.rule_name) now;
           changed := true))
      g.expr
  done;
  (* ---- FOLLOW ---- *)
  let follow = Hashtbl.create 16 in
  List.iter
    (fun (p : production) ->
       Hashtbl.replace follow (Name.Rule.to_string p.kind_name) SS.empty)
    g.productions;
  List.iter
    (fun (e : expr_def) ->
       Hashtbl.replace follow (Name.Rule.to_string e.rule_name) SS.empty)
    g.expr;
  let follow_of r = Option.value ~default:SS.empty (Hashtbl.find_opt follow r) in
  let changed = ref true in
  let propagate r added =
    let cur = follow_of r in
    let next = SS.union cur added in
    if not (SS.equal cur next)
    then (
      Hashtbl.replace follow r next;
      changed := true)
  in
  let rec remaining_first = function
    | [] -> SS.empty, true
    | (c : child) :: rest ->
      let mine = csym_first c.sym in
      if child_nullable c
      then (
        let rf, re = remaining_first rest in
        SS.union mine rf, re)
      else mine, false
  in
  let targets cs =
    List.filter_map
      (function
        | Rule r -> Some r
        | Token _ -> None)
      (syms_of cs)
  in
  while !changed do
    changed := false;
    List.iter
      (fun (p : production) ->
         let parent = follow_of (Name.Rule.to_string p.kind_name) in
         let trailing =
           match p.framing with
           | Delimited { close_tok; _ } -> SS.singleton (Name.Token.to_string close_tok)
           | Separated _ | Plain | Committed _ -> parent
         in
         let rec walk = function
           | [] -> ()
           | (c : child) :: rest ->
             let rf, re = remaining_first rest in
             let rf =
               match c.modifier with
               | Exactly_one | Zero_or_one -> rf
               | Zero_or_more | One_or_more ->
                 (match p.framing with
                  | Delimited { sep_policy = With_sep { sep; _ }; _ } ->
                    SS.add (Name.Token.to_string sep) rf
                  | Separated { sep; _ } -> SS.add (Name.Token.to_string sep) rf
                  | _ -> SS.union rf (csym_first c.sym))
             in
             List.iter
               (fun r ->
                  propagate r rf;
                  if re then propagate r trailing)
               (targets c.sym);
             walk rest
         in
         walk p.children)
      g.productions;
    List.iter
      (fun (e : expr_def) ->
         let own =
           let s =
             List.fold_left
               (fun acc (o : operator) -> SS.add (Name.Token.to_string o.op_token) acc)
               SS.empty
               e.infix_ops
           in
           List.fold_left
             (fun acc (p : postfix_op) ->
                let acc = SS.add (Name.Token.to_string p.lead) acc in
                match p.body with
                | Nothing | Then _ -> acc
                | Enclosed { close; content } ->
                  let acc = SS.add (Name.Token.to_string close) acc in
                  (match content with
                   | One _ -> acc
                   | Many { sep = With_sep { sep; _ }; _ } ->
                     SS.add (Name.Token.to_string sep) acc
                   | Many { sep = No_sep; _ } -> acc))
             s
             e.postfix
         in
         propagate (Name.Rule.to_string e.rule_name) own;
         let ef = follow_of (Name.Rule.to_string e.rule_name) in
         List.iter
           (function
             | Rule r -> propagate r ef
             | Token _ -> ())
           e.atoms;
         List.iter
           (fun (p : postfix_op) ->
              match p.body with
              | Nothing -> ()
              | Then (Rule r) -> propagate r ef
              | Then (Token _) -> ()
              | Enclosed { close; content } ->
                let inside =
                  match content with
                  | Many { sep = With_sep { sep; _ }; _ } ->
                    SS.of_list [ Name.Token.to_string close; Name.Token.to_string sep ]
                  | _ -> SS.singleton (Name.Token.to_string close)
                in
                let syms =
                  match content with
                  | One s -> [ s ]
                  | Many { elem; _ } -> [ elem ]
                in
                List.iter
                  (function
                    | Rule r -> propagate r inside
                    | Token _ -> ())
                  syms)
           e.postfix)
      g.expr
  done;
  { nullable = null; first; follow }
;;
