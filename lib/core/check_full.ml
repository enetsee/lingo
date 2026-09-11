open StdLabels

type ctx =
  { shape : Stage.shape
  ; fixpoint_tables : Fixpoint.tables
  ; fixpoint_reader : Fixpoint.Reader.t
  ; kind_table : Kind.Table.t
  }

let kind_refs (ctx : ctx) (set : Kind.Set.t) : Error.kind_ref list =
  Error.kind_refs ctx.kind_table (Kind.Set.elements set)
;;

let user_rules (ctx : ctx) : Rule.def list =
  List.filter (Array.to_list ctx.shape.rules) ~f:(fun (d : Rule.def) ->
    d.origin = Rule.User)
;;

(* -- first/first over one child's alternatives ----------------------------- *)

(* Alternative dispatch is a cascade. It takes the first arm whose FIRST
   admits the cursor. A leading token shared by two arms therefore always
   goes to the first of them. *)
let first_first (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    Array.fold_left d.children ~init:acc ~f:(fun acc (ch : Rule.child) ->
      if Array.length ch.alts < 2
      then acc
      else (
        let firsts =
          Array.to_list
            (Array.map ~f:(Fixpoint.Reader.kind_first ctx.fixpoint_reader) ch.alts)
        in
        let rec overlap acc = function
          | [] | [ _ ] -> acc
          | x :: rest ->
            overlap
              (List.fold_left rest ~init:acc ~f:(fun a y ->
                 Kind.Set.union a (Kind.Set.inter x y)))
              rest
        in
        let common = overlap Kind.Set.empty firsts in
        if Kind.Set.is_empty common
        then acc
        else
          Error.make
            ~detail:(Error.First_first_conflict { common = kind_refs ctx common })
            (Error.At_child { production = d.name; child = ch.child_name })
          :: acc)))
;;

(* -- first/follow at a skippable position ---------------------------------- *)

let first_follow (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    let cs = d.children in
    let len = Array.length cs in
    let sfirst, spasses = Fixpoint.Reader.suffix_first ctx.fixpoint_reader cs in
    let acc = ref acc in
    for idx = 0 to len - 1 do
      let ch = cs.(idx) in
      (* The parser can pass over an optional or repeated slot. It can also pass
         over a required one whose target rule derives empty. Those are the
         positions where it has to choose. *)
      let skippable =
        match ch.modifier with
        | Grammar.Optional | Grammar.Repeated -> true
        | Grammar.Required ->
          Array.exists ~f:(Fixpoint.Reader.kind_nullable ctx.fixpoint_reader) ch.alts
      in
      if skippable && not ch.greedy
      then (
        let rest_first = sfirst.(idx + 1) in
        let rest_passes = spasses.(idx + 1) in
        let tail = idx = len - 1 in
        let delim_addition =
          if not tail
          then Kind.Set.empty
          else (
            match d.frame with
            | Rule.Delimited { close; sep; _ } ->
              let s = Kind.Set.singleton close in
              (match sep with
               | Some { sep_tok; _ } -> Kind.Set.add sep_tok s
               | None -> s)
            | Rule.Separated _ | Rule.Plain | Rule.Committed _ -> Kind.Set.empty)
        in
        (* In a delimited production, the close token sits between the tail position
           and the parent's continuation. So the parent's FOLLOW lies past it and
           does not apply here. *)
        let in_delimited_tail =
          tail
          &&
          match d.frame with
          | Rule.Delimited _ -> true
          | _ -> false
        in
        let parent_follow =
          if rest_passes && not in_delimited_tail
          then ctx.fixpoint_tables.follow.(d.id)
          else Kind.Set.empty
        in
        let following =
          match d.frame with
          (* A separated list runs its loop off the separator. It takes a separator,
             then re-enters the element. It leaves the loop when the cursor holds
             anything but a separator.

             So the question here is whether the separator appears in FIRST of the
             element. *)
          | Rule.Separated { sep_tok; _ } -> Kind.Set.singleton sep_tok
          | Rule.Delimited _ | Rule.Plain | Rule.Committed _ ->
            Kind.Set.unions [ rest_first; delim_addition; parent_follow ]
        in
        let common =
          Kind.Set.inter (Fixpoint.Reader.alts_first ctx.fixpoint_reader ch) following
        in
        if not (Kind.Set.is_empty common)
        then
          acc
          := Error.make
               ~detail:(Error.First_follow_conflict { common = kind_refs ctx common })
               (Error.At_child { production = d.name; child = ch.child_name })
             :: !acc)
    done;
    !acc)
;;

(* -- left recursion -------------------------------------------------------- *)

(* Joins names for a sentence: ["A"], then ["A and B"], then ["A, B and C"].
   The members of a left recursion read as a list in prose. A token set
   reads as a set in braces instead. *)
let rec join_names = function
  | [] -> ""
  | [ x ] -> x
  | [ x; y ] -> x ^ " and " ^ y
  | x :: rest -> x ^ ", " ^ join_names rest
;;

(* [r -> s] where r's parser reaches s without taking a token first. A cycle
   here is a left recursion.

   A delimited production takes its opener before any rule call, so it gives
   no edges. An expression block reaches its atoms without taking a token,
   so a cycle through a block counts. *)
let left_recursion (ctx : ctx) (acc : Error.t list) : Error.t list =
  let adj = Hashtbl.create 32 in
  let add r s =
    Hashtbl.replace
      adj
      r
      (s
       ::
       (try Hashtbl.find adj r with
        | Not_found -> []))
  in
  Array.iter
    ~f:(fun (d : Rule.def) ->
      match d.origin with
      | Rule.Pratt_role _ -> ()
      | Rule.Pratt_block | Rule.User ->
        (match d.frame with
         | Rule.Delimited _ -> ()
         | Rule.Plain | Rule.Committed _ | Rule.Separated _ ->
           let n = Array.length d.children in
           let rec walk i =
             if i < n
             then (
               let ch = d.children.(i) in
               Array.iter
                 ~f:(fun k ->
                   let t = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
                   if t >= 0 then add d.id t)
                 ch.alts;
               if Fixpoint.Reader.child_nullable ctx.fixpoint_reader ch then walk (i + 1))
           in
           walk 0))
    ctx.shape.rules;
  Array.iter
    ~f:(fun (b : Block.def) ->
      Array.iter
        ~f:(fun k ->
          let t = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
          if t >= 0 then add b.rule_id t)
        b.atoms)
    ctx.shape.blocks;
  let succs r =
    try Hashtbl.find adj r with
    | Not_found -> []
  in
  (* Tarjan, one pass over the edge set. A component names a left recursion
     once.

     Cycles do not. A cycle has one rotation per node on it, so reporting
     cycles reports the same left recursion once per member. Something then
     has to walk the reports and recognise the rotations as one.

     Every cycle lies inside a strongly connected component, and a component
     of more than one rule holds a cycle. So the components to report are the
     ones with more than one rule, plus any rule that reaches itself. Every
     rule is a component on its own, which is why the single-rule case has to
     ask its edges. *)
  let n = Array.length ctx.shape.rules in
  let index = Array.make n (-1) in
  let low = Array.make n 0 in
  let stacked = Array.make n false in
  let stack = ref [] in
  let next = ref 0 in
  let components = ref [] in
  let rec visit v =
    index.(v) <- !next;
    low.(v) <- !next;
    incr next;
    stack := v :: !stack;
    stacked.(v) <- true;
    List.iter
      ~f:(fun w ->
        if index.(w) < 0
        then (
          visit w;
          low.(v) <- min low.(v) low.(w))
        else if stacked.(w)
        then low.(v) <- min low.(v) index.(w))
      (succs v);
    if low.(v) = index.(v)
    then (
      let rec pop acc =
        match !stack with
        | [] -> acc
        | w :: rest ->
          stack := rest;
          stacked.(w) <- false;
          if w = v then w :: acc else pop (w :: acc)
      in
      components := pop [] :: !components)
  in
  for v = 0 to n - 1 do
    if index.(v) < 0 then visit v
  done;
  (* Rule ids are assigned in declaration order. Sorting the members puts
     them in the author's order, and fixes which site the finding is filed
     at. The traversal can enter the component anywhere and the answer is the
     same. *)
  let cyclic =
    let elems =
      List.filter_map
        ~f:(fun members ->
          match List.sort members ~cmp:compare with
          | [ r ] when not (List.mem r ~set:(succs r)) -> None
          | ms -> Some ms)
        !components
    in
    List.sort elems ~cmp:compare
  in
  List.fold_left (List.rev cyclic) ~init:acc ~f:(fun acc ms ->
    let names = List.map ~f:(fun r -> ctx.shape.rules.(r).Rule.name) ms in
    Error.make
      ~detail:(Error.Left_recursion { members = names })
      (Error.At_production (List.hd names))
    :: acc)
;;

(* -- nullability hazards --------------------------------------------------- *)

(* A repeated child needs one arm that takes a token, or the loop can never
   make progress.

   A separated production is skipped here. Its loop runs off the separator
   and never off element FIRST, so a nullable element still lets the loop
   advance. That element is rejected anyway, by
   [nullable_pratt_and_separated] below, for a different reason. *)
let nullable_repeated (ctx : ctx) (acc : Error.t list) : Error.t list =
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    match d.frame with
    | Rule.Separated _ -> acc
    | _ ->
      Array.fold_left d.children ~init:acc ~f:(fun acc (ch : Rule.child) ->
        if ch.modifier <> Grammar.Repeated
        then acc
        else if
          Array.length ch.alts = 0
          || not
               (Array.for_all
                  ~f:(Fixpoint.Reader.kind_nullable ctx.fixpoint_reader)
                  ch.alts)
        then acc
        else (
          let rep =
            ctx.shape.rules.(Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader ch.alts.(0))
              .Rule.name
          in
          Error.make
            ~detail:(Error.Nullable_repeated { rule = rep })
            (Error.At_child { production = d.name; child = ch.child_name })
          :: acc)))
;;

(* [Fixpoint] works out nullability from a rule's children. It does not do
   that for an expression block or a separated production. It answers false
   for those two whatever their children look like.

   That answer is right so long as every atom and every separated element
   consumes a token. An expression is then at least one atom or one prefix
   operator, and a separated list is at least one element. Neither can be
   empty.

   This check rejects a nullable atom and a nullable element, so the answer
   stays right.

   Without it the table would say a construct cannot derive empty when it
   can. FIRST and FOLLOW would come out short. [first_follow] would stop
   seeing a slot that holds the construct as skippable, so its test would
   never run there. *)
let nullable_pratt_and_separated (ctx : ctx) (acc : Error.t list) : Error.t list =
  let acc =
    Array.fold_left ctx.shape.blocks ~init:acc ~f:(fun acc (b : Block.def) ->
      Array.fold_left b.atoms ~init:acc ~f:(fun acc k ->
        let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
        if r >= 0 && ctx.fixpoint_tables.nullable.(r)
        then
          Error.make
            ~detail:
              (Error.Nullable_pratt_atom
                 { atom = Grammar.Name.Rule.to_string ctx.shape.rules.(r).Rule.name })
            (Error.At_block b.name)
          :: acc
        else acc))
  in
  List.fold_left (user_rules ctx) ~init:acc ~f:(fun acc (d : Rule.def) ->
    match d.frame with
    | Rule.Separated _ ->
      Array.fold_left d.children ~init:acc ~f:(fun acc (ch : Rule.child) ->
        Array.fold_left ch.alts ~init:acc ~f:(fun acc k ->
          let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
          if r >= 0 && ctx.fixpoint_tables.nullable.(r)
          then
            Error.make
              ~detail:
                (Error.Nullable_separated_element
                   { element = Grammar.Name.Rule.to_string ctx.shape.rules.(r).Rule.name })
              (Error.At_child { production = d.name; child = ch.child_name })
            :: acc
          else acc))
    | _ -> acc)
;;

(* -- a referenced rule with nothing to start it ---------------------------- *)

(* A rule with empty FIRST gives a [can_start] predicate that is always
   false. An optional or repeated reference to it is dropped. A required one
   is unreachable. *)
let empty_first_sets (ctx : ctx) (acc : Error.t list) : Error.t list =
  let sites = Hashtbl.create 16 in
  let note ~from k =
    let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
    if r >= 0
    then
      Hashtbl.replace
        sites
        r
        (from
         ::
         (try Hashtbl.find sites r with
          | Not_found -> []))
  in
  let () =
    List.iter (user_rules ctx) ~f:(fun (d : Rule.def) ->
      Array.iter
        ~f:(fun (ch : Rule.child) ->
          Array.iter
            ~f:
              (note
                 ~from:
                   (Grammar.Name.Rule.to_string d.name
                    ^ "."
                    ^ Grammar.Name.Child.to_string ch.child_name))
            ch.alts)
        d.children)
  in
  let () =
    Array.iter
      ~f:(fun (b : Block.def) ->
        Array.iter ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " atoms")) b.atoms;
        Array.iter
          ~f:(fun (p : Block.postfix) ->
            match p.p_body with
            | Block.Nothing -> ()
            | Block.Then rhs ->
              Array.iter
                ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " postfix rhs"))
                rhs
            | Block.Enclosed { content = Block.One s; _ } ->
              Array.iter
                ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " postfix body"))
                s
            | Block.Enclosed { content = Block.Many { elem; _ }; _ } ->
              Array.iter
                ~f:(note ~from:(Grammar.Name.Rule.to_string b.name ^ " postfix element"))
                elem)
          b.postfix)
      ctx.shape.blocks
  in
  Hashtbl.fold
    (fun r froms acc ->
       let d = ctx.shape.rules.(r) in
       if d.origin <> Rule.User
       then acc
       else if not (Kind.Set.is_empty ctx.fixpoint_tables.first.(r))
       then acc
       else
         Error.make
           ~detail:
             (Error.Empty_first_set
                { referenced_from = List.sort_uniq ~cmp:String.compare froms })
           (Error.At_production d.name)
         :: acc)
    sites
    acc
;;

(* -- operator conflicts that read first ------------------------------------ *)

let pratt_atom_first_first (ctx : ctx) (acc : Error.t list) : Error.t list =
  Array.fold_left ~init:acc ctx.shape.blocks ~f:(fun acc (b : Block.def) ->
    if Array.length b.atoms < 2
    then acc
    else (
      let firsts =
        Array.to_list
          (Array.map ~f:(Fixpoint.Reader.kind_first ctx.fixpoint_reader) b.atoms)
      in
      let rec overlap acc = function
        | [] | [ _ ] -> acc
        | x :: rest ->
          overlap
            (List.fold_left rest ~init:acc ~f:(fun a y ->
               Kind.Set.union a (Kind.Set.inter x y)))
            rest
      in
      let common = overlap Kind.Set.empty firsts in
      if Kind.Set.is_empty common
      then acc
      else
        Error.make
          ~detail:(Error.Pratt_atom_conflict { common = kind_refs ctx common })
          (Error.At_block b.name)
        :: acc))
;;

(* The left-hand-side dispatch reads the prefix table before the atoms. A
   prefix operator token that also appears in FIRST of an atom therefore
   always takes that token, and the atom never gets it. *)
let prefix_atom_conflict (ctx : ctx) (acc : Error.t list) : Error.t list =
  Array.fold_left ctx.shape.blocks ~init:acc ~f:(fun acc (b : Block.def) ->
    Array.fold_left b.prefix ~init:acc ~f:(fun acc (o : Block.op) ->
      Array.fold_left b.atoms ~init:acc ~f:(fun acc k ->
        let r = Fixpoint.Reader.rule_of_kind ctx.fixpoint_reader k in
        if r < 0 || ctx.shape.rules.(r).Rule.origin <> Rule.User
        then acc
        else if Kind.Set.mem ctx.fixpoint_tables.first.(r) o.op_kind
        then
          Error.make
            ~detail:
              (Error.Prefix_atom_conflict
                 { atom = Grammar.Name.Rule.to_string ctx.shape.rules.(r).Rule.name })
            (Error.At_operator
               { block = b.name
               ; token =
                   Grammar.Name.Token.of_string
                     (Kind.Name.to_string (Kind.Table.name ctx.kind_table o.op_kind))
               })
          :: acc
        else acc)))
;;

(* -- token reachability ---------------------------------------------------- *)

(* A token whose case id never heads a state's accepts list wins max-munch
   from no state, so the lexer never emits it. There are two reasons that
   happens:

   1) the regex matches nothing; or
   2) an earlier-declared token beats it wherever it could match.

   For the second, the diagnostic names one token seen beating it, at one
   such state. *)
let token_reachability (ctx : ctx) (dfa : Redfa.Dfa.t) (acc : Error.t list) : Error.t list
  =
  let toks = ctx.shape.names.tokens in
  let n = Array.length toks in
  if n = 0
  then acc
  else (
    let head_seen = Array.make n false in
    let subsumer = Array.make n (-1) in
    Redfa.Dfa.iter_states dfa (fun st ->
      match Redfa.Dfa.accepts dfa st with
      | [] -> ()
      | head :: rest ->
        head_seen.(head) <- true;
        List.iter rest ~f:(fun cid ->
          if subsumer.(cid) < 0 || head < subsumer.(cid) then subsumer.(cid) <- head));
    let acc = ref acc in
    for i = n - 1 downto 0 do
      if not head_seen.(i)
      then (
        let t = toks.(i) in
        let reason : Error.token_unreachable_reason =
          if Redfa.Regex.is_empty_language t.regex
          then Error.Empty_language
          else if subsumer.(i) >= 0
          then Error.Subsumed_by toks.(subsumer.(i)).Token.name
          else Error.Empty_language
        in
        acc
        := Error.make ~detail:(Error.Token_unreachable { reason }) (Error.At_token t.name)
           :: !acc)
    done;
    !acc)
;;

let run (shape : Stage.shape) (fixpoint_tables : Fixpoint.tables) (dfa : Redfa.Dfa.t)
  : Error.t list
  =
  let fixpoint_reader =
    Fixpoint.Reader.of_tables
      ~rules:shape.rules
      ~blocks:shape.blocks
      ~kind_rule:shape.names.kind_rule
      fixpoint_tables
  in
  let ctx = { shape; fixpoint_tables; fixpoint_reader; kind_table = shape.names.kinds } in
  first_first ctx []
  |> first_follow ctx
  |> left_recursion ctx
  |> nullable_repeated ctx
  |> nullable_pratt_and_separated ctx
  |> empty_first_sets ctx
  |> pratt_atom_first_first ctx
  |> prefix_atom_conflict ctx
  |> token_reachability ctx dfa
;;
