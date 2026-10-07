open StdLabels

type problem =
  | Rule_out_of_range of
      { at : string
      ; id : int
      }
  | Block_out_of_range of
      { at : string
      ; id : int
      }
  | Kinds_unordered of
      { at : string
      ; kinds : Ir.Kind.t list
      }
  | Negative_kind of
      { at : string
      ; kind : Ir.Kind.t
      }
  | Empty_alt of { at : string }
  | Empty_arm of { at : string }
  | Kind_taken_twice of
      { at : string
      ; kind : Ir.Kind.t
      }
  | Loop_state_out_of_range of
      { at : string
      ; state : int
      }
  | Pairs_unordered of { at : string }
  | Empty_resume of { at : string }
  | Unbalanced of
      { at : string
      ; depth : int
      }
  | Close_without_open of { at : string }
  | Commit_matches_nothing of { at : string }
  | Takes_what_follows of
      { at : string
      ; kinds : Ir.Kind.t list
      }

let pp_problem (fmt : Format.formatter) : problem -> unit = function
  | Rule_out_of_range { at; id } -> Format.fprintf fmt "%s: no rule %d" at id
  | Block_out_of_range { at; id } -> Format.fprintf fmt "%s: no block %d" at id
  | Kinds_unordered { at; kinds } ->
    Format.fprintf
      fmt
      "@[<h>%s: the kinds are not ascending and distinct: %a@]"
      at
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         Format.pp_print_int)
      kinds
  | Negative_kind { at; kind } -> Format.fprintf fmt "%s: the kind %d is negative" at kind
  | Empty_alt { at } -> Format.fprintf fmt "%s: an alt with no arms" at
  | Empty_arm { at } -> Format.fprintf fmt "%s: an arm no kind can take" at
  | Kind_taken_twice { at; kind } ->
    Format.fprintf fmt "%s: an earlier arm of this dispatch takes the kind %d" at kind
  | Loop_state_out_of_range { at; state } ->
    Format.fprintf fmt "%s: no loop state %d" at state
  | Empty_resume { at } -> Format.fprintf fmt "%s: an empty resume set; write none" at
  | Unbalanced { at; depth } ->
    Format.fprintf fmt "%s: open and close leave a depth of %d" at depth
  | Close_without_open { at } -> Format.fprintf fmt "%s: a close with nothing open" at
  | Commit_matches_nothing { at } ->
    Format.fprintf fmt "%s: a required child that can be empty" at
  | Takes_what_follows { at; kinds } ->
    Format.fprintf
      fmt
      "@[<h>%s: %a can start what is optional here, and can also come after it@]"
      at
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         Format.pp_print_int)
      kinds
  | Pairs_unordered { at } ->
    Format.fprintf fmt "%s: the delimiter pairs are not ascending and distinct" at
;;

(* -- check ----------------------------------------------------------------- *)

(* A path names the rule or block, then the way in. A finding in a nested arm
   is otherwise reported against the whole rule, which on a rule with four
   alternatives is four places to look. *)
let sub at step = at ^ "/" ^ step

(* Whether [i] can finish without taking a token, given which rules can.
   [under] is what the cursor may be on where [i] starts, and [None] is any
   kind. A commit enters its body only on a kind in its [first], and the
   lowering writes a required choice as an [Alt] whose arms take every one of
   them. That [Alt]'s [otherwise] takes nothing, and it never runs.

   [Ir.Residual] works out nullability too, but it indexes rules, blocks and
   loop states as it finds them and raises on one that is not there. A plan
   that reaches here may be that broken, and the other checks report it. So an
   index out of range reads as taking a token here.

   A commit reads as taking a token. One whose body can take nothing is
   reported where it sits, and reading it the same way here keeps that to one
   report. *)
let rec nullable (null : bool array) (under : Ir.Kind.t list option) (i : Ir.Plan.instr)
  : bool
  =
  (* The part of [under] a dispatch on [on] takes, and the part it leaves. *)
  let split (on : Ir.Kind.t list) : Ir.Kind.t list option * Ir.Kind.t list option =
    match under with
    | None -> Some on, None
    | Some kinds ->
      let taken, left = List.partition kinds ~f:(fun k -> List.mem k ~set:on) in
      Some taken, Some left
  in
  let reachable : Ir.Kind.t list option -> bool = function
    | None -> true
    | Some kinds -> kinds <> []
  in
  match i with
  | Seq xs -> Array.for_all xs ~f:(nullable null under)
  | Open _ | Close | Trivia | Drain _ -> true
  | Bump | Bump_reporting _ | Expect _ | Pratt _ | Commit _ -> false
  | Call r -> r >= 0 && r < Array.length null && null.(r)
  | Alt a ->
    let all_on =
      List.concat_map (Array.to_list a.arms) ~f:(fun (on, _) -> Array.to_list on)
    in
    let _, left = split all_on in
    Array.exists a.arms ~f:(fun (on, body) ->
      let taken, _ = split (Array.to_list on) in
      reachable taken && nullable null taken body)
    || (reachable left && nullable null left a.otherwise)
  | Loop l ->
    l.entry >= 0
    && l.entry < Array.length l.states
    &&
    let state = l.states.(l.entry) in
    let accepted =
      List.concat_map (Array.to_list state.accepts) ~f:(fun (on, _) -> Array.to_list on)
    in
    let _, left = split accepted in
    reachable left
    &&
      (match state.exit with
      | May_exit -> true
      | May_exit_reporting _ -> false)
;;

(* Reading a [Call] needs the result for the rule it names, and a grammar may
   be recursive, so this is a fixpoint. A rule only ever goes from false to
   true, so the loop settles. *)
let nullable_rules (p : Ir.Plan.t) : bool array =
  let null = Array.make (Array.length p.rules) false in
  let settled = ref false in
  while not !settled do
    settled := true;
    Array.iteri p.rules ~f:(fun ri (r : Ir.Plan.rule) ->
      if (not null.(ri)) && nullable null None r.body
      then (
        null.(ri) <- true;
        settled := false))
  done;
  null
;;

(* -- what may follow --------------------------------------------------------- *)

let union (a : Ir.Kind.t list) (b : Ir.Kind.t list) : Ir.Kind.t list =
  List.sort_uniq ~cmp:Int.compare (a @ b)
;;

let inter (a : Ir.Kind.t list) (b : Ir.Kind.t list) : Ir.Kind.t list =
  List.sort_uniq ~cmp:Int.compare (List.filter a ~f:(fun k -> List.mem k ~set:b))
;;

let kinds_on (dispatch : (Ir.Kind.t array * 'a) array) : Ir.Kind.t list =
  List.concat_map (Array.to_list dispatch) ~f:(fun (on, _) -> Array.to_list on)
;;

(* The kinds [i] can start with, read as [nullable] reads it. A bump names no
   kind. It takes what the dispatch above it let through, so under [None] it
   adds nothing. *)
let rec first
          (p : Ir.Plan.t)
          (null : bool array)
          (under : Ir.Kind.t list option)
          (i : Ir.Plan.instr)
  : Ir.Kind.t list
  =
  match i with
  | Seq xs ->
    let acc = ref []
    and going = ref true in
    Array.iter xs ~f:(fun x ->
      if !going
      then (
        acc := union !acc (first p null under x);
        going := nullable null under x));
    !acc
  | Open _ | Close | Trivia | Drain _ -> []
  | Bump | Bump_reporting _ -> Option.value under ~default:[]
  | Expect e -> [ e.tok ]
  | Call r ->
    if r >= 0 && r < Array.length p.rules then Array.to_list p.rules.(r).first else []
  | Pratt b ->
    if b.block >= 0 && b.block < Array.length p.blocks
    then (
      let block = p.blocks.(b.block) in
      union (kinds_on block.atoms) (List.map (Array.to_list block.prefix) ~f:fst))
    else []
  | Commit c -> Array.to_list c.first
  | Alt a ->
    let on = kinds_on a.arms in
    let left =
      match under with
      | None -> None
      | Some kinds -> Some (List.filter kinds ~f:(fun k -> not (List.mem k ~set:on)))
    in
    union on (first p null left a.otherwise)
  | Loop l ->
    if l.entry >= 0 && l.entry < Array.length l.states
    then kinds_on l.states.(l.entry).accepts
    else []
;;

(* What can come straight after an expression's atom, besides what follows the
   expression: an infix operator, or a postfix one. *)
let operators (b : Ir.Plan.block) : Ir.Kind.t list =
  union
    (List.map (Array.to_list b.infix) ~f:fst)
    (List.map (Array.to_list b.postfix) ~f:(fun (q : Ir.Plan.postfix) -> q.lead))
;;

(* A walk that carries what can come after each instruction, and gives each
   choice the plan makes without taking a token to [choice].

   There are two such choices. An [Alt] runs [otherwise] where no arm holds
   the kind under the cursor, and an [otherwise] that takes nothing moves on.
   A loop state that may exit ends the loop where nothing it accepts is under
   the cursor. Either way, a kind the choice takes must not also be one that
   can come after it. The parse would take it, and the rest of the rule would
   never see it.

   [rule_follow] and [block_follow] grow as the walk meets each [Call] and
   [Pratt]. A rule called as an atom is followed by what follows its block,
   and by the block's operators. *)
let follow_walk
      (p : Ir.Plan.t)
      (null : bool array)
      ~(rule_follow : Ir.Kind.t list array)
      ~(block_follow : Ir.Kind.t list array)
      ~(grew : bool ref)
      ~(choice : string -> Ir.Kind.t list -> unit)
  : unit
  =
  let add (sets : Ir.Kind.t list array) (index : int) (kinds : Ir.Kind.t list) =
    if index >= 0 && index < Array.length sets
    then (
      let next = union sets.(index) kinds in
      if List.length next > List.length sets.(index)
      then (
        sets.(index) <- next;
        grew := true))
  in
  let rec visit at (under : Ir.Kind.t list option) (follow : Ir.Kind.t list) i =
    match (i : Ir.Plan.instr) with
    | Seq xs ->
      let n = Array.length xs in
      (* [after.(k)] is what can come after [xs.(k - 1)]. *)
      let after = Array.make (n + 1) follow in
      for k = n - 1 downto 0 do
        after.(k)
        <- (if nullable null None xs.(k)
            then union (first p null None xs.(k)) after.(k + 1)
            else first p null None xs.(k))
      done;
      let under = ref under in
      Array.iteri xs ~f:(fun k x ->
        visit (sub at (Printf.sprintf "%d" k)) !under after.(k + 1) x;
        if not (nullable null !under x) then under := None)
    | Open _ | Close | Trivia | Bump | Bump_reporting _ | Drain _ | Expect _ -> ()
    | Call r -> add rule_follow r follow
    | Pratt b -> add block_follow b.block follow
    | Commit c -> visit (sub at "body") (Some (Array.to_list c.first)) follow c.body
    | Alt a ->
      let on = kinds_on a.arms in
      let left =
        match under with
        | None -> None
        | Some kinds -> Some (List.filter kinds ~f:(fun k -> not (List.mem k ~set:on)))
      in
      let reachable =
        match left with
        | None -> true
        | Some kinds -> kinds <> []
      in
      if reachable && nullable null left a.otherwise
      then (
        let taken =
          match under with
          | None -> on
          | Some kinds -> inter on kinds
        in
        choice at (inter taken follow));
      Array.iteri a.arms ~f:(fun n (on, body) ->
        visit (sub at (Printf.sprintf "arm %d" n)) (Some (Array.to_list on)) follow body);
      visit (sub at "otherwise") left follow a.otherwise
    | Loop l ->
      let n = Array.length l.states in
      let exits (s : Ir.Plan.loop_state) : bool =
        match s.exit with
        | May_exit -> true
        | May_exit_reporting _ -> false
      in
      (* What can come once the loop is in [state]: something it accepts, or,
         where it may end there, what follows the loop. *)
      let at_state (state : int) : Ir.Kind.t list =
        if state < 0 || state >= n
        then []
        else (
          let s = l.states.(state) in
          if exits s then union (kinds_on s.accepts) follow else kinds_on s.accepts)
      in
      Array.iteri l.states ~f:(fun si (s : Ir.Plan.loop_state) ->
        let at = sub at (Printf.sprintf "state %d" si) in
        if exits s then choice at (inter (kinds_on s.accepts) follow);
        let after =
          Array.fold_left s.accepts ~init:[] ~f:(fun acc (_, target) ->
            union acc (at_state target))
        in
        visit (sub at "emits") (Some (kinds_on s.accepts)) after s.emits)
  in
  Array.iteri p.rules ~f:(fun ri (r : Ir.Plan.rule) ->
    visit (Printf.sprintf "rule %d %s" ri r.name) None rule_follow.(ri) r.body);
  Array.iteri p.blocks ~f:(fun bi (b : Ir.Plan.block) ->
    let after_atom = union block_follow.(bi) (operators b) in
    Array.iter b.atoms ~f:(fun (_, atom) ->
      match (atom : Ir.Plan.atom) with
      | Atom_token -> ()
      | Atom_rule r -> add rule_follow r after_atom);
    Array.iteri b.postfix ~f:(fun qi (q : Ir.Plan.postfix) ->
      visit (Printf.sprintf "block %d/postfix %d" bi qi) None after_atom q.body))
;;

(* Every overlap between what a choice takes and what can follow it. FOLLOW
   comes from a fixpoint over the walk: a set only grows, so the walk settles,
   and the last run, with nothing left to grow, is the one that reports. *)
let overlaps (p : Ir.Plan.t) (null : bool array) : problem list =
  let rule_follow = Array.make (Array.length p.rules) []
  and block_follow = Array.make (Array.length p.blocks) [] in
  let grew = ref true in
  while !grew do
    grew := false;
    follow_walk p null ~rule_follow ~block_follow ~grew ~choice:(fun _ _ -> ())
  done;
  let found = ref [] in
  follow_walk p null ~rule_follow ~block_follow ~grew ~choice:(fun at kinds ->
    if kinds <> [] then found := Takes_what_follows { at; kinds } :: !found);
  List.rev !found
;;

let run ?(template : bool = false) (p : Ir.Plan.t) : (unit, problem list) result =
  let found = ref [] in
  let report x = found := x :: !found in
  let n_rules = Array.length p.rules in
  let n_blocks = Array.length p.blocks in
  let null = nullable_rules p in
  let kinds at (ks : Ir.Kind.t array) =
    Array.iter ks ~f:(fun k -> if k < 0 then report (Negative_kind { at; kind = k }));
    let ordered = ref true in
    Array.iteri ks ~f:(fun i k -> if i > 0 && k <= ks.(i - 1) then ordered := false);
    if not !ordered then report (Kinds_unordered { at; kinds = Array.to_list ks })
  in
  let kind at k = if k < 0 then report (Negative_kind { at; kind = k }) in
  (* Every dispatch in a plan is an ordered cascade. The entry that runs is the
     first one whose set holds the kind under the cursor. So a kind an earlier
     entry already takes never reaches a later one, and the later entry is dead
     code on that kind.

     Call [taker ()] once per cascade. Call what it gives once per entry, in
     the order the entries are written. *)
  let taker () =
    let seen = Hashtbl.create 16 in
    fun at (ks : Ir.Kind.t array) ->
      Array.iter ks ~f:(fun k ->
        if Hashtbl.mem seen k
        then report (Kind_taken_twice { at; kind = k })
        else Hashtbl.add seen k ())
  in
  let opt_kind at = function
    | None -> ()
    | Some k -> kind at k
  in
  (* Answers the depth this instruction leaves behind, and the lowest depth it
     passed through. A branch is walked on its own: it has to come back to
     zero, so the tree's shape does not depend on which one ran, and it has to
     stay above zero, or it closes a node the branch never opened. Counting
     alone misses the second, since a close and an open in that order come to
     nothing. *)
  let rec walk at (i : Ir.Plan.instr) : int * int =
    match i with
    | Seq xs ->
      let net = ref 0
      and low = ref 0 in
      Array.iteri xs ~f:(fun n x ->
        let d, l = walk (sub at (Printf.sprintf "%d" n)) x in
        low := min !low (!net + l);
        net := !net + d);
      !net, !low
    | Open k ->
      kind at k;
      1, 0
    | Close -> -1, -1
    | Trivia | Bump | Bump_reporting _ | Drain _ -> 0, 0
    | Expect e ->
      kind at e.tok;
      opt_kind at e.hole;
      opt_kind at e.placeholder;
      0, 0
    | Call r ->
      if r < 0 || r >= n_rules then report (Rule_out_of_range { at; id = r });
      0, 0
    | Pratt b ->
      if b.block < 0 || b.block >= n_blocks
      then report (Block_out_of_range { at; id = b.block });
      0, 0
    | Alt a ->
      if Array.length a.arms = 0 then report (Empty_alt { at });
      let takes = taker () in
      Array.iteri a.arms ~f:(fun n (on, body) ->
        let at = sub at (Printf.sprintf "arm %d" n) in
        if Array.length on = 0 then report (Empty_arm { at });
        kinds at on;
        takes at on;
        balanced at body);
      balanced (sub at "otherwise") a.otherwise;
      0, 0
    | Commit c ->
      kinds (sub at "first") c.first;
      kinds (sub at "recover") c.recover;
      opt_kind at c.hole;
      kind at c.placeholder;
      (match c.resume with
       | Some r when Array.length r = 0 -> report (Empty_resume { at })
       | Some r -> kinds (sub at "resume") r
       | None -> ());
      if nullable null (Some (Array.to_list c.first)) c.body
      then report (Commit_matches_nothing { at });
      balanced (sub at "body") c.body;
      0, 0
    | Loop l ->
      let n = Array.length l.states in
      if l.entry < 0 || l.entry >= n
      then report (Loop_state_out_of_range { at; state = l.entry });
      Option.iter (fun ks -> kinds (sub at "ends-on") ks) l.ends_on;
      Array.iteri l.states ~f:(fun si (s : Ir.Plan.loop_state) ->
        let at = sub at (Printf.sprintf "state %d" si) in
        let takes = taker () in
        Array.iteri s.accepts ~f:(fun ai (on, target) ->
          let at = sub at (Printf.sprintf "accepts %d" ai) in
          if Array.length on = 0 then report (Empty_arm { at });
          kinds at on;
          takes at on;
          if target < 0 || target >= n
          then report (Loop_state_out_of_range { at; state = target }));
        Option.iter
          (fun (m : Ir.Plan.missing) ->
             kind (sub at "missing") m.tok;
             if m.goto < 0 || m.goto >= n
             then report (Loop_state_out_of_range { at; state = m.goto }))
          s.when_missing;
        balanced (sub at "emits") s.emits);
      0, 0
  and balanced at i =
    let net, low = walk at i in
    if low < 0 then report (Close_without_open { at });
    if net <> 0 then report (Unbalanced { at; depth = net })
  in
  Array.iteri p.rules ~f:(fun ri (r : Ir.Plan.rule) ->
    let at = Printf.sprintf "rule %d %s" ri r.name in
    kind at r.kind;
    kinds (sub at "first") r.first;
    kinds (sub at "adds") r.adds;
    balanced at r.body);
  Array.iteri p.blocks ~f:(fun bi (b : Ir.Plan.block) ->
    let at = Printf.sprintf "block %d" bi in
    kind at b.base_kind;
    opt_kind at b.prefix_kind;
    opt_kind at b.infix_kind;
    kind at b.hole_kind;
    kinds (sub at "expected") b.expected;
    (* Four dispatches, and a token can sit in more than one of them: [-] is
       normally both a prefix and an infix operator. So each table gets its
       own cascade rather than the four sharing one. *)
    let takes_infix = taker ()
    and takes_prefix = taker ()
    and takes_atom = taker ()
    and takes_postfix = taker () in
    Array.iteri b.infix ~f:(fun i (t, _) ->
      let at = sub at (Printf.sprintf "infix %d" i) in
      kind at t;
      takes_infix at [| t |]);
    Array.iteri b.prefix ~f:(fun i (t, _) ->
      let at = sub at (Printf.sprintf "prefix %d" i) in
      kind at t;
      takes_prefix at [| t |]);
    Array.iteri b.atoms ~f:(fun ai (on, atom) ->
      let at = sub at (Printf.sprintf "atom %d" ai) in
      if Array.length on = 0 then report (Empty_arm { at });
      kinds at on;
      takes_atom at on;
      match atom with
      | Ir.Plan.Atom_token -> ()
      | Ir.Plan.Atom_rule r ->
        if r < 0 || r >= n_rules then report (Rule_out_of_range { at; id = r }));
    Array.iteri b.postfix ~f:(fun pi (q : Ir.Plan.postfix) ->
      let at = sub at (Printf.sprintf "postfix %d" pi) in
      kind at q.lead;
      kind at q.kind;
      takes_postfix at [| q.lead |];
      balanced at q.body));
  Array.iteri p.roots ~f:(fun i r ->
    if r < 0 || r >= n_rules
    then report (Rule_out_of_range { at = Printf.sprintf "root %d" i; id = r }));
  (* A balanced skip reads the pairs as a table and the printer writes them in
     the order it finds them, so their order is part of the plan rather than
     an accident of how it was built. *)
  Array.iteri p.pairs ~f:(fun i (o, c) ->
    let at = Printf.sprintf "pairs %d" i in
    kind at o;
    kind at c;
    if i > 0 && compare (o, c) p.pairs.(i - 1) <= 0 then report (Pairs_unordered { at }));
  kinds "trivia" p.trivia;
  kind "error kind" p.error_kind;
  kind "missing kind" p.missing_kind;
  if not template then List.iter (overlaps p null) ~f:report;
  match List.rev !found with
  | [] -> Ok ()
  | problems -> Error problems
;;
