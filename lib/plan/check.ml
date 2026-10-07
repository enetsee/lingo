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

let run (p : Ir.Plan.t) : (unit, problem list) result =
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
  match List.rev !found with
  | [] -> Ok ()
  | problems -> Error problems
;;
