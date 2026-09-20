open StdLabels

(* -- the text form --------------------------------------------------------- *)

let atom s = Sexp.Atom s
let num n = Sexp.Atom (string_of_int n)
let keyed name xs = Sexp.List (atom name :: xs)

(* A list of numbers. Whether they are kinds or indices into the rules is the
   checker's business; the printer writes them in the order it is given. *)
let ints name xs = keyed name (Array.to_list (Array.map xs ~f:num))

let kopt (name : string) : int option -> Sexp.t = function
  | None -> keyed name []
  | Some k -> keyed name [ num k ]
;;

let flag name b = keyed name [ atom (if b then "true" else "false") ]
let msg m = keyed "message" [ num (Ir.Message.to_int m) ]

let rec sexp_of_instr (i : Ir.Plan.instr) : Sexp.t =
  match i with
  | Close -> atom "close"
  | Trivia -> atom "trivia"
  | Bump -> atom "bump"
  | Drain m -> keyed "drain" [ msg m ]
  | Open k -> keyed "open" [ num k ]
  | Call r -> keyed "call" [ num r ]
  | Seq xs -> keyed "seq" (List.map (Array.to_list xs) ~f:sexp_of_instr)
  | Pratt b -> keyed "pratt" [ num b.block; num b.min_bp ]
  | Expect e ->
    keyed
      "expect"
      [ num e.tok
      ; msg e.message
      ; keyed
          "at-child"
          (match e.at_child with
           | None -> []
           | Some s -> [ atom s ])
      ; kopt "hole" e.hole
      ; kopt "placeholder" e.placeholder
      ]
  | Alt a ->
    keyed
      "alt"
      (List.map (Array.to_list a.arms) ~f:(fun (on, body) ->
         Sexp.List [ ints "on" on; sexp_of_instr body ]))
  | Commit c ->
    keyed
      "commit"
      [ ints "first" c.first
      ; ints "recover" c.recover
      ; keyed "at-child" [ atom c.at_child ]
      ; msg c.message
      ; kopt "hole" c.hole
      ; keyed "placeholder" [ num c.placeholder ]
      ; (match c.resume with
         | None -> keyed "resume" []
         | Some r -> ints "resume" r)
      ; sexp_of_instr c.body
      ]
  | Loop l ->
    keyed
      "loop"
      (num l.entry
       :: (match l.ends_on with
           | None -> keyed "ends-on" []
           | Some ks -> keyed "ends-on" [ ints "kinds" ks ])
       :: List.map (Array.to_list l.states) ~f:(fun (s : Ir.Plan.loop_state) ->
         keyed
           "state"
           [ keyed
               "accepts"
               (List.map (Array.to_list s.accepts) ~f:(fun (on, target) ->
                  Sexp.List [ ints "on" on; num target ]))
           ; keyed
               "exit"
               (match s.exit with
                | Ir.Plan.May_exit -> [ atom "may" ]
                | Ir.Plan.May_exit_reporting m -> [ atom "may"; msg m ])
           ; (match s.when_missing with
              | None -> keyed "missing" []
              | Some m -> keyed "missing" [ num m.tok; msg m.message; num m.goto ])
           ; sexp_of_instr s.emits
           ]))
;;

let sexp_of_postfix (q : Ir.Plan.postfix) : Sexp.t =
  keyed "postfix" [ num q.lead; num q.bp; num q.kind; sexp_of_instr q.body ]
;;

let sexp_of_block (b : Ir.Plan.block) : Sexp.t =
  keyed
    "block"
    [ atom b.name
    ; keyed "base-kind" [ num b.base_kind ]
    ; kopt "prefix-kind" b.prefix_kind
    ; kopt "infix-kind" b.infix_kind
    ; keyed "hole-kind" [ num b.hole_kind ]
    ; ints "expected" b.expected
    ; msg b.message
    ; keyed
        "infix"
        (List.map (Array.to_list b.infix) ~f:(fun (t, (l, r)) ->
           Sexp.List [ num t; num l; num r ]))
    ; keyed
        "prefix"
        (List.map (Array.to_list b.prefix) ~f:(fun (t, bp) -> Sexp.List [ num t; num bp ]))
    ; keyed "postfix" (List.map (Array.to_list b.postfix) ~f:sexp_of_postfix)
    ; keyed
        "atoms"
        (List.map (Array.to_list b.atoms) ~f:(fun (on, a) ->
           Sexp.List
             [ ints "on" on
             ; (match a with
                | Ir.Plan.Atom_token -> atom "token"
                | Ir.Plan.Atom_rule r -> keyed "rule" [ num r ])
             ]))
    ]
;;

let sexp_of_rule (r : Ir.Plan.rule) : Sexp.t =
  keyed
    "rule"
    [ atom r.name
    ; keyed "kind" [ num r.kind ]
    ; ints "first" r.first
    ; ints "adds" r.adds
    ; flag "boundary" r.boundary
    ; sexp_of_instr r.body
    ]
;;

let sexp_of_plan (p : Ir.Plan.t) : Sexp.t =
  keyed
    "plan"
    [ keyed "error-kind" [ num p.error_kind ]
    ; keyed "missing-kind" [ num p.missing_kind ]
    ; ints "trivia" p.trivia
    ; keyed
        "pairs"
        (List.map (Array.to_list p.pairs) ~f:(fun (o, c) -> Sexp.List [ num o; num c ]))
    ; ints "roots" p.roots
    ; keyed "rules" (List.map (Array.to_list p.rules) ~f:sexp_of_rule)
    ; keyed "blocks" (List.map (Array.to_list p.blocks) ~f:sexp_of_block)
    ]
;;

let pp fmt (p : Ir.Plan.t) = Sexp.pp fmt (sexp_of_plan p)

exception Bad of string

let bad fmt = Format.kasprintf (fun m -> raise (Bad m)) fmt
let show = Sexp.to_string

let as_int (s : Sexp.t) =
  match s with
  | Sexp.Atom a ->
    (match int_of_string_opt a with
     | Some n -> n
     | None -> bad "%S is not a number" a)
  | Sexp.List _ -> bad "a list where a number was wanted: %s" (show s)
;;

let as_atom (s : Sexp.t) =
  match s with
  | Sexp.Atom a -> a
  | Sexp.List _ -> bad "a list where an atom was wanted: %s" (show s)
;;

(* The tail of [(name ...)]. *)
let key (name : string) (s : Sexp.t) : Sexp.t list =
  match s with
  | Sexp.List (Sexp.Atom k :: rest) when String.equal k name -> rest
  | _ -> bad "wanted (%s ...), got %s" name (show s)
;;

let one (name : string) (s : Sexp.t) : Sexp.t =
  match key name s with
  | [ x ] -> x
  | _ -> bad "(%s ...) takes one value: %s" name (show s)
;;

let ints_of name s = Array.of_list (List.map (key name s) ~f:as_int)

let kopt_of (name : string) (s : Sexp.t) : int option =
  match key name s with
  | [] -> None
  | [ x ] -> Some (as_int x)
  | _ -> bad "(%s ...) takes at most one kind: %s" name (show s)
;;

let flag_of (name : string) (s : Sexp.t) : bool =
  match as_atom (one name s) with
  | "true" -> true
  | "false" -> false
  | a -> bad "(%s ...) wanted true or false, got %S" name a
;;

let msg_of s = Ir.Message.of_int (as_int (one "message" s))

let rec instr_of_sexp (s : Sexp.t) : Ir.Plan.instr =
  match s with
  | Sexp.Atom "close" -> Close
  | Sexp.Atom "trivia" -> Trivia
  | Sexp.Atom "bump" -> Bump
  | Sexp.List (Sexp.Atom "seq" :: xs) ->
    Seq (Array.of_list (List.map xs ~f:instr_of_sexp))
  | Sexp.List [ Sexp.Atom "drain"; m ] -> Drain (msg_of m)
  | Sexp.List [ Sexp.Atom "open"; k ] -> Open (as_int k)
  | Sexp.List [ Sexp.Atom "call"; r ] -> Call (as_int r)
  | Sexp.List [ Sexp.Atom "pratt"; b; bp ] ->
    Pratt { block = as_int b; min_bp = as_int bp }
  | Sexp.List [ Sexp.Atom "expect"; tok; m; ac; hole; ph ] ->
    Expect
      { tok = as_int tok
      ; message = msg_of m
      ; at_child =
          (match key "at-child" ac with
           | [] -> None
           | [ x ] -> Some (as_atom x)
           | _ -> bad "(at-child ...) takes at most one name: %s" (show ac))
      ; hole = kopt_of "hole" hole
      ; placeholder = kopt_of "placeholder" ph
      }
  | Sexp.List (Sexp.Atom "alt" :: arms) ->
    Alt
      { arms =
          Array.of_list
            (List.map arms ~f:(fun arm ->
               match arm with
               | Sexp.List [ on; body ] -> ints_of "on" on, instr_of_sexp body
               | _ -> bad "an alt arm is (on body): %s" (show arm)))
      }
  | Sexp.List [ Sexp.Atom "commit"; first; recover; ac; m; hole; ph; resume; body ] ->
    Commit
      { first = ints_of "first" first
      ; recover = ints_of "recover" recover
      ; at_child = as_atom (one "at-child" ac)
      ; message = msg_of m
      ; hole = kopt_of "hole" hole
      ; placeholder = as_int (one "placeholder" ph)
      ; resume =
          (match key "resume" resume with
           | [] -> None
           | ks -> Some (Array.of_list (List.map ks ~f:as_int)))
      ; body = instr_of_sexp body
      }
  | Sexp.List (Sexp.Atom "loop" :: entry :: ends :: states) ->
    Loop
      { entry = as_int entry
      ; ends_on =
          (match key "ends-on" ends with
           | [] -> None
           | [ ks ] -> Some (ints_of "kinds" ks)
           | _ -> bad "(ends-on ...) takes at most one set: %s" (show ends))
      ; states =
          Array.of_list
            (List.map states ~f:(fun st ->
               match key "state" st with
               | [ accepts; exit_; missing; emits ] ->
                 { Ir.Plan.accepts =
                     Array.of_list
                       (List.map (key "accepts" accepts) ~f:(fun a ->
                          match a with
                          | Sexp.List [ on; target ] -> ints_of "on" on, as_int target
                          | _ -> bad "an accepts arm is (on state): %s" (show a)))
                 ; exit =
                     (match key "exit" exit_ with
                      | [ Sexp.Atom "may" ] -> Ir.Plan.May_exit
                      | [ Sexp.Atom "may"; m ] -> Ir.Plan.May_exit_reporting (msg_of m)
                      | _ -> bad "an exit is may, or may with a message: %s" (show exit_))
                 ; when_missing =
                     (match key "missing" missing with
                      | [] -> None
                      | [ tok; m; goto ] ->
                        Some
                          { Ir.Plan.tok = as_int tok
                          ; message = msg_of m
                          ; goto = as_int goto
                          }
                      | _ ->
                        bad "a missing is (missing tok message goto): %s" (show missing))
                 ; emits = instr_of_sexp emits
                 }
               | _ ->
                 bad "a loop state is (state accepts exit missing emits): %s" (show st)))
      }
  | _ -> bad "not an instruction: %s" (show s)
;;

let postfix_of_sexp (s : Sexp.t) : Ir.Plan.postfix =
  match key "postfix" s with
  | [ lead; bp; kind; body ] ->
    { lead = as_int lead; bp = as_int bp; kind = as_int kind; body = instr_of_sexp body }
  | _ -> bad "a postfix is (postfix lead bp kind body): %s" (show s)
;;

let block_of_sexp (s : Sexp.t) : Ir.Plan.block =
  match key "block" s with
  | [ name; base; pre_k; in_k; hole; expected; m; infix; prefix; postfix; atoms ] ->
    { name = as_atom name
    ; base_kind = as_int (one "base-kind" base)
    ; prefix_kind = kopt_of "prefix-kind" pre_k
    ; infix_kind = kopt_of "infix-kind" in_k
    ; hole_kind = as_int (one "hole-kind" hole)
    ; expected = ints_of "expected" expected
    ; message = msg_of m
    ; infix =
        Array.of_list
          (List.map (key "infix" infix) ~f:(fun e ->
             match e with
             | Sexp.List [ t; l; r ] -> as_int t, (as_int l, as_int r)
             | _ -> bad "an infix entry is (token left right): %s" (show e)))
    ; prefix =
        Array.of_list
          (List.map (key "prefix" prefix) ~f:(fun e ->
             match e with
             | Sexp.List [ t; bp ] -> as_int t, as_int bp
             | _ -> bad "a prefix entry is (token bp): %s" (show e)))
    ; postfix = Array.of_list (List.map (key "postfix" postfix) ~f:postfix_of_sexp)
    ; atoms =
        Array.of_list
          (List.map (key "atoms" atoms) ~f:(fun e ->
             match e with
             | Sexp.List [ on; Sexp.Atom "token" ] -> ints_of "on" on, Ir.Plan.Atom_token
             | Sexp.List [ on; r ] ->
               ints_of "on" on, Ir.Plan.Atom_rule (as_int (one "rule" r))
             | _ -> bad "an atom is (on token) or (on (rule n)): %s" (show e)))
    }
  | _ -> bad "a block has eleven fields: %s" (show s)
;;

let rule_of_sexp (s : Sexp.t) : Ir.Plan.rule =
  match key "rule" s with
  | [ name; kind; first; adds; boundary; body ] ->
    { name = as_atom name
    ; kind = as_int (one "kind" kind)
    ; first = ints_of "first" first
    ; adds = ints_of "adds" adds
    ; boundary = flag_of "boundary" boundary
    ; body = instr_of_sexp body
    }
  | _ -> bad "a rule has six fields: %s" (show s)
;;

let plan_of_sexp (s : Sexp.t) : Ir.Plan.t =
  match key "plan" s with
  | [ err; missing; trivia; pairs; roots; rules; blocks ] ->
    { error_kind = as_int (one "error-kind" err)
    ; missing_kind = as_int (one "missing-kind" missing)
    ; trivia = ints_of "trivia" trivia
    ; pairs =
        Array.of_list
          (List.map (key "pairs" pairs) ~f:(fun e ->
             match e with
             | Sexp.List [ o; c ] -> as_int o, as_int c
             | _ -> bad "a pair is (open close): %s" (show e)))
    ; roots = ints_of "roots" roots
    ; rules = Array.of_list (List.map (key "rules" rules) ~f:rule_of_sexp)
    ; blocks = Array.of_list (List.map (key "blocks" blocks) ~f:block_of_sexp)
    }
  | _ -> bad "a plan has seven fields: %s" (show s)
;;

let parse (s : string) : (Ir.Plan.t, string) result =
  match Sexp.of_string s with
  | Error m -> Error m
  | Ok x ->
    (try Ok (plan_of_sexp x) with
     | Bad m -> Error m)
;;
