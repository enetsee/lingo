open StdLabels

type t =
  { kinds : Kind.Table.t
  ; tokens : Token.def array
  ; rules : Rule.def array
  ; blocks : Block.def array
  ; first : Kind.Set.t array
  ; follow : Kind.Set.t array
  ; nullable : bool array
  ; min_size : int array
  ; enclosing : Kind.Set.t array
  ; lexer : Redfa.Dfa.t
  ; names : Manifest.t
  ; kind_rule : int array
  ; kind_token : int array
  ; roots : Rule.id list
  ; trivia : Kind.Set.t
  ; error_kind : Kind.t
  ; missing_kind : Kind.t
  ; unterminated_kind : Kind.t
  ; error_token_kind : Kind.t
  }

let sorted es = List.sort_uniq ~cmp:Error.compare es

(* [keep] says which findings stop the derivation. *)
let derive ~(keep : Error.t -> bool) (g : Grammar.t) : (t, Error.t list) result =
  let stop (es : Error.t list) : Error.t list = List.filter es ~f:keep in
  let n = Stage.names g in
  match stop (Check_names.run n) with
  | _ :: _ as es -> Error (sorted es)
  | [] ->
    let s = Stage.shape n in
    (match stop (Check_shape.run s) with
     | _ :: _ as es -> Error (sorted es)
     | [] ->
       let tables =
         Fixpoint.compute ~rules:s.rules ~blocks:s.blocks ~kind_rule:s.names.kind_rule
       in
       let dfa = Stage.lexer n in
       (match stop (Check_full.run s tables dfa) with
        | _ :: _ as es -> Error (sorted es)
        | [] ->
          let kind_of name =
            match Kind.Table.find n.kinds name with
            | Some k -> k
            (* Every one of these went into the table from the manifest that built it,
               so the lookup cannot fail. The fallback is here to say so without an
               assert. *)
            | None -> List.hd (Kind.Table.kinds n.kinds)
          in
          let roots =
            List.filter_map ~f:(fun r -> Stage.find_rule n r) n.grammar.Grammar.roots
          in
          let trivia =
            Array.fold_left n.tokens ~init:Kind.Set.empty ~f:(fun acc (t : Token.def) ->
              if Token.is_trivia t then Kind.Set.add t.kind acc else acc)
          in
          Ok
            { kinds = n.kinds
            ; tokens = n.tokens
            ; rules = s.rules
            ; blocks = s.blocks
            ; first = tables.first
            ; follow = tables.follow
            ; nullable = tables.nullable
            ; min_size = tables.min_size
            ; enclosing = tables.enclosing
            ; lexer = dfa
            ; names = n.manifest
            ; kind_rule = n.kind_rule
            ; kind_token = n.kind_token
            ; roots
            ; trivia
            ; error_kind = kind_of Kind.Name.error
            ; missing_kind = kind_of Kind.Name.missing
            ; unterminated_kind = kind_of Kind.Name.unterminated
            ; error_token_kind = kind_of Kind.Name.error_token
            }))
;;

let of_grammar (g : Grammar.t) : (t, Error.t list) result = derive ~keep:(fun _ -> true) g

(* The three checks a template grammar fails by design. Each one keeps the
   typed views' slot walk unambiguous, and a template grammar gets no views:
   its metavariable is in every slot. *)
let view_codes =
  [ "overlapping-single-kinds"; "repeated-vs-single-kinds"; "binder-not-pattern-token" ]
;;

(* Two places a metavariable can start both of: two alternatives, two atoms,
   or an optional or repeated child and what follows it. A template takes the
   first, as the parser's dispatch does anyway. *)
let first_codes =
  [ "first-first-conflict"; "pratt-atom-conflict"; "first-follow-conflict" ]
;;

(* A name that collides only in the views. A child with alternatives has a
   sum type there, and a template grammar gives most children some. *)
let view_collision (e : Error.t) : bool =
  match e.detail with
  | Error.Name_collision { scope; _ } ->
    (match scope with
     | Manifest.Scope.View_module
     | Manifest.Scope.View_type
     | Manifest.Scope.View_accessor _
     | Manifest.Scope.View_constructor _ -> true
     | Manifest.Scope.Kind_enum
     | Manifest.Scope.Parser_cluster
     | Manifest.Scope.Parser_toplevel
     | Manifest.Scope.Format_cluster -> false)
  | _ -> false
;;

(* The language's own grammar is checked first. That is where its
   metavariables are checked against its tokens, since the derived grammar
   carries them as tokens of its own. *)
let of_template (g : Grammar.t) : (t, Error.t list) result option =
  Option.map
    (fun (template : Grammar.t) ->
       match of_grammar g with
       | Error _ as failed -> failed
       | Ok _ ->
         derive
           ~keep:(fun (e : Error.t) ->
             not
               (List.mem (Error.code e) ~set:view_codes
                || List.mem (Error.code e) ~set:first_codes
                || view_collision e))
           template)
    (Template.grammar g)
;;

(* -- reading --------------------------------------------------------------- *)

let kind_name t k = Kind.Table.name t.kinds k
let kind_count t = Kind.Table.count t.kinds
let find_kind t s = Kind.Table.find t.kinds s
let rule t (i : Rule.id) = t.rules.(i)
let token t (i : Token.id) = t.tokens.(i)

(* The arrays are as long as the kind table was when they were built. A kind
   from a later table reads as absent, and does not run off the end. *)
let rule_of_kind (t : t) (k : Kind.t) : Rule.def option =
  let i = Kind.to_int k in
  if i >= Array.length t.kind_rule || t.kind_rule.(i) < 0
  then None
  else Some t.rules.(t.kind_rule.(i))
;;

let token_of_kind (t : t) (k : Kind.t) : Token.def option =
  let i = Kind.to_int k in
  if i >= Array.length t.kind_token || t.kind_token.(i) < 0
  then None
  else Some t.tokens.(t.kind_token.(i))
;;

let is_token_kind t k = token_of_kind t k <> None
let is_trivia_kind t k = Kind.Set.mem t.trivia k
let first_of t (i : Rule.id) = t.first.(i)
let follow_of t (i : Rule.id) = t.follow.(i)
let is_nullable t (i : Rule.id) = t.nullable.(i)
let min_size t (i : Rule.id) = t.min_size.(i)

let first_of_kind (t : t) (k : Kind.t) : Kind.Set.t =
  match rule_of_kind t k with
  | Some d -> t.first.(d.id)
  | None -> Kind.Set.singleton k
;;

let local_recovery_set (t : t) (i : Rule.id) ~(child : int) : Kind.Set.t =
  let d = t.rules.(i) in
  let cs = d.children in
  let len = Array.length cs in
  if child < 0 || child >= len
  then Kind.Set.empty
  else (
    match cs.(child).recover_to with
    (* The author's override replaces the computed set. *)
    | Some over -> over
    | None ->
      let kind_nullable k =
        match rule_of_kind t k with
        | Some d -> t.nullable.(d.id)
        | None -> false
      in
      let child_nullable (c : Rule.child) =
        match c.modifier with
        | Grammar.Zero_or_one | Grammar.Zero_or_more _ -> true
        | Grammar.Exactly_one | Grammar.One_or_more _ ->
          Array.exists ~f:kind_nullable c.alts
      in
      let alts_first (c : Rule.child) =
        Array.fold_left c.alts ~init:Kind.Set.empty ~f:(fun acc k ->
          Kind.Set.union acc (first_of_kind t k))
      in
      let rest = Array.sub cs ~pos:(child + 1) ~len:(len - child - 1) in
      (* What comes next inside the rule. *)
      let local, rest_passes =
        let rec go i acc =
          if i >= Array.length rest
          then acc, true
          else (
            let c = rest.(i) in
            let acc = Kind.Set.union acc (alts_first c) in
            if child_nullable c then go (i + 1) acc else acc, false)
        in
        go 0 Kind.Set.empty
      in
      (* The closers of this rule's own frame. They bound any position inside its
         body. *)
      let frame =
        match d.frame with
        | Rule.Delimited { close; sep; _ } ->
          let s = Kind.Set.singleton close in
          (match sep with
           | Some { sep_tok; _ } -> Kind.Set.add sep_tok s
           | None -> s)
        | Rule.Separated { sep_tok; _ } -> Kind.Set.singleton sep_tok
        | Rule.Plain | Rule.Committed _ -> Kind.Set.empty
      in
      (* What can follow the rule, taken only at a trailing position. Anywhere
         else, a missing child would skip past the next item at the parent's
         level. *)
      let ambient = if rest_passes then t.follow.(i) else Kind.Set.empty in
      Kind.Set.unions [ local; frame; ambient ])
;;

let recovery_set (t : t) (i : Rule.id) ~(child : int) : Kind.Set.t =
  let d = t.rules.(i) in
  if child < 0 || child >= Array.length d.children
  then Kind.Set.empty
  else (
    (* An author's [recover_to] reaches what this position computes, and stops
       there. The enclosing closers go on top either way. *)
    let enclosing = t.enclosing.(i) in
    match d.children.(child).recover_to with
    | Some over -> Kind.Set.union over enclosing
    | None -> Kind.Set.union (local_recovery_set t i ~child) enclosing)
;;

let delimiter_pairs (t : t) : (Kind.t * Kind.t) list =
  let kinds =
    Array.fold_left t.rules ~init:[] ~f:(fun acc (d : Rule.def) ->
      match d.frame with
      | Rule.Delimited { open_; close; _ } -> (open_, close) :: acc
      | Rule.Plain | Rule.Committed _ | Rule.Separated _ -> acc)
  in
  let cmp (a, b) (c, d) =
    match Kind.compare a c with
    | 0 -> Kind.compare b d
    | n -> n
  in
  List.sort_uniq kinds ~cmp
;;

(* -- printing -------------------------------------------------------------- *)

let pp_modifier (fmt : Format.formatter) : Grammar.modifier -> unit = function
  | Grammar.Exactly_one -> Format.pp_print_string fmt "1"
  | Grammar.Zero_or_one -> Format.pp_print_string fmt "?"
  | Grammar.Zero_or_more _ -> Format.pp_print_string fmt "*"
  | Grammar.One_or_more _ -> Format.pp_print_string fmt "+"
;;

(* Where a separator goes when its body breaks. The default prints nothing. *)
let starts_line (p : Grammar.operator_position) : string =
  match p with
  | Grammar.Op_after -> ""
  | Grammar.Op_before -> "/starts-line"
;;

(* A separator's policy at one end, leading or trailing. *)
let optional_sep (p : Grammar.optional_sep) : string =
  match p with
  | Grammar.Never -> "never"
  | Grammar.On_break -> "on_break"
  | Grammar.Always -> "always"
;;

let pp_frame (t : t) (fmt : Format.formatter) (f : Rule.frame) : unit =
  let k (kind : Kind.t) : string = Kind.Name.to_string (Kind.Table.name t.kinds kind) in
  match f with
  | Rule.Plain -> Format.pp_print_string fmt "plain"
  | Rule.Committed { boundary } -> Format.fprintf fmt "committed(boundary=%b)" boundary
  | Rule.Delimited { open_; close; sep; pad; boundary } ->
    Format.fprintf
      fmt
      "delimited(%s .. %s%s%s, boundary=%b)"
      (k open_)
      (k close)
      (match sep with
       | None -> ""
       | Some { sep_tok; leading; trailing; position } ->
         Printf.sprintf
           ", sep=%s/%s/%s%s"
           (k sep_tok)
           (optional_sep leading)
           (optional_sep trailing)
           (starts_line position))
      (if pad then ", pad" else "")
      boundary
  | Rule.Separated { sep_tok; leading; trailing; position; boundary } ->
    Format.fprintf
      fmt
      "separated(%s/%s/%s%s, boundary=%b)"
      (k sep_tok)
      (optional_sep leading)
      (optional_sep trailing)
      (starts_line position)
      boundary
;;

let pp (fmt : Format.formatter) (t : t) : unit =
  let set = Kind.Table.pp_set t.kinds in
  Format.fprintf fmt "@[<v>kinds (%d):@," (Kind.Table.count t.kinds);
  List.iteri
    ~f:(fun i n -> Format.fprintf fmt "  %3d %a@," i Kind.Name.pp n)
    (Kind.Table.names t.kinds);
  Format.fprintf fmt "@,rules (%d):@," (Array.length t.rules);
  Array.iter
    ~f:(fun (d : Rule.def) ->
      Format.fprintf
        fmt
        "  %3d %s [%s] kind=%a%s@,"
        d.id
        (Grammar.Name.Rule.to_string d.name)
        (match d.origin with
         | Rule.User -> "user"
         | Rule.Pratt_block -> "block"
         | Rule.Pratt_role _ -> "role")
        Kind.Name.pp
        (Kind.Table.name t.kinds d.kind)
        (match d.hole with
         | None -> ""
         | Some h -> " hole=" ^ Kind.Name.to_string (Kind.Table.name t.kinds h));
      Format.fprintf
        fmt
        "        frame=%a body_from=%d@,"
        (pp_frame t)
        d.frame
        d.body_from;
      Array.iteri
        ~f:(fun i (c : Rule.child) ->
          Format.fprintf
            fmt
            "        child %d %s%a %a@,"
            i
            (Grammar.Name.Child.to_string c.child_name)
            pp_modifier
            c.modifier
            set
            c.kinds)
        d.children;
      Format.fprintf
        fmt
        "        nullable=%b min=%d first=%a follow=%a@,"
        t.nullable.(d.id)
        t.min_size.(d.id)
        set
        t.first.(d.id)
        set
        t.follow.(d.id))
    t.rules;
  Format.fprintf fmt "@]"
;;
