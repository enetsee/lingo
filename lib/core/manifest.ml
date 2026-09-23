type name = string

let to_string (n : name) = n

module Scope = struct
  type t =
    | Kind_enum
    | Parser_cluster
    | Parser_toplevel
    | Format_cluster
    | View_module
    | View_type
    | View_accessor of string

  let name : t -> string = function
    | Kind_enum -> "kind constructor"
    | Parser_cluster -> "parser binding"
    | Parser_toplevel -> "parser entry point"
    | Format_cluster -> "formatter binding"
    | View_module -> "view module"
    | View_type -> "view type"
    | View_accessor m -> "accessor in view module " ^ m
  ;;

  (* A repeat among the parser's separate top-level [let]s shadows, so it
     still compiles. They are listed anyway, so that a cross-check against the
     emitted text can classify every name it finds. *)
  let fails_to_compile : t -> bool = function
    | Parser_toplevel -> false
    | Kind_enum
    | Parser_cluster
    | Format_cluster
    | View_module
    | View_type
    | View_accessor _ -> true
  ;;

  let rank : t -> int = function
    | Kind_enum -> 0
    | Parser_cluster -> 1
    | Parser_toplevel -> 2
    | Format_cluster -> 3
    | View_module -> 4
    | View_type -> 5
    | View_accessor _ -> 6
  ;;

  let compare (a : t) (b : t) : int =
    match a, b with
    | View_accessor x, View_accessor y -> String.compare x y
    | _ -> Int.compare (rank a) (rank b)
  ;;
end

let builtin = "<builtin>"

type entry =
  { emitted : name
  ; scope : Scope.t
  ; base : string
  ; derivation : string
  }

(* -- kind names ------------------------------------------------------------ *)

module Kind_name = struct
  let of_production (p : Grammar.production) : Kind.Name.t =
    Kind.Name.node (Grammar.Name.Rule.to_string p.kind_name)
  ;;

  let of_token (td : Grammar.token_def) : Kind.Name.t =
    Kind.Name.token (Grammar.Name.Token.to_string td.token_name)
  ;;

  let of_role (e : Grammar.expr_def) (role : Role.t) : Kind.Name.t =
    Kind.Name.node (Role.kind_name e role)
  ;;

  let hole_of_production (p : Grammar.production) : Kind.Name.t =
    Kind.Name.hole (of_production p)
  ;;

  let hole_of_block (e : Grammar.expr_def) : Kind.Name.t =
    Kind.Name.hole (Kind.Name.node (Grammar.Name.Rule.to_string e.rule_name))
  ;;
end

(* -- name derivations ------------------------------------------------------ *)

let parse_fn n = "parse_" ^ Mangle.safe_snake n
let root_parse_fn n = parse_fn n ^ "__root"
let can_start_fn n = "can_start_" ^ Mangle.safe_snake n
let pratt_lhs_fn n = parse_fn n ^ "_lhs"
let pratt_infix_fn n = parse_fn n ^ "_infix"
let pratt_infix_bp_fn n = "infix_bp_" ^ Mangle.safe_snake n
let pratt_prefix_bp_fn n = "prefix_bp_" ^ Mangle.safe_snake n
let entry_point_fn n = "parse_tokens_" ^ Mangle.safe_snake n
let kind_constructor raw = "K_" ^ raw
let format_fn n = "format_" ^ Mangle.safe_snake n
let view_module n = Mangle.upper_first n
let view_accessor n = Mangle.safe_snake n
let sum_type ~prod ~child = Mangle.safe_snake prod ^ "_" ^ Mangle.safe_snake child
let block_position_type b = Mangle.safe_snake b ^ "_position"
let block_entry_type b = Mangle.safe_snake b ^ "_entry"

(* -- the manifest ---------------------------------------------------------- *)

type t =
  { entries : entry list
  ; kind_names : Kind.Name.t list
  }

let entries t = t.entries
let kind_names t = t.kind_names

type collision =
  { c_scope : Scope.t
  ; c_emitted : string
  ; c_entries : entry list
  }

(* Every raw kind name the grammar defines paired with the declaration it came 
   from or with [builtin]. That pairing lets a collision name what it hit.

   Kind integers are assigned in the order of this list: productions, then
   tokens, then Pratt roles, then holes, then the built-ins. Changing it
   renumbers every kind. So the order is fixed here, and
   [Kind.Table.of_names] reads the list verbatim. *)
let raw_kind_sources (g : Grammar.t) : (Kind.Name.t * string) list =
  let node_kinds =
    List.map
      (fun (p : Grammar.production) ->
         Kind_name.of_production p, Grammar.Name.Rule.to_string p.kind_name)
      g.productions
  and token_kinds =
    List.map
      (fun (td : Grammar.token_def) ->
         Kind_name.of_token td, Grammar.Name.Token.to_string td.token_name)
      g.tokens
  and expr_kinds =
    List.concat_map
      (fun (e : Grammar.expr_def) ->
         List.map
           (fun r -> Kind_name.of_role e r, Grammar.Name.Rule.to_string e.rule_name)
           (Role.of_block e))
      g.expr
  and prod_holes =
    List.filter_map
      (fun (p : Grammar.production) ->
         if p.has_hole
         then
           Some (Kind_name.hole_of_production p, Grammar.Name.Rule.to_string p.kind_name)
         else None)
      g.productions
  and expr_holes =
    (* One hole per block. A missing expression takes the block's base kind,
       whichever shape was being parsed. *)
    List.map
      (fun (e : Grammar.expr_def) ->
         Kind_name.hole_of_block e, Grammar.Name.Rule.to_string e.rule_name)
      g.expr
  in
  node_kinds
  @ token_kinds
  @ expr_kinds
  @ prod_holes
  @ expr_holes
  @ List.map
      (fun k -> k, builtin)
      [ Kind.Name.error_token
      ; Kind.Name.error
      ; Kind.Name.missing
        (* [T_UNTERMINATED] is a lexeme the input ended in the middle of, such as an
           unclosed string or an unclosed block comment.

           Nothing can follow one. Re-lexing takes any bytes appended after it back
           into the same lexeme, whatever glue goes between. The formatter reads
           that off the kind and leaves the delimiter alone.

           [T_ERROR] is a byte the automaton could not classify. *)
      ; Kind.Name.unterminated
      ]
;;

let e ~(scope : Scope.t) ~(base : string) ~(derivation : string) (emitted : name) : entry =
  { emitted; scope; base; derivation }
;;

(* Whether a child's alternatives define a top-level sum type. Two cases give
   no sum: a single arm, and arms that are all tokens. The accessor returns
   a token cursor for both.

   The condition here matches the one the view layer uses, so every name
   listed is a name that gets emitted. *)
let child_emits_sum (c : Grammar.child) =
  match c.rest with
  | [] -> false
  | _ :: _ ->
    List.exists
      (function
        | Grammar.Rule _ -> true
        | Token _ -> false)
      (c.head :: c.rest)
;;

let production_entries (p : Grammar.production) : entry list =
  let n = Grammar.Name.Rule.to_string p.kind_name in
  let base = n in
  let mname = view_module n in
  [ e ~scope:Scope.Parser_cluster ~base ~derivation:"parse_fn" (parse_fn n)
  ; e ~scope:Scope.Parser_cluster ~base ~derivation:"can_start_fn" (can_start_fn n)
    (* Every production gets the root variant and the entry point, so a grammar
       that passes here is safe under any emission. *)
  ; e ~scope:Scope.Parser_cluster ~base ~derivation:"root_parse_fn" (root_parse_fn n)
  ; e ~scope:Scope.Parser_toplevel ~base ~derivation:"entry_point_fn" (entry_point_fn n)
  ; e ~scope:Scope.Format_cluster ~base ~derivation:"format_fn" (format_fn n)
  ; e ~scope:Scope.View_module ~base ~derivation:"view_module" mname
  ]
  @ List.concat_map
      (fun (c : Grammar.child) ->
         e
           ~scope:(Scope.View_accessor mname)
           ~base:(Grammar.Name.Child.to_string c.name)
           ~derivation:"view_accessor"
           (view_accessor (Grammar.Name.Child.to_string c.name))
         ::
         (if child_emits_sum c
          then
            [ e
                ~scope:Scope.View_type
                ~base:(n ^ "." ^ Grammar.Name.Child.to_string c.name)
                ~derivation:"sum_type"
                (sum_type ~prod:n ~child:(Grammar.Name.Child.to_string c.name))
            ]
          else []))
      p.children
;;

let format_fn_of_role (e_ : Grammar.expr_def) (role : Role.t) : string =
  "format_" ^ Role.format_fn e_ role
;;

let block_entries (e_ : Grammar.expr_def) : entry list =
  let n = Grammar.Name.Rule.to_string e_.rule_name in
  let base = n in
  [ e ~scope:Scope.Parser_cluster ~base ~derivation:"parse_fn" (parse_fn n)
  ; e ~scope:Scope.Parser_cluster ~base ~derivation:"can_start_fn" (can_start_fn n)
  ; e ~scope:Scope.Parser_cluster ~base ~derivation:"pratt_lhs_fn" (pratt_lhs_fn n)
  ; e ~scope:Scope.Parser_cluster ~base ~derivation:"pratt_infix_fn" (pratt_infix_fn n)
  ; e
      ~scope:Scope.Parser_toplevel
      ~base
      ~derivation:"pratt_infix_bp_fn"
      (pratt_infix_bp_fn n)
  ; e
      ~scope:Scope.Parser_toplevel
      ~base
      ~derivation:"pratt_prefix_bp_fn"
      (pratt_prefix_bp_fn n)
  ; e ~scope:Scope.Parser_toplevel ~base ~derivation:"entry_point_fn" (entry_point_fn n)
  ; e ~scope:Scope.View_type ~base ~derivation:"block_entry_type" (block_entry_type n)
  ; e
      ~scope:Scope.View_type
      ~base
      ~derivation:"block_position_type"
      (block_position_type n)
  ]
  @ List.concat_map
      (fun role ->
         let fmt =
           e
             ~scope:Scope.Format_cluster
             ~base
             ~derivation:"role_format_fn"
             (format_fn_of_role e_ role)
         in
         if Role.is_active e_ role
         then
           [ fmt
           ; e
               ~scope:Scope.View_module
               ~base
               ~derivation:"role_view_module"
               (view_module (Role.synthetic_name e_ role))
           ]
         else [ fmt ])
      (Role.of_block e_)
;;

(* The names a backend emits for every grammar. They are entries like any
   other, so colliding with one goes through the same check and the
   diagnostic names what was hit. *)
let builtin_entries : entry list =
  [ e
      ~scope:Scope.Format_cluster
      ~base:builtin
      ~derivation:"format dispatch"
      "format_node"
  ; e
      ~scope:Scope.Format_cluster
      ~base:builtin
      ~derivation:"format fallback"
      "format_generic"
    (* This is a top-level alias for the first root's entry point, in the parser
       module. A cluster binding of that name would hide it, so it is listed in
       the scope it can be hidden from. *)
  ; e ~scope:Scope.Parser_cluster ~base:builtin ~derivation:"entry alias" "parse_tokens"
  ]
;;

let of_grammar (g : Grammar.t) : t =
  let sources = raw_kind_sources g in
  let kind_names = List.map fst sources in
  let kind_entries =
    List.map
      (fun (raw, base) ->
         e
           ~scope:Scope.Kind_enum
           ~base
           ~derivation:"kind_constructor"
           (kind_constructor (Kind.Name.to_string raw)))
      sources
  in
  let entries =
    kind_entries
    @ builtin_entries
    @ List.concat_map production_entries g.productions
    @ List.concat_map block_entries g.expr
  in
  { entries; kind_names }
;;

let collisions (t : t) : collision list =
  let module M = Map.Make (struct
      type nonrec t = Scope.t * string

      let compare (s1, n1) (s2, n2) =
        match Scope.compare s1 s2 with
        | 0 -> String.compare n1 n2
        | c -> c
      ;;
    end)
  in
  let grouped =
    List.fold_left
      (fun m en ->
         let key = en.scope, (en.emitted : name :> string) in
         M.add
           key
           (en
            ::
            (try M.find key m with
             | Not_found -> []))
           m)
      M.empty
      t.entries
  in
  M.fold
    (fun (s, n) es acc ->
       match es with
       | _ :: _ :: _ -> { c_scope = s; c_emitted = n; c_entries = List.rev es } :: acc
       | _ -> acc)
    grouped
    []
  |> List.rev
;;
