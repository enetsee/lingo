open StdLabels

type holds =
  | Nodes
  | Tokens
  | Both

let holds (f : Core.Facts.t) (c : Core.Rule.child) : holds =
  let tokens = Array.exists c.alts ~f:(Core.Facts.is_token_kind f) in
  let nodes = Array.exists c.alts ~f:(fun k -> not (Core.Facts.is_token_kind f k)) in
  match tokens, nodes with
  | true, true -> Both
  | true, false -> Tokens
  | false, (true | false) -> Nodes
;;

let repeats (c : Core.Rule.child) : bool =
  match c.modifier with
  | Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _ -> true
  | Core.Grammar.Exactly_one | Core.Grammar.Zero_or_one -> false
;;

let label (c : Core.Rule.child) : string =
  Core.Manifest.view_accessor (Core.Grammar.Name.Child.to_string c.child_name)
;;

let runtime (name : string) : string = "Lingo_runtime.Rewrite." ^ name
let kind (d : Core.Rule.def) : int = Core.Kind.to_int d.kind

(* -- types ------------------------------------------------------------------- *)

let state : Emit.ty = Emit.tvar "s"
let rule_t : Emit.ty = Emit.tcon (runtime "t") [ state ]

let child_t (f : Core.Facts.t) (c : Core.Rule.child) : Emit.ty =
  let element =
    match holds f c with
    | Nodes -> rule_t
    | Tokens -> Emit.tcon (runtime "Token.t") [ state ]
    | Both -> Emit.tcon (runtime "Elem.t") [ state ]
  in
  if repeats c then Emit.tcon (runtime "Elems.t") [ element ] else element
;;

let congr_t (f : Core.Facts.t) (d : Core.Rule.def) : Emit.ty =
  Array.fold_right
    d.children
    ~init:(Emit.tarrow ~domain:(Emit.tcon "unit" []) ~codomain:rule_t)
    ~f:(fun (c : Core.Rule.child) (codomain : Emit.ty) ->
      Emit.tarrow_optional (label c) ~domain:(child_t f c) ~codomain)
;;

(* -- the modules ------------------------------------------------------------- *)

(* The slot wrapper a child's rule goes through. The view module names the
   grammar gives can shadow [Option] or [Array], so the standard library is
   reached through [Stdlib]. *)
let slot (f : Core.Facts.t) (c : Core.Rule.child) : Emit.expr =
  let wrap =
    match holds f c, repeats c with
    | Nodes, false -> "Slot.node"
    | Tokens, false -> "Slot.token"
    | Both, false -> "Slot.elem"
    | Nodes, true -> "Slot.nodes"
    | Tokens, true -> "Slot.tokens"
    | Both, true -> "Slot.elems"
  in
  Emit.ecall "Stdlib.Option.map" [ Emit.evar (runtime wrap); Emit.evar (label c) ]
;;

let congr ~(views : string) (f : Core.Facts.t) (d : Core.Rule.def) : Emit.item =
  Emit.ilet
    "congr"
    ~args:
      (List.map (Array.to_list d.children) ~f:(fun (c : Core.Rule.child) ->
         Emit.Opt (label c, None))
       @ [ Emit.Plain (Emit.pconstruct "()" []) ])
    (Emit.ecall
       (runtime "congruence")
       [ Emit.eint (kind d)
       ; Emit.evar (views ^ "." ^ Core.Manifest.view_support ^ ".slots")
       ; Emit.earray (List.map (Array.to_list d.children) ~f:(slot f))
       ])
;;

(* The rule [probe] hands a child, lifted to the child's type. *)
let lifted (f : Core.Facts.t) (c : Core.Rule.child) : Emit.expr =
  let element =
    match holds f c with
    | Nodes -> Emit.evar "node"
    | Tokens -> Emit.evar "token"
    | Both ->
      Emit.eapply_labelled
        (Emit.evar (runtime "Elem.make"))
        [ Labelled "node", Emit.evar "node"
        ; Labelled "token", Emit.evar "token"
        ; Nolabel, Emit.eunit
        ]
  in
  if repeats c then Emit.ecall (runtime "Elems.all") [ element ] else element
;;

let probe (f : Core.Facts.t) (modules : (string * Core.Rule.def) list) : Emit.item =
  let some (e : Emit.expr) : Emit.expr = Emit.econstruct "Some" [ e ] in
  let call (module_name : string) (args : (Ppxlib.arg_label * Emit.expr) list) =
    some
      (Emit.eapply_labelled
         (Emit.evar (module_name ^ ".congr"))
         (args @ [ Nolabel, Emit.eunit ]))
  in
  let cases =
    List.concat_map modules ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
      Emit.ecase
        (Emit.ptuple [ Emit.pint (kind d); Emit.pconstruct "None" [] ])
        (call module_name [])
      :: List.mapi (Array.to_list d.children) ~f:(fun (i : int) (c : Core.Rule.child) ->
        Emit.ecase
          (Emit.ptuple [ Emit.pint (kind d); Emit.pconstruct "Some" [ Emit.pint i ] ])
          (call module_name [ Optional (label c), Emit.econstruct "Some" [ lifted f c ] ])))
  in
  Emit.ilet
    "probe"
    ~args:
      [ Emit.arg_var "kind"; Emit.arg_var "slot"; Emit.Named "node"; Emit.Named "token" ]
    (Emit.ematch
       (Emit.etuple [ Emit.evar "kind"; Emit.evar "slot" ])
       (cases @ [ Emit.ecase Emit.pany (Emit.econstruct "None" []) ]))
;;

(* -- constructors ------------------------------------------------------------ *)

(* One symbol of a child, as a constructor takes it. A token with fixed text
   is written by the constructor, so it takes no text. *)
type symbol =
  | Fixed of
      { kind : int
      ; text : string
      }
  | Pattern of int
  | View of string
  | Position of
      { module_path : string
      ; type_path : string
      }

type arm =
  { tag : string
  ; symbol : symbol
  ; name : string
  }

let symbol ~(views : string) (f : Core.Facts.t) (k : Core.Kind.t) : arm =
  match Core.Facts.token_of_kind f k with
  | Some t ->
    let name = Core.Grammar.Name.Token.to_string t.name in
    let symbol =
      match Core.Token.text t with
      | Some text -> Fixed { kind = Core.Kind.to_int k; text }
      | None -> Pattern (Core.Kind.to_int k)
    in
    { tag = Core.Mangle.upper_first name; symbol; name }
  | None ->
    (match
       Array.find_opt f.blocks ~f:(fun (b : Core.Block.def) -> Core.Kind.equal b.kind k)
     with
     | Some b ->
       let name = Core.Grammar.Name.Rule.to_string b.name in
       let position = Core.Manifest.block_position_type name in
       { tag = Core.Mangle.upper_first name
       ; symbol =
           Position
             { module_path = views ^ "." ^ Core.Manifest.view_module position
             ; type_path = views ^ "." ^ position
             }
       ; name
       }
     | None ->
       let d = Option.get (Core.Facts.rule_of_kind f k) in
       let name = Core.Grammar.Name.Rule.to_string d.name in
       { tag = Core.Mangle.upper_first name
       ; symbol = View (views ^ "." ^ Core.Manifest.view_module name)
       ; name
       })
;;

let arms ~(views : string) (f : Core.Facts.t) (c : Core.Rule.child) : arm list =
  List.map (Array.to_list c.alts) ~f:(symbol ~views f)
;;

let required (c : Core.Rule.child) : bool =
  match c.modifier with
  | Core.Grammar.Exactly_one -> true
  | Core.Grammar.Zero_or_one | Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _ ->
    false
;;

let symbol_t (symbol : symbol) : Emit.ty option =
  match symbol with
  | Fixed _ -> None
  | Pattern _ -> Some (Emit.tcon "string" [])
  | View module_path -> Some (Emit.tcon (module_path ^ ".t") [])
  | Position { type_path; _ } -> Some (Emit.tcon type_path [])
;;

(* The type of one element of a child. A single token with fixed text has
   none, and its child takes a flag or a count. *)
let element_t (arms : arm list) : Emit.ty option =
  match arms with
  | [ { symbol; _ } ] -> symbol_t symbol
  | arms ->
    Some (Emit.tvariant (List.map arms ~f:(fun (a : arm) -> a.tag, symbol_t a.symbol)))
;;

let piece_of_symbol (cache : string) (symbol : symbol) (value : Emit.expr) : Emit.expr =
  let construct (name : string) (args : Emit.expr list) =
    Emit.ecall (runtime ("Construct." ^ name)) args
  in
  let one (e : Emit.expr) : Emit.expr = Emit.elist [ e ] in
  match symbol with
  | Fixed { kind; text } ->
    one (construct "token" [ Emit.evar cache; Emit.eint kind; Emit.estr text ])
  | Pattern kind -> one (construct "token" [ Emit.evar cache; Emit.eint kind; value ])
  | View module_path ->
    one (construct "node" [ Emit.ecall (module_path ^ ".syntax") [ value ] ])
  | Position { module_path; _ } ->
    one (construct "node" [ Emit.ecall (module_path ^ ".syntax") [ value ] ])
;;

(* One element's children, as a [Green.child list]. *)
let piece (cache : string) (arms : arm list) (value : Emit.expr) : Emit.expr =
  match arms with
  | [ { symbol; _ } ] -> piece_of_symbol cache symbol value
  | arms ->
    Emit.ematch
      value
      (List.map arms ~f:(fun (a : arm) ->
         match a.symbol with
         | Fixed _ ->
           Emit.ecase (Emit.pvariant a.tag None) (piece_of_symbol cache a.symbol value)
         | Pattern _ | View _ | Position _ ->
           Emit.ecase
             (Emit.pvariant a.tag (Some (Emit.pvar "element")))
             (piece_of_symbol cache a.symbol (Emit.evar "element"))))
;;

(* A child as [make] takes it: the argument, if it takes one, and its
   children in the node. A repeated child gives its elements apart, so a
   frame can put separators between them. *)
type argument =
  { label : string
  ; ty : Emit.ty
  ; optional : bool
  ; default : Emit.expr option
  }

type children =
  | Pieces of Emit.expr
  | Elements of Emit.expr

let child ~(views : string) (f : Core.Facts.t) (cache : string) (c : Core.Rule.child)
  : argument option * children
  =
  let label = label c in
  let var = Emit.evar label in
  let arms = arms ~views f c in
  match element_t arms, c.modifier with
  | None, Core.Grammar.Exactly_one -> None, Pieces (piece cache arms Emit.eunit)
  | None, Core.Grammar.Zero_or_one ->
    ( Some
        { label
        ; ty = Emit.tcon "bool" []
        ; optional = true
        ; default = Some (Emit.ebool false)
        }
    , Pieces
        (Emit.eif
           ~condition:var
           ~then_:(piece cache arms Emit.eunit)
           ~else_:(Emit.elist [])) )
  | None, (Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _) ->
    ( Some { label; ty = Emit.tcon "int" []; optional = false; default = None }
    , Elements
        (Emit.ecall
           "Stdlib.List.init"
           [ var; Emit.elambda [ Emit.arg_any ] (piece cache arms Emit.eunit) ]) )
  | Some ty, Core.Grammar.Exactly_one ->
    Some { label; ty; optional = false; default = None }, Pieces (piece cache arms var)
  | Some ty, Core.Grammar.Zero_or_one ->
    ( Some { label; ty; optional = true; default = None }
    , Pieces
        (Emit.ematch
           var
           [ Emit.ecase (Emit.pconstruct "None" []) (Emit.elist [])
           ; Emit.ecase
               (Emit.pconstruct "Some" [ Emit.pvar label ])
               (piece cache arms var)
           ]) )
  | Some ty, (Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _) ->
    ( Some { label; ty = Emit.tcon "list" [ ty ]; optional = false; default = None }
    , Elements
        (Emit.ecall
           "Stdlib.List.map"
           [ Emit.elambda [ Emit.arg_var label ] (piece cache arms var); var ]) )
;;

let fixed_text (f : Core.Facts.t) (k : Core.Kind.t) : string option =
  Option.bind (Core.Facts.token_of_kind f k) Core.Token.text
;;

let always (policy : Core.Grammar.optional_sep) : bool =
  match policy with
  | Core.Grammar.Always -> true
  | Core.Grammar.Never | Core.Grammar.On_break -> false
;;

(* The node's parts in order, or the reason the frame cannot be written. *)
let frame
      (f : Core.Facts.t)
      (d : Core.Rule.def)
      (cache : string)
      (children : children list)
  : (Emit.expr list, string) result
  =
  let construct (name : string) : string = runtime ("Construct." ^ name) in
  let token (k : Core.Kind.t) (text : string) : Emit.expr =
    Emit.ecall
      (construct "token")
      [ Emit.evar cache; Emit.eint (Core.Kind.to_int k); Emit.estr text ]
  in
  let elements (c : children) : Emit.expr =
    match c with
    | Pieces e -> e
    | Elements e -> Emit.ecall "Stdlib.List.concat" [ e ]
  in
  let slot (index : int) (c : children) : Emit.expr =
    Emit.ecall (construct "slot") [ Emit.eint index; elements c ]
  in
  let separated (index : int) (sep : Core.Rule.sep) (c : children)
    : (Emit.expr, string) result
    =
    match c, fixed_text f sep.sep_tok with
    | Pieces _, _ -> Ok (slot index c)
    | Elements _, None -> Error "the separator has no fixed text"
    | Elements _, Some text ->
      Ok
        (Emit.eapply_labelled
           (Emit.evar (construct "separated"))
           [ Nolabel, Emit.eint index
           ; Labelled "leading", Emit.ebool (always sep.leading)
           ; Labelled "trailing", Emit.ebool (always sep.trailing)
           ; Nolabel, token sep.sep_tok text
           ; Nolabel, elements c
           ])
  in
  let delimiter (k : Core.Kind.t) (text : string) : Emit.expr =
    Emit.ecall (construct "frame") [ token k text ]
  in
  (* A postfix role's operand comes before its frame, and [body_from] says
     where the frame starts. A production's frame starts at its first child. *)
  let before = List.filteri children ~f:(fun i _ -> i < d.body_from) in
  let framed = List.filteri children ~f:(fun i _ -> i >= d.body_from) in
  match d.frame, framed with
  | (Core.Rule.Plain | Core.Rule.Committed _), _ -> Ok (List.mapi children ~f:slot)
  | Core.Rule.Delimited { open_; close; sep; _ }, [ body ] ->
    (match fixed_text f open_, fixed_text f close with
     | Some opener, Some closer ->
       let body =
         match sep with
         | None -> Ok (slot d.body_from body)
         | Some sep -> separated d.body_from sep body
       in
       Result.map
         (fun (body : Emit.expr) ->
            List.mapi before ~f:slot
            @ [ delimiter open_ opener; body; delimiter close closer ])
         body
     | None, _ | _, None -> Error "a delimiter has no fixed text")
  | Core.Rule.Separated { sep_tok; leading; trailing; position; _ }, [ body ] ->
    Result.map
      (fun (body : Emit.expr) -> List.mapi before ~f:slot @ [ body ])
      (separated d.body_from { Core.Rule.sep_tok; leading; trailing; position } body)
  | (Core.Rule.Delimited _ | Core.Rule.Separated _), _ ->
    invalid_arg "Rewrite.frame: a framed rule with other than one child in its frame"
;;

let cache_var (d : Core.Rule.def) : string =
  if Array.exists d.children ~f:(fun c -> String.equal (label c) "cache")
  then "cache'"
  else "cache"
;;

(* A lone argument that is always given is positional. Every other is
   labelled with its child's name. [?replacing] comes first, and a lone
   positional argument after it is what lets a caller leave it out.
   Otherwise [unit] closes the list. *)
let positional (arguments : argument list) : bool =
  match arguments with
  | [ { optional = false; _ } ] -> true
  | _ -> false
;;

let replacing : string = Core.Manifest.rewrite_replacing

(* A function from a kind to whether it is one of [kinds]. *)
let kind_test (kinds : int list) : Emit.expr =
  match kinds with
  | [] -> Emit.elambda [ Emit.arg_any ] (Emit.ebool false)
  | k :: rest ->
    Emit.elambda
      [ Emit.arg_var "kind" ]
      (Emit.ematch
         (Emit.evar "kind")
         [ Emit.ecase
             (Emit.por (Emit.pint k) (List.map rest ~f:Emit.pint))
             (Emit.ebool true)
         ; Emit.ecase Emit.pany (Emit.ebool false)
         ])
;;

let trivia_kinds (f : Core.Facts.t) ~(comments_only : bool) : int list =
  Array.to_list f.tokens
  |> List.filter_map ~f:(fun (t : Core.Token.def) ->
    match t.trivia with
    | Some Core.Grammar.Preserve -> Some (Core.Kind.to_int t.kind)
    | Some Core.Grammar.Reformat when not comments_only -> Some (Core.Kind.to_int t.kind)
    | Some Core.Grammar.Reformat | None -> None)
;;

(* -- parentheses ----------------------------------------------------------------- *)

(* What the parentheses code reads of one block. *)
type block =
  { def : Core.Block.def
  ; position : string
    (** The views' type for the block's nodes, such as [expr_position]. *)
  ; position_module : string
  ; paren : (string * string) option
    (** The bracketing atom's module, and its arm in the position type. *)
  }

let rule_name (d : Core.Rule.def) : string = Core.Grammar.Name.Rule.to_string d.name

(* A bracketing atom is a delimited production with one required child, and
   that child is the block. Writing an expression inside it gives the same
   expression back as one atom. A list that takes one element brackets the
   same way, and means something else, so it does not count. *)
let paren_atom (f : Core.Facts.t) (b : Core.Block.def) : Core.Rule.def option =
  Array.find_map b.atoms ~f:(fun (k : Core.Kind.t) ->
    match Core.Facts.rule_of_kind f k with
    | Some
        ({ origin = Core.Rule.User; frame = Core.Rule.Delimited { open_; close; _ }; _ }
         as d) ->
      (match d.children, fixed_text f open_, fixed_text f close with
       | ( [| { modifier = Core.Grammar.Exactly_one; alts = [| child |]; _ } |]
         , Some _
         , Some _ )
         when Core.Kind.equal child b.kind -> Some d
       | _ -> None)
    | Some _ | None -> None)
;;

let blocks (f : Core.Facts.t) : block list =
  Array.to_list f.blocks
  |> List.map ~f:(fun (b : Core.Block.def) ->
    let name = Core.Grammar.Name.Rule.to_string b.name in
    let position = Core.Manifest.block_position_type name in
    { def = b
    ; position
    ; position_module = Core.Manifest.view_module position
    ; paren =
        Option.map
          (fun (d : Core.Rule.def) ->
             ( Core.Manifest.view_module (rule_name d)
             , Core.Manifest.view_constructor ~sum:position ~arm:(rule_name d) ))
          (paren_atom f b)
    })
;;

let block_of_role (f : Core.Facts.t) (d : Core.Rule.def) : (block * Core.Role.t) option =
  match d.origin with
  | Core.Rule.Pratt_role { block; role } ->
    List.find_map (blocks f) ~f:(fun (b : block) ->
      if b.def.rule_id = block then Some (b, role) else None)
  | Core.Rule.User | Core.Rule.Pratt_block -> None
;;

(* Every kind a block reference can hold, in any block: its roles, its base,
   its rule atoms and its hole. *)
let expression_kinds (f : Core.Facts.t) : int list =
  List.concat_map (Array.to_list f.blocks) ~f:(fun (b : Core.Block.def) ->
    (Core.Kind.to_int b.kind
     :: Core.Kind.to_int b.hole_kind
     :: List.filter_map (Array.to_list b.atoms) ~f:(fun (k : Core.Kind.t) ->
       if Core.Facts.is_token_kind f k then None else Some (Core.Kind.to_int k)))
    @ List.filter_map (Array.to_list f.rules) ~f:(fun (d : Core.Rule.def) ->
      match d.origin with
      | Core.Rule.Pratt_role { block; _ } when block = b.rule_id -> Some (kind d)
      | Core.Rule.Pratt_role _ | Core.Rule.User | Core.Rule.Pratt_block -> None))
  |> List.sort_uniq ~cmp:Int.compare
;;

(* A role's rule, with the module its view is in. *)
let roles (f : Core.Facts.t) (b : block) : (Core.Role.t * string * Core.Rule.def) list =
  List.filter_map
    (Views.modules f)
    ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
      match d.origin with
      | Core.Rule.Pratt_role { block; role } when block = b.def.rule_id ->
        Some (role, module_name, d)
      | Core.Rule.Pratt_role _ | Core.Rule.User | Core.Rule.Pratt_block -> None)
;;

let table (name : string) (rows : (Core.Kind.t * int) list) : Emit.item =
  Emit.ilet
    name
    ~args:[ Emit.arg_typed ~arg_name:"kind" ~type_path:"int" ]
    (Emit.econstraint
       (Emit.ematch
          (Emit.evar "kind")
          (List.map rows ~f:(fun ((k : Core.Kind.t), (bp : int)) ->
             Emit.ecase (Emit.pint (Core.Kind.to_int k)) (Emit.eint bp))
           @ [ Emit.ecase Emit.pany (Emit.evar "Stdlib.max_int") ]))
       (Emit.tcon "int" []))
;;

(* [read view k] reads a role node through its view: [k] gets the view, and
   the node of any other shape gives [default]. *)
let through
      ~(views : string)
      (module_name : string)
      (node : Emit.expr)
      ~(default : Emit.expr)
      (k : Emit.expr -> Emit.expr)
  : Emit.expr
  =
  Emit.ematch
    (Emit.ecall (views ^ "." ^ module_name ^ ".cast") [ node ])
    [ Emit.ecase (Emit.pconstruct "Some" [ Emit.pvar "view" ]) (k (Emit.evar "view"))
    ; Emit.ecase (Emit.pconstruct "None" []) default
    ]
;;

(* [both a b k] runs [k] on two optional reads, and gives [default] where
   either is missing. *)
let both
      (a : Emit.expr)
      (b : Emit.expr)
      ~(default : Emit.expr)
      (k : Emit.expr -> Emit.expr -> Emit.expr)
  : Emit.expr
  =
  Emit.ematch
    (Emit.etuple [ a; b ])
    [ Emit.ecase
        (Emit.ptuple
           [ Emit.pconstruct "Some" [ Emit.pvar "a" ]
           ; Emit.pconstruct "Some" [ Emit.pvar "b" ]
           ])
        (k (Emit.evar "a") (Emit.evar "b"))
    ; Emit.ecase Emit.pany default
    ]
;;

let block_module ~(views : string) (f : Core.Facts.t) (b : block) : Emit.item =
  let accessor (module_name : string) (d : Core.Rule.def) (i : int) (view : Emit.expr) =
    Emit.ecall (views ^ "." ^ module_name ^ "." ^ label d.children.(i)) [ view ]
  in
  let syntax (position : Emit.expr) : Emit.expr =
    Emit.ecall (views ^ "." ^ b.position_module ^ ".syntax") [ position ]
  in
  let token_kind (token : Emit.expr) = Emit.ecall "Siesta.Syntax.Token.kind" [ token ] in
  let max_int = Emit.evar "Stdlib.max_int" in
  let min (a : Emit.expr) (b : Emit.expr) = Emit.ecall "Stdlib.min" [ a; b ] in
  let roles = roles f b in
  let infix = Array.to_list b.def.infix in
  let left_cases =
    List.filter_map
      roles
      ~f:(fun ((role : Core.Role.t), (module_name : string), (d : Core.Rule.def)) ->
        let node = Emit.evar "node" in
        match role with
        | Core.Role.Bin ->
          Some
            (Emit.ecase
               (Emit.pint (kind d))
               (through ~views module_name node ~default:max_int (fun view ->
                  both
                    (accessor module_name d 1 view)
                    (accessor module_name d 0 view)
                    ~default:max_int
                    (fun op lhs ->
                       min
                         (Emit.ecall "infix_left" [ token_kind op ])
                         (Emit.ecall "left" [ syntax lhs ])))))
        | Core.Role.Postfix i ->
          Some
            (Emit.ecase
               (Emit.pint (kind d))
               (through ~views module_name node ~default:max_int (fun view ->
                  Emit.ematch
                    (accessor module_name d 0 view)
                    [ Emit.ecase
                        (Emit.pconstruct "Some" [ Emit.pvar "operand" ])
                        (min
                           (Emit.eint b.def.postfix.(i).p_bp)
                           (Emit.ecall "left" [ syntax (Emit.evar "operand") ]))
                    ; Emit.ecase (Emit.pconstruct "None" []) max_int
                    ])))
        | Core.Role.Base | Core.Role.Prefix -> None)
  in
  let right_cases =
    List.filter_map
      roles
      ~f:(fun ((role : Core.Role.t), (module_name : string), (d : Core.Rule.def)) ->
        let node = Emit.evar "node" in
        let open_right (table : string) (op : int) (operand : int) =
          Emit.ecase
            (Emit.pint (kind d))
            (through ~views module_name node ~default:max_int (fun view ->
               both
                 (accessor module_name d op view)
                 (accessor module_name d operand view)
                 ~default:max_int
                 (fun op operand ->
                    min
                      (Emit.ecall table [ token_kind op ])
                      (Emit.ecall "right" [ syntax operand ]))))
        in
        match role with
        | Core.Role.Bin -> Some (open_right "infix_right" 1 2)
        | Core.Role.Prefix -> Some (open_right "prefix_right" 0 1)
        | Core.Role.Base | Core.Role.Postfix _ -> None)
  in
  let ends_open =
    Emit.eif
      ~condition:
        (Emit.eapply_labelled
           (Emit.evar (runtime "Parens.ends_open"))
           [ Labelled "trivia", kind_test (trivia_kinds f ~comments_only:false)
           ; Labelled "expression", kind_test (expression_kinds f)
           ; Nolabel, Emit.evar "node"
           ])
      ~then_:(Emit.eint 0)
      ~else_:max_int
  in
  let edges =
    Emit.ilet_rec
      [ ( "left"
        , [ Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t" ]
        , Emit.econstraint
            (Emit.ematch
               (Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "node" ])
               (left_cases @ [ Emit.ecase Emit.pany max_int ]))
            (Emit.tcon "int" []) )
      ; ( "right"
        , [ Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t" ]
        , Emit.econstraint
            (Emit.ematch
               (Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "node" ])
               (right_cases @ [ Emit.ecase Emit.pany ends_open ]))
            (Emit.tcon "int" []) )
      ]
  in
  let position_t = Emit.tcon (views ^ "." ^ b.position) [] in
  let parens =
    Emit.ilet
      "parens"
      ~args:
        [ Emit.arg_typed ~arg_name:"cache" ~type_path:"Siesta.Cache.t"
        ; Emit.arg_typed ~arg_name:"inner" ~type_path:(views ^ "." ^ b.position)
        ]
      (Emit.econstraint
         (match b.paren with
          | None ->
            Emit.econstruct
              "Error"
              [ Emit.estr
                  (Core.Grammar.Name.Rule.to_string b.def.name ^ " has no bracketing atom")
              ]
          | Some (module_name, arm) ->
            Emit.ecall
              "Stdlib.Result.map"
              [ Emit.elambda
                  [ Emit.arg_var "atom" ]
                  (Emit.econstruct (views ^ "." ^ arm) [ Emit.evar "atom" ])
              ; Emit.ecall
                  (module_name ^ ".make")
                  [ Emit.evar "cache"; Emit.evar "inner" ]
              ])
         (Emit.tcon "result" [ position_t; Emit.tcon "string" [] ]))
  in
  let wrap =
    Emit.ilet
      "wrap"
      ~args:
        [ Emit.arg_typed ~arg_name:"cache" ~type_path:"Siesta.Cache.t"
        ; Emit.arg_typed ~arg_name:"needed" ~type_path:"bool"
        ; Emit.arg_typed ~arg_name:"inner" ~type_path:(views ^ "." ^ b.position)
        ]
      (Emit.eif
         ~condition:(Emit.evar "needed")
         ~then_:(Emit.ecall "parens" [ Emit.evar "cache"; Emit.evar "inner" ])
         ~else_:(Emit.econstruct "Ok" [ Emit.evar "inner" ]))
  in
  (* Whether [node] needs parentheses to sit where [at] sits. *)
  let needs_parens =
    let here (read : Emit.expr) : Emit.expr =
      Emit.ematch
        read
        [ Emit.ecase
            (Emit.pconstruct "Some" [ Emit.pvar "x" ])
            (Emit.ecall "Siesta.Syntax.equal" [ syntax (Emit.evar "x"); Emit.evar "at" ])
        ; Emit.ecase (Emit.pconstruct "None" []) (Emit.ebool false)
        ]
    in
    let no = Emit.ebool false in
    let operand_case (role : Core.Role.t) (module_name : string) (d : Core.Rule.def) =
      let parent = Emit.evar "parent" in
      let node = Emit.evar "node" in
      let op_kind (view : Emit.expr) (k : Emit.expr -> Emit.expr) : Emit.expr =
        Emit.ematch
          (accessor
             module_name
             d
             (match role with
              | Core.Role.Prefix -> 0
              | _ -> 1)
             view)
          [ Emit.ecase
              (Emit.pconstruct "Some" [ Emit.pvar "op" ])
              (k (token_kind (Emit.evar "op")))
          ; Emit.ecase (Emit.pconstruct "None" []) no
          ]
      in
      match role with
      | Core.Role.Bin ->
        Some
          (Emit.ecase
             (Emit.pint (kind d))
             (through ~views module_name parent ~default:no (fun view ->
                op_kind view (fun k ->
                  Emit.eif
                    ~condition:(here (accessor module_name d 0 view))
                    ~then_:
                      (Emit.egreater_equal
                         ~left:(Emit.ecall "infix_left" [ k ])
                         ~right:(Emit.ecall "right" [ node ]))
                    ~else_:
                      (Emit.eand
                         ~left:(here (accessor module_name d 2 view))
                         ~right:
                           (Emit.eless
                              ~left:(Emit.ecall "left" [ node ])
                              ~right:(Emit.ecall "infix_right" [ k ])))))))
      | Core.Role.Prefix ->
        Some
          (Emit.ecase
             (Emit.pint (kind d))
             (through ~views module_name parent ~default:no (fun view ->
                op_kind view (fun k ->
                  Emit.eand
                    ~left:(here (accessor module_name d 1 view))
                    ~right:
                      (Emit.eless
                         ~left:(Emit.ecall "left" [ node ])
                         ~right:(Emit.ecall "prefix_right" [ k ]))))))
      | Core.Role.Postfix i ->
        Some
          (Emit.ecase
             (Emit.pint (kind d))
             (through ~views module_name parent ~default:no (fun view ->
                Emit.eand
                  ~left:(here (accessor module_name d 0 view))
                  ~right:
                    (Emit.egreater_equal
                       ~left:(Emit.eint b.def.postfix.(i).p_bp)
                       ~right:(Emit.ecall "right" [ node ])))))
      | Core.Role.Base -> None
    in
    Emit.ilet
      "needs_parens"
      ~args:
        [ Emit.Named_pat ("at", Emit.pvar "at")
        ; Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t"
        ]
      (Emit.econstraint
         (Emit.ematch
            (Emit.ecall "Siesta.Syntax.parent" [ Emit.evar "at" ])
            [ Emit.ecase (Emit.pconstruct "None" []) no
            ; Emit.ecase
                (Emit.pconstruct "Some" [ Emit.pvar "parent" ])
                (Emit.ematch
                   (Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "parent" ])
                   (List.filter_map roles ~f:(fun (role, module_name, d) ->
                      operand_case role module_name d)
                    @ [ Emit.ecase Emit.pany no ]))
            ])
         (Emit.tcon "bool" []))
  in
  Emit.imodule
    b.position_module
    [ table
        "infix_left"
        (List.map infix ~f:(fun (o : Core.Block.op) ->
           o.op_kind, fst (Core.Block.op_bps o)))
    ; table
        "infix_right"
        (List.map infix ~f:(fun (o : Core.Block.op) ->
           o.op_kind, snd (Core.Block.op_bps o)))
    ; table
        "prefix_right"
        (List.map (Array.to_list b.def.prefix) ~f:(fun (o : Core.Block.op) ->
           o.op_kind, snd (Core.Block.op_bps o)))
    ; edges
    ; parens
    ; wrap
    ; needs_parens
    ]
;;

let block_signature ~(views : string) (b : block) : Emit.sig_item =
  let position_t = Emit.tcon (views ^ "." ^ b.position) [] in
  let syntax_t = Emit.tcon "Siesta.Syntax.t" [] in
  Emit.smodule
    b.position_module
    [ Emit.sval
        "parens"
        (Emit.tarrow
           ~domain:(Emit.tcon "Siesta.Cache.t" [])
           ~codomain:
             (Emit.tarrow
                ~domain:position_t
                ~codomain:(Emit.tcon "result" [ position_t; Emit.tcon "string" [] ])))
    ; Emit.sval
        "needs_parens"
        (Emit.tarrow_labelled
           "at"
           ~domain:syntax_t
           ~codomain:(Emit.tarrow ~domain:syntax_t ~codomain:(Emit.tcon "bool" [])))
    ]
;;

(* What a role's [make] does before it builds: settle the operator's kind,
   then wrap each operand that would not keep its place without
   parentheses. *)
let role_guard ~(views : string) (f : Core.Facts.t) (cache : string) (d : Core.Rule.def)
  : (Emit.expr -> Emit.expr) option
  =
  match block_of_role f d with
  | None -> None
  | Some (b, role) ->
    let m (name : string) : string = b.position_module ^ "." ^ name in
    let syntax (position : Emit.expr) : Emit.expr =
      Emit.ecall (views ^ "." ^ b.position_module ^ ".syntax") [ position ]
    in
    (* The operator's kind, from the tag [make] was given for it, or the one
       kind a lone operator has. *)
    let op_kind (i : int) : Emit.expr =
      let c = d.children.(i) in
      match arms ~views f c with
      | [ { symbol = Fixed { kind; _ }; _ } ] -> Emit.eint kind
      | arms ->
        Emit.ematch
          (Emit.evar (label c))
          (List.filter_map arms ~f:(fun (a : arm) ->
             match a.symbol with
             | Fixed { kind; _ } ->
               Some (Emit.ecase (Emit.pvariant a.tag None) (Emit.eint kind))
             | Pattern _ | View _ | Position _ -> None))
    in
    let wrap (i : int) (needed : Emit.expr) (rest : Emit.expr) : Emit.expr =
      let name = label d.children.(i) in
      Emit.ematch
        (Emit.ecall (m "wrap") [ Emit.evar cache; needed; Emit.evar name ])
        [ Emit.ecase
            (Emit.pconstruct "Error" [ Emit.pvar "reason" ])
            (Emit.econstruct "Error" [ Emit.evar "reason" ])
        ; Emit.ecase (Emit.pconstruct "Ok" [ Emit.pvar name ]) rest
        ]
    in
    let operand (i : int) : Emit.expr = syntax (Emit.evar (label d.children.(i))) in
    (match role with
     | Core.Role.Bin ->
       Some
         (fun body ->
           Emit.elet
             "op_kind"
             ~body:(op_kind 1)
             ~rest:
               (wrap
                  0
                  (Emit.egreater_equal
                     ~left:(Emit.ecall (m "infix_left") [ Emit.evar "op_kind" ])
                     ~right:(Emit.ecall (m "right") [ operand 0 ]))
                  (wrap
                     2
                     (Emit.eless
                        ~left:(Emit.ecall (m "left") [ operand 2 ])
                        ~right:(Emit.ecall (m "infix_right") [ Emit.evar "op_kind" ]))
                     body)))
     | Core.Role.Prefix ->
       Some
         (fun body ->
           Emit.elet
             "op_kind"
             ~body:(op_kind 0)
             ~rest:
               (wrap
                  1
                  (Emit.eless
                     ~left:(Emit.ecall (m "left") [ operand 1 ])
                     ~right:(Emit.ecall (m "prefix_right") [ Emit.evar "op_kind" ]))
                  body))
     | Core.Role.Postfix p ->
       Some
         (fun body ->
           wrap
             0
             (Emit.egreater_equal
                ~left:(Emit.eint b.def.postfix.(p).p_bp)
                ~right:(Emit.ecall (m "right") [ operand 0 ]))
             body)
     | Core.Role.Base -> None)
;;

let make ~(views : string) (f : Core.Facts.t) (module_name : string) (d : Core.Rule.def)
  : Emit.item
  =
  let cache = cache_var d in
  let parts = List.map (Array.to_list d.children) ~f:(child ~views f cache) in
  let arguments = List.filter_map parts ~f:fst in
  let args =
    Emit.arg_var cache
    :: Emit.Opt (replacing, None)
    ::
    (if positional arguments
     then List.map arguments ~f:(fun (a : argument) -> Emit.arg_var a.label)
     else
       List.map arguments ~f:(fun (a : argument) ->
         if a.optional then Emit.Opt (a.label, a.default) else Emit.Named a.label)
       @ [ Emit.Plain (Emit.pconstruct "()" []) ])
  in
  let body =
    match frame f d cache (List.map parts ~f:snd) with
    | Error reason -> Emit.econstruct "Error" [ Emit.estr reason ]
    | Ok parts ->
      let built =
        Emit.eapply_labelled
          (Emit.evar (runtime "Construct.finish"))
          [ Optional replacing, Emit.evar replacing
          ; ( Labelled "slots"
            , Emit.evar (views ^ "." ^ Core.Manifest.view_support ^ ".slots") )
          ; Labelled "trivia", kind_test (trivia_kinds f ~comments_only:false)
          ; Labelled "comment", kind_test (trivia_kinds f ~comments_only:true)
          ; Nolabel, Emit.evar cache
          ; Nolabel, Emit.eint (kind d)
          ; Nolabel, Emit.evar (views ^ "." ^ module_name ^ ".cast")
          ; Nolabel, Emit.elist parts
          ]
      in
      let built =
        match role_guard ~views f cache d with
        | Some guard -> guard built
        | None -> built
      in
      Array.fold_right
        d.children
        ~init:built
        ~f:(fun (c : Core.Rule.child) (rest : Emit.expr) ->
          match c.modifier with
          | Core.Grammar.One_or_more _ ->
            Emit.eif
              ~condition:(Emit.eequal ~left:(Emit.evar (label c)) ~right:(Emit.elist []))
              ~then_:
                (Emit.econstruct
                   "Error"
                   [ Emit.estr (label c ^ ": at least one is required") ])
              ~else_:rest
          | Core.Grammar.Exactly_one
          | Core.Grammar.Zero_or_one
          | Core.Grammar.Zero_or_more _ -> rest)
  in
  Emit.ilet "make" ~args body
;;

let make_t ~(views : string) (f : Core.Facts.t) (module_name : string) (d : Core.Rule.def)
  : Emit.ty
  =
  let arguments =
    List.filter_map (Array.to_list d.children) ~f:(fun c ->
      fst (child ~views f "cache" c))
  in
  let result =
    Emit.tcon
      "result"
      [ Emit.tcon (views ^ "." ^ module_name ^ ".t") []; Emit.tcon "string" [] ]
  in
  let codomain =
    if positional arguments
    then
      List.fold_right arguments ~init:result ~f:(fun (a : argument) codomain ->
        Emit.tarrow ~domain:a.ty ~codomain)
    else
      List.fold_right
        arguments
        ~init:(Emit.tarrow ~domain:(Emit.tcon "unit" []) ~codomain:result)
        ~f:(fun (a : argument) codomain ->
          if a.optional
          then Emit.tarrow_optional a.label ~domain:a.ty ~codomain
          else Emit.tarrow_labelled a.label ~domain:a.ty ~codomain)
  in
  Emit.tarrow
    ~domain:(Emit.tcon "Siesta.Cache.t" [])
    ~codomain:
      (Emit.tarrow_optional replacing ~domain:(Emit.tcon "Siesta.Syntax.t" []) ~codomain)
;;

(* The value [rebuild] passes for one child, read through the view's
   accessor. A clean parse has every required child, so [Option.get] holds
   there. *)
let read
      ~(views : string)
      (f : Core.Facts.t)
      (module_name : string)
      (d : Core.Rule.def)
      (c : Core.Rule.child)
  : (string * Emit.expr) option
  =
  let accessor =
    Emit.ecall (views ^ "." ^ module_name ^ "." ^ label c) [ Emit.evar "view" ]
  in
  let arms = arms ~views f c in
  let text (e : Emit.expr) : Emit.expr = Emit.ecall "Siesta.Syntax.Token.text" [ e ] in
  (* One element of the accessor's result, as [make] takes it. *)
  let convert : (Emit.expr -> Emit.expr) option =
    match arms with
    | [ { symbol = Fixed _; _ } ] -> None
    | [ { symbol = Pattern _; _ } ] -> Some text
    | [ { symbol = View _ | Position _; _ } ] -> Some (fun e -> e)
    | arms
      when List.for_all arms ~f:(fun (a : arm) ->
             match a.symbol with
             | Fixed _ | Pattern _ -> true
             | View _ | Position _ -> false) ->
      Some
        (fun e ->
          Emit.ematch
            (Emit.ecall "Siesta.Syntax.Token.kind" [ e ])
            (List.map arms ~f:(fun (a : arm) ->
               match a.symbol with
               | Fixed { kind; _ } ->
                 Emit.ecase (Emit.pint kind) (Emit.evariant a.tag None)
               | Pattern kind ->
                 Emit.ecase (Emit.pint kind) (Emit.evariant a.tag (Some (text e)))
               | View _ | Position _ -> invalid_arg "Rewrite.read")
             @ [ Emit.ecase
                   Emit.pany
                   (Emit.ecall "Stdlib.invalid_arg" [ Emit.estr "rebuild" ])
               ]))
    | arms ->
      let sum =
        Core.Manifest.sum_type
          ~prod:(Core.Grammar.Name.Rule.to_string d.name)
          ~child:(Core.Grammar.Name.Child.to_string c.child_name)
      in
      Some
        (fun e ->
          Emit.ematch
            e
            (List.map arms ~f:(fun (a : arm) ->
               let ctor = views ^ "." ^ Core.Manifest.view_constructor ~sum ~arm:a.name in
               let pattern = Emit.pconstruct ctor [ Emit.pvar "x" ] in
               match a.symbol with
               | Fixed _ -> Emit.ecase pattern (Emit.evariant a.tag None)
               | Pattern _ ->
                 Emit.ecase pattern (Emit.evariant a.tag (Some (text (Emit.evar "x"))))
               | View _ | Position _ ->
                 Emit.ecase pattern (Emit.evariant a.tag (Some (Emit.evar "x"))))))
  in
  let each (convert : Emit.expr -> Emit.expr) : Emit.expr =
    Emit.elambda [ Emit.arg_var "x" ] (convert (Emit.evar "x"))
  in
  match convert, c.modifier with
  | None, Core.Grammar.Exactly_one -> None
  | None, Core.Grammar.Zero_or_one ->
    Some (label c, Emit.ecall "Stdlib.Option.is_some" [ accessor ])
  | None, (Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _) ->
    Some (label c, Emit.ecall "Stdlib.List.length" [ accessor ])
  | Some convert, Core.Grammar.Exactly_one ->
    Some (label c, convert (Emit.ecall "Stdlib.Option.get" [ accessor ]))
  | Some convert, Core.Grammar.Zero_or_one ->
    Some (label c, Emit.ecall "Stdlib.Option.map" [ each convert; accessor ])
  | Some convert, (Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _) ->
    Some (label c, Emit.ecall "Stdlib.List.map" [ each convert; accessor ])
;;

let rebuild
      ~(views : string)
      (f : Core.Facts.t)
      (productions : (string * Core.Rule.def) list)
  : Emit.item
  =
  let case ((module_name : string), (d : Core.Rule.def)) : Emit.case =
    let parts =
      List.map (Array.to_list d.children) ~f:(fun c -> fst (child ~views f "cache" c))
    in
    let arguments = List.filter_map parts ~f:(fun a -> a) in
    let reads =
      List.filter_map (Array.to_list d.children) ~f:(read ~views f module_name d)
    in
    let args =
      ((Ppxlib.Nolabel, Emit.evar "cache")
       :: (Ppxlib.Labelled replacing, Emit.evar "node")
       :: List.map2
            arguments
            reads
            ~f:(fun (a : argument) ((_ : string), (e : Emit.expr)) ->
              if positional arguments
              then Ppxlib.Nolabel, e
              else (
                match a.optional, a.default with
                | true, None -> Ppxlib.Optional a.label, e
                | true, Some _ | false, _ -> Ppxlib.Labelled a.label, e)))
      @ if positional arguments then [] else [ Ppxlib.Nolabel, Emit.eunit ]
    in
    let make = Emit.eapply_labelled (Emit.evar (module_name ^ ".make")) args in
    Emit.ecase
      (Emit.pint (kind d))
      (Emit.ecall
         "Stdlib.Option.map"
         [ Emit.elambda
             [ Emit.arg_var "view" ]
             (Emit.ecall
                "Stdlib.Result.map"
                [ Emit.elambda
                    [ Emit.arg_var "built" ]
                    (Emit.ecall
                       "Siesta.Syntax.green"
                       [ Emit.ecall
                           (views ^ "." ^ module_name ^ ".syntax")
                           [ Emit.evar "built" ]
                       ])
                ; make
                ])
         ; Emit.ecall (views ^ "." ^ module_name ^ ".cast") [ Emit.evar "node" ]
         ])
  in
  Emit.ilet
    "rebuild"
    ~args:[ Emit.arg_var "cache"; Emit.arg_var "node" ]
    (Emit.ematch
       (Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "node" ])
       (List.map productions ~f:case
        @ [ Emit.ecase Emit.pany (Emit.econstruct "None" []) ]))
;;

(* Productions and a block's base first, since a block's module calls its
   bracketing atom's [make]. Then each block's module, then the roles, whose
   [make] calls it. *)
let generate ~(views : string) (f : Core.Facts.t) : Emit.item list =
  let modules = Views.modules f in
  let is_role ((_ : string), (d : Core.Rule.def)) : bool =
    match d.origin with
    | Core.Rule.Pratt_role _ -> true
    | Core.Rule.User | Core.Rule.Pratt_block -> false
  in
  let view_module ((module_name : string), (d : Core.Rule.def)) : Emit.item =
    Emit.imodule module_name [ congr ~views f d; make ~views f module_name d ]
  in
  List.map (List.filter modules ~f:(fun m -> not (is_role m))) ~f:view_module
  @ List.map (blocks f) ~f:(block_module ~views f)
  @ List.map (List.filter modules ~f:is_role) ~f:view_module
  @ [ probe f modules
    ; rebuild ~views f modules
    ; Emit.ilet
        "needs_parens"
        ~args:
          [ Emit.Named_pat ("at", Emit.pvar "at")
          ; Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t"
          ]
        (List.fold_right (blocks f) ~init:(Emit.ebool false) ~f:(fun (b : block) rest ->
           Emit.eor
             ~left:
               (Emit.eapply_labelled
                  (Emit.evar (b.position_module ^ ".needs_parens"))
                  [ Labelled "at", Emit.evar "at"; Nolabel, Emit.evar "node" ])
             ~right:rest))
    ]
;;

let signature ~(views : string) (f : Core.Facts.t) : Emit.sig_item list =
  List.map (Views.modules f) ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
    Emit.smodule
      module_name
      [ Emit.sval "congr" (congr_t f d)
      ; Emit.sval "make" (make_t ~views f module_name d)
      ])
  @ List.map (blocks f) ~f:(block_signature ~views)
;;
