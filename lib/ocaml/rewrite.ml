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
        [ Ppxlib.Labelled "node", Emit.evar "node"
        ; Ppxlib.Labelled "token", Emit.evar "token"
        ; Ppxlib.Nolabel, Emit.eunit
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
         (args @ [ Ppxlib.Nolabel, Emit.eunit ]))
  in
  let cases =
    List.concat_map modules ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
      Emit.ecase
        (Emit.ptuple [ Emit.pint (kind d); Emit.pconstruct "None" [] ])
        (call module_name [])
      :: List.mapi (Array.to_list d.children) ~f:(fun (i : int) (c : Core.Rule.child) ->
        Emit.ecase
          (Emit.ptuple [ Emit.pint (kind d); Emit.pconstruct "Some" [ Emit.pint i ] ])
          (call
             module_name
             [ Ppxlib.Optional (label c), Emit.econstruct "Some" [ lifted f c ] ])))
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
           [ Ppxlib.Nolabel, Emit.eint index
           ; Ppxlib.Labelled "leading", Emit.ebool (always sep.leading)
           ; Ppxlib.Labelled "trailing", Emit.ebool (always sep.trailing)
           ; Ppxlib.Nolabel, token sep.sep_tok text
           ; Ppxlib.Nolabel, elements c
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
           [ Ppxlib.Labelled "trivia", kind_test (trivia_kinds f ~comments_only:false)
           ; Ppxlib.Labelled "expression", kind_test (expression_kinds f)
           ; Ppxlib.Nolabel, Emit.evar "node"
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
          [ Ppxlib.Optional replacing, Emit.evar replacing
          ; ( Ppxlib.Labelled "slots"
            , Emit.evar (views ^ "." ^ Core.Manifest.view_support ^ ".slots") )
          ; Ppxlib.Labelled "trivia", kind_test (trivia_kinds f ~comments_only:false)
          ; Ppxlib.Labelled "comment", kind_test (trivia_kinds f ~comments_only:true)
          ; ( Ppxlib.Labelled "takes"
            , match d.origin with
              | Core.Rule.Pratt_role _ ->
                Emit.elambda [ Emit.arg_any; Emit.arg_any ] (Emit.ebool false)
              | Core.Rule.User | Core.Rule.Pratt_block -> Emit.evar "takes_next" )
          ; Ppxlib.Nolabel, Emit.evar cache
          ; Ppxlib.Nolabel, Emit.eint (kind d)
          ; Ppxlib.Nolabel, Emit.evar (views ^ "." ^ module_name ^ ".cast")
          ; Ppxlib.Nolabel, Emit.elist parts
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

(* -- list edits ------------------------------------------------------------- *)

(* What a repeated child's edits need: its slot, the separator between its
   elements with its text, the opener before them, and whether one must
   stay. A child whose separator has no fixed text to write has none. *)
type edit =
  { child : Core.Rule.child
  ; index : int
  ; sep : (int * string) option
  ; opener : int option
  ; must_stay : bool
  }

let edits_of (f : Core.Facts.t) (d : Core.Rule.def) : edit list =
  List.filter_map
    (List.mapi (Array.to_list d.children) ~f:(fun i c -> i, c))
    ~f:(fun ((i : int), (c : Core.Rule.child)) ->
      if not (repeats c)
      then None
      else (
        let framed = i = d.body_from in
        let sep =
          match d.frame with
          | Core.Rule.Delimited { sep = Some sep; _ } when framed ->
            Some (sep.sep_tok, fixed_text f sep.sep_tok)
          | Core.Rule.Separated { sep_tok; _ } when framed ->
            Some (sep_tok, fixed_text f sep_tok)
          | Core.Rule.Delimited _
          | Core.Rule.Separated _
          | Core.Rule.Plain
          | Core.Rule.Committed _ -> None
        in
        let opener =
          match d.frame with
          | Core.Rule.Delimited { open_; _ } when framed -> Some (Core.Kind.to_int open_)
          | Core.Rule.Delimited _
          | Core.Rule.Separated _
          | Core.Rule.Plain
          | Core.Rule.Committed _ -> None
        in
        let must_stay =
          match c.modifier with
          | Core.Grammar.One_or_more _ -> true
          | Core.Grammar.Zero_or_more _
          | Core.Grammar.Exactly_one
          | Core.Grammar.Zero_or_one -> false
        in
        match sep with
        | Some (_, None) -> None
        | Some (k, Some text) ->
          Some
            { child = c
            ; index = i
            ; sep = Some (Core.Kind.to_int k, text)
            ; opener
            ; must_stay
            }
        | None -> Some { child = c; index = i; sep = None; opener; must_stay }))
;;

let edit_args ~(views : string) (f : Core.Facts.t) (d : Core.Rule.def) (e : edit)
  : (Ppxlib.arg_label * Emit.expr) list
  =
  [ Ppxlib.Labelled "kind", Emit.eint (kind d)
  ; ( Ppxlib.Labelled "slots"
    , Emit.evar (views ^ "." ^ Core.Manifest.view_support ^ ".slots") )
  ; Ppxlib.Labelled "trivia", kind_test (trivia_kinds f ~comments_only:false)
  ; Ppxlib.Labelled "slot", Emit.eint e.index
  ; ( Ppxlib.Labelled "sep"
    , match e.sep with
      | Some (k, text) ->
        Emit.econstruct "Some" [ Emit.etuple [ Emit.eint k; Emit.estr text ] ]
      | None -> Emit.econstruct "None" [] )
  ]
;;

let opener_arg (e : edit) : Ppxlib.arg_label * Emit.expr =
  ( Ppxlib.Labelled "opener"
  , match e.opener with
    | Some k -> Emit.econstruct "Some" [ Emit.eint k ]
    | None -> Emit.econstruct "None" [] )
;;

let insert_name (e : edit) : string = "insert_" ^ label e.child
let delete_name (e : edit) : string = "delete_" ^ label e.child

let edit_items ~(views : string) (f : Core.Facts.t) (d : Core.Rule.def) : Emit.item list =
  List.concat_map (edits_of f d) ~f:(fun (e : edit) ->
    let arms = arms ~views f e.child in
    let element =
      Emit.ecall "Stdlib.List.hd" [ piece "cache" arms (Emit.evar (label e.child)) ]
    in
    let args = edit_args ~views f d e in
    (* The cache is needed only where the element is a token to build. *)
    let with_cache (body : Emit.expr) : Emit.expr =
      if
        List.exists arms ~f:(fun (a : arm) ->
          match a.symbol with
          | Fixed _ | Pattern _ -> true
          | View _ | Position _ -> false)
      then
        Emit.elet
          "cache"
          ~body:(Emit.ecall (runtime "Ctx.cache") [ Emit.evar "ctx" ])
          ~rest:body
      else body
    in
    let insert =
      Emit.ilet
        (insert_name e)
        ~args:
          (Emit.Named "at"
           ::
           (match element_t arms with
            | Some _ -> [ Emit.arg_var (label e.child) ]
            | None -> []))
        (Emit.elambda
           [ Emit.arg_var "ctx"; Emit.arg_var "node" ]
           (with_cache
              (Emit.eapply_labelled
                 (Emit.evar (runtime "Edit.insert"))
                 (args
                  @ [ opener_arg e
                    ; Ppxlib.Labelled "at", Emit.evar "at"
                    ; Ppxlib.Nolabel, element
                    ; Ppxlib.Nolabel, Emit.evar "ctx"
                    ; Ppxlib.Nolabel, Emit.evar "node"
                    ]))))
    in
    let delete =
      Emit.ilet
        (delete_name e)
        ~args:[ Emit.Named "at" ]
        (Emit.eapply_labelled
           (Emit.evar (runtime "Edit.delete"))
           (args
            @ [ Ppxlib.Labelled "required", Emit.ebool e.must_stay
              ; Ppxlib.Labelled "at", Emit.evar "at"
              ]))
    in
    [ insert; delete ])
;;

let edit_sigs ~(views : string) (f : Core.Facts.t) (d : Core.Rule.def)
  : Emit.sig_item list
  =
  List.concat_map (edits_of f d) ~f:(fun (e : edit) ->
    let inserted =
      match element_t (arms ~views f e.child) with
      | Some ty -> Emit.tarrow ~domain:ty ~codomain:rule_t
      | None -> rule_t
    in
    [ Emit.sval
        (insert_name e)
        (Emit.tarrow_labelled "at" ~domain:(Emit.tcon "int" []) ~codomain:inserted)
    ; Emit.sval
        (delete_name e)
        (Emit.tarrow_labelled "at" ~domain:(Emit.tcon "int" []) ~codomain:rule_t)
    ])
;;

(* [insert_at k slot ~at element] and [delete_at k slot ~at], for a test. They
   take the element as a green child, so a test needs no view types. *)
let edit_probes
      ~(views : string)
      (f : Core.Facts.t)
      (modules : (string * Core.Rule.def) list)
  : Emit.item list
  =
  let dispatch
        (name : string)
        (extra : Emit.arg list)
        (body : Core.Rule.def -> edit -> Emit.expr)
    =
    Emit.ilet
      name
      ~args:([ Emit.arg_var "kind"; Emit.arg_var "slot"; Emit.Named "at" ] @ extra)
      (Emit.ematch
         (Emit.etuple [ Emit.evar "kind"; Emit.evar "slot" ])
         (List.concat_map modules ~f:(fun ((_ : string), (d : Core.Rule.def)) ->
            List.map (edits_of f d) ~f:(fun (e : edit) ->
              Emit.ecase
                (Emit.ptuple [ Emit.pint (kind d); Emit.pint e.index ])
                (Emit.econstruct "Some" [ body d e ])))
          @ [ Emit.ecase Emit.pany (Emit.econstruct "None" []) ]))
  in
  [ dispatch
      "insert_at"
      [ Emit.arg_var "element" ]
      (fun d e ->
         Emit.eapply_labelled
           (Emit.evar (runtime "Edit.insert"))
           (edit_args ~views f d e
            @ [ opener_arg e
              ; Ppxlib.Labelled "at", Emit.evar "at"
              ; Ppxlib.Nolabel, Emit.evar "element"
              ]))
  ; dispatch "delete_at" [] (fun d e ->
      Emit.eapply_labelled
        (Emit.evar (runtime "Edit.delete"))
        (edit_args ~views f d e
         @ [ Ppxlib.Labelled "required", Emit.ebool e.must_stay
           ; Ppxlib.Labelled "at", Emit.evar "at"
           ]))
  ]
;;

(* -- applying a result -------------------------------------------------------- *)

(* A root's items: its first repeated child of rules, where its frame puts
   no separator between them. The text between two items is the child's
   [between] break, as line breaks. *)
let root_items (f : Core.Facts.t) : (Core.Rule.def * int * string) list =
  List.filter_map f.roots ~f:(fun (id : Core.Rule.id) ->
    let d = f.rules.(id) in
    let separated =
      match d.frame with
      | Core.Rule.Separated _ | Core.Rule.Delimited { sep = Some _; _ } -> true
      | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Delimited { sep = None; _ } ->
        false
    in
    let found =
      List.find_map
        (List.mapi (Array.to_list d.children) ~f:(fun i c -> i, c))
        ~f:(fun ((i : int), (c : Core.Rule.child)) ->
          let gap (b : Core.Grammar.break_style) : string =
            match b with
            | Core.Grammar.Always lines -> String.make (max 1 (lines :> int)) '\n'
            | Core.Grammar.Fit -> "\n"
            | Core.Grammar.Never -> " "
          in
          match c.modifier with
          | (Core.Grammar.Zero_or_more b | Core.Grammar.One_or_more b)
            when Array.for_all c.alts ~f:(fun k -> not (Core.Facts.is_token_kind f k)) ->
            Some (i, gap b)
          | Core.Grammar.Zero_or_more _
          | Core.Grammar.One_or_more _
          | Core.Grammar.Exactly_one
          | Core.Grammar.Zero_or_one -> None)
    in
    match found with
    | Some (i, between) when not separated -> Some (d, i, between)
    | Some _ | None -> None)
;;

let apply_item ~(views : string) (f : Core.Facts.t) : Emit.item =
  let roots = root_items f in
  let kind_of_root = Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "root" ] in
  let items =
    Emit.elambda
      [ Emit.arg_var "root" ]
      (Emit.ematch
         kind_of_root
         (List.map roots ~f:(fun ((d : Core.Rule.def), (slot : int), _) ->
            Emit.ecase
              (Emit.pint (kind d))
              (Emit.ecall
                 "Stdlib.Option.map"
                 [ Emit.elambda
                     [ Emit.arg_var "slots" ]
                     (Emit.ecall
                        "Stdlib.List.filter_map"
                        [ Emit.elambda
                            [ Emit.arg_var "elem" ]
                            (Emit.ematch
                               (Emit.evar "elem")
                               [ Emit.ecase
                                   (Emit.pconstruct
                                      "Siesta.Syntax.Node"
                                      [ Emit.pvar "node" ])
                                   (Emit.econstruct "Some" [ Emit.evar "node" ])
                               ; Emit.ecase
                                   (Emit.pconstruct "Siesta.Syntax.Token" [ Emit.pany ])
                                   (Emit.econstruct "None" [])
                               ])
                        ; Emit.ecall
                            "Stdlib.Array.get"
                            [ Emit.evar "slots"; Emit.eint slot ]
                        ])
                 ; Emit.ecall
                     (views ^ "." ^ Core.Manifest.view_support ^ ".slots")
                     [ Emit.evar "root" ]
                 ]))
          @ [ Emit.ecase Emit.pany (Emit.econstruct "None" []) ]))
  in
  let between =
    Emit.ematch
      (Emit.ecall "Siesta.Green.kind" [ Emit.evar "before" ])
      (List.map roots ~f:(fun ((d : Core.Rule.def), _, (gap : string)) ->
         Emit.ecase (Emit.pint (kind d)) (Emit.estr gap))
       @ [ Emit.ecase Emit.pany (Emit.estr "\n") ])
  in
  Emit.ilet
    "apply"
    ~args:[ Emit.Named "format"; Emit.Named "before"; Emit.Named "after" ]
    (Emit.eapply_labelled
       (Emit.evar (runtime "apply"))
       [ Ppxlib.Labelled "format", Emit.evar "format"
       ; Ppxlib.Labelled "items", items
       ; Ppxlib.Labelled "between", between
       ; Ppxlib.Labelled "trivia", kind_test (trivia_kinds f ~comments_only:false)
       ; Ppxlib.Labelled "comment", kind_test (trivia_kinds f ~comments_only:true)
       ; Ppxlib.Labelled "before", Emit.evar "before"
       ; Ppxlib.Labelled "after", Emit.evar "after"
       ])
;;

let apply_signature : Emit.sig_item =
  let green = Emit.tcon "Siesta.Green.node" [] in
  Emit.sval
    "apply"
    (Emit.tarrow_labelled
       "format"
       ~domain:(Emit.tarrow ~domain:green ~codomain:(Emit.tcon "string" []))
       ~codomain:
         (Emit.tarrow_labelled
            "before"
            ~domain:green
            ~codomain:
              (Emit.tarrow_labelled
                 "after"
                 ~domain:green
                 ~codomain:(Emit.tcon "list" [ Emit.tcon (runtime "splice") [] ]))))
;;

(* -- what a node takes at its end ------------------------------------------- *)

(* The kinds a node of rule [d], whose last filled slot is [j], takes at its
   end. An optional or repeated child after [j] takes what it begins with,
   and so does more of [j] where it repeats. A separated list takes its
   separator. A slot that holds an expression takes any of its block's
   operators, since a block reference is parsed from binding power 0. A
   delimited rule ends at its closer and takes nothing. *)
let open_kinds (f : Core.Facts.t) (d : Core.Rule.def) (j : int) : int list =
  let first (c : Core.Rule.child) : Core.Kind.Set.t =
    Core.Kind.Set.unions (List.map (Array.to_list c.alts) ~f:(Core.Facts.first_of_kind f))
  in
  let optional (c : Core.Rule.child) : bool = (not (required c)) || repeats c in
  let operators (c : Core.Rule.child) : Core.Kind.t list =
    List.concat_map (Array.to_list f.blocks) ~f:(fun (b : Core.Block.def) ->
      if Array.exists c.alts ~f:(Core.Kind.equal b.kind)
      then
        List.map (Array.to_list b.infix) ~f:(fun (o : Core.Block.op) -> o.op_kind)
        @ List.map (Array.to_list b.postfix) ~f:(fun (p : Core.Block.postfix) -> p.p_lead)
      else [])
  in
  let kinds =
    match d.frame with
    | Core.Rule.Delimited _ -> Core.Kind.Set.empty
    | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ ->
      let later =
        Array.to_list d.children
        |> List.filteri ~f:(fun (s : int) (c : Core.Rule.child) -> s > j && optional c)
        |> List.map ~f:first
      in
      let here =
        if j < 0
        then []
        else (
          let c = d.children.(j) in
          (if repeats c then [ first c ] else [])
          @ [ Core.Kind.Set.of_list (operators c) ]
          @
          match d.frame with
          | Core.Rule.Separated { sep_tok; _ } -> [ Core.Kind.Set.singleton sep_tok ]
          | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Delimited _ -> [])
      in
      Core.Kind.Set.unions (later @ here)
  in
  List.map (Core.Kind.Set.elements kinds) ~f:Core.Kind.to_int
;;

let takes_next ~(views : string) (f : Core.Facts.t) : Emit.item list =
  let rule_case (d : Core.Rule.def) : Emit.case option =
    let rows =
      List.filter_map
        (List.init ~len:(Array.length d.children + 1) ~f:(fun i -> i - 1))
        ~f:(fun (j : int) ->
          match open_kinds f d j with
          | [] -> None
          | kinds ->
            Some
              (Emit.ecase
                 (Emit.pint j)
                 (Emit.eapply (kind_test kinds) [ Emit.evar "next" ])))
    in
    if rows = []
    then None
    else
      Some
        (Emit.ecase
           (Emit.pint (kind d))
           (Emit.ematch
              (Emit.evar "slot")
              (rows @ [ Emit.ecase Emit.pany (Emit.ebool false) ])))
  in
  [ Emit.ilet
      "open_after"
      ~args:
        [ Emit.arg_typed ~arg_name:"kind" ~type_path:"int"
        ; Emit.arg_typed ~arg_name:"slot" ~type_path:"int"
        ; Emit.arg_typed ~arg_name:"next" ~type_path:"int"
        ]
      (Emit.econstraint
         (Emit.ematch
            (Emit.evar "kind")
            (List.filter_map (Array.to_list f.rules) ~f:rule_case
             @ [ Emit.ecase Emit.pany (Emit.ebool false) ]))
         (Emit.tcon "bool" []))
  ; Emit.ilet
      "takes_next"
      ~args:
        [ Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t"
        ; Emit.arg_typed ~arg_name:"next" ~type_path:"int"
        ]
      (Emit.eapply_labelled
         (Emit.evar (runtime "Construct.takes"))
         [ ( Ppxlib.Labelled "slots"
           , Emit.evar (views ^ "." ^ Core.Manifest.view_support ^ ".slots") )
         ; Ppxlib.Labelled "trivia", kind_test (trivia_kinds f ~comments_only:false)
         ; Ppxlib.Labelled "open_after", Emit.evar "open_after"
         ; Ppxlib.Nolabel, Emit.evar "node"
         ; Ppxlib.Nolabel, Emit.evar "next"
         ])
  ]
;;

(* -- templates ------------------------------------------------------------------- *)

type template =
  { grammar : Core.Grammar.t
  ; lexer : string
  ; parser : string
  }

(* What the template code needs: the template facts, and the kinds in the
   language's numbering. A kind the language does not have, a metavariable or
   a root made for a rule, is numbered after the language's own. *)
type template_facts =
  { facts : Core.Facts.t
  ; map : int array
  ; metavariables : Core.Grammar.metavariables
  }

let template_facts (f : Core.Facts.t) (t : template) : template_facts =
  match Core.Facts.of_template t.grammar, t.grammar.metavariables with
  | Some (Ok facts), Some metavariables ->
    let count = Core.Facts.kind_count f in
    let map = Array.make (Core.Facts.kind_count facts) 0 in
    List.iter (Core.Kind.Table.kinds facts.kinds) ~f:(fun (k : Core.Kind.t) ->
      let i = Core.Kind.to_int k in
      map.(i)
      <- (match Core.Kind.Table.find f.kinds (Core.Kind.Table.name facts.kinds k) with
          | Some language -> Core.Kind.to_int language
          | None -> count + i));
    { facts; map; metavariables }
  | (None | Some (Error _)), _ | _, None ->
    invalid_arg "Rewrite.template_facts: the grammar has no template grammar that checks"
;;

let token_kinds (t : template_facts) (wanted : string -> bool) : int list =
  Array.to_list t.facts.tokens
  |> List.filter_map ~f:(fun (d : Core.Token.def) ->
    if wanted (Core.Grammar.Name.Token.to_string d.name)
    then Some t.map.(Core.Kind.to_int d.kind)
    else None)
;;

let template_items (f : Core.Facts.t) (t : template) : Emit.item list =
  let tf = template_facts f t in
  let m = tf.metavariables in
  let single_name = Core.Grammar.Name.Token.to_string m.single.token_name in
  let sequence_name = Core.Grammar.Name.Token.to_string m.sequence.token_name in
  let rules =
    List.map t.grammar.productions ~f:(fun (p : Core.Grammar.production) ->
      Core.Grammar.Name.Rule.to_string p.kind_name)
    @ List.map t.grammar.expr ~f:(fun (e : Core.Grammar.expr_def) ->
      Core.Grammar.Name.Rule.to_string e.rule_name)
  in
  let singles =
    token_kinds tf (fun name ->
      String.equal name single_name
      || List.exists rules ~f:(fun r -> String.equal name (Core.Template.typed_name m r)))
  in
  let sequence = token_kinds tf (String.equal sequence_name) in
  let bases =
    List.map (Array.to_list f.blocks) ~f:(fun (b : Core.Block.def) ->
      Core.Kind.to_int b.kind)
  in
  let kinds =
    Emit.erecord
      [ runtime "Template.trivia", kind_test (trivia_kinds f ~comments_only:false)
      ; runtime "Template.single", kind_test singles
      ; runtime "Template.sequence", kind_test sequence
      ; runtime "Template.base", kind_test bases
      ]
  in
  let parse =
    Emit.ematch
      (Emit.evar "rule")
      (List.map rules ~f:(fun (rule : string) ->
         Emit.ecase
           (Emit.pstr rule)
           (Emit.econstruct
              "Some"
              [ Emit.ecall
                  (t.parser
                   ^ "."
                   ^ Core.Manifest.entry_point_fn (Core.Template.entry t.grammar rule))
                  [ Emit.evar "tokens" ]
              ]))
       @ [ Emit.ecase Emit.pany (Emit.econstruct "None" []) ])
  in
  let template =
    Emit.ilet
      "template"
      ~args:[ Emit.Named "rule"; Emit.arg_typed ~arg_name:"text" ~type_path:"string" ]
      (Emit.econstraint
         (Emit.elet
            "tokens"
            ~body:(Emit.ecall (t.lexer ^ ".lex") [ Emit.evar "text" ])
            ~rest:
              (Emit.ematch
                 parse
                 [ Emit.ecase
                     (Emit.pconstruct "None" [])
                     (Emit.econstruct "Error" [ Emit.estr "no such rule" ])
                 ; Emit.ecase
                     (Emit.pconstruct
                        "Some"
                        [ Emit.ptuple
                            [ Emit.pany; Emit.pconstruct "::" [ Emit.pany; Emit.pany ] ]
                        ])
                     (Emit.econstruct "Error" [ Emit.estr "the template does not parse" ])
                 ; Emit.ecase
                     (Emit.pconstruct
                        "Some"
                        [ Emit.ptuple [ Emit.pvar "tree"; Emit.pconstruct "[]" [] ] ])
                     (Emit.ematch
                        (Emit.ecall
                           (runtime "Template.fragment")
                           [ Emit.evar "template_kinds"
                           ; Emit.ecall
                               (runtime "Template.relabel")
                               [ Emit.ecall "Siesta.Cache.create" [ Emit.eunit ]
                               ; Emit.elambda
                                   [ Emit.arg_var "kind" ]
                                   (Emit.ecall
                                      "Stdlib.Array.get"
                                      [ Emit.evar "template_map"; Emit.evar "kind" ])
                               ; Emit.evar "tree"
                               ]
                           ])
                        [ Emit.ecase
                            (Emit.pconstruct "Some" [ Emit.pvar "fragment" ])
                            (Emit.econstruct "Ok" [ Emit.evar "fragment" ])
                        ; Emit.ecase
                            (Emit.pconstruct "None" [])
                            (Emit.econstruct
                               "Error"
                               [ Emit.estr "the template is not one node" ])
                        ])
                 ]))
         (Emit.tcon "result" [ Emit.tcon "Siesta.Green.node" []; Emit.tcon "string" [] ]))
  in
  let rule =
    Emit.ilet
      "template_rule"
      ~args:[ Emit.Named "rule"; Emit.Named "lhs"; Emit.Named "rhs" ]
      (Emit.ematch
         (Emit.etuple
            [ Emit.eapply_labelled
                (Emit.evar "template")
                [ Ppxlib.Labelled "rule", Emit.evar "rule"
                ; Ppxlib.Nolabel, Emit.evar "lhs"
                ]
            ; Emit.eapply_labelled
                (Emit.evar "template")
                [ Ppxlib.Labelled "rule", Emit.evar "rule"
                ; Ppxlib.Nolabel, Emit.evar "rhs"
                ]
            ])
         [ Emit.ecase
             (Emit.ptuple
                [ Emit.pconstruct "Ok" [ Emit.pvar "lhs" ]
                ; Emit.pconstruct "Ok" [ Emit.pvar "rhs" ]
                ])
             (Emit.econstruct
                "Ok"
                [ Emit.elambda
                    [ Emit.arg_var "ctx"; Emit.arg_var "node" ]
                    (Emit.ematch
                       (Emit.ecall
                          (runtime "Template.matches")
                          [ Emit.evar "template_kinds"
                          ; Emit.evar "lhs"
                          ; Emit.evar "node"
                          ])
                       [ Emit.ecase
                           (Emit.pconstruct "None" [])
                           (Emit.econstruct
                              "Error"
                              [ Emit.estr "the template does not match" ])
                       ; Emit.ecase
                           (Emit.pconstruct "Some" [ Emit.pvar "bound" ])
                           (Emit.ecall
                              (runtime "Template.instantiate")
                              [ Emit.ecall (runtime "Ctx.cache") [ Emit.evar "ctx" ]
                              ; Emit.evar "template_kinds"
                              ; Emit.evar "rhs"
                              ; Emit.evar "bound"
                              ])
                       ])
                ])
         ; Emit.ecase
             (Emit.por
                (Emit.ptuple
                   [ Emit.pconstruct "Error" [ Emit.pvar "reason" ]; Emit.pany ])
                [ Emit.ptuple
                    [ Emit.pany; Emit.pconstruct "Error" [ Emit.pvar "reason" ] ]
                ])
             (Emit.econstruct "Error" [ Emit.evar "reason" ])
         ])
  in
  [ Emit.ilet "template_map" (Emit.earray (List.map (Array.to_list tf.map) ~f:Emit.eint))
  ; Emit.ilet
      "template_kinds"
      (Emit.econstraint kinds (Emit.tcon (runtime "Template.kinds") []))
  ; template
  ; rule
  ]
;;

let template_signature : Emit.sig_item list =
  let result (ty : Emit.ty) = Emit.tcon "result" [ ty; Emit.tcon "string" [] ] in
  [ Emit.sval
      "template"
      (Emit.tarrow_labelled
         "rule"
         ~domain:(Emit.tcon "string" [])
         ~codomain:
           (Emit.tarrow
              ~domain:(Emit.tcon "string" [])
              ~codomain:(result (Emit.tcon "Siesta.Green.node" []))))
  ; Emit.sval
      "template_rule"
      (Emit.tarrow_labelled
         "rule"
         ~domain:(Emit.tcon "string" [])
         ~codomain:
           (Emit.tarrow_labelled
              "lhs"
              ~domain:(Emit.tcon "string" [])
              ~codomain:
                (Emit.tarrow_labelled
                   "rhs"
                   ~domain:(Emit.tcon "string" [])
                   ~codomain:(result rule_t))))
  ]
;;

(* -- binders --------------------------------------------------------------------- *)

let binder_items ~(views : string) (f : Core.Facts.t) : Emit.item list =
  let rules = Array.to_list f.rules in
  let scopes =
    List.filter_map rules ~f:(fun (d : Core.Rule.def) ->
      if d.opens_scope then Some (kind d) else None)
  in
  let with_binders =
    List.filter rules ~f:(fun (d : Core.Rule.def) -> Array.length d.binders > 0)
  in
  let references =
    List.concat_map with_binders ~f:(fun (d : Core.Rule.def) ->
      List.concat_map (Array.to_list d.binders) ~f:(fun (i : int) ->
        List.map (Array.to_list d.children.(i).alts) ~f:Core.Kind.to_int))
    |> List.sort_uniq ~cmp:Int.compare
  in
  let bases =
    List.map (Array.to_list f.blocks) ~f:(fun (b : Core.Block.def) ->
      Core.Kind.to_int b.kind)
  in
  let binder_slots =
    Emit.elambda
      [ Emit.arg_var "kind" ]
      (Emit.ematch
         (Emit.evar "kind")
         (List.map with_binders ~f:(fun (d : Core.Rule.def) ->
            Emit.ecase
              (Emit.pint (kind d))
              (Emit.elist (List.map (Array.to_list d.binders) ~f:Emit.eint)))
          @ [ Emit.ecase Emit.pany (Emit.elist []) ]))
  in
  let call (name : string) (args : (Ppxlib.arg_label * Emit.expr) list) : Emit.expr =
    Emit.eapply_labelled
      (Emit.evar (runtime ("Binders." ^ name)))
      ((Ppxlib.Nolabel, Emit.evar "binders") :: args)
  in
  [ Emit.ilet
      "binders"
      (Emit.econstraint
         (Emit.erecord
            [ ( runtime "Binders.slots"
              , Emit.evar (views ^ "." ^ Core.Manifest.view_support ^ ".slots") )
            ; runtime "Binders.trivia", kind_test (trivia_kinds f ~comments_only:false)
            ; runtime "Binders.scope", kind_test scopes
            ; runtime "Binders.binders", binder_slots
            ; runtime "Binders.reference", kind_test references
            ; runtime "Binders.base", kind_test bases
            ])
         (Emit.tcon (runtime "Binders.t") []))
  ; Emit.ilet
      "visible"
      ~args:[ Emit.arg_var "at" ]
      (call "visible" [ Ppxlib.Nolabel, Emit.evar "at" ])
  ; Emit.ilet
      "fresh"
      ~args:[ Emit.arg_var "at"; Emit.Named "base" ]
      (call
         "fresh"
         [ Ppxlib.Nolabel, Emit.evar "at"; Ppxlib.Labelled "base", Emit.evar "base" ])
  ; Emit.ilet
      "resolve"
      ~args:[ Emit.arg_var "token" ]
      (call "resolve" [ Ppxlib.Nolabel, Emit.evar "token" ])
  ; Emit.ilet
      "rename"
      ~args:[ Emit.arg_var "cache"; Emit.arg_var "binder"; Emit.Named "to_" ]
      (call
         "rename"
         [ Ppxlib.Nolabel, Emit.evar "cache"
         ; Ppxlib.Nolabel, Emit.evar "binder"
         ; Ppxlib.Labelled "to_", Emit.evar "to_"
         ])
  ; Emit.ilet
      "substitute"
      ~args:[ Emit.Named "name"; Emit.Named "by" ]
      (call
         "substitute"
         [ Ppxlib.Labelled "name", Emit.evar "name"
         ; Ppxlib.Labelled "by", Emit.evar "by"
         ])
  ]
;;

let binder_signature : Emit.sig_item list =
  let syntax = Emit.tcon "Siesta.Syntax.t" [] in
  let token = Emit.tcon "Siesta.Syntax.token_cursor" [] in
  let string = Emit.tcon "string" [] in
  [ Emit.sval
      "visible"
      (Emit.tarrow
         ~domain:syntax
         ~codomain:(Emit.tcon "list" [ Emit.ttuple [ string; token ] ]))
  ; Emit.sval
      "fresh"
      (Emit.tarrow
         ~domain:syntax
         ~codomain:(Emit.tarrow_labelled "base" ~domain:string ~codomain:string))
  ; Emit.sval
      "resolve"
      (Emit.tarrow ~domain:token ~codomain:(Emit.tcon "option" [ token ]))
  ; Emit.sval
      "rename"
      (Emit.tarrow
         ~domain:(Emit.tcon "Siesta.Cache.t" [])
         ~codomain:
           (Emit.tarrow
              ~domain:token
              ~codomain:
                (Emit.tarrow_labelled
                   "to_"
                   ~domain:string
                   ~codomain:
                     (Emit.tcon "result" [ Emit.tcon "Siesta.Green.node" []; string ]))))
  ; Emit.sval
      "substitute"
      (Emit.tarrow_labelled
         "name"
         ~domain:string
         ~codomain:
           (Emit.tarrow_labelled
              "by"
              ~domain:(Emit.tcon "Siesta.Green.node" [])
              ~codomain:rule_t))
  ]
;;

(* Productions and a block's base first, since a block's module calls its
   bracketing atom's [make]. Then each block's module, then the roles, whose
   [make] calls it. *)
let generate ~(views : string) ?(template : template option) (f : Core.Facts.t)
  : Emit.item list
  =
  let modules = Views.modules f in
  let is_role ((_ : string), (d : Core.Rule.def)) : bool =
    match d.origin with
    | Core.Rule.Pratt_role _ -> true
    | Core.Rule.User | Core.Rule.Pratt_block -> false
  in
  let view_module ((module_name : string), (d : Core.Rule.def)) : Emit.item =
    Emit.imodule
      module_name
      ([ congr ~views f d; make ~views f module_name d ] @ edit_items ~views f d)
  in
  takes_next ~views f
  @ List.map (List.filter modules ~f:(fun m -> not (is_role m))) ~f:view_module
  @ List.map (blocks f) ~f:(block_module ~views f)
  @ List.map (List.filter modules ~f:is_role) ~f:view_module
  @ edit_probes ~views f modules
  @ binder_items ~views f
  @ [ apply_item ~views f
    ; probe f modules
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
                  [ Ppxlib.Labelled "at", Emit.evar "at"
                  ; Ppxlib.Nolabel, Emit.evar "node"
                  ])
             ~right:rest))
    ]
  @
  match template with
  | Some t -> template_items f t
  | None -> []
;;

let signature ~(views : string) ?(template : template option) (f : Core.Facts.t)
  : Emit.sig_item list
  =
  List.map (Views.modules f) ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
    Emit.smodule
      module_name
      ([ Emit.sval "congr" (congr_t f d)
       ; Emit.sval "make" (make_t ~views f module_name d)
       ]
       @ edit_sigs ~views f d))
  @ List.map (blocks f) ~f:(block_signature ~views)
  @ [ apply_signature ]
  @ binder_signature
  @
  match template with
  | Some _ -> template_signature
  | None -> []
;;
