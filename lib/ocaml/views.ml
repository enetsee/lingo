open StdLabels

(* -- what an accessor returns ----------------------------------------------- *)

(* One arm of a variant: the constructor, and what the arm carries. *)
type arm =
  { ctor : string
  ; carries : carries
  }

and carries =
  | Arm_token of Ir.Kind.t
  | Arm_node of
      { kind : Ir.Kind.t
      ; view : string
      }
  | Arm_position of
      { kinds : Ir.Kind.t list
      ; position : string
      }

(* What one element of a slot reads as. A child of one token, or of several,
   reads as the token. A child of one rule reads as that rule's view, and a
   child of an expression block as the block's position. Anything else is a
   variant of its own. *)
type shape =
  | Token
  | Node of
      { kind : Ir.Kind.t
      ; view : string
      }
  | Position of string
  | Sum of
      { name : string
      ; arms : arm list
      }

let kind (k : Core.Kind.t) : Ir.Kind.t = Core.Kind.to_int k
let rule_name (d : Core.Rule.def) : string = Core.Grammar.Name.Rule.to_string d.name

let block_of_kind (f : Core.Facts.t) (k : Core.Kind.t) : Core.Block.def option =
  Array.find_opt f.blocks ~f:(fun (b : Core.Block.def) -> Core.Kind.equal b.kind k)
;;

let block_name (b : Core.Block.def) : string = Core.Grammar.Name.Rule.to_string b.name

(* The base role has no rule of its own. The block's rule is its node, the one
   a token atom is wrapped in, and it has the atom as its child exactly when
   the block has a token atom to wrap. *)
let has_base (f : Core.Facts.t) (b : Core.Block.def) : bool =
  Array.length (Core.Facts.rule f b.rule_id).children > 0
;;

(* The nodes a block's parse builds: its base, its other roles, then its rule
   atoms. A token atom is wrapped in the base, so it is no arm of its own. *)
let position_arms (f : Core.Facts.t) (b : Core.Block.def) : (Ir.Kind.t * string) list =
  let base = if has_base f b then [ kind b.kind, block_name b ] else [] in
  let roles =
    Array.to_list f.rules
    |> List.filter_map ~f:(fun (d : Core.Rule.def) ->
      match d.origin with
      | Core.Rule.Pratt_role { block; _ } when block = b.rule_id ->
        Some (kind d.kind, rule_name d)
      | Core.Rule.User | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> None)
  in
  let atoms =
    Array.to_list b.atoms
    |> List.filter_map ~f:(fun (a : Core.Kind.t) ->
      match Core.Facts.rule_of_kind f a with
      | Some d -> Some (kind a, rule_name d)
      | None -> None)
  in
  base @ roles @ atoms
;;

let shape (f : Core.Facts.t) (d : Core.Rule.def) (c : Core.Rule.child) : shape =
  let node (k : Core.Kind.t) : Ir.Kind.t * string =
    match Core.Facts.rule_of_kind f k with
    | Some rule -> kind k, Core.Manifest.view_type (rule_name rule)
    | None -> invalid_arg "Views.shape: a rule child whose kind has no rule"
  in
  if Array.for_all c.alts ~f:(Core.Facts.is_token_kind f)
  then Token
  else (
    match c.alts with
    | [| k |] ->
      (match block_of_kind f k with
       | Some b -> Position (Core.Manifest.block_position_type (block_name b))
       | None ->
         let k, view = node k in
         Node { kind = k; view })
    | alts ->
      let name =
        Core.Manifest.sum_type
          ~prod:(rule_name d)
          ~child:(Core.Grammar.Name.Child.to_string c.child_name)
      in
      let arm (k : Core.Kind.t) : arm =
        if Core.Facts.is_token_kind f k
        then (
          let t = Core.Facts.token_of_kind f k |> Option.get in
          { ctor =
              Core.Manifest.view_constructor
                ~sum:name
                ~arm:(Core.Grammar.Name.Token.to_string t.name)
          ; carries = Arm_token (kind k)
          })
        else (
          match block_of_kind f k with
          | Some b ->
            { ctor = Core.Manifest.view_constructor ~sum:name ~arm:(block_name b)
            ; carries =
                Arm_position
                  { kinds = List.map (position_arms f b) ~f:fst
                  ; position = Core.Manifest.block_position_type (block_name b)
                  }
            }
          | None ->
            let rule = Core.Facts.rule_of_kind f k |> Option.get in
            let k, view = node k in
            { ctor = Core.Manifest.view_constructor ~sum:name ~arm:(rule_name rule)
            ; carries = Arm_node { kind = k; view }
            })
      in
      Sum { name; arms = List.map (Array.to_list alts) ~f:arm })
;;

let repeats (c : Core.Rule.child) : bool =
  match c.modifier with
  | Core.Grammar.Zero_or_more _ | Core.Grammar.One_or_more _ -> true
  | Core.Grammar.Exactly_one | Core.Grammar.Zero_or_one -> false
;;

(* -- what a slot admits ------------------------------------------------------ *)

(* Every kind that can fill a child. A block reference is filled by any node
   the block's parse builds, and by the block's hole.

   A required child the parse lowers to a commit leaves a placeholder where it
   could not find one, of the rule's hole kind or of the missing kind. The
   slot takes it, so the child after it is not read into the empty slot.
   That is the one way this walk differs from the formatter's. A required
   token leaves nothing behind, so its slot takes no placeholder. *)
let admits (f : Core.Facts.t) (d : Core.Rule.def) (c : Core.Rule.child) : Ir.Kind.t list =
  let expanded =
    Array.to_list c.alts
    |> List.concat_map ~f:(fun (k : Core.Kind.t) ->
      match block_of_kind f k with
      | Some b -> kind b.hole_kind :: List.map (position_arms f b) ~f:fst
      | None -> [ kind k ])
  in
  let placeholder =
    match c.modifier, c.alts with
    | (Core.Grammar.Exactly_one | Core.Grammar.One_or_more _), [| k |]
      when Core.Facts.is_token_kind f k -> []
    | (Core.Grammar.Exactly_one | Core.Grammar.One_or_more _), _ ->
      [ kind (Option.value d.hole ~default:f.missing_kind) ]
    | (Core.Grammar.Zero_or_one | Core.Grammar.Zero_or_more _), _ -> []
  in
  List.sort_uniq ~cmp:Int.compare (placeholder @ expanded)
;;

(* -- the modules --------------------------------------------------------------- *)

type view =
  { module_name : string
  ; view_type : string
  ; view_kind : Ir.Kind.t
  ; def : Core.Rule.def
  }

(* A view per production and per role. The block's own rule is the base
   role's view, where the block has one. *)
let views_of (f : Core.Facts.t) : view list =
  Array.to_list f.rules
  |> List.filter_map ~f:(fun (d : Core.Rule.def) ->
    let view : view =
      { module_name = Core.Manifest.view_module (rule_name d)
      ; view_type = Core.Manifest.view_type (rule_name d)
      ; view_kind = kind d.kind
      ; def = d
      }
    in
    match d.origin with
    | Core.Rule.User | Core.Rule.Pratt_role _ -> Some view
    | Core.Rule.Pratt_block -> if Array.length d.children > 0 then Some view else None)
;;

let modules (f : Core.Facts.t) : (string * Core.Rule.def) list =
  List.map (views_of f) ~f:(fun (v : view) -> v.module_name, v.def)
;;

let positions (f : Core.Facts.t) : (string * arm list) list =
  Array.to_list f.blocks
  |> List.map ~f:(fun (b : Core.Block.def) ->
    let position = Core.Manifest.block_position_type (block_name b) in
    ( position
    , List.map (position_arms f b) ~f:(fun (k, name) ->
        { ctor = Core.Manifest.view_constructor ~sum:position ~arm:name
        ; carries = Arm_node { kind = k; view = Core.Manifest.view_type name }
        }) ))
;;

let sums (f : Core.Facts.t) (views : view list) : (string * arm list) list =
  List.concat_map views ~f:(fun (v : view) ->
    Array.to_list v.def.children
    |> List.filter_map ~f:(fun (c : Core.Rule.child) ->
      match shape f v.def c with
      | Sum { name; arms } -> Some (name, arms)
      | Token | Node _ | Position _ -> None))
;;

(* -- types ----------------------------------------------------------------------- *)

let syntax_t : Emit.ty = Emit.tcon "Siesta.Syntax.t" []
let token_t : Emit.ty = Emit.tcon "Siesta.Syntax.token_cursor" []
let elem_t : Emit.ty = Emit.tcon "Siesta.Syntax.elem" []

let arm_ty (a : arm) : Emit.ty =
  match a.carries with
  | Arm_token _ -> token_t
  | Arm_node { view; _ } -> Emit.tcon view []
  | Arm_position { position; _ } -> Emit.tcon position []
;;

let shape_ty (s : shape) : Emit.ty =
  match s with
  | Token -> token_t
  | Node { view; _ } -> Emit.tcon view []
  | Position name | Sum { name; _ } -> Emit.tcon name []
;;

let accessor_ty (f : Core.Facts.t) (v : view) (c : Core.Rule.child) : Emit.ty =
  Emit.tcon (if repeats c then "list" else "option") [ shape_ty (shape f v.def c) ]
;;

let variants (f : Core.Facts.t) (views : view list)
  : (string * (string * Emit.ty list) list) list
  =
  List.map
    (positions f @ sums f views)
    ~f:(fun (name, arms) -> name, List.map arms ~f:(fun a -> a.ctor, [ arm_ty a ]))
;;

(* -- the support module ------------------------------------------------------------ *)

let support (name : string) : string = Core.Manifest.view_support ^ "." ^ name
let some (e : Emit.expr) : Emit.expr = Emit.econstruct "Some" [ e ]
let none : Emit.expr = Emit.econstruct "None" []
let deref (name : string) : Emit.expr = Emit.ecall "!" [ Emit.evar name ]

(* A child goes to the slot it last filled while that slot repeats and takes
   it, and otherwise to the first slot further on that takes it. A child no
   slot takes is trivia, an error node, or one of the frame's own tokens, and
   no accessor reads it. *)
let walk : Emit.item =
  let push (slot : Emit.expr) : Emit.expr =
    Emit.ecall
      "Array.set"
      [ Emit.evar "slots"
      ; slot
      ; Emit.econstruct
          "::"
          [ Emit.evar "elem"; Emit.ecall "Array.get" [ Emit.evar "slots"; slot ] ]
      ]
  in
  let count = Emit.ecall "Array.length" [ Emit.evar "repeats" ] in
  let fits (slot : Emit.expr) : Emit.expr =
    Emit.ecall "fits" [ slot; Emit.evar "kind" ]
  in
  let scan =
    Emit.elet
      "slot"
      ~body:(Emit.ecall "ref" [ Emit.ecall "+" [ deref "last"; Emit.eint 1 ] ])
      ~rest:
        (Emit.eseq
           [ Emit.ewhile
               ~condition:
                 (Emit.eand
                    ~left:(Emit.eless ~left:(deref "slot") ~right:count)
                    ~right:(Emit.enot (fits (deref "slot"))))
               ~body:(Emit.ecall "incr" [ Emit.evar "slot" ])
           ; Emit.ewhen
               ~condition:(Emit.eless ~left:(deref "slot") ~right:count)
               ~then_:
                 (Emit.eseq
                    [ push (deref "slot")
                    ; Emit.ecall ":=" [ Emit.evar "last"; deref "slot" ]
                    ])
           ])
  in
  let step =
    Emit.elambda
      [ Emit.arg_var "elem" ]
      (Emit.elet
         "kind"
         ~body:(Emit.ecall "Siesta.Syntax.elem_kind" [ Emit.evar "elem" ])
         ~rest:
           (Emit.eif
              ~condition:
                (Emit.eand
                   ~left:(Emit.egreater_equal ~left:(deref "last") ~right:(Emit.eint 0))
                   ~right:
                     (Emit.eand
                        ~left:
                          (Emit.ecall "Array.get" [ Emit.evar "repeats"; deref "last" ])
                        ~right:(fits (deref "last"))))
              ~then_:(push (deref "last"))
              ~else_:scan))
  in
  Emit.ilet
    "walk"
    ~args:[ Emit.arg_var "fits"; Emit.arg_var "repeats"; Emit.arg_var "node" ]
    (Emit.econstraint
       (Emit.elet
          "slots"
          ~body:(Emit.ecall "Array.make" [ count; Emit.econstruct "[]" [] ])
          ~rest:
            (Emit.elet
               "last"
               ~body:(Emit.ecall "ref" [ Emit.eint (-1) ])
               ~rest:
                 (Emit.eseq
                    [ Emit.ecall
                        "Array.iter"
                        [ step
                        ; Emit.ecall "Siesta.Syntax.children_array" [ Emit.evar "node" ]
                        ]
                    ; Emit.ecall "Array.map" [ Emit.evar "List.rev"; Emit.evar "slots" ]
                    ])))
       (Emit.tcon "array" [ Emit.tcon "list" [ elem_t ] ]))
;;

let helpers : Emit.item list =
  [ walk
  ; Emit.ilet
      "cast"
      ~args:
        [ Emit.arg_var "kind"
        ; Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t"
        ]
      (Emit.eif
         ~condition:
           (Emit.eequal
              ~left:(Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "node" ])
              ~right:(Emit.evar "kind"))
         ~then_:(some (Emit.evar "node"))
         ~else_:none)
  ; Emit.ilet
      "token"
      ~args:[ Emit.arg_typed ~arg_name:"elem" ~type_path:"Siesta.Syntax.elem" ]
      (Emit.ematch
         (Emit.evar "elem")
         [ Emit.ecase
             (Emit.pconstruct "Siesta.Syntax.Token" [ Emit.pvar "token" ])
             (some (Emit.evar "token"))
         ; Emit.ecase (Emit.pconstruct "Siesta.Syntax.Node" [ Emit.pany ]) none
         ])
  ; Emit.ilet
      "node"
      ~args:
        [ Emit.arg_var "kind"
        ; Emit.arg_typed ~arg_name:"elem" ~type_path:"Siesta.Syntax.elem"
        ]
      (Emit.ematch
         (Emit.evar "elem")
         [ Emit.ecase
             (Emit.pconstruct "Siesta.Syntax.Node" [ Emit.pvar "node" ])
             (Emit.ecall "cast" [ Emit.evar "kind"; Emit.evar "node" ])
         ; Emit.ecase (Emit.pconstruct "Siesta.Syntax.Token" [ Emit.pany ]) none
         ])
    (* A required child the parse could not find holds a placeholder, which no
       reading takes. So [one] gives [None] there, and [many] leaves it out. *)
  ; Emit.ilet
      "one"
      ~args:[ Emit.arg_var "read"; Emit.arg_var "slots"; Emit.arg_var "slot" ]
      (Emit.ematch
         (Emit.ecall "Array.get" [ Emit.evar "slots"; Emit.evar "slot" ])
         [ Emit.ecase (Emit.pconstruct "[]" []) none
         ; Emit.ecase
             (Emit.pconstruct "::" [ Emit.pvar "elem"; Emit.pany ])
             (Emit.ecall "read" [ Emit.evar "elem" ])
         ])
  ; Emit.ilet
      "many"
      ~args:[ Emit.arg_var "read"; Emit.arg_var "slots"; Emit.arg_var "slot" ]
      (Emit.ecall
         "List.filter_map"
         [ Emit.evar "read"
         ; Emit.ecall "Array.get" [ Emit.evar "slots"; Emit.evar "slot" ]
         ])
  ]
;;

let kind_pattern (kinds : Ir.Kind.t list) : Emit.pat =
  match kinds with
  | [] -> invalid_arg "Views.kind_pattern: no kinds"
  | k :: rest -> Emit.por (Emit.pint k) (List.map rest ~f:Emit.pint)
;;

(* One function per variant, reading an element as an arm. A node of a kind no
   arm carries is a placeholder or a hole, and reads as [None]. *)
let reader ((name, arms) : string * arm list) : Emit.item =
  let tokens =
    List.filter_map arms ~f:(fun a ->
      match a.carries with
      | Arm_token k ->
        Some
          (Emit.ecase (Emit.pint k) (some (Emit.econstruct a.ctor [ Emit.evar "token" ])))
      | Arm_node _ | Arm_position _ -> None)
  in
  let nodes =
    List.filter_map arms ~f:(fun a ->
      match a.carries with
      | Arm_token _ -> None
      | Arm_node { kind = k; _ } ->
        Some
          (Emit.ecase (Emit.pint k) (some (Emit.econstruct a.ctor [ Emit.evar "node" ])))
      | Arm_position { kinds; position } ->
        Some
          (Emit.ecase
             (kind_pattern kinds)
             (Emit.ecall
                "Option.map"
                [ Emit.elambda
                    [ Emit.arg_var "position" ]
                    (Emit.econstruct a.ctor [ Emit.evar "position" ])
                ; Emit.ecall position [ Emit.evar "elem" ]
                ])))
  in
  let default = Emit.ecase Emit.pany none in
  Emit.ilet
    name
    ~args:[ Emit.arg_typed ~arg_name:"elem" ~type_path:"Siesta.Syntax.elem" ]
    (Emit.econstraint
       (Emit.ematch
          (Emit.evar "elem")
          [ Emit.ecase
              (Emit.pconstruct "Siesta.Syntax.Token" [ Emit.pvar "token" ])
              (if tokens = []
               then none
               else
                 Emit.ematch
                   (Emit.ecall "Siesta.Syntax.Token.kind" [ Emit.evar "token" ])
                   (tokens @ [ default ]))
          ; Emit.ecase
              (Emit.pconstruct "Siesta.Syntax.Node" [ Emit.pvar "node" ])
              (if nodes = []
               then none
               else
                 Emit.ematch
                   (Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "node" ])
                   (nodes @ [ default ]))
          ])
       (Emit.tcon "option" [ Emit.tcon name [] ]))
;;

(* Named after the view type, which no helper and no variant shares. *)
let slots (f : Core.Facts.t) (v : view) : Emit.item =
  let children = Array.to_list v.def.children in
  let fits =
    Emit.elambda
      [ Emit.arg_var "slot"; Emit.arg_var "kind" ]
      (Emit.ematch
         (Emit.etuple [ Emit.evar "slot"; Emit.evar "kind" ])
         (List.mapi children ~f:(fun i (c : Core.Rule.child) ->
            Emit.ecase
              (Emit.ptuple [ Emit.pint i; kind_pattern (admits f v.def c) ])
              (Emit.ebool true))
          @ [ Emit.ecase Emit.pany (Emit.ebool false) ]))
  in
  Emit.ilet
    v.view_type
    ~args:[ Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t" ]
    (Emit.ecall
       "walk"
       [ fits
       ; Emit.earray (List.map children ~f:(fun c -> Emit.ebool (repeats c)))
       ; Emit.evar "node"
       ])
;;

(* Every view's walk, by the node's kind. No accessor reads it. A test walks
   any node through it, and a generated congruence finds its children with
   it, so it is the one helper in the signature. *)
let dispatch (views : view list) : Emit.item =
  Emit.ilet
    "slots"
    ~args:[ Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t" ]
    (Emit.econstraint
       (Emit.ematch
          (Emit.ecall "Siesta.Syntax.kind" [ Emit.evar "node" ])
          (List.map views ~f:(fun (v : view) ->
             Emit.ecase
               (Emit.pint v.view_kind)
               (some
                  (if Array.length v.def.children = 0
                   then Emit.earray []
                   else Emit.ecall v.view_type [ Emit.evar "node" ])))
           @ [ Emit.ecase Emit.pany none ]))
       (Emit.tcon "option" [ Emit.tcon "array" [ Emit.tcon "list" [ elem_t ] ] ]))
;;

let reading (f : Core.Facts.t) (v : view) (c : Core.Rule.child) : Emit.expr =
  match shape f v.def c with
  | Token -> Emit.evar (support "token")
  | Node { kind = k; _ } -> Emit.ecall (support "node") [ Emit.eint k ]
  | Position name | Sum { name; _ } -> Emit.evar (support name)
;;

(* -- the variant modules ----------------------------------------------------------- *)

(* A variant's module says what each arm is in the tree. Every arm is an
   element, and [syntax] is there too where no arm is a token.

   A variant that nests a block's position reaches it through that position's
   module. Those come first, and they nest nothing. *)
let all_nodes (arms : arm list) : bool =
  List.for_all arms ~f:(fun a ->
    match a.carries with
    | Arm_token _ -> false
    | Arm_node _ | Arm_position _ -> true)
;;

let variant_items ((name, arms) : string * arm list) : Emit.item list =
  let over (read : arm -> Emit.expr) : Emit.expr =
    Emit.ematch
      (Emit.evar "variant")
      (List.map arms ~f:(fun a ->
         Emit.ecase (Emit.pconstruct a.ctor [ Emit.pvar "x" ]) (read a)))
  in
  let position_syntax (position : string) : Emit.expr =
    Emit.ecall (Core.Manifest.view_module position ^ ".syntax") [ Emit.evar "x" ]
  in
  let elem =
    over (fun a ->
      match a.carries with
      | Arm_token _ -> Emit.econstruct "Siesta.Syntax.Token" [ Emit.evar "x" ]
      | Arm_node _ -> Emit.econstruct "Siesta.Syntax.Node" [ Emit.evar "x" ]
      | Arm_position { position; _ } ->
        Emit.econstruct "Siesta.Syntax.Node" [ position_syntax position ])
  in
  let syntax () =
    over (fun a ->
      match a.carries with
      | Arm_token _ -> invalid_arg "Views.variant_items: a token arm has no node"
      | Arm_node _ -> Emit.evar "x"
      | Arm_position { position; _ } -> position_syntax position)
  in
  let fn (fn_name : string) (body : Emit.expr) (result : Emit.ty) : Emit.item =
    Emit.ilet
      fn_name
      ~args:[ Emit.arg_typed ~arg_name:"variant" ~type_path:"t" ]
      (Emit.econstraint body result)
  in
  Emit.itype_alias "t" (Emit.tcon name [])
  :: fn "elem" elem elem_t
  :: (if all_nodes arms then [ fn "syntax" (syntax ()) syntax_t ] else [])
;;

let variant_sig ((name, arms) : string * arm list) : Emit.sig_item =
  let fn (fn_name : string) (result : Emit.ty) : Emit.sig_item =
    Emit.sval fn_name (Emit.tarrow ~domain:(Emit.tcon "t" []) ~codomain:result)
  in
  Emit.smodule
    (Core.Manifest.view_module name)
    (Emit.stype_alias "t" (Emit.tcon name [])
     :: fn "elem" elem_t
     :: (if all_nodes arms then [ fn "syntax" syntax_t ] else []))
;;

(* -- generation ------------------------------------------------------------------- *)

(* Every name in a view module is reached through [Slots] or [Siesta]. The
   modules before it are the grammar's own, and a production called [Array]
   would otherwise hide the standard one from the modules after it. *)
let view_module (f : Core.Facts.t) (v : view) : Emit.item =
  let accessors =
    List.mapi (Array.to_list v.def.children) ~f:(fun i (c : Core.Rule.child) ->
      Emit.ilet
        (Core.Manifest.view_accessor (Core.Grammar.Name.Child.to_string c.child_name))
        ~args:[ Emit.arg_typed ~arg_name:"view" ~type_path:"t" ]
        (Emit.econstraint
           (Emit.ecall
              (support (if repeats c then "many" else "one"))
              [ reading f v c
              ; Emit.ecall (support v.view_type) [ Emit.evar "view" ]
              ; Emit.eint i
              ])
           (accessor_ty f v c)))
  in
  Emit.imodule
    v.module_name
    ([ Emit.itype_alias "t" (Emit.tcon v.view_type [])
     ; Emit.ilet
         "cast"
         ~args:[ Emit.arg_typed ~arg_name:"node" ~type_path:"Siesta.Syntax.t" ]
         (Emit.econstraint
            (Emit.ecall (support "cast") [ Emit.eint v.view_kind; Emit.evar "node" ])
            (Emit.tcon "option" [ Emit.tcon "t" [] ]))
     ; Emit.ilet
         "syntax"
         ~args:[ Emit.arg_typed ~arg_name:"view" ~type_path:"t" ]
         (Emit.econstraint (Emit.evar "view") syntax_t)
     ]
     @ accessors)
;;

let generate (f : Core.Facts.t) : Emit.item list =
  let views = views_of f in
  List.map views ~f:(fun (v : view) -> Emit.itype_alias v.view_type syntax_t)
  @ List.map (variants f views) ~f:(fun (name, ctors) -> Emit.itype_variant name ctors)
  @ [ Emit.imodule
        Core.Manifest.view_support
        (helpers
         @ List.map (positions f @ sums f views) ~f:reader
         @ List.filter_map views ~f:(fun (v : view) ->
           if Array.length v.def.children = 0 then None else Some (slots f v))
         @ [ dispatch views ])
    ]
  @ List.map
      (positions f @ sums f views)
      ~f:(fun ((name, _) as variant) ->
        Emit.imodule (Core.Manifest.view_module name) (variant_items variant))
  @ List.map views ~f:(view_module f)
;;

let signature (f : Core.Facts.t) : Emit.sig_item list =
  let views = views_of f in
  List.map views ~f:(fun (v : view) -> Emit.stype_private v.view_type syntax_t)
  @ List.map (variants f views) ~f:(fun (name, ctors) -> Emit.stype_variant name ctors)
  @ [ Emit.smodule
        Core.Manifest.view_support
        [ Emit.sval
            "slots"
            (Emit.tarrow
               ~domain:syntax_t
               ~codomain:
                 (Emit.tcon
                    "option"
                    [ Emit.tcon "array" [ Emit.tcon "list" [ elem_t ] ] ]))
        ]
    ]
  @ List.map (positions f @ sums f views) ~f:variant_sig
  @ List.map views ~f:(fun (v : view) ->
    Emit.smodule
      v.module_name
      ([ Emit.stype_alias "t" (Emit.tcon v.view_type [])
       ; Emit.sval
           "cast"
           (Emit.tarrow
              ~domain:syntax_t
              ~codomain:(Emit.tcon "option" [ Emit.tcon "t" [] ]))
       ; Emit.sval "syntax" (Emit.tarrow ~domain:(Emit.tcon "t" []) ~codomain:syntax_t)
       ]
       @ List.map (Array.to_list v.def.children) ~f:(fun (c : Core.Rule.child) ->
         Emit.sval
           (Core.Manifest.view_accessor (Core.Grammar.Name.Child.to_string c.child_name))
           (Emit.tarrow ~domain:(Emit.tcon "t" []) ~codomain:(accessor_ty f v c)))))
;;
