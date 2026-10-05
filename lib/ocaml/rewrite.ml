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
  match d.frame, children with
  | (Core.Rule.Plain | Core.Rule.Committed _), children -> Ok (List.mapi children ~f:slot)
  | Core.Rule.Delimited { open_; close; sep; _ }, [ body ] ->
    (match fixed_text f open_, fixed_text f close with
     | Some opener, Some closer ->
       let body =
         match sep with
         | None -> Ok (slot 0 body)
         | Some sep -> separated 0 sep body
       in
       Result.map
         (fun (body : Emit.expr) ->
            [ delimiter open_ opener; body; delimiter close closer ])
         body
     | None, _ | _, None -> Error "a delimiter has no fixed text")
  | Core.Rule.Separated { sep_tok; leading; trailing; position; _ }, [ body ] ->
    Result.map
      (fun (body : Emit.expr) -> [ body ])
      (separated 0 { Core.Rule.sep_tok; leading; trailing; position } body)
  | (Core.Rule.Delimited _ | Core.Rule.Separated _), _ ->
    invalid_arg "Rewrite.frame: a framed production with other than one child"
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

let productions (f : Core.Facts.t) : (string * Core.Rule.def) list =
  List.filter (Views.modules f) ~f:(fun ((_ : string), (d : Core.Rule.def)) ->
    match d.origin with
    | Core.Rule.User -> true
    | Core.Rule.Pratt_block | Core.Rule.Pratt_role _ -> false)
;;

(* -- generation -------------------------------------------------------------- *)

let generate ~(views : string) (f : Core.Facts.t) : Emit.item list =
  let modules = Views.modules f in
  let productions = productions f in
  List.map modules ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
    Emit.imodule
      module_name
      (congr ~views f d
       ::
       (if List.mem_assoc module_name ~map:productions
        then [ make ~views f module_name d ]
        else [])))
  @ [ probe f modules; rebuild ~views f productions ]
;;

let signature ~(views : string) (f : Core.Facts.t) : Emit.sig_item list =
  let productions = productions f in
  List.map (Views.modules f) ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
    Emit.smodule
      module_name
      (Emit.sval "congr" (congr_t f d)
       ::
       (if List.mem_assoc module_name ~map:productions
        then [ Emit.sval "make" (make_t ~views f module_name d) ]
        else [])))
;;
