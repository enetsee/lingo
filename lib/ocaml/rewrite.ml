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

(* -- generation -------------------------------------------------------------- *)

let generate ~(views : string) (f : Core.Facts.t) : Emit.item list =
  let modules = Views.modules f in
  List.map modules ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
    Emit.imodule module_name [ congr ~views f d ])
  @ [ probe f modules ]
;;

let signature (f : Core.Facts.t) : Emit.sig_item list =
  List.map (Views.modules f) ~f:(fun ((module_name : string), (d : Core.Rule.def)) ->
    Emit.smodule module_name [ Emit.sval "congr" (congr_t f d) ])
;;
