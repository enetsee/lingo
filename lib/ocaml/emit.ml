open Ppxlib

module B = Ast_builder.Make (struct
    let loc = Location.none
  end)

type expr = expression
type pat = pattern
type item = structure_item
type sig_item = signature_item
type ty = core_type
type case = Ppxlib.case

type arg =
  | Plain of pat
  | Named of string
  | Named_pat of string * pat
  | Opt of string * expr option

(* [String.split_on_char] answers a non-empty list whatever it is given, so
   there is no empty case to report. A path of [""] gives the identifier [""],
   which the OCaml printer writes back and the compiler then rejects: a name
   an emitter got wrong is caught where it is read, not here. *)
let longident (s : string) : longident =
  match String.split_on_char '.' s with
  | [] -> Longident.Lident s
  | first :: rest ->
    List.fold_left
      (fun acc part -> Longident.Ldot (acc, part))
      (Longident.Lident first)
      rest
;;

let lid (s : string) : longident loc = { txt = longident s; loc = Location.none }
let name (s : string) : string loc = { txt = s; loc = Location.none }

(* -- expressions ----------------------------------------------------------- *)

let evar (s : label) : expr = B.pexp_ident (lid s)
let eint (n : int) : expr = B.eint n
let estr (s : label) : expr = B.estring s
let ebool (b : bool) : expr = B.pexp_construct (lid (if b then "true" else "false")) None
let eunit : expr = B.eunit
let efield (record : expr) (field : label) : expr = B.pexp_field record (lid field)
let earray (exprs : expr list) : expr = B.pexp_array exprs

let erecord (fields : (label * expr) list) : expr =
  B.pexp_record (List.map (fun (field, expr) -> lid field, expr) fields) None
;;

(* OCaml has no one-tuple, and the empty tuple is [()], so both answer
   something rather than failing. *)
let etuple (exprs : expr list) : expr =
  match exprs with
  | [] -> eunit
  | [ x ] -> x
  | xs -> B.pexp_tuple xs
;;

let econstruct (ctor : label) (args : expr list) : expr =
  let payload =
    match args with
    | [] -> None
    | [ arg ] -> Some arg
    | args -> Some (B.pexp_tuple args)
  in
  B.pexp_construct (lid ctor) payload
;;

(* [\[ a; b \]] is [::] cells ending in [\[\]]. *)
let elist (exprs : expr list) : expr =
  List.fold_right
    (fun expr acc -> econstruct "::" [ expr; acc ])
    exprs
    (econstruct "[]" [])
;;

let eapply_labelled (fn : expr) (args : (arg_label * expr) list) : expr =
  B.pexp_apply fn args
;;

let eapply (fn : expr) (args : expr list) : expr =
  eapply_labelled fn (List.map (fun arg -> Nolabel, arg) args)
;;

let ecall (fn : label) (args : expr list) : expr = eapply (evar fn) args
let eand ~(left : expr) ~(right : expr) : expr = ecall "&&" [ left; right ]
let eor ~(left : expr) ~(right : expr) : expr = ecall "||" [ left; right ]
let enot (operand : expr) : expr = ecall "not" [ operand ]
let eequal ~(left : expr) ~(right : expr) : expr = ecall "=" [ left; right ]
let enot_equal ~(left : expr) ~(right : expr) : expr = ecall "<>" [ left; right ]
let eless ~(left : expr) ~(right : expr) : expr = ecall "<" [ left; right ]
let eless_equal ~(left : expr) ~(right : expr) : expr = ecall "<=" [ left; right ]
let egreater ~(left : expr) ~(right : expr) : expr = ecall ">" [ left; right ]
let egreater_equal ~(left : expr) ~(right : expr) : expr = ecall ">=" [ left; right ]

let eif ~(condition : expr) ~(then_ : expr) ~(else_ : expr) : expr =
  B.pexp_ifthenelse condition then_ (Some else_)
;;

let ewhen ~(condition : expr) ~(then_ : expr) : expr =
  B.pexp_ifthenelse condition then_ None
;;

(* Right-nested, so the printer writes [a; b; c] rather than parenthesising
   every step. *)
let rec eseq (exprs : expr list) : expr =
  match exprs with
  | [] -> eunit
  | [ expr ] -> expr
  | expr :: rest -> B.pexp_sequence expr (eseq rest)
;;

let ewhile ~(condition : expr) ~(body : expr) : expr = B.pexp_while condition body

let ecase ?(guard : expr option) (pattern : pat) (body : expr) : case =
  B.case ~lhs:pattern ~guard ~rhs:body
;;

let ematch (scrutinee : expr) (cases : case list) : expr = B.pexp_match scrutinee cases

(* -- functions ------------------------------------------------------------- *)

let arg_var (arg_name : label) : arg = Plain (B.ppat_var (name arg_name))
let arg_any : arg = Plain B.ppat_any

let arg_typed ~(arg_name : label) ~(type_path : label) : arg =
  Plain
    (B.ppat_constraint (B.ppat_var (name arg_name)) (B.ptyp_constr (lid type_path) []))
;;

let elambda (args : arg list) (body : expr) : expr =
  let wrap (arg : arg) (body : expr) : expr =
    match arg with
    | Plain pattern -> B.pexp_fun Nolabel None pattern body
    | Named arg_name ->
      B.pexp_fun (Labelled arg_name) None (B.ppat_var (name arg_name)) body
    | Named_pat (arg_name, pattern) -> B.pexp_fun (Labelled arg_name) None pattern body
    | Opt (arg_name, default) ->
      B.pexp_fun (Optional arg_name) default (B.ppat_var (name arg_name)) body
  in
  List.fold_right wrap args body
;;

let ethunk (body : expr) : expr =
  elambda [ Plain (B.ppat_construct (lid "()") None) ] body
;;

let bindings (defs : (label * arg list * expr) list) : value_binding list =
  List.map
    (fun (def_name, args, body) ->
       let body = if args = [] then body else elambda args body in
       B.value_binding ~pat:(B.ppat_var (name def_name)) ~expr:body)
    defs
;;

let elet ?(rec_ : bool = false) (name : label) ~(body : expr) ~(rest : expr) : expr =
  B.pexp_let (if rec_ then Recursive else Nonrecursive) (bindings [ name, [], body ]) rest
;;

let elet_rec (defs : (label * arg list * expr) list) (rest : expr) : expr =
  match defs with
  | [] -> rest
  | defs -> B.pexp_let Recursive (bindings defs) rest
;;

(* -- patterns -------------------------------------------------------------- *)

let pvar (var_name : label) : pat = B.ppat_var (name var_name)
let pany : pat = B.ppat_any
let pint (n : int) : pat = B.pint n
let pstr (s : label) : pat = B.pstring s

let pconstruct (ctor : label) (args : pat list) : pat =
  let payload =
    match args with
    | [] -> None
    | [ arg ] -> Some arg
    | args -> Some (B.ppat_tuple args)
  in
  B.ppat_construct (lid ctor) payload
;;

let ptuple (pats : pat list) : pat =
  match pats with
  | [] -> B.ppat_construct (lid "()") None
  | [ pattern ] -> pattern
  | pats -> B.ppat_tuple pats
;;

let por (first : pat) (rest : pat list) : pat = List.fold_left B.ppat_or first rest

(* -- structure items ------------------------------------------------------- *)

let ilet ?(rec_ : bool = false) ?(args : arg list = []) (name : label) (body : expr)
  : item
  =
  B.pstr_value (if rec_ then Recursive else Nonrecursive) (bindings [ name, args, body ])
;;

let ilet_rec (defs : (label * arg list * expr) list) : item =
  B.pstr_value Recursive (bindings defs)
;;

let iopen (path : label) : item =
  B.pstr_open (B.open_infos ~expr:(B.pmod_ident (lid path)) ~override:Fresh)
;;

let imodule (name : label) (items : item list) : item =
  B.pstr_module
    (B.module_binding
       ~name:{ txt = Some name; loc = Location.none }
       ~expr:(B.pmod_structure items))
;;

let variant (constructors : (label * ty list) list) : constructor_declaration list =
  List.map
    (fun (ctor, args) ->
       B.constructor_declaration ~name:(name ctor) ~args:(Pcstr_tuple args) ~res:None)
    constructors
;;

let record (fields : (label * ty) list) : label_declaration list =
  List.map
    (fun (field, ty) ->
       B.label_declaration ~name:(name field) ~mutable_:Immutable ~type_:ty)
    fields
;;

let declaration ~(name : label) ~(kind : type_kind) ~(manifest : ty option)
  : type_declaration
  =
  B.type_declaration
    ~name:(Ppxlib.Loc.make ~loc:Location.none name)
    ~params:[]
    ~cstrs:[]
    ~kind
    ~private_:Public
    ~manifest
;;

let itype_variant (name : label) (constructors : (label * ty list) list) : item =
  B.pstr_type
    Recursive
    [ declaration ~name ~kind:(Ptype_variant (variant constructors)) ~manifest:None ]
;;

let itype_alias (name : label) (manifest : ty) : item =
  B.pstr_type
    Nonrecursive
    [ declaration ~name ~kind:Ptype_abstract ~manifest:(Some manifest) ]
;;

let itype_record (name : label) (fields : (label * ty) list) : item =
  B.pstr_type
    Recursive
    [ declaration ~name ~kind:(Ptype_record (record fields)) ~manifest:None ]
;;

(* -- types ----------------------------------------------------------------- *)

let tcon (path : label) (args : ty list) : ty = B.ptyp_constr (lid path) args
let ttuple (tys : ty list) : ty = B.ptyp_tuple tys
let tarrow ~(domain : ty) ~(codomain : ty) : ty = B.ptyp_arrow Nolabel domain codomain

let tarrow_labelled (label : label) ~(domain : ty) ~(codomain : ty) : ty =
  B.ptyp_arrow (Labelled label) domain codomain
;;

let tarrow_optional (label : label) ~(domain : ty) ~(codomain : ty) : ty =
  B.ptyp_arrow (Optional label) domain codomain
;;

(* -- signature items ------------------------------------------------------- *)

let sval (name : label) (type_ : ty) : sig_item =
  B.psig_value
    (B.value_description ~name:(Ppxlib.Loc.make ~loc:Location.none name) ~type_ ~prim:[])
;;

let stype_variant (name : label) (constructors : (label * ty list) list) : sig_item =
  B.psig_type
    Recursive
    [ declaration ~name ~kind:(Ptype_variant (variant constructors)) ~manifest:None ]
;;

let stype_alias (name : label) (manifest : ty) : sig_item =
  B.psig_type
    Nonrecursive
    [ declaration ~name ~kind:Ptype_abstract ~manifest:(Some manifest) ]
;;

let stype_record (name : label) (fields : (label * ty) list) : sig_item =
  B.psig_type
    Recursive
    [ declaration ~name ~kind:(Ptype_record (record fields)) ~manifest:None ]
;;

let stype_abstract (name : label) : sig_item =
  B.psig_type Nonrecursive [ declaration ~name ~kind:Ptype_abstract ~manifest:None ]
;;

let smodule (name : label) (items : sig_item list) : sig_item =
  B.psig_module
    (B.module_declaration
       ~name:{ txt = Some name; loc = Location.none }
       ~type_:(B.pmty_signature items))
;;

(* -- rendering ------------------------------------------------------------- *)

let header = "(* Generated by lingo. Do not edit. *)\n\n"

let print (pp : Format.formatter -> 'a -> unit) (tree : 'a) : string =
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer header;
  let formatter = Format.formatter_of_buffer buffer in
  pp formatter tree;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer
;;

let render (items : item list) : string = print Pprintast.structure items
let render_signature (items : sig_item list) : string = print Pprintast.signature items
