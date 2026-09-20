module Ast = Ppxlib.Ast_builder.Make (struct
    let loc = Ppxlib.Location.none
  end)

type expr = Ppxlib.expression
type pat = Ppxlib.pattern
type item = Ppxlib.structure_item
type sig_item = Ppxlib.signature_item
type ty = Ppxlib.core_type
type case = Ppxlib.case

type arg =
  | Plain of pat
  | Named of string
  | Named_pat of string * pat
  | Opt of string * expr option

(* [String.split_on_char] gives a non-empty list whatever it is given, so
   there is no empty case to report. A path of [""] gives the identifier [""],
   which the OCaml printer writes back and the compiler then rejects: a name
   an emitter got wrong is caught where it is read rather than here. *)
let longident (s : string) : Ppxlib.longident =
  match String.split_on_char '.' s with
  | [] -> Ppxlib.Longident.Lident s
  | first :: rest ->
    List.fold_left
      (fun acc part -> Ppxlib.Longident.Ldot (acc, part))
      (Ppxlib.Longident.Lident first)
      rest
;;

let lid (s : string) : Ppxlib.longident Ppxlib.loc =
  { txt = longident s; loc = Ppxlib.Location.none }
;;

let name (s : string) : string Ppxlib.loc = { txt = s; loc = Ppxlib.Location.none }

(* -- expressions ----------------------------------------------------------- *)

let evar (s : Ppxlib.label) : expr = Ast.pexp_ident (lid s)
let eint (n : int) : expr = Ast.eint n
let estr (s : Ppxlib.label) : expr = Ast.estring s

let ebool (b : bool) : expr =
  Ast.pexp_construct (lid (if b then "true" else "false")) None
;;

let eunit : expr = Ast.eunit

let efield (record : expr) (field : Ppxlib.label) : expr =
  Ast.pexp_field record (lid field)
;;

let earray (exprs : expr list) : expr = Ast.pexp_array exprs

let erecord (fields : (Ppxlib.label * expr) list) : expr =
  Ast.pexp_record (List.map (fun (field, expr) -> lid field, expr) fields) None
;;

(* OCaml has no one-tuple, and the empty tuple is [()], so both give
   something rather than failing. *)
let etuple (exprs : expr list) : expr =
  match exprs with
  | [] -> eunit
  | [ x ] -> x
  | xs -> Ast.pexp_tuple xs
;;

let econstruct (ctor : Ppxlib.label) (args : expr list) : expr =
  let payload =
    match args with
    | [] -> None
    | [ arg ] -> Some arg
    | args -> Some (Ast.pexp_tuple args)
  in
  Ast.pexp_construct (lid ctor) payload
;;

(* [\[ a; b \]] is [::] cells ending in [\[\]]. *)
let elist (exprs : expr list) : expr =
  List.fold_right
    (fun expr acc -> econstruct "::" [ expr; acc ])
    exprs
    (econstruct "[]" [])
;;

let eapply_labelled (fn : expr) (args : (Ppxlib.arg_label * expr) list) : expr =
  Ast.pexp_apply fn args
;;

let eapply (fn : expr) (args : expr list) : expr =
  eapply_labelled fn (List.map (fun arg -> Ppxlib.Nolabel, arg) args)
;;

let ecall (fn : Ppxlib.label) (args : expr list) : expr = eapply (evar fn) args
let econstraint (expr : expr) (type_ : ty) : expr = Ast.pexp_constraint expr type_
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
  Ast.pexp_ifthenelse condition then_ (Some else_)
;;

let ewhen ~(condition : expr) ~(then_ : expr) : expr =
  Ast.pexp_ifthenelse condition then_ None
;;

(* Right-nested, so the printer writes [a; b; c] rather than parenthesising
   every step. *)
let rec eseq (exprs : expr list) : expr =
  match exprs with
  | [] -> eunit
  | [ expr ] -> expr
  | expr :: rest -> Ast.pexp_sequence expr (eseq rest)
;;

let ewhile ~(condition : expr) ~(body : expr) : expr = Ast.pexp_while condition body

let ecase ?(guard : expr option) (pattern : pat) (body : expr) : case =
  Ast.case ~lhs:pattern ~guard ~rhs:body
;;

let ematch (scrutinee : expr) (cases : case list) : expr = Ast.pexp_match scrutinee cases

(* -- functions ------------------------------------------------------------- *)

let arg_var (arg_name : Ppxlib.label) : arg = Plain (Ast.ppat_var (name arg_name))
let arg_any : arg = Plain Ast.ppat_any

let arg_typed ~(arg_name : Ppxlib.label) ~(type_path : Ppxlib.label) : arg =
  Plain
    (Ast.ppat_constraint
       (Ast.ppat_var (name arg_name))
       (Ast.ptyp_constr (lid type_path) []))
;;

let elambda (args : arg list) (body : expr) : expr =
  let wrap (arg : arg) (body : expr) : expr =
    match arg with
    | Plain pattern -> Ast.pexp_fun Nolabel None pattern body
    | Named arg_name ->
      Ast.pexp_fun (Labelled arg_name) None (Ast.ppat_var (name arg_name)) body
    | Named_pat (arg_name, pattern) -> Ast.pexp_fun (Labelled arg_name) None pattern body
    | Opt (arg_name, default) ->
      Ast.pexp_fun (Optional arg_name) default (Ast.ppat_var (name arg_name)) body
  in
  List.fold_right wrap args body
;;

let ethunk (body : expr) : expr =
  elambda [ Plain (Ast.ppat_construct (lid "()") None) ] body
;;

let bindings (defs : (Ppxlib.label * arg list * expr) list) : Ppxlib.value_binding list =
  List.map
    (fun (def_name, args, body) ->
       let body = if args = [] then body else elambda args body in
       Ast.value_binding ~pat:(Ast.ppat_var (name def_name)) ~expr:body)
    defs
;;

let elet ?(rec_ : bool = false) (name : Ppxlib.label) ~(body : expr) ~(rest : expr) : expr
  =
  Ast.pexp_let
    (if rec_ then Recursive else Nonrecursive)
    (bindings [ name, [], body ])
    rest
;;

let elet_rec (defs : (Ppxlib.label * arg list * expr) list) (rest : expr) : expr =
  match defs with
  | [] -> rest
  | defs -> Ast.pexp_let Recursive (bindings defs) rest
;;

(* -- patterns -------------------------------------------------------------- *)

let pvar (var_name : Ppxlib.label) : pat = Ast.ppat_var (name var_name)
let pany : pat = Ast.ppat_any
let pint (n : int) : pat = Ast.pint n
let pstr (s : Ppxlib.label) : pat = Ast.pstring s
let pchar (c : char) : pat = Ast.pchar c

let pchar_range ~(lo : char) ~(hi : char) : pat =
  Ast.ppat_interval (Pconst_char lo) (Pconst_char hi)
;;

let pconstruct (ctor : Ppxlib.label) (args : pat list) : pat =
  let payload =
    match args with
    | [] -> None
    | [ arg ] -> Some arg
    | args -> Some (Ast.ppat_tuple args)
  in
  Ast.ppat_construct (lid ctor) payload
;;

let ptuple (pats : pat list) : pat =
  match pats with
  | [] -> Ast.ppat_construct (lid "()") None
  | [ pattern ] -> pattern
  | pats -> Ast.ppat_tuple pats
;;

let por (first : pat) (rest : pat list) : pat = List.fold_left Ast.ppat_or first rest

(* -- structure items ------------------------------------------------------- *)

let ilet
      ?(rec_ : bool = false)
      ?(args : arg list = [])
      (name : Ppxlib.label)
      (body : expr)
  : item
  =
  Ast.pstr_value
    (if rec_ then Recursive else Nonrecursive)
    (bindings [ name, args, body ])
;;

let ilet_rec (defs : (Ppxlib.label * arg list * expr) list) : item =
  Ast.pstr_value Recursive (bindings defs)
;;

let iopen (path : Ppxlib.label) : item =
  Ast.pstr_open (Ast.open_infos ~expr:(Ast.pmod_ident (lid path)) ~override:Fresh)
;;

let imodule (name : Ppxlib.label) (items : item list) : item =
  Ast.pstr_module
    (Ast.module_binding
       ~name:{ txt = Some name; loc = Ppxlib.Location.none }
       ~expr:(Ast.pmod_structure items))
;;

let variant (constructors : (Ppxlib.label * ty list) list)
  : Ppxlib.constructor_declaration list
  =
  List.map
    (fun (ctor, args) ->
       Ast.constructor_declaration ~name:(name ctor) ~args:(Pcstr_tuple args) ~res:None)
    constructors
;;

let record (fields : (Ppxlib.label * ty) list) : Ppxlib.label_declaration list =
  List.map
    (fun (field, ty) ->
       Ast.label_declaration ~name:(name field) ~mutable_:Immutable ~type_:ty)
    fields
;;

let declaration ~(name : Ppxlib.label) ~(kind : Ppxlib.type_kind) ~(manifest : ty option)
  : Ppxlib.type_declaration
  =
  Ast.type_declaration
    ~name:(Ppxlib.Loc.make ~loc:Ppxlib.Location.none name)
    ~params:[]
    ~cstrs:[]
    ~kind
    ~private_:Public
    ~manifest
;;

let itype_variant (name : Ppxlib.label) (constructors : (Ppxlib.label * ty list) list)
  : item
  =
  Ast.pstr_type
    Recursive
    [ declaration ~name ~kind:(Ptype_variant (variant constructors)) ~manifest:None ]
;;

let itype_alias (name : Ppxlib.label) (manifest : ty) : item =
  Ast.pstr_type
    Nonrecursive
    [ declaration ~name ~kind:Ptype_abstract ~manifest:(Some manifest) ]
;;

let itype_record (name : Ppxlib.label) (fields : (Ppxlib.label * ty) list) : item =
  Ast.pstr_type
    Recursive
    [ declaration ~name ~kind:(Ptype_record (record fields)) ~manifest:None ]
;;

(* -- types ----------------------------------------------------------------- *)

let tcon (path : Ppxlib.label) (args : ty list) : ty = Ast.ptyp_constr (lid path) args
let ttuple (tys : ty list) : ty = Ast.ptyp_tuple tys
let tarrow ~(domain : ty) ~(codomain : ty) : ty = Ast.ptyp_arrow Nolabel domain codomain

let tarrow_labelled (label : Ppxlib.label) ~(domain : ty) ~(codomain : ty) : ty =
  Ast.ptyp_arrow (Labelled label) domain codomain
;;

let tarrow_optional (label : Ppxlib.label) ~(domain : ty) ~(codomain : ty) : ty =
  Ast.ptyp_arrow (Optional label) domain codomain
;;

(* -- signature items ------------------------------------------------------- *)

let sval (name : Ppxlib.label) (type_ : ty) : sig_item =
  Ast.psig_value
    (Ast.value_description
       ~name:(Ppxlib.Loc.make ~loc:Ppxlib.Location.none name)
       ~type_
       ~prim:[])
;;

let stype_variant (name : Ppxlib.label) (constructors : (Ppxlib.label * ty list) list)
  : sig_item
  =
  Ast.psig_type
    Recursive
    [ declaration ~name ~kind:(Ptype_variant (variant constructors)) ~manifest:None ]
;;

let stype_alias (name : Ppxlib.label) (manifest : ty) : sig_item =
  Ast.psig_type
    Nonrecursive
    [ declaration ~name ~kind:Ptype_abstract ~manifest:(Some manifest) ]
;;

let stype_record (name : Ppxlib.label) (fields : (Ppxlib.label * ty) list) : sig_item =
  Ast.psig_type
    Recursive
    [ declaration ~name ~kind:(Ptype_record (record fields)) ~manifest:None ]
;;

let stype_abstract (name : Ppxlib.label) : sig_item =
  Ast.psig_type Nonrecursive [ declaration ~name ~kind:Ptype_abstract ~manifest:None ]
;;

let smodule (name : Ppxlib.label) (items : sig_item list) : sig_item =
  Ast.psig_module
    (Ast.module_declaration
       ~name:{ txt = Some name; loc = Ppxlib.Location.none }
       ~type_:(Ast.pmty_signature items))
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

let render (items : item list) : string = print Ppxlib.Pprintast.structure items

let render_signature (items : sig_item list) : string =
  print Ppxlib.Pprintast.signature items
;;
