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

let evariant (tag : Ppxlib.label) (arg : expr option) : expr = Ast.pexp_variant tag arg

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

let pvariant (tag : Ppxlib.label) (arg : pat option) : pat = Ast.ppat_variant tag arg
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

let iinclude (path : Ppxlib.label) : item =
  Ast.pstr_include (Ast.include_infos (Ast.pmod_ident (lid path)))
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

let declaration
      ?(private_ : Ppxlib.private_flag = Public)
      ~(name : Ppxlib.label)
      ~(kind : Ppxlib.type_kind)
      ~(manifest : ty option)
      ()
  : Ppxlib.type_declaration
  =
  Ast.type_declaration
    ~name:(Ppxlib.Loc.make ~loc:Ppxlib.Location.none name)
    ~params:[]
    ~cstrs:[]
    ~kind
    ~private_
    ~manifest
;;

let itype_variant (name : Ppxlib.label) (constructors : (Ppxlib.label * ty list) list)
  : item
  =
  Ast.pstr_type
    Recursive
    [ declaration ~name ~kind:(Ptype_variant (variant constructors)) ~manifest:None () ]
;;

let itype_alias (name : Ppxlib.label) (manifest : ty) : item =
  Ast.pstr_type
    Nonrecursive
    [ declaration ~name ~kind:Ptype_abstract ~manifest:(Some manifest) () ]
;;

let itype_record (name : Ppxlib.label) (fields : (Ppxlib.label * ty) list) : item =
  Ast.pstr_type
    Recursive
    [ declaration ~name ~kind:(Ptype_record (record fields)) ~manifest:None () ]
;;

(* -- types ----------------------------------------------------------------- *)

let tcon (path : Ppxlib.label) (args : ty list) : ty = Ast.ptyp_constr (lid path) args
let ttuple (tys : ty list) : ty = Ast.ptyp_tuple tys
let tvar (name : Ppxlib.label) : ty = Ast.ptyp_var name

let tvariant (tags : (Ppxlib.label * ty option) list) : ty =
  Ast.ptyp_variant
    (List.map
       (fun (tag, arg) ->
          match arg with
          | None -> Ast.rtag (Ast.Located.mk tag) true []
          | Some ty -> Ast.rtag (Ast.Located.mk tag) false [ ty ])
       tags)
    Closed
    None
;;

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
    [ declaration ~name ~kind:(Ptype_variant (variant constructors)) ~manifest:None () ]
;;

let stype_alias (name : Ppxlib.label) (manifest : ty) : sig_item =
  Ast.psig_type
    Nonrecursive
    [ declaration ~name ~kind:Ptype_abstract ~manifest:(Some manifest) () ]
;;

let stype_record (name : Ppxlib.label) (fields : (Ppxlib.label * ty) list) : sig_item =
  Ast.psig_type
    Recursive
    [ declaration ~name ~kind:(Ptype_record (record fields)) ~manifest:None () ]
;;

let stype_private (name : Ppxlib.label) (manifest : ty) : sig_item =
  Ast.psig_type
    Nonrecursive
    [ declaration
        ~private_:Private
        ~name
        ~kind:Ptype_abstract
        ~manifest:(Some manifest)
        ()
    ]
;;

let stype_abstract (name : Ppxlib.label) : sig_item =
  Ast.psig_type Nonrecursive [ declaration ~name ~kind:Ptype_abstract ~manifest:None () ]
;;

let smodule (name : Ppxlib.label) (items : sig_item list) : sig_item =
  Ast.psig_module
    (Ast.module_declaration
       ~name:{ txt = Some name; loc = Ppxlib.Location.none }
       ~type_:(Ast.pmty_signature items))
;;

(* -- tidying -------------------------------------------------------------- *)

(* The variables a pattern binds. *)
let bound_by (pattern : pat) : string list =
  let names = ref [] in
  object
    inherit Ppxlib.Ast_traverse.iter as super

    method! pattern (p : pat) : unit =
      (match p.ppat_desc with
       | Ppat_var { txt; _ } | Ppat_alias (_, { txt; _ }) -> names := txt :: !names
       | _ -> ());
      super#pattern p
  end
    #pattern
    pattern;
  !names
;;

(* Every name an expression reads, counted, where the read is not of a
   variable bound inside what is being walked. A function's parameters, a
   case's pattern, a [let] and a [for] each bind one.

   [qualified] also counts a path by its last two parts, so
   [Lingo_views.Slots.many] reads [Slots.many]. That is how one module reads
   a value of another. A local variable has to leave paths out, or
   [Lingo_runtime.Token.kind] reads a parameter called [kind]. Two modules of
   the same name in different places count as one, which keeps a value that
   could have gone. Keeping too much costs a warning, and dropping too much
   costs a build. *)
let count_reads ~(qualified : bool) (table : (string, int) Hashtbl.t)
  : Ppxlib.Ast_traverse.iter
  =
  let read (n : string) : unit =
    Hashtbl.replace table n (1 + Option.value (Hashtbl.find_opt table n) ~default:0)
  in
  object (self)
    inherit Ppxlib.Ast_traverse.iter as super
    val mutable bound : string list = []

    method private within (names : string list) (visit : unit -> unit) : unit =
      let outer = bound in
      bound <- names @ bound;
      visit ();
      bound <- outer

    method! case (c : case) : unit =
      self#within (bound_by c.pc_lhs) (fun () ->
        Option.iter self#expression c.pc_guard;
        self#expression c.pc_rhs)

    method! expression (e : expr) : unit =
      match e.pexp_desc with
      | Pexp_ident { txt = Lident n; _ } -> if not (List.mem n bound) then read n
      | Pexp_ident { txt = Ldot (path, n); _ } ->
        if qualified then read (Ppxlib.Longident.last_exn path ^ "." ^ n)
      | Pexp_ident _ -> ()
      | Pexp_function (params, _, body) ->
        let names =
          List.concat_map
            (fun (p : Ppxlib.function_param) ->
               match p.pparam_desc with
               | Pparam_val (_, _, pattern) -> bound_by pattern
               | Pparam_newtype _ -> [])
            params
        in
        self#within names (fun () ->
          List.iter
            (fun (p : Ppxlib.function_param) ->
               match p.pparam_desc with
               | Pparam_val (_, default, _) -> Option.iter self#expression default
               | Pparam_newtype _ -> ())
            params;
          match body with
          | Pfunction_body body -> self#expression body
          | Pfunction_cases (cases, _, _) -> List.iter self#case cases)
      | Pexp_match (scrutinee, cases) | Pexp_try (scrutinee, cases) ->
        self#expression scrutinee;
        List.iter self#case cases
      | Pexp_let (flag, bindings, body) ->
        let names =
          List.concat_map (fun (b : Ppxlib.value_binding) -> bound_by b.pvb_pat) bindings
        in
        let bodies () =
          List.iter
            (fun (b : Ppxlib.value_binding) -> self#expression b.pvb_expr)
            bindings
        in
        (match flag with
         | Recursive -> self#within names bodies
         | Nonrecursive -> bodies ());
        self#within names (fun () -> self#expression body)
      | Pexp_for (index, low, high, _, body) ->
        self#expression low;
        self#expression high;
        self#within (bound_by index) (fun () -> self#expression body)
      | _ -> super#expression e
  end
;;

let reads_of ~(qualified : bool) (visit : Ppxlib.Ast_traverse.iter -> unit)
  : (string, int) Hashtbl.t
  =
  let table = Hashtbl.create 64 in
  visit (count_reads ~qualified table);
  table
;;

(* [pattern] with every variable [read] leaves out made [_]. *)
let blank (read : (string, int) Hashtbl.t) (pattern : pat) : pat =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! pattern (p : pat) : pat =
      match p.ppat_desc with
      | Ppat_var { txt; _ } when not (Hashtbl.mem read txt) ->
        { p with ppat_desc = Ppat_any }
      | _ -> super#pattern p
  end
    #pattern
    pattern
;;

let blank_case (c : case) : case =
  let read =
    reads_of ~qualified:false (fun it ->
      Option.iter it#expression c.pc_guard;
      it#expression c.pc_rhs)
  in
  { c with pc_lhs = blank read c.pc_lhs }
;;

(* A function's parameters and a match's cases are where a name is bound
   for a body to read. A [let] binds one too, and nothing here writes a local
   [let] that goes unread. *)
let blank_unread : Ppxlib.Ast_traverse.map =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! expression (e : expr) : expr =
      let e = super#expression e in
      match e.pexp_desc with
      | Pexp_function (params, constraint_, body) ->
        let body =
          match body with
          | Pfunction_body _ -> body
          | Pfunction_cases (cases, loc, attributes) ->
            Pfunction_cases (List.map blank_case cases, loc, attributes)
        in
        let read =
          reads_of ~qualified:false (fun it ->
            it#function_body body;
            List.iter
              (fun (p : Ppxlib.function_param) ->
                 match p.pparam_desc with
                 | Pparam_val (_, default, _) -> Option.iter it#expression default
                 | Pparam_newtype _ -> ())
              params)
        in
        let params =
          List.map
            (fun (p : Ppxlib.function_param) ->
               match p.pparam_desc with
               | Pparam_val (label, default, pattern) ->
                 { p with pparam_desc = Pparam_val (label, default, blank read pattern) }
               | Pparam_newtype _ -> p)
            params
        in
        { e with pexp_desc = Pexp_function (params, constraint_, body) }
      | Pexp_match (scrutinee, cases) ->
        { e with pexp_desc = Pexp_match (scrutinee, List.map blank_case cases) }
      | Pexp_try (body, cases) ->
        { e with pexp_desc = Pexp_try (body, List.map blank_case cases) }
      | _ -> e
  end
;;

let bound_name (binding : Ppxlib.value_binding) : string option =
  match binding.pvb_pat.ppat_desc with
  | Ppat_var { txt; _ } | Ppat_constraint ({ ppat_desc = Ppat_var { txt; _ }; _ }, _) ->
    Some txt
  | _ -> None
;;

(* One round of dropping, over the items of one module. A value inside a
   module is read by name from inside it, and by path from outside it. An
   [open] lets anything read it by name, so where the code has one, a read by
   name anywhere counts.

   A binding goes where nothing outside its own body reads it. Dropping one
   can leave another unread, so [tidy] runs this until a round drops
   nothing. *)
let rec drop_unread
          ~(paths : (string, int) Hashtbl.t)
          ~(everywhere : (string, int) Hashtbl.t option)
          ~(module_name : string option)
          (kept : string -> bool)
          (dropped : bool ref)
          (items : item list)
  : item list
  =
  let count (table : (string, int) Hashtbl.t) (key : string) : int =
    Option.value (Hashtbl.find_opt table key) ~default:0
  in
  let by_name =
    match everywhere with
    | Some table -> table
    | None -> reads_of ~qualified:false (fun it -> it#structure items)
  in
  List.filter_map
    (fun (item : item) ->
       match item.pstr_desc with
       | Pstr_value (flag, bindings) ->
         let live =
           List.filter
             (fun (binding : Ppxlib.value_binding) ->
                match bound_name binding with
                | None -> true
                | Some n when kept n -> true
                | Some n ->
                  let own =
                    reads_of ~qualified:false (fun it -> it#expression binding.pvb_expr)
                  in
                  let by_path =
                    match module_name with
                    | Some m -> count paths (m ^ "." ^ n)
                    | None -> 0
                  in
                  if count by_name n - count own n + by_path > 0
                  then true
                  else (
                    dropped := true;
                    false))
             bindings
         in
         if live = []
         then None
         else Some { item with pstr_desc = Pstr_value (flag, live) }
       | Pstr_module
           ({ pmb_name = { txt = name; _ }
            ; pmb_expr = { pmod_desc = Pmod_structure inner; _ } as m
            ; _
            } as mb) ->
         let inner =
           drop_unread ~paths ~everywhere ~module_name:name kept dropped inner
         in
         Some
           { item with
             pstr_desc =
               Pstr_module
                 { mb with pmb_expr = { m with pmod_desc = Pmod_structure inner } }
           }
       | _ -> Some item)
    items
;;

let has_open (items : item list) : bool =
  let found = ref false in
  object
    inherit Ppxlib.Ast_traverse.iter as super

    method! structure_item (i : item) : unit =
      (match i.pstr_desc with
       | Pstr_open _ -> found := true
       | _ -> ());
      super#structure_item i

    method! expression (e : expr) : unit =
      (match e.pexp_desc with
       | Pexp_open _ -> found := true
       | _ -> ());
      super#expression e
  end
    #structure
    items;
  !found
;;

let tidy ?(keep : string list = []) ~(exports : sig_item list) (items : item list)
  : item list
  =
  let exported = Hashtbl.create 64 in
  object
    inherit Ppxlib.Ast_traverse.iter as super

    method! signature_item (s : sig_item) : unit =
      (match s.psig_desc with
       | Psig_value { pval_name; _ } -> Hashtbl.replace exported pval_name.txt ()
       | _ -> ());
      super#signature_item s
  end
    #signature
    exports;
  let kept (n : string) : bool = Hashtbl.mem exported n || List.mem n keep in
  let rec settle (items : item list) : item list =
    let paths = reads_of ~qualified:true (fun it -> it#structure items) in
    let everywhere =
      if has_open items
      then Some (reads_of ~qualified:false (fun it -> it#structure items))
      else None
    in
    let dropped = ref false in
    let items = drop_unread ~paths ~everywhere ~module_name:None kept dropped items in
    if !dropped then settle items else items
  in
  blank_unread#structure (settle items)
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
