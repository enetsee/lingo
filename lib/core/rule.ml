open StdLabels

type id = int

type origin =
  | User
  | Pratt_block
  | Pratt_role of
      { block : id
      ; role : Role.t
      }

type sep =
  { sep_tok : Kind.t
  ; trailing : Grammar.trailing_sep
  }

type frame =
  | Plain
  | Committed of { boundary : bool }
  | Delimited of
      { open_ : Kind.t
      ; close : Kind.t
      ; sep : sep option
      ; boundary : bool
      }
  | Separated of
      { sep_tok : Kind.t
      ; trailing : Grammar.trailing_sep
      ; boundary : bool
      }

type child =
  { child_name : Grammar.Name.Child.t
  ; alts : Kind.t array
  ; kinds : Kind.Set.t
  ; modifier : Grammar.modifier
  ; greedy : bool
  ; recover_to : Kind.Set.t option
  }

type def =
  { id : id
  ; kind : Kind.t
  ; name : Grammar.Name.Rule.t
  ; children : child array
  ; frame : frame
  ; body_from : int
  ; hole : Kind.t option
  ; origin : origin
  ; recovery : Grammar.recovery_spec
  ; messages : (Grammar.Name.Child.t * string) array
  ; resync : Kind.Set.t
  ; format : Grammar.production_format
  ; edge_space_before : bool option
  ; edge_space_after : bool option
  ; identity : int option
  ; binders : int array
  ; opens_scope : bool
  }

let body_children (d : def) =
  let n = Array.length d.children in
  List.init ~len:(max 0 (n - d.body_from)) ~f:(fun i -> d.children.(d.body_from + i))
;;

let is_synthetic (d : def) =
  match d.origin with
  | Pratt_role _ -> true
  | User | Pratt_block -> false
;;
