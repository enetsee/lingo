type id = int

let id_of_int n = n

type op =
  { op_kind : Kind.t
  ; bp : int
  ; assoc : Grammar.assoc
  }

type body =
  | Nothing
  | Then of Kind.t array
  | Enclosed of
      { close : Kind.t
      ; content : content
      }

and content =
  | One of Kind.t array
  | Many of
      { elem : Kind.t array
      ; sep : Rule.sep option
      }

type postfix =
  { p_lead : Kind.t
  ; p_bp : int
  ; p_body : body
  ; p_rule : Rule.id
  }

type def =
  { id : id
  ; rule_id : Rule.id
  ; kind : Kind.t
  ; hole_kind : Kind.t
  ; name : Grammar.Name.Rule.t
  ; atoms : Kind.t array
  ; prefix : op array
  ; infix : op array
  ; postfix : postfix array
  ; infix_recovery : bool
  ; format : Grammar.expr_format
  }

let op_bps (o : op) : id * id =
  match o.assoc with
  | Grammar.Left -> o.bp, o.bp + 1
  | Grammar.Right -> o.bp, o.bp
;;
