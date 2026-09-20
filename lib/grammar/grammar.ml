module Name = Name

type modifier =
  | Exactly_one
  | Zero_or_one
  | Zero_or_more
  | One_or_more

type symbol =
  | Token of string
  | Rule of string

type child_sym =
  | Single of symbol
  | Alternatives of symbol list

type child_parse =
  { recover_to : Name.Token.t list option
  ; greedy : bool
  }

type child =
  { name : Name.Child.t
  ; sym : child_sym
  ; modifier : modifier
  ; c_parse : child_parse
  }

type break_style =
  | Fit
  | Always
  | Never

type trailing_sep =
  | Never
  | On_break
  | Always

type sep_policy =
  | No_sep
  | With_sep of
      { sep : Name.Token.t
      ; trailing : trailing_sep
      }

type lookahead_n = int

type recovery_strategy =
  | Insert_only
  | Lookahead of lookahead_n

type framing =
  | Plain
  | Committed of { boundary : bool }
  | Delimited of
      { open_tok : Name.Token.t
      ; close_tok : Name.Token.t
      ; sep_policy : sep_policy
      ; boundary : bool
      }
  | Separated of
      { sep : Name.Token.t
      ; trailing : trailing_sep
      ; boundary : bool
      }

type recovery_spec = { strategy : recovery_strategy }

type production_format =
  { break_style : break_style
  ; indent_width : int
  ; separator_lines : int
  }

type production =
  { kind_name : Name.Rule.t
  ; children : child list
  ; identity_child : Name.Child.t option
  ; framing : framing
  ; recovery : recovery_spec
  ; error_messages : (Name.Child.t * string) list
  ; format : production_format
  ; has_hole : bool
  ; edge_space_before : bool option
  ; edge_space_after : bool option
  ; resync_anchors : Name.Token.t list
  }

type assoc =
  | Left
  | Right

type operator =
  { op_token : Name.Token.t
  ; bp : int
  ; op_assoc : assoc
  }

type postfix_body =
  | Nothing
  | Then of symbol
  | Enclosed of
      { close : Name.Token.t
      ; content : content
      }

and content =
  | One of symbol
  | Many of
      { elem : symbol
      ; sep : sep_policy
      }

and postfix_op =
  { bp : int
  ; lead : Name.Token.t
  ; body : postfix_body
  ; kind_suffix : string
  }

type operator_position =
  | Op_before
  | Op_after

type expr_format =
  { operator_position : operator_position
  ; continuation_indent : int
  }

type expr_def =
  { rule_name : Name.Rule.t
  ; atoms : symbol list
  ; prefix_ops : operator list
  ; infix_ops : operator list
  ; postfix : postfix_op list
  ; infix_recovery : bool
  ; e_format : expr_format
  }

type trivia_class =
  | Reformat
  | Preserve

type pattern_spec =
  { lexer : Redfa.Regex.t
  ; textmate : string option
  }

type token_class =
  | Keyword of string
  | Punctuation of string
  | Pattern of pattern_spec

type token_format =
  { space_before : bool
  ; space_after : bool
  }

type token_def =
  { token_name : Name.Token.t
  ; token_class : token_class
  ; t_format : token_format
  ; trivia : trivia_class option
  }

type t =
  { productions : production list
  ; expr : expr_def list
  ; tokens : token_def list
  ; roots : Name.Rule.t list
  }

let create
      ?(expr = [])
      ~(tokens : token_def list)
      ~(roots : string list)
      (productions : production list)
  : t
  =
  { productions; expr; tokens; roots = List.map Name.Rule.of_string roots }
;;

(* -- predicates ------------------------------------------------------------ *)

let is_token (symbol : symbol) =
  match symbol with
  | Token _ -> true
  | Rule _ -> false
;;

let is_rule (symbol : symbol) =
  match symbol with
  | Rule _ -> true
  | Token _ -> false
;;

(* -- expression blocks ----------------------------------------------------- *)

let infix ?(assoc = Left) ~(token : string) ~(bp : int) () : operator =
  { op_token = Name.Token.of_string token; bp; op_assoc = assoc }
;;

let prefix ?(assoc = Right) ~(token : string) ~(bp : int) () : operator =
  { op_token = Name.Token.of_string token; bp; op_assoc = assoc }
;;

let expr_block
      ~rule_name
      ~atoms
      ?(prefix_ops = [])
      ?(infix_ops = [])
      ?(postfix = [])
      ?(infix_recovery = false)
      ?(operator_position = Op_after)
      ?(continuation_indent = 2)
      ()
  : expr_def
  =
  { rule_name = Name.Rule.of_string rule_name
  ; atoms
  ; prefix_ops
  ; infix_ops
  ; postfix
  ; infix_recovery
  ; e_format = { operator_position; continuation_indent }
  }
;;

let with_sep ?(trailing = Never) (sep : string) : sep_policy =
  With_sep { sep = Name.Token.of_string sep; trailing }
;;

(* -- postfix operators ----------------------------------------------------- *)

let postfix_simple ?(kind_suffix = "") ~(token : string) ~(bp : int) () : postfix_op =
  { bp; lead = Name.Token.of_string token; body = Nothing; kind_suffix }
;;

let postfix_access ?(kind_suffix = "") ~(token : string) ~(rhs : symbol) ~(bp : int) ()
  : postfix_op
  =
  { bp; lead = Name.Token.of_string token; body = Then rhs; kind_suffix }
;;

let postfix_index
      ?(kind_suffix = "")
      ~(open_tok : string)
      ~(close_tok : string)
      ~(index : symbol)
      ~(bp : int)
      ()
  : postfix_op
  =
  { bp
  ; lead = Name.Token.of_string open_tok
  ; body = Enclosed { close = Name.Token.of_string close_tok; content = One index }
  ; kind_suffix
  }
;;

let postfix_brace
      ?(kind_suffix = "")
      ~(open_tok : string)
      ~(close_tok : string)
      ~(body : symbol)
      ~(bp : int)
      ()
  : postfix_op
  =
  { bp
  ; lead = Name.Token.of_string open_tok
  ; body = Enclosed { close = Name.Token.of_string close_tok; content = One body }
  ; kind_suffix
  }
;;

let postfix_call
      ?(kind_suffix = "")
      ~open_tok
      ~close_tok
      ~elem
      ?(sep_policy = No_sep)
      ~bp
      ()
  =
  { bp
  ; lead = Name.Token.of_string open_tok
  ; body =
      Enclosed
        { close = Name.Token.of_string close_tok
        ; content = Many { elem; sep = sep_policy }
        }
  ; kind_suffix
  }
;;

(* -- children -------------------------------------------------------------- *)

let recover_to_names = Option.map (List.map Name.Token.of_string)

let child
      ?recover_to
      ?(greedy = false)
      ~(modifier : modifier)
      (name : string)
      (sym : symbol)
  : child
  =
  { name = Name.Child.of_string name
  ; sym = Single sym
  ; modifier
  ; c_parse = { recover_to = recover_to_names recover_to; greedy }
  }
;;

let child_req ?recover_to name sym = child ?recover_to ~modifier:Exactly_one name sym

let child_opt ?recover_to ?greedy (name : string) (sym : symbol) : child =
  child ?recover_to ?greedy ~modifier:Zero_or_one name sym
;;

let child_rep ?recover_to ?greedy (name : string) (sym : symbol) : child =
  child ?recover_to ?greedy ~modifier:Zero_or_more name sym
;;

let child_rep1 ?recover_to ?greedy (name : string) (sym : symbol) : child =
  child ?recover_to ?greedy ~modifier:One_or_more name sym
;;

let child_alt ?recover_to ~(modifier : modifier) (name : string) (syms : symbol list)
  : child
  =
  { name = Name.Child.of_string name
  ; sym = Alternatives syms
  ; modifier
  ; c_parse = { recover_to = recover_to_names recover_to; greedy = false }
  }
;;

let child_alt_rules
      ?recover_to
      ~(modifier : modifier)
      (name : string)
      (rule_names : string list)
  : child
  =
  child_alt ?recover_to ~modifier name (List.map (fun r -> Rule r) rule_names)
;;

(* -- tokens ---------------------------------------------------------------- *)

let kw ?name ?trivia (literal : string) : token_def =
  let token_name =
    match name with
    | Some n -> n
    | None -> literal
  in
  { token_name = Name.Token.of_string token_name
  ; token_class = Keyword literal
  ; t_format = { space_before = true; space_after = true }
  ; trivia
  }
;;

let punct
      ?(space_before = true)
      ?(space_after = true)
      ?trivia
      ~(name : string)
      (literal : string)
  : token_def
  =
  { token_name = Name.Token.of_string name
  ; token_class = Punctuation literal
  ; t_format = { space_before; space_after }
  ; trivia
  }
;;

let punct_tight ?trivia ~(name : string) (literal : string) : token_def =
  punct ~space_before:false ~space_after:false ?trivia ~name literal
;;

let pat ?textmate ?trivia (n : string) (lexer : Redfa.Regex.t) : token_def =
  { token_name = Name.Token.of_string n
  ; token_class = Pattern { lexer; textmate }
  ; t_format = { space_before = true; space_after = true }
  ; trivia
  }
;;

(* -- productions ----------------------------------------------------------- *)

let prod
      ?(break_style = Fit)
      ?(indent_width = 2)
      ?(separator_lines = 1)
      (name : string)
      (children : child list)
  : production
  =
  { kind_name = Name.Rule.of_string name
  ; children
  ; identity_child = None
  ; framing = Plain
  ; recovery = { strategy = Insert_only }
  ; error_messages = []
  ; format = { break_style; indent_width; separator_lines }
  ; has_hole = true
  ; edge_space_before = None
  ; edge_space_after = None
  ; resync_anchors = []
  }
;;

let with_messages (msgs : (string * string) list) (p : production) : production =
  { p with
    error_messages = List.map (fun (c, entry) -> Name.Child.of_string c, entry) msgs
  }
;;

(* Reads back a boundary already set on the production, so the [with_]
   functions compose in any order. *)
let framing_boundary = function
  | Plain -> false
  | Committed { boundary } -> boundary
  | Delimited { boundary; _ } -> boundary
  | Separated { boundary; _ } -> boundary
;;

let with_delimited_internal
      ~(open_tok : Name.Token.t)
      ~(close_tok : Name.Token.t)
      ~(sep_policy : sep_policy)
      ?boundary
      (p : production)
  : production
  =
  let boundary =
    match boundary with
    | Some b -> b
    | None -> framing_boundary p.framing
  in
  { p with framing = Delimited { open_tok; close_tok; sep_policy; boundary } }
;;

let with_delimited ~(open_tok : string) ~(close_tok : string) ?boundary (p : production)
  : production
  =
  with_delimited_internal
    ~open_tok:(Name.Token.of_string open_tok)
    ~close_tok:(Name.Token.of_string close_tok)
    ~sep_policy:No_sep
    ?boundary
    p
;;

let with_delimited_sep
      ~(open_tok : string)
      ~(close_tok : string)
      ~(sep : string)
      ?(trailing_sep = Never)
      ?boundary
      (p : production)
  : production
  =
  with_delimited_internal
    ~open_tok:(Name.Token.of_string open_tok)
    ~close_tok:(Name.Token.of_string close_tok)
    ~sep_policy:(With_sep { sep = Name.Token.of_string sep; trailing = trailing_sep })
    ?boundary
    p
;;

let with_separator ~(sep : string) ?(trailing_sep = Never) ?boundary (p : production)
  : production
  =
  let boundary =
    match boundary with
    | Some b -> b
    | None -> framing_boundary p.framing
  in
  { p with
    framing =
      Separated { sep = Name.Token.of_string sep; trailing = trailing_sep; boundary }
  }
;;

let with_committed ?boundary (p : production) : production =
  let boundary =
    match boundary with
    | Some b -> b
    | None -> framing_boundary p.framing
  in
  let framing =
    match p.framing with
    | Plain | Committed _ -> Committed { boundary }
    | Delimited d -> Delimited { d with boundary }
    | Separated s -> Separated { s with boundary }
  in
  { p with framing }
;;

let with_identity (child_name : string) (p : production) : production =
  { p with identity_child = Some (Name.Child.of_string child_name) }
;;

let with_no_hole p = { p with has_hole = false }
let with_leading_space b p = { p with edge_space_before = Some b }
let with_trailing_space b p = { p with edge_space_after = Some b }
let with_resync_to toks p = { p with resync_anchors = List.map Name.Token.of_string toks }
let with_recovery_strategy s p = { p with recovery = { strategy = s } }

(* The emitter unrolls the peek-ahead loop to this bound. [lookahead_n]
   raises outside it, so a [Lookahead n] that reaches the checker is always
   in range. *)
let max_recovery_lookahead = 8

let lookahead_n (n : int) : lookahead_n =
  if n < 1 || n > max_recovery_lookahead
  then
    invalid_arg
      (Printf.sprintf
         "Grammar.lookahead_n: %d out of range [1, %d]"
         n
         max_recovery_lookahead);
  n
;;
