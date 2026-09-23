open StdLabels

type t =
  | Rule of string
  | Reference of string
  | Token of string
  | Literal of string
  | Child
  | Notation
  | Scoped of Scopes.Scope.t

let prefix = "lg-"
let rule_anchor (name : string) : string = "rule-" ^ name
let token_anchor (name : string) : string = "token-" ^ name

let link (t : t) : string option =
  match t with
  | Reference name -> Some ("#" ^ rule_anchor name)
  | Token name | Literal name -> Some ("#" ^ token_anchor name)
  (* A reference lands on a production's own name, so that name carries the
     anchor. *)
  | Rule _ | Child | Notation | Scoped _ -> None
;;

let classes (t : t) : string list =
  match t with
  | Rule _ -> [ prefix ^ "rule" ]
  | Reference _ -> [ prefix ^ "ref" ]
  | Token _ -> [ prefix ^ "token" ]
  | Literal _ -> [ prefix ^ "literal" ]
  | Child -> [ prefix ^ "child" ]
  | Notation -> [ prefix ^ "notation" ]
  | Scoped scope ->
    let segments = String.split_on_char ~sep:'.' (Scopes.Scope.to_string scope) in
    let rec walk (so_far : string list) (rest : string list) : string list =
      match rest with
      | [] -> []
      | segment :: rest ->
        let here = so_far @ [ segment ] in
        (prefix ^ "s-" ^ String.concat ~sep:"-" here) :: walk here rest
    in
    walk [] segments
;;
