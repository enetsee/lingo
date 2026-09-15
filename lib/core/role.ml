type t =
  | Base
  | Bin
  | Prefix
  | Postfix of int

let equal (t1 : t) (t2 : t) =
  match t1, t2 with
  | Base, Base | Bin, Bin | Prefix, Prefix -> true
  | Postfix i, Postfix j -> Int.equal i j
  | _ -> false
;;

let of_block (e : Grammar.expr_def) : t list =
  [ Base; Bin; Prefix ] @ List.mapi (fun i _ -> Postfix i) e.postfix
;;

let postfix_at (e : Grammar.expr_def) (i : int) : Grammar.postfix_op =
  List.nth e.postfix i
;;

let is_active (e : Grammar.expr_def) : t -> bool = function
  | Base ->
    List.exists
      (function
        | Grammar.Token _ -> true
        | Rule _ -> false)
      e.atoms
  | Bin -> e.infix_ops <> []
  | Prefix -> e.prefix_ops <> []
  (* Each declared postfix operator gets its own role, so all of them are
     active. *)
  | Postfix _ -> true
;;

(* ["type_args"] becomes ["TypeArgs"]. *)
let pascal_of_suffix (s : string) : string =
  String.split_on_char '_' s
  |> List.filter (fun p -> p <> "")
  |> List.map (fun p ->
    String.make 1 (Char.uppercase_ascii p.[0]) ^ String.sub p 1 (String.length p - 1))
  |> String.concat ""
;;

let kind_suffix (e : Grammar.expr_def) : t -> string = function
  | Base -> ""
  | Bin -> "_BIN"
  | Prefix -> "_PREFIX"
  | Postfix i ->
    let p = postfix_at e i in
    "_POSTFIX"
    ^ if p.kind_suffix = "" then "" else "_" ^ String.uppercase_ascii p.kind_suffix
;;

let pascal_suffix (e : Grammar.expr_def) : t -> string = function
  | Base -> ""
  | Bin -> "Bin"
  | Prefix -> "Prefix"
  | Postfix i -> "Postfix" ^ pascal_of_suffix (postfix_at e i).kind_suffix
;;

let snake_suffix (e : Grammar.expr_def) : t -> string = function
  | Base -> ""
  | Bin -> "_bin"
  | Prefix -> "_prefix"
  | Postfix i ->
    let p = postfix_at e i in
    "_postfix" ^ if p.kind_suffix = "" then "" else "_" ^ p.kind_suffix
;;

let kind_name (e : Grammar.expr_def) (role : t) : string =
  String.uppercase_ascii (Grammar.Name.Rule.to_string e.rule_name) ^ kind_suffix e role
;;

let synthetic_name (e : Grammar.expr_def) (role : t) : string =
  Grammar.Name.Rule.to_string e.rule_name ^ pascal_suffix e role
;;

let format_fn (e : Grammar.expr_def) (role : t) : string =
  Mangle.snake_case (Grammar.Name.Rule.to_string e.rule_name) ^ snake_suffix e role
;;
