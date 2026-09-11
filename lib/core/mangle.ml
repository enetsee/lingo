open StdLabels

let snake_case s =
  let buf = Buffer.create (String.length s) in
  String.iteri
    ~f:(fun i c ->
      if Char.uppercase_ascii c = c && c <> Char.lowercase_ascii c && i > 0
      then Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c))
    s;
  Buffer.contents buf
;;

let snake_case_acronym s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let is_upper c = Char.uppercase_ascii c = c && c <> Char.lowercase_ascii c in
  let is_lower c = Char.lowercase_ascii c = c && c <> Char.uppercase_ascii c in
  String.iteri
    ~f:(fun i c ->
      if i > 0 && is_upper c
      then (
        let prev = s.[i - 1] in
        let next_is_lower = i + 1 < n && is_lower s.[i + 1] in
        if is_lower prev || (is_upper prev && next_is_lower) then Buffer.add_char buf '_');
      Buffer.add_char buf (Char.lowercase_ascii c))
    s;
  Buffer.contents buf
;;

let ocaml_reserved =
  [ "and"
  ; "as"
  ; "assert"
  ; "asr"
  ; "begin"
  ; "class"
  ; "constraint"
  ; "do"
  ; "done"
  ; "downto"
  ; "else"
  ; "end"
  ; "exception"
  ; "external"
  ; "false"
  ; "for"
  ; "fun"
  ; "function"
  ; "functor"
  ; "if"
  ; "in"
  ; "include"
  ; "inherit"
  ; "initializer"
  ; "land"
  ; "lazy"
  ; "let"
  ; "lor"
  ; "lsl"
  ; "lsr"
  ; "lxor"
  ; "match"
  ; "method"
  ; "mod"
  ; "module"
  ; "mutable"
  ; "new"
  ; "nonrec"
  ; "object"
  ; "of"
  ; "open"
  ; "or"
  ; "private"
  ; "rec"
  ; "sig"
  ; "struct"
  ; "then"
  ; "to"
  ; "true"
  ; "try"
  ; "type"
  ; "val"
  ; "virtual"
  ; "when"
  ; "while"
  ; "with"
  ]
;;

let escape_reserved s = if List.mem s ~set:ocaml_reserved then s ^ "_" else s
let safe_snake s = escape_reserved (snake_case s)

let upper_first s =
  let s = snake_case s in
  if s = ""
  then s
  else
    String.make 1 (Char.uppercase_ascii s.[0])
    ^ String.sub s ~pos:1 ~len:(String.length s - 1)
;;

let screaming_snake s = String.uppercase_ascii s

let ident_error (s : string) : string option =
  let is_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let is_rest c = is_start c || (c >= '0' && c <= '9') || c = '\'' in
  if String.length s = 0
  then Some "empty"
  else if not (is_start s.[0])
  then Some (Printf.sprintf "begins with %C" s.[0])
  else (
    let rec first_bad i =
      if i >= String.length s
      then None
      else if not (is_rest s.[i])
      then Some s.[i]
      else first_bad (i + 1)
    in
    match first_bad 1 with
    | Some c -> Some (Printf.sprintf "contains %C" c)
    | None -> None)
;;

let is_ident s = ident_error s = None
