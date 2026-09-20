open StdLabels

type t =
  | Atom of string
  | List of t list

(* The escapes this writes and the ones the reader takes are one list. Using
   [%S] for the writing put a byte such as 233 out as [\233] and the reader
   took that as the character 2 followed by the text 33, so a name needing
   quotes and holding a byte above 127 came back wrong and said nothing. *)
let escaped (c : char) : string option =
  match c with
  | '"' -> Some "\\\""
  | '\\' -> Some "\\\\"
  | '\n' -> Some "\\n"
  | '\t' -> Some "\\t"
  | '\r' -> Some "\\r"
  | c when Char.code c < 0x20 || Char.code c = 0x7f ->
    Some (Printf.sprintf "\\%03d" (Char.code c))
  | _ -> None
;;

let needs_quote (s : string) : bool =
  s = ""
  || String.exists s ~f:(fun c ->
    match c with
    | ' ' | '(' | ')' | ';' -> true
    | c -> escaped c <> None)
;;

(* A byte above 127 goes out as it stands, so a name in UTF-8 reads as itself
   rather than as a run of escapes. *)
let pp_atom (fmt : Format.formatter) (s : string) : unit =
  if not (needs_quote s)
  then Format.pp_print_string fmt s
  else (
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter s ~f:(fun c ->
      match escaped c with
      | Some e -> Buffer.add_string b e
      | None -> Buffer.add_char b c);
    Buffer.add_char b '"';
    Format.pp_print_string fmt (Buffer.contents b))
;;

(* A box per list, so a form that fits stays on its line and one that does not
   breaks between its elements at the depth it sits at. *)
let rec pp (fmt : Format.formatter) : t -> unit = function
  | Atom s -> pp_atom fmt s
  | List xs ->
    Format.fprintf
      fmt
      "@[<hv 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp)
      xs
;;

let to_string x = Format.asprintf "%a" pp x

exception Bad of string

let of_string (s : string) : (t, string) result =
  let n = String.length s in
  let i = ref 0 in
  let fail fmt = Format.kasprintf (fun m -> raise (Bad m)) fmt in
  let rec skip () =
    if !i < n
    then (
      match s.[!i] with
      | ' ' | '\t' | '\n' | '\r' ->
        incr i;
        skip ()
      | ';' ->
        while !i < n && s.[!i] <> '\n' do
          incr i
        done;
        skip ()
      | _ -> ())
  in
  let digit c = c >= '0' && c <= '9' in
  let quoted () =
    let b = Buffer.create 16 in
    incr i;
    let rec go () =
      if !i >= n
      then fail "a quoted atom runs to the end of the input"
      else (
        match s.[!i] with
        | '"' -> incr i
        | '\\' ->
          if !i + 1 >= n then fail "a backslash at the end of the input";
          (match s.[!i + 1] with
           | 'n' ->
             Buffer.add_char b '\n';
             i := !i + 2
           | 't' ->
             Buffer.add_char b '\t';
             i := !i + 2
           | 'r' ->
             Buffer.add_char b '\r';
             i := !i + 2
           | '"' ->
             Buffer.add_char b '"';
             i := !i + 2
           | '\\' ->
             Buffer.add_char b '\\';
             i := !i + 2
           | c when digit c ->
             if !i + 3 >= n then fail "a numeric escape runs off the end";
             let d j = Char.code s.[!i + j] - Char.code '0' in
             if not (digit s.[!i + 2] && digit s.[!i + 3])
             then fail "a numeric escape takes three digits, at byte %d" !i;
             let code = (d 1 * 100) + (d 2 * 10) + d 3 in
             if code > 255 then fail "the escape \\%d is not a byte" code;
             Buffer.add_char b (Char.chr code);
             i := !i + 4
           | c -> fail "an escape with no case here: \\%c, at byte %d" c !i);
          go ()
        | c ->
          Buffer.add_char b c;
          incr i;
          go ())
    in
    go ();
    Buffer.contents b
  in
  let bare () =
    let start = !i in
    while
      !i < n
      &&
      match s.[!i] with
      | ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';' -> false
      | _ -> true
    do
      incr i
    done;
    String.sub s ~pos:start ~len:(!i - start)
  in
  let rec one () =
    skip ();
    if !i >= n
    then fail "the input ran out where a form was expected"
    else (
      match s.[!i] with
      | '(' ->
        incr i;
        let acc = ref [] in
        let rec go () =
          skip ();
          if !i >= n
          then fail "a list runs to the end of the input"
          else if s.[!i] = ')'
          then incr i
          else (
            acc := one () :: !acc;
            go ())
        in
        go ();
        List (Stdlib.List.rev !acc)
      | ')' -> fail "a close paren with no list open, at byte %d" !i
      | '"' -> Atom (quoted ())
      | _ -> Atom (bare ()))
  in
  match
    let x = one () in
    skip ();
    if !i < n then fail "trailing text after the form, at byte %d" !i;
    x
  with
  | x -> Ok x
  | exception Bad m -> Error m
;;
