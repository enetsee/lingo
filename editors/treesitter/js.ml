open StdLabels

type t =
  | Atom of string
  | Call of string * t list

let atom (s : string) : t = Atom s
let call (name : string) (args : t list) : t = Call (name, args)

let string (s : string) : t =
  let buf = Buffer.create (String.length s + 4) in
  Buffer.add_char buf '\'';
  String.iter s ~f:(fun c ->
    match c with
    | '\'' | '\\' ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf c
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c);
  Buffer.add_char buf '\'';
  Atom (Buffer.contents buf)
;;

(* -- regexes --------------------------------------------------------------- *)

exception Cannot of string

(* A codepoint, as it appears inside a regex.

   Above ASCII it goes out as itself. The alternative is an escape, and no
   escape reaches both engines. JavaScript takes [\u{...}] only with a flag
   the source does not carry, and a four-digit [\uXXXX] stops at the basic
   plane. Both engines agree on the bytes. *)
let codepoint (buf : Buffer.t) (codepoint : int) ~(in_class : bool) : unit =
  let escape (c : char) : unit =
    Buffer.add_char buf '\\';
    Buffer.add_char buf c
  in
  match codepoint with
  | 0x09 -> Buffer.add_string buf "\\t"
  | 0x0A -> Buffer.add_string buf "\\n"
  | 0x0D -> Buffer.add_string buf "\\r"
  | c when c < 0x20 || c = 0x7F -> Buffer.add_string buf (Printf.sprintf "\\x%02X" c)
  | c when c <= 0x7E ->
    let c = Char.chr c in
    if in_class
    then (
      match c with
      | ']' | '\\' | '^' | '-' | '[' | '/' -> escape c
      | c -> Buffer.add_char buf c)
    else (
      match c with
      | '\\'
      | '.'
      | '['
      | ']'
      | '('
      | ')'
      | '{'
      | '}'
      | '*'
      | '+'
      | '?'
      | '|'
      | '^'
      | '$'
      | '/' -> escape c
      | c -> Buffer.add_char buf c)
  | c -> Buffer.add_utf_8_uchar buf (Uchar.of_int c)
;;

let brackets (buf : Buffer.t) (set : Ucharset.t) ~(negated : bool) : unit =
  Buffer.add_char buf '[';
  if negated then Buffer.add_char buf '^';
  Ucharset.iter_intervals
    (fun lo hi ->
       codepoint buf lo ~in_class:true;
       if hi = lo + 1
       then codepoint buf hi ~in_class:true
       else if hi > lo
       then (
         Buffer.add_char buf '-';
         codepoint buf hi ~in_class:true))
    set;
  Buffer.add_char buf ']'
;;

(* A class, written whichever way round is shorter.

   "any character but a quote or a backslash" reaches here as the set of
   every other codepoint, which is four intervals ending at the top of the
   codespace. Writing those out puts the last assignable codepoint into the
   file as itself. Negating instead gives a two-interval class holding the
   quote and the backslash, which is the same set and what a person would
   have written. *)
let charset (buf : Buffer.t) (set : Ucharset.t) ~(negated : bool) : unit =
  if Ucharset.is_empty set then raise (Cannot "the empty language matches nothing");
  if (not negated) && Ucharset.is_all set
  then
    (* A dot will not do. It stops at a newline in both engines, and the
       whole codespace runs past one. This pair of shorthand classes reads
       alike in each. *)
    Buffer.add_string buf "[\\s\\S]"
  else if (not negated) && Ucharset.is_singleton set
  then (
    match Ucharset.min_elt_opt set with
    | Some c -> codepoint buf c ~in_class:false
    | None -> raise (Cannot "the empty language matches nothing"))
  else (
    let other = Ucharset.comp set in
    if
      (not (Ucharset.is_empty other))
      && Ucharset.num_intervals other < Ucharset.num_intervals set
    then brackets buf other ~negated:(not negated)
    else brackets buf set ~negated)
;;

(* The codepoints an operand of an intersection admits.

   A complement is read here as the complement of a set. That is sound only
   under the guard in {!as_charset}: some sibling of the intersection already
   matches single codepoints alone, so the complement is being met with a
   language of length one and its longer strings cannot survive. *)
let rec narrows (regex : Redfa.Regex.t) : Ucharset.t option =
  match regex with
  | Redfa.Regex.Chars set -> Some set
  | Redfa.Regex.Neg_chars set -> Some (Ucharset.comp set)
  | Redfa.Regex.Complement inner -> Option.map Ucharset.comp (narrows inner)
  | Redfa.Regex.Inter items ->
    List.fold_left items ~init:(Some Ucharset.all) ~f:(fun acc item ->
      match acc, narrows item with
      | Some acc, Some set -> Some (Ucharset.inter acc set)
      | _ -> None)
  | _ -> None
;;

(* An intersection that denotes single codepoints, as the set of them.

   [inter any (complement [*/])] is one way to write "a character other than
   a star or a slash", and it is a character class wearing a complement.
   Recognising the shape here writes it out as a class, so this emitter
   takes a term redfa's own Oniguruma emitter declines. *)
let as_charset (regex : Redfa.Regex.t) : Ucharset.t option =
  match regex with
  | Redfa.Regex.Inter items
    when List.exists items ~f:(fun item ->
           match item with
           | Redfa.Regex.Chars _ | Redfa.Regex.Neg_chars _ -> true
           | _ -> false) -> narrows regex
  | _ -> None
;;

(* Whether a postfix needs a group around the term before it can bind. A
   sequence or an alternation does. *)
let needs_group (regex : Redfa.Regex.t) : bool =
  match regex with
  | Redfa.Regex.Chars _ | Redfa.Regex.Neg_chars _ | Redfa.Regex.Eps -> false
  | Redfa.Regex.Seq [ _ ] | Redfa.Regex.Alt [ _ ] -> false
  | _ -> true
;;

let regex (source : Redfa.Regex.t) : (t, string) result =
  let buf = Buffer.create 64 in
  let rec emit (regex : Redfa.Regex.t) : unit =
    match as_charset regex with
    | Some set -> charset buf set ~negated:false
    | None ->
      (match regex with
       | Redfa.Regex.Chars set -> charset buf set ~negated:false
       | Redfa.Regex.Neg_chars set ->
         if Ucharset.is_empty set
         then charset buf Ucharset.all ~negated:false
         else charset buf set ~negated:true
       | Redfa.Regex.Eps -> Buffer.add_string buf "(?:)"
       | Redfa.Regex.Seq [] -> Buffer.add_string buf "(?:)"
       | Redfa.Regex.Seq items -> List.iter items ~f:factor
       | Redfa.Regex.Alt [] -> raise (Cannot "the empty language matches nothing")
       | Redfa.Regex.Alt [ item ] -> emit item
       (* No group around the bar here. A group is added at the two
          positions below that need one. Grouping at both ends would give a
          term one wrapper per level of nesting. *)
       | Redfa.Regex.Alt (first :: rest) ->
         emit first;
         List.iter rest ~f:(fun item ->
           Buffer.add_char buf '|';
           emit item)
       | Redfa.Regex.Star inner -> postfix inner '*'
       | Redfa.Regex.Plus inner -> postfix inner '+'
       | Redfa.Regex.Opt inner -> postfix inner '?'
       | Redfa.Regex.Complement _ -> raise (Cannot "a complement has no JavaScript form")
       | Redfa.Regex.Inter _ ->
         raise
           (Cannot
              "an intersection over anything but character classes has no JavaScript form"))
  (* One factor of a sequence. An alternation has to be wrapped here, or
     its bar would reach across the whole sequence. *)
  and factor (regex : Redfa.Regex.t) : unit =
    match regex with
    | Redfa.Regex.Alt (_ :: _ :: _) ->
      Buffer.add_string buf "(?:";
      emit regex;
      Buffer.add_char buf ')'
    | _ -> emit regex
  and postfix (inner : Redfa.Regex.t) (mark : char) : unit =
    if needs_group inner && as_charset inner = None
    then (
      Buffer.add_string buf "(?:";
      emit inner;
      Buffer.add_char buf ')')
    else emit inner;
    Buffer.add_char buf mark
  in
  match emit source with
  | () -> Ok (Atom ("/" ^ Buffer.contents buf ^ "/"))
  | exception Cannot reason -> Error reason
;;

(* -- layout ---------------------------------------------------------------- *)

let rec flat (t : t) : string =
  match t with
  | Atom s -> s
  | Call (name, args) ->
    name ^ "(" ^ String.concat ~sep:", " (List.map args ~f:flat) ^ ")"
;;

let render ~(width : int) ~(column : int) ~(indent : int) (t : t) : string =
  let buf = Buffer.create 256 in
  let pad (columns : int) : unit = Buffer.add_string buf (String.make columns ' ') in
  let rec go ~(column : int) ~(indent : int) (t : t) : unit =
    let one_line = flat t in
    if column + String.length one_line <= width
    then Buffer.add_string buf one_line
    else (
      match t with
      | Atom s -> Buffer.add_string buf s
      | Call (name, args) ->
        Buffer.add_string buf name;
        Buffer.add_string buf "(\n";
        let inner = indent + 2 in
        List.iteri args ~f:(fun index arg ->
          if index > 0 then Buffer.add_string buf ",\n";
          pad inner;
          go ~column:inner ~indent:inner arg);
        Buffer.add_char buf '\n';
        pad indent;
        Buffer.add_char buf ')')
  in
  go ~column ~indent t;
  Buffer.contents buf
;;
