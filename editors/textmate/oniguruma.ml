open StdLabels

(* [-] is a metacharacter inside a character class and literal outside one,
   and nothing here emits a class, so it is left alone. [/] is not a
   metacharacter at all; escaping it would be noise in a file a person
   reads. *)
let metacharacters = "\\.[]{}()*+?|^$"

let escape (s : string) : string =
  let buf = Buffer.create (String.length s + 4) in
  String.iter s ~f:(fun c ->
    if String.contains metacharacters c then Buffer.add_char buf '\\';
    Buffer.add_char buf c);
  Buffer.contents buf
;;

(* A word character, as the [\b] test below reads one.

   Oniguruma reads UTF-8 and keys [\w] off the Unicode general category, so
   a letter such as [é] or [λ] is one and an arrow such as [→] is not.
   Carrying a category table here to separate those would be a dependency
   for one predicate, so everything above ASCII counts as a word character.
   The error runs one way: a keyword spelled in a non-ASCII symbol takes a
   boundary anchor it did not need, and it matches anyway, because the
   anchor is zero-width beside a non-word neighbour. ASCII is exact, and a
   grammar writes its symbols in ASCII. *)
let is_word_byte (c : char) : bool =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | c -> Char.code c >= 0x80
;;

let is_word_shaped (text : string) : bool =
  String.length text > 0
  && is_word_byte text.[0]
  && is_word_byte text.[String.length text - 1]
;;

(* A literal, anchored where an anchor fires.

   A keyword takes [\bfoo\b], so that the [in] inside [int] stays plain.
   A keyword spelled [->] takes neither anchor, because [\b->\b] matches
   nothing at all. The predecessor rejected such a grammar. Emitting the
   punctuation form instead reaches the same place without turning a legal
   grammar into an error. *)
let literal (text : string) : string =
  if is_word_shaped text then "\\b" ^ escape text ^ "\\b" else escape text
;;

let of_token (token : Core.Token.def) : (string, string) result =
  match token.klass with
  | Core.Grammar.Keyword text -> Ok (literal text)
  | Core.Grammar.Punctuation text -> Ok (escape text)
  | Core.Grammar.Pattern { textmate = Some source; _ } -> Ok source
  | Core.Grammar.Pattern { textmate = None; lexer } -> Redfa.Regex.to_oniguruma lexer
;;

(* The bytes that would carry a literal on into a longer one.

   Keywords and punctuation are both searched. A grammar may declare [->] as
   a keyword, and a keyword spelled in punctuation takes no boundary anchor,
   so it is exposed to the same race. *)
let longer_literals (facts : Core.Facts.t) (text : string) : string list =
  let width = String.length text in
  let extensions =
    Array.fold_left
      Core.Facts.(facts.tokens)
      ~init:[]
      ~f:(fun acc (other : Core.Token.def) ->
        match Core.Token.text other with
        | Some other_text
          when String.length other_text > width
               && String.equal (String.sub other_text ~pos:0 ~len:width) text ->
          (* One byte is not one codepoint, so take the whole tail. An
             alternation of tails is as tight as an alternation of first
             characters and needs no decoding. *)
          String.sub other_text ~pos:width ~len:(String.length other_text - width) :: acc
        | _ -> acc)
  in
  List.sort_uniq ~cmp:String.compare extensions
;;

let of_token_at_child (facts : Core.Facts.t) (token : Core.Token.def)
  : (string, string) result
  =
  match of_token token, Core.Token.text token with
  | Ok regex, Some text when not (is_word_shaped text) ->
    (match longer_literals facts text with
     | [] -> Ok regex
     | tails ->
       let alternatives = String.concat ~sep:"|" (List.map tails ~f:escape) in
       Ok (regex ^ "(?!" ^ alternatives ^ ")"))
  | result, _ -> result
;;

let neutralise (s : string) : string =
  let width = String.length s in
  let buf = Buffer.create (width + 8) in
  let in_class = ref false in
  let i = ref 0 in
  while !i < width do
    let c = s.[!i] in
    if c = '\\' && !i + 1 < width
    then (
      Buffer.add_char buf c;
      Buffer.add_char buf s.[!i + 1];
      i := !i + 2)
    else (
      if c = '[' && not !in_class
      then in_class := true
      else if c = ']' && !in_class
      then in_class := false;
      if c = '(' && (not !in_class) && not (!i + 1 < width && s.[!i + 1] = '?')
      then Buffer.add_string buf "(?:"
      else Buffer.add_char buf c;
      incr i)
  done;
  Buffer.contents buf
;;

let trivia_separator (facts : Core.Facts.t) : string =
  let parts =
    Array.fold_left
      Core.Facts.(facts.tokens)
      ~init:[]
      ~f:(fun acc (token : Core.Token.def) ->
        if not (Core.Token.is_trivia token)
        then acc
        else (
          match of_token token with
          | Ok regex -> ("(?:" ^ regex ^ ")") :: acc
          | Error _ -> acc))
  in
  match List.rev parts with
  | [] -> "\\s*+"
  | parts -> "(?>" ^ String.concat ~sep:"|" parts ^ ")*"
;;
