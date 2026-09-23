open StdLabels

(* The longest token starting at [pos], and how many bytes it takes.

   No match gives [None] and one codepoint, so a scan always moves on: an
   example with a stray character is still a page rather than a loop. *)
let scan (lexer : Core.Lexer.t) (source : string) ~(pos : int) : Core.Kind.t option * int =
  let width = String.length source in
  let best = ref None in
  let state = ref Core.Lexer.initial in
  let at = ref pos in
  let running = ref true in
  while !running && !at < width do
    let decoded = String.get_utf_8_uchar source !at in
    let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
    let taken = Uchar.utf_decode_length decoded in
    let next =
      Core.Lexer.step lexer ~state:!state ~klass:(Core.Lexer.class_of lexer code)
    in
    if next < 0
    then running := false
    else (
      state := next;
      at := !at + taken;
      match Core.Lexer.(lexer.accept).(next) with
      | Some kind -> best := Some (kind, !at)
      | None -> ())
  done;
  match !best with
  | Some (kind, stop) -> Some kind, stop - pos
  | None ->
    if pos >= width
    then None, 0
    else None, Uchar.utf_decode_length (String.get_utf_8_uchar source pos)
;;

let ( ^^ ) = Handsome.Utf8.( ^^ )

(* Text with its line breaks kept as breaks. handsome rejects a newline
   inside a text node, because a node the model cannot measure leaves every
   column after it counted wrong. *)
let lines (text : string) : Render.document =
  match String.split_on_char ~sep:'\n' text with
  | [] -> Handsome.Utf8.empty
  | first :: rest ->
    List.fold_left rest ~init:(Handsome.Utf8.text first) ~f:(fun acc line ->
      acc ^^ Handsome.Utf8.hardline ^^ Handsome.Utf8.text line)
;;

let example (scopes : Scopes.t) (source : string) : Render.document =
  let facts = Scopes.facts scopes in
  let lexer = Core.Lexer.of_facts facts in
  let width = String.length source in
  let parts = ref [] in
  let at = ref 0 in
  while !at < width do
    let kind, taken = scan lexer source ~pos:!at in
    let taken = max taken 1 in
    let text = String.sub source ~pos:!at ~len:(min taken (width - !at)) in
    let scope =
      match kind with
      | None -> None
      | Some kind ->
        (match Core.Facts.token_of_kind facts kind with
         | None -> None
         | Some token -> Scopes.token scopes token.id)
    in
    let part =
      match scope with
      | None -> lines text
      | Some scope -> Handsome.Utf8.annotate (Mark.Scoped scope) (lines text)
    in
    parts := part :: !parts;
    at := !at + taken
  done;
  Handsome.Utf8.concat (List.rev !parts)
;;
