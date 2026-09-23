open StdLabels

type document = Mark.t Handsome.Utf8.t

let escape (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  String.iter s ~f:(fun c ->
    match c with
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#39;"
    | c -> Buffer.add_char buf c);
  Buffer.contents buf
;;

(* A push and its pop are a matched pair in the stream, so a span opened on
   one is closed on the other. *)
let fold ~(markup : bool) ~(width : int) (document : document) : string =
  let stream, _ = Handsome.Utf8.render ~width document in
  let buf = Buffer.create 1024 in
  (* Whether the span now open was wrapped in a link, innermost first. *)
  let depth = ref [] in
  let rec walk (stream : Mark.t Handsome.Utf8.stream) : unit =
    match stream with
    | Handsome.Utf8.S_empty -> ()
    | Handsome.Utf8.S_text (_, text, rest) ->
      Buffer.add_string buf (if markup then escape text else text);
      walk rest
    | Handsome.Utf8.S_line (indent, rest) ->
      Buffer.add_char buf '\n';
      Buffer.add_string buf (String.make indent ' ');
      walk rest
    | Handsome.Utf8.S_ann_push (mark, rest) ->
      if markup
      then (
        (* The class goes on the span, whether or not a link wraps it, so
           one stylesheet rule reaches both. *)
        (match Mark.link mark with
         | None -> ()
         | Some href -> Buffer.add_string buf (Printf.sprintf "<a href=%S>" (escape href)));
        depth := (Mark.link mark <> None) :: !depth;
        Buffer.add_string buf "<span class=\"";
        Buffer.add_string buf (String.concat ~sep:" " (Mark.classes mark));
        Buffer.add_string buf "\">");
      walk rest
    | Handsome.Utf8.S_ann_pop rest ->
      if markup
      then (
        Buffer.add_string buf "</span>";
        match !depth with
        | linked :: rest ->
          depth := rest;
          if linked then Buffer.add_string buf "</a>"
        | [] -> ());
      walk rest
  in
  walk stream;
  Buffer.contents buf
;;

let html ~(width : int) (document : document) : string = fold ~markup:true ~width document

let text ~(width : int) (document : document) : string =
  fold ~markup:false ~width document
;;
