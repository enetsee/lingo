open StdLabels

type finding =
  | Lossy of { rebuilt : string }
  | Unstable of
      { width : int
      ; once : string
      ; twice : string
      }
  | Lost of
      { width : int
      ; once : string
      ; at : string
      }
  | Refused of
      { width : int
      ; once : string
      ; at : string
      }
  | Recast of
      { width : int
      ; once : string
      ; before : string
      ; after : string
      }
  | Uncounted of
      { width : int
      ; once : string
      ; line : int
      ; reached : int
      }
  | Varies of
      { widths : int * int
      ; narrow : string
      ; wide : string
      }
  | Wrapped of { newlines : int }
  | Unframed of { conditionals : int }
  | Raised of { message : string }

let laws = [ "A"; "B"; "C"; "D"; "E"; "F"; "W"; "doc"; "frame"; "raised" ]

let law (f : finding) : string =
  match f with
  | Lossy _ -> "A"
  | Unstable _ -> "B"
  | Lost _ -> "C"
  | Refused _ -> "D"
  | Recast _ -> "E"
  | Uncounted _ -> "F"
  | Varies _ -> "W"
  | Wrapped _ -> "doc"
  | Unframed _ -> "frame"
  | Raised _ -> "raised"
;;

let describe (f : finding) : string =
  match f with
  | Lossy _ -> "A: the tree does not rebuild its input"
  | Unstable _ -> "B: format is not a fixed point"
  | Lost { at; _ } -> Printf.sprintf "C: %s" at
  | Refused { at; _ } -> Printf.sprintf "D: %s" at
  | Recast _ -> "E: the output parses to a different shape"
  | Uncounted _ -> "F: a line past the ruler had a break the printer declined"
  | Varies _ -> "W: the tokens depend on the width"
  | Wrapped _ -> "doc: a text node holds a newline"
  | Unframed _ -> "frame: a conditional sits outside the frame that hands it out"
  | Raised { message } -> Printf.sprintf "raised: %s" message
;;

(* -- the tree as its kinds ------------------------------------------------- *)

let shape (root : Siesta.Green.node) : string =
  let b = Buffer.create 64 in
  let rec go (node : Siesta.Green.node) : unit =
    Buffer.add_string b ("(" ^ string_of_int (Siesta.Green.kind node));
    Array.iter (Siesta.Green.children_array node) ~f:(function
      | Siesta.Green.Node child -> go child
      | Siesta.Green.Token _ -> ());
    Buffer.add_char b ')'
  in
  go root;
  Buffer.contents b
;;

(* -- the token comparisons ------------------------------------------------- *)

(* Every token of [before] is in [after], in the same order. [after] may hold
   one extra where a body's policy added a separator, and nothing else: the
   fold writes the tree's tokens and, in a frame the parse closed, one
   separator. It may never lose one. *)
let rec keeps
          ~(seps : string list)
          (before : (int * string) list)
          (after : (int * string) list)
  : bool
  =
  match before, after with
  | [], [] -> true
  | b, (_, y) :: a
    when match b with
         | (_, x) :: _ -> not (String.equal x y)
         | [] -> true -> List.mem y ~set:seps && keeps ~seps b a
  | (_, x) :: b, (_, y) :: a when String.equal x y -> keeps ~seps b a
  | _ -> false
;;

let difference
      ~(seps : string list)
      (before : (int * string) list)
      (after : (int * string) list)
  : string
  =
  let rec go i b a =
    match b, a with
    | [], [] -> "?"
    | (_, x) :: b, (_, y) :: a when String.equal x y -> go (i + 1) b a
    | b, (_, y) :: a when List.mem y ~set:seps -> go i b a
    | (_, x) :: _, (_, y) :: _ -> Printf.sprintf "token %d is %S and came back %S" i x y
    | (_, x) :: _, [] -> Printf.sprintf "token %d is %S and did not come back" i x
    | [], (_, y) :: _ -> Printf.sprintf "%S came back and was never written" y
  in
  go 0 before after
;;

(* The same tokens in the same order, the separators a policy may add aside. *)
let agree ~(seps : string list) (one : (int * string) list) (other : (int * string) list)
  : bool
  =
  let without = List.filter ~f:(fun (_, text) -> not (List.mem text ~set:seps)) in
  List.equal ~eq:(fun (_, x) (_, y) -> String.equal x y) (without one) (without other)
;;

let texts (tokens : (int * string) list) : string =
  String.concat ~sep:" " (List.map tokens ~f:snd)
;;

(* -- one input ------------------------------------------------------------- *)

let run (h : Harness.t) ~(widths : int list) (src : string) : finding list =
  let found = ref [] in
  let add (f : finding) : unit = found := f :: !found in
  let seps = h.separators in
  (try
     let tree, _ = Harness.parse h src in
     let rebuilt = Siesta.Green.to_source tree in
     if not (String.equal rebuilt src) then add (Lossy { rebuilt });
     let before = Harness.written h tree in
     let before_shape = shape tree in
     (* The document does not depend on the width, so it is built and checked
        once and rendered at each. *)
     let d = Harness.doc h tree in
     (* Two defects, reported apart. A newline in a text node is the fold
        writing bytes the column model cannot see. A conditional outside its
        frame is the fold handing one out of the callback that gave it, which
        would make a trailing separator follow a group nobody named. *)
     (match Handsome.Utf8.check d with
      | Ok () -> ()
      | Error es ->
        let newlines, strays =
          List.partition
            ~f:(function
              | Handsome.Utf8.Newline_in_text _ -> true
              | Handsome.Utf8.Alt_outside_frame _ -> false)
            es
        in
        if newlines <> [] then add (Wrapped { newlines = List.length newlines });
        if strays <> [] then add (Unframed { conditionals = List.length strays }));
     let at_width (width : int) : (int * string) list =
       let stream, resolved = Handsome.Utf8.render ~width d in
       let lines = Handsome.Utf8.lines stream in
       let once = Handsome.Utf8.to_string stream in
       List.iter resolved.declined ~f:(fun (line, _) ->
         if line < Array.length lines && lines.(line) > width
         then add (Uncounted { width; once; line; reached = lines.(line) }));
       (* D reads the lexer and C reads the parse, over one comparison. A
          boundary that fused two tokens shows here first: the reparse holds
          the fused lexeme as one token, and the tree it builds can still lay
          out to the same bytes. *)
       let lexed = Harness.relex h once in
       if not (keeps ~seps before lexed)
       then add (Refused { width; once; at = difference ~seps before lexed });
       let again, _ = Harness.parse h once in
       let after = Harness.written h again in
       if not (keeps ~seps before after)
       then add (Lost { width; once; at = difference ~seps before after });
       let after_shape = shape again in
       if not (String.equal before_shape after_shape)
       then add (Recast { width; once; before = before_shape; after = after_shape });
       let twice = Harness.format h ~width again in
       if not (String.equal once twice) then add (Unstable { width; once; twice });
       after
     in
     match widths with
     | [] -> ()
     | first_width :: rest_widths ->
       let first = at_width first_width in
       List.iter2 rest_widths (List.map rest_widths ~f:at_width) ~f:(fun width tokens ->
         if not (agree ~seps first tokens)
         then
           add
             (Varies
                { widths = first_width, width; narrow = texts first; wide = texts tokens }))
   with
   | e -> add (Raised { message = Printexc.to_string e }));
  List.rev !found
;;
