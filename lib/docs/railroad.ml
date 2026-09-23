open StdLabels

type polarity =
  | Forward
  | Back

type tip = Logical of int

type t =
  | Station of
      { label : string
      ; terminal : bool
      ; link : string option
      }
  | Nothing
  | Seq of t list
  | Stack of
      { polarity : polarity
      ; tip : tip
      ; rows : t list
      }
  | Labelled of
      { name : string
      ; body : t
      }
  | Wrapped of t list

let station ?link ?(terminal = false) (label : string) : t =
  Station { label; terminal; link }
;;

let choice (rows : t list) : t =
  match rows with
  | [ one ] -> one
  | rows -> Stack { polarity = Forward; tip = Logical 0; rows }
;;

let optional (body : t) : t =
  Stack { polarity = Forward; tip = Logical 0; rows = [ Nothing; body ] }
;;

let any (body : t) : t =
  Stack { polarity = Back; tip = Logical 0; rows = [ Nothing; body ] }
;;

let repeat ?(sep = Nothing) (body : t) : t =
  Stack { polarity = Back; tip = Logical 0; rows = [ body; sep ] }
;;

(* -- building one from a production --------------------------------------- *)

let rule_link (name : string) : string = "#rule-" ^ name
let token_link (name : string) : string = "#token-" ^ name

let find_token (grammar : Core.Grammar.t) (name : string) : Core.Grammar.token_def option =
  List.find_opt grammar.tokens ~f:(fun (token : Core.Grammar.token_def) ->
    String.equal (Core.Grammar.Name.Token.to_string token.token_name) name)
;;

let of_symbol (grammar : Core.Grammar.t) (symbol : Core.Grammar.symbol) : t =
  match symbol with
  | Core.Grammar.Rule name -> station ~link:(rule_link name) name
  | Core.Grammar.Token name ->
    let link = token_link name in
    (match find_token grammar name with
     | Some { token_class = Core.Grammar.Keyword text; _ }
     | Some { token_class = Core.Grammar.Punctuation text; _ } ->
       station ~link ~terminal:true text
     | _ -> station ~link name)
;;

let of_token (grammar : Core.Grammar.t) (name : Core.Grammar.Name.Token.t) : t =
  of_symbol grammar (Core.Grammar.Token (Core.Grammar.Name.Token.to_string name))
;;

let of_child_symbol (grammar : Core.Grammar.t) (sym : Core.Grammar.child_sym) : t =
  match sym with
  | Core.Grammar.Single one -> of_symbol grammar one
  | Core.Grammar.Alternatives many -> choice (List.map many ~f:(of_symbol grammar))
;;

(* Zero or more with no separator collapses into one stack: an empty row on
   the line, the body on the return row. The paper writes it [(- () d)].

   A separator takes two stacks. The skip, the body and the separator are
   three rows, and the body reads forward while the separator reads back, so
   no single stack holds them. *)
let repeated (body : t) ~(sep : t option) (modifier : Core.Grammar.modifier) : t =
  match modifier, sep with
  | Core.Grammar.Exactly_one, _ -> body
  | Core.Grammar.Zero_or_one, _ -> optional body
  | Core.Grammar.One_or_more, sep -> repeat ?sep body
  | Core.Grammar.Zero_or_more, None -> any body
  | Core.Grammar.Zero_or_more, Some sep -> optional (repeat ~sep body)
;;

let of_child (grammar : Core.Grammar.t) ?(sep : t option) (child : Core.Grammar.child) : t
  =
  Labelled
    { name = Core.Grammar.Name.Child.to_string child.name
    ; body = repeated (of_child_symbol grammar child.sym) ~sep child.modifier
    }
;;

let sep_shape (grammar : Core.Grammar.t) (policy : Core.Grammar.sep_policy) : t option =
  match policy with
  | Core.Grammar.No_sep -> None
  | Core.Grammar.With_sep { sep; _ } -> Some (of_token grammar sep)
;;

let of_production (grammar : Core.Grammar.t) (production : Core.Grammar.production) : t =
  let plain () : t =
    Seq (List.map production.children ~f:(fun child -> of_child grammar child))
  in
  match production.framing with
  | Core.Grammar.Plain | Core.Grammar.Committed _ -> plain ()
  | Core.Grammar.Delimited { open_tok; close_tok; sep_policy; _ } ->
    let body =
      match production.children with
      | [ one ] -> [ of_child grammar ?sep:(sep_shape grammar sep_policy) one ]
      | children -> List.map children ~f:(fun child -> of_child grammar child)
    in
    Seq ((of_token grammar open_tok :: body) @ [ of_token grammar close_tok ])
  | Core.Grammar.Separated { sep; _ } ->
    (match production.children with
     | [ one ] -> of_child grammar ~sep:(of_token grammar sep) one
     | _ -> plain ())
;;

let of_block (grammar : Core.Grammar.t) (block : Core.Grammar.expr_def) : t =
  let atoms = choice (List.map block.atoms ~f:(of_symbol grammar)) in
  let operator (op : Core.Grammar.operator) : t = of_token grammar op.op_token in
  let with_prefix =
    match block.prefix_ops with
    | [] -> atoms
    | ops ->
      Seq
        [ Labelled { name = "prefix"; body = any (choice (List.map ops ~f:operator)) }
        ; atoms
        ]
  in
  match block.infix_ops with
  | [] -> with_prefix
  | ops -> repeat ~sep:(choice (List.map ops ~f:operator)) with_prefix
;;

(* -- geometry -------------------------------------------------------------- *)

(* Every measurement is in the SVG's own units, and the page scales them.
   The character width suits a monospace face at font size 12. The drawing
   sets that face and size on every label. A different face leaves the boxes
   loose. Every box is sized from the same number, so they stay
   consistent. *)
let char_width = 8
let box_height = 26
let box_pad = 10
let run = 12 (* the straight stretch between two boxes *)
let branch = 12 (* how far in from the edge a stack turns *)
let gap = 12 (* the vertical stretch between two rows *)
let wrap_gap = 24 (* the same between two rows of a wrapped sequence *)
let label_height = 14
let margin = 8

let text_width (s : string) : int =
  (Handsome.Utf8_width.measure s * char_width) + (2 * box_pad)
;;

type metrics =
  { width : int
  ; up : int (** How far the shape reaches above the entry line. *)
  ; down : int
  ; exit : int
    (** How far below the entry line the track leaves. Only a wrapped
          sequence moves the line down, because it enters on its first row
          and leaves on its last. A shape holding one carries that drop
          along. *)
  }

(* The paper carries a direction on every node. A row under [Back] is
   travelled right to left, so its contents are laid in reverse. Reading
   such a row backwards then reads the grammar forwards. *)
type dir =
  | Ltr
  | Rtl

let flip (dir : dir) : dir =
  match dir with
  | Ltr -> Rtl
  | Rtl -> Ltr
;;

let nth (parts : metrics list) (index : int) : metrics = List.nth parts index

let row_of (tip : tip) (rows : 'a list) : int =
  match tip with
  | Logical row -> max 0 (min row (List.length rows - 1))
;;

let rec measure (shape : t) : metrics =
  match shape with
  | Station { label; _ } ->
    { width = text_width label; up = box_height / 2; down = box_height / 2; exit = 0 }
  | Nothing -> { width = 0; up = box_height / 2; down = box_height / 2; exit = 0 }
  | Labelled { name; body } ->
    let inner = measure body in
    { width = max inner.width (Handsome.Utf8_width.measure name * char_width)
    ; up = inner.up + label_height
    ; down = inner.down
    ; exit = inner.exit
    }
  | Seq [] -> { width = run; up = box_height / 2; down = box_height / 2; exit = 0 }
  | Seq items ->
    let parts = List.map items ~f:measure in
    let width =
      List.fold_left parts ~init:0 ~f:(fun acc part -> acc + part.width)
      + (run * (List.length parts - 1))
    in
    (* The line steps down wherever a part leaves lower than it entered.
       Every part after it sits on that lower line. *)
    let up, down, exit =
      List.fold_left parts ~init:(0, 0, 0) ~f:(fun (up, down, cur) part ->
        max up (part.up - cur), max down (part.down + cur), cur + part.exit)
    in
    { width; up; down; exit }
  | Stack { rows = []; _ } ->
    { width = run; up = box_height / 2; down = box_height / 2; exit = 0 }
  | Stack { tip; rows; _ } ->
    let parts = List.map rows ~f:measure in
    let at = row_of tip rows in
    let widest = List.fold_left parts ~init:0 ~f:(fun acc part -> max acc part.width) in
    let above =
      List.filteri parts ~f:(fun index _ -> index < at)
      |> List.fold_left ~init:0 ~f:(fun acc part -> acc + part.up + part.down + gap)
    in
    let below =
      List.filteri parts ~f:(fun index _ -> index > at)
      |> List.fold_left ~init:0 ~f:(fun acc part -> acc + gap + part.up + part.down)
    in
    let here = nth parts at in
    { width = widest + (4 * branch)
    ; up = above + here.up
    ; down = here.down + below
    ; exit = here.exit
    }
  | Wrapped rows ->
    let parts = List.map rows ~f:measure in
    let widest = List.fold_left parts ~init:0 ~f:(fun acc part -> max acc part.width) in
    let last = List.length parts - 1 in
    let drop = ref 0 in
    for index = 1 to last do
      drop := !drop + (nth parts (index - 1)).down + wrap_gap + (nth parts index).up
    done;
    { width = widest + (2 * branch)
    ; up = (nth parts 0).up
    ; down = !drop + (nth parts last).down
    ; exit = !drop + (nth parts last).exit
    }
;;

(* -- wrapping -------------------------------------------------------------- *)

(* The paper searches candidate break points and orders them, preferring
   greater content width, less depth and lower height. The rule here is
   simpler. Each row is filled to the limit. The limit is then pulled in as
   far as it goes without adding a row, so the rows come out even rather
   than one long and one short.

   An item wider than the page ends up on a row of its own, and the diagram
   comes out wider than the page. *)
let row_width (parts : metrics list) : int =
  match parts with
  | [] -> 0
  | parts ->
    List.fold_left parts ~init:0 ~f:(fun acc part -> acc + part.width)
    + (run * (List.length parts - 1))
;;

let fill ~(limit : int) (items : (t * metrics) list) : (t * metrics) list list =
  let rows = ref [] in
  let current = ref [] in
  List.iter items ~f:(fun (shape, part) ->
    let taken = List.rev ((shape, part) :: !current) in
    if !current <> [] && row_width (List.map taken ~f:snd) > limit
    then (
      rows := List.rev !current :: !rows;
      current := [ shape, part ])
    else current := (shape, part) :: !current);
  if !current <> [] then rows := List.rev !current :: !rows;
  List.rev !rows
;;

let break ~(limit : int) (items : (t * metrics) list) : t list =
  let count (limit : int) : int = List.length (fill ~limit items) in
  let wanted = count limit in
  let widest =
    List.fold_left items ~init:0 ~f:(fun acc (_, part) -> max acc part.width)
  in
  (* The narrowest limit that still fits in the same number of rows. *)
  let rec settle (lo : int) (hi : int) : int =
    if lo >= hi
    then hi
    else (
      let mid = (lo + hi) / 2 in
      if count mid = wanted then settle lo mid else settle (mid + 1) hi)
  in
  let limit = settle widest limit in
  List.map (fill ~limit items) ~f:(fun row -> Seq (List.map row ~f:fst))
;;

let rec wrap ~(width : int) (shape : t) : t =
  match shape with
  | Station _ | Nothing -> shape
  | Labelled { name; body } -> Labelled { name; body = wrap ~width body }
  | Stack { polarity; tip; rows } ->
    (* A stack takes [4 * branch] for its own turns, so its rows have that
       much less room. *)
    Stack { polarity; tip; rows = List.map rows ~f:(wrap ~width:(width - (4 * branch))) }
  | Wrapped rows -> Wrapped (List.map rows ~f:(wrap ~width))
  | Seq items ->
    let items = List.map items ~f:(wrap ~width) in
    let parts = List.map items ~f:measure in
    if row_width parts <= width || List.length items < 2
    then Seq items
    else (
      match break ~limit:(width - (2 * branch)) (List.combine items parts) with
      | [ one ] -> one
      | rows -> Wrapped rows)
;;

(* -- placing --------------------------------------------------------------- *)

type placed =
  | Box of
      { x : int
      ; y : int
      ; width : int
      ; height : int
      ; terminal : bool
      ; label : string
      ; link : string option
      }
  | Rail of
      { points : (int * int) list
      ; dir : dir
      }
  | Caption of
      { x : int
      ; y : int
      ; text : string
      }

type extent =
  { width : int
  ; height : int
  }

(* [lay shape ~x ~y ~dir] puts the track's entry at [(x, y)] and leaves its
   exit at [(x + width, y + exit)]. So two shapes join with one rail,
   whatever is inside either of them.

   A stack lays every row the same way, whichever row the track enters on and
   whichever way the rows are read. Each row's rails stop at its edges, so no
   rail crosses a station. *)
let rec lay (shape : t) ~(x : int) ~(y : int) ~(dir : dir) (out : placed list ref) : unit =
  let here = measure shape in
  let emit (p : placed) : unit = out := p :: !out in
  let rail ?(dir = dir) (points : (int * int) list) : unit =
    emit (Rail { points; dir })
  in
  match shape with
  | Nothing -> rail [ x, y; x + here.width, y ]
  | Station { label; terminal; link } ->
    emit
      (Box
         { x
         ; y = y - (box_height / 2)
         ; width = here.width
         ; height = box_height
         ; terminal
         ; label
         ; link
         })
  | Labelled { name; body } ->
    let inner = measure body in
    emit (Caption { x; y = y - inner.up - 4; text = name });
    lay body ~x ~y ~dir out;
    if inner.width < here.width
    then rail [ x + inner.width, y + inner.exit; x + here.width, y + inner.exit ]
  | Seq [] -> rail [ x, y; x + here.width, y ]
  | Seq items ->
    let parts = List.map items ~f:measure in
    let cur = ref 0 in
    let at =
      ref
        (match dir with
         | Ltr -> x
         | Rtl -> x + here.width)
    in
    List.iteri items ~f:(fun index item ->
      let part = nth parts index in
      (match dir with
       | Ltr ->
         if index > 0
         then (
           rail [ !at, y + !cur; !at + run, y + !cur ];
           at := !at + run);
         lay item ~x:!at ~y:(y + !cur) ~dir out;
         at := !at + part.width
       | Rtl ->
         if index > 0
         then (
           rail [ !at, y + !cur; !at - run, y + !cur ];
           at := !at - run);
         at := !at - part.width;
         lay item ~x:!at ~y:(y + !cur) ~dir out);
      cur := !cur + part.exit)
  | Stack { rows = []; _ } -> rail [ x, y; x + here.width, y ]
  | Stack { polarity; tip; rows } ->
    let parts = List.map rows ~f:measure in
    let entry = row_of tip rows in
    let inner_x = x + (2 * branch) in
    let inner_width = here.width - (4 * branch) in
    let centred (part : metrics) : int = inner_x + ((inner_width - part.width) / 2) in
    let line_of = Array.make (List.length rows) y in
    let above = ref y in
    for index = entry - 1 downto 0 do
      let part = nth parts index in
      above := !above - (nth parts (index + 1)).up - gap - part.down;
      line_of.(index) <- !above
    done;
    let below = ref y in
    for index = entry + 1 to List.length rows - 1 do
      let part = nth parts index in
      below := !below + (nth parts (index - 1)).down + gap + part.up;
      line_of.(index) <- !below
    done;
    let leaves = y + here.exit in
    List.iteri rows ~f:(fun index row ->
      let part = nth parts index in
      let at = line_of.(index) in
      let start = centred part in
      (* Under [Back] every row but the entry row is travelled from the
         right. Those rails carry the flipped direction, so the arrowheads
         on them point back. *)
      let row_dir =
        match polarity with
        | Forward -> dir
        | Back -> if index = entry then dir else flip dir
      in
      if index = entry
      then (
        rail [ x, y; start, y ];
        lay row ~x:start ~y ~dir:row_dir out;
        rail [ start + part.width, y + part.exit; x + here.width, y + part.exit ])
      else (
        rail ~dir:row_dir [ x, y; x + branch, y; x + branch, at; start, at ];
        lay row ~x:start ~y:at ~dir:row_dir out;
        rail
          ~dir:row_dir
          [ start + part.width, at + part.exit
          ; x + here.width - branch, at + part.exit
          ; x + here.width - branch, leaves
          ; x + here.width, leaves
          ]))
  | Wrapped rows ->
    let parts = List.map rows ~f:measure in
    let last = List.length rows - 1 in
    let inner_x = x + branch in
    let line_of = Array.make (List.length rows) y in
    let running = ref y in
    for index = 1 to last do
      running := !running + (nth parts (index - 1)).down + wrap_gap + (nth parts index).up;
      line_of.(index) <- !running
    done;
    rail [ x, y; inner_x, y ];
    List.iteri rows ~f:(fun index row ->
      let part = nth parts index in
      let at = line_of.(index) in
      lay row ~x:inner_x ~y:at ~dir out;
      let ends = inner_x + part.width
      and leaves = at + part.exit in
      if index < last
      then (
        let next = line_of.(index + 1) in
        (* The rail runs out to the right margin, back along the gap, then
           down into the next row. The run along the gap carries [Rtl],
           because a reader needs its direction. *)
        let midway = (at + part.down + (next - (nth parts (index + 1)).up)) / 2 in
        rail [ ends, leaves; x + here.width, leaves; x + here.width, midway ];
        rail ~dir:Rtl [ x + here.width, midway; x, midway ];
        rail [ x, midway; x, next; inner_x, next ])
      else rail [ ends, leaves; x + here.width, leaves ])
;;

let place ?(width = 760) (shape : t) : placed list * extent =
  let shape = wrap ~width:(width - (4 * margin)) shape in
  let here = measure shape in
  let width = here.width + (4 * margin) in
  let height = here.up + here.down + (2 * margin) in
  let y = margin + here.up in
  let out = ref [] in
  (* A tick at each end marks where the track starts and where it stops. *)
  out := Rail { points = [ 2, y - 7; 2, y + 7 ]; dir = Ltr } :: !out;
  out := Rail { points = [ 2, y; 2 * margin, y ]; dir = Ltr } :: !out;
  lay shape ~x:(2 * margin) ~y ~dir:Ltr out;
  let leaves = y + here.exit in
  out
  := Rail { points = [ (2 * margin) + here.width, leaves; width - 2, leaves ]; dir = Ltr }
     :: !out;
  out
  := Rail { points = [ width - 2, leaves - 7; width - 2, leaves + 7 ]; dir = Ltr } :: !out;
  List.rev !out, { width; height }
;;

type fault =
  | Rail_through_station of
      { label : string
      ; at : int * int
      }
  | Outside_extent of { at : int * int }

let pp_fault (fmt : Format.formatter) (fault : fault) : unit =
  match fault with
  | Rail_through_station { label; at = x, y } ->
    Format.fprintf fmt "a rail crosses the station %S at %d,%d" label x y
  | Outside_extent { at = x, y } -> Format.fprintf fmt "%d,%d is outside the box" x y
;;

(* A rail may touch a station's edge. Crossing the inside is a fault. The
   segments are axis aligned, so the test compares two open intervals. *)
let crosses (box : placed) ((x1, y1) : int * int) ((x2, y2) : int * int)
  : (int * int) option
  =
  match box with
  | Box { x; y; width; height; _ } ->
    let inside (lo : int) (hi : int) (a : int) (b : int) : bool =
      max lo (min a b) < min hi (max a b)
    in
    let strictly (lo : int) (hi : int) (v : int) : bool = lo < v && v < hi in
    if y1 = y2 && strictly y (y + height) y1 && inside x (x + width) x1 x2
    then Some (max x (min x1 x2), y1)
    else if x1 = x2 && strictly x (x + width) x1 && inside y (y + height) y1 y2
    then Some (x1, max y (min y1 y2))
    else None
  | Rail _ | Caption _ -> None
;;

let check (placed : placed list) (extent : extent) : fault list =
  let boxes =
    List.filter placed ~f:(fun p ->
      match p with
      | Box _ -> true
      | _ -> false)
  in
  let faults = ref [] in
  let note (fault : fault) : unit = faults := fault :: !faults in
  let within ((x, y) : int * int) : unit =
    if x < 0 || x > extent.width || y < 0 || y > extent.height
    then note (Outside_extent { at = x, y })
  in
  List.iter placed ~f:(fun p ->
    match p with
    | Caption { x; y; _ } -> within (x, y)
    | Box { x; y; width; height; _ } ->
      within (x, y);
      within (x + width, y + height)
    | Rail { points; _ } ->
      List.iter points ~f:within;
      let rec segments (points : (int * int) list) : unit =
        match points with
        | a :: (b :: _ as rest) ->
          List.iter boxes ~f:(fun box ->
            match crosses box a b with
            | None -> ()
            | Some at ->
              (match box with
               | Box { label; _ } -> note (Rail_through_station { label; at })
               | _ -> ()));
          segments rest
        | _ -> ()
      in
      segments points);
  List.rev !faults
;;

(* -- drawing --------------------------------------------------------------- *)

(* Every element carries its own presentation, as well as the class a
   stylesheet reaches it by.

   An SVG shape defaults to a black fill and no stroke. Left at that, a rail
   becomes a zero-area polygon that draws nothing, and a station becomes a
   black slab with black text inside it. *)
let track = "fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\""

(* A head sits on the longest horizontal run of a rail, and points the way
   the track is travelled.

   A grammar is directed, and a row read right to left separates a loop from
   a choice. A head marks that direction, so a reader can tell the two
   apart. Short connectors take no head, because a head on every twelve-unit
   stub is noise. *)
let head (buf : Buffer.t) (points : (int * int) list) (dir : dir) : unit =
  let best = ref None in
  let rec widest (points : (int * int) list) : unit =
    match points with
    | (x1, y1) :: ((x2, y2) :: _ as rest) ->
      if y1 = y2
      then (
        let span = abs (x2 - x1) in
        match !best with
        | Some (seen, _, _) when seen >= span -> ()
        | _ -> best := Some (span, (x1 + x2) / 2, y1));
      widest rest
    | _ -> ()
  in
  widest points;
  (* A rail travelled right to left takes a head on any run of 10 units or
     more. A reader cannot guess that direction, and a loop's return rails
     are short. A rail travelled the ordinary way needs 20, which is long
     enough to carry a head without crowding. *)
  let least =
    match dir with
    | Ltr -> 20
    | Rtl -> 10
  in
  match !best with
  | Some (span, x, y) when span >= least ->
    let reach = min 5 (span / 3) in
    let step =
      match dir with
      | Ltr -> reach
      | Rtl -> -reach
    in
    Buffer.add_string
      buf
      (Printf.sprintf
         "    <path class=\"lg-arrow\" fill=\"currentColor\" d=\"M %d %d L %d %d L %d %d \
          Z\" />\n"
         (x + step)
         y
         (x - step)
         (y - 4)
         (x - step)
         (y + 4))
  | _ -> ()
;;

let draw (buf : Buffer.t) (p : placed) : unit =
  match p with
  | Rail { points; dir } ->
    Buffer.add_string buf ("    <polyline class=\"lg-track\" " ^ track ^ " points=\"");
    Buffer.add_string
      buf
      (String.concat
         ~sep:" "
         (List.map points ~f:(fun (x, y) -> Printf.sprintf "%d,%d" x y)));
    Buffer.add_string buf "\" />\n";
    head buf points dir
  | Caption { x; y; text } ->
    Buffer.add_string
      buf
      (Printf.sprintf
         "    <text class=\"lg-slot\" fill=\"currentColor\" font-family=\"system-ui, \
          sans-serif\" font-size=\"10\" x=\"%d\" y=\"%d\">%s</text>\n"
         x
         y
         (Render.escape text))
  | Box { x; y; width; height; terminal; label; link } ->
    let open_link, close_link =
      match link with
      | None -> "", ""
      | Some href -> Printf.sprintf "    <a href=%S>\n" (Render.escape href), "    </a>\n"
    in
    Buffer.add_string buf open_link;
    Buffer.add_string
      buf
      (Printf.sprintf
         "    <rect class=%S fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.2\" \
          x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"%d\" />\n"
         (if terminal then "lg-terminal" else "lg-nonterminal")
         x
         y
         width
         height
         (if terminal then height / 2 else 3));
    Buffer.add_string
      buf
      (Printf.sprintf
         "    <text class=%S fill=\"currentColor\" font-family=\"ui-monospace, \
          monospace\" font-size=\"12\" text-anchor=\"middle\" x=\"%d\" y=\"%d\">%s</text>\n"
         (if terminal then "lg-terminal-text" else "lg-nonterminal-text")
         (x + (width / 2))
         (y + (height / 2) + 4)
         (Render.escape label));
    Buffer.add_string buf close_link
;;

let svg ?title ?width (shape : t) : string =
  let placed, extent = place ?width shape in
  let buf = Buffer.create 2048 in
  Buffer.add_string
    buf
    (Printf.sprintf
       "  <svg class=\"lg-diagram\" role=\"img\" viewBox=\"0 0 %d %d\" width=\"%d\" \
        height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">\n"
       extent.width
       extent.height
       extent.width
       extent.height);
  (* A reader who cannot see the picture gets the title read out. *)
  (match title with
   | None -> ()
   | Some title ->
     Buffer.add_string
       buf
       (Printf.sprintf "    <title>%s</title>\n" (Render.escape title)));
  List.iter placed ~f:(draw buf);
  Buffer.add_string buf "  </svg>\n";
  Buffer.contents buf
;;
