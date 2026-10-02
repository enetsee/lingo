open StdLabels

let ( ^^ ) = Handsome.Utf8.( ^^ )
let empty = Handsome.Utf8.empty

type break = Ir.Layout.break =
  | Flat
  | Fit
  | Hard of int

type trailing = Ir.Layout.trailing =
  | Never
  | On_break
  | Always

type sep = Ir.Layout.sep =
  { sep_kind : Ir.Kind.t
  ; text : string
  ; trailing : trailing
  }

type frame = Ir.Layout.frame =
  | Plain
  | Delimited of
      { open_ : Ir.Kind.t
      ; close : Ir.Kind.t
      ; sep : sep option
      ; open_space : bool
      }
  | Separated of sep

type slot = Ir.Layout.slot =
  { kinds : Ir.Kind.t array
  ; repeats : bool
  ; before : break
  ; between : break
  ; space : bool
  }

type rule = Ir.Layout.rule =
  { name : string
  ; kind : Ir.Kind.t
  ; frame : frame
  ; slots : slot array
  ; body : break
  ; inner : break
  ; indent : int
  }

type trivia = Ir.Layout.trivia =
  | Reformat
  | Preserve

type token = Ir.Layout.token =
  { space_before : bool
  ; space_after : bool
  ; trivia : trivia option
  }

type t = Ir.Layout.t =
  { rules : rule array
  ; of_kind : int array
  ; tokens : token option array
  }

(* Byte zero is a boundary because every lexeme starts after it. *)
let boundary ~(lex : string -> Token.t array) (s : string) (i : int) : bool =
  if i = 0
  then true
  else (
    let at = ref 0 in
    let hit = ref false in
    Array.iter (lex s) ~f:(fun (t : Token.t) ->
      at := !at + String.length t.text;
      if !at = i then hit := true);
    !hit)
;;

(* The last token written, and whether a space may follow it. *)
type edge =
  { kind : Ir.Kind.t
  ; space_after : bool
  }

(* [run] is every byte written since the last blank. That is the left-hand side
   of the max-munch test.

   [brk] holds the strongest break any boundary crossed since the last token,
   and [gap] whether a space goes at the boundary in front of the next one. The
   rule holding that boundary sets [gap], and a node's first child leaves it
   alone, because nothing of that node precedes it. Both are requests, and a
   token discharges them. A child that writes nothing discharges nothing, so
   {!node} puts them back rather than letting them reach a token in the frame
   above. *)
type state =
  { last : edge option
  ; run : string
  ; brk : Ir.Layout.break
  ; gap : bool option
  ; held : bool (* The last token written keeps the line the source gave it. *)
  ; written : int (* How many tokens have gone into the document. *)
  ; hard : int
    (* How many line breaks the document holds whatever the ruler: one at a
       boundary that broke hard, and each newline inside a token. *)
  }

let start =
  { last = None
  ; run = ""
  ; brk = Ir.Layout.Flat
  ; gap = None
  ; held = false
  ; written = 0
  ; hard = 0
  }
;;

let stronger (a : Ir.Layout.break) (b : Ir.Layout.break) : Ir.Layout.break =
  match a, b with
  | Hard m, Hard n -> Hard (if m > n then m else n)
  | (Hard _ as h), _ | _, (Hard _ as h) -> h
  | Fit, _ | _, Fit -> Fit
  | Flat, Flat -> Flat
;;

let rec breaks (n : int) : Ir.Kind.t Handsome.Utf8.t =
  if n <= 1 then Handsome.Utf8.hardline else Handsome.Utf8.hardline ^^ breaks (n - 1)
;;

(* What has to go between the bytes already written and the ones about to be,
   for the pair to read back as two lexemes.

   The candidates are tried cheapest first: nothing, then a space, then a line
   break. The first that keeps the boundary wins.

   D9's two degenerate cases fall out rather than being named. A lexeme running
   to the end of its line gives [Newline], and one running to the end of the
   input gives [None_at_all]. Both come from the grammar's own automaton, and no
   class of characters here could stand in for it. *)
type join =
  | Touching
  | Blank
  | Newline
  | None_at_all

let join ~(boundary : string -> int -> bool) ~(run : string) ~(next : string) : join =
  if run = "" || next = ""
  then Touching
  else (
    let at glue = boundary (run ^ glue ^ next) (String.length run + String.length glue) in
    if at ""
    then Touching
    else if at " "
    then Blank
    else if at "\n"
    then Newline
    else None_at_all)
;;

let name_of (j : join) : string =
  match j with
  | Touching -> "join-touching"
  | Blank -> "join-blank"
  | Newline -> "join-newline"
  | None_at_all -> "join-none"
;;

(* [boundary] is the grammar's own lexer. [trace] is how a law counts which of
   the fold's steps a corpus reaches, because a step no input reaches is one no
   law covers. *)
type env =
  { lay : Ir.Layout.t
  ; boundary : string -> int -> bool
  ; trace : step:string -> kind:Ir.Kind.t -> unit
  ; kind : Ir.Kind.t (* The rule the fold is inside. *)
  }

let say (e : env) (step : string) : unit = e.trace ~step ~kind:e.kind

(* Only a token's spacing flags are read at a boundary, so a kind that is not a
   token gives the neutral entry. [trivia] is read of any child. *)
let token (lay : Ir.Layout.t) (k : Ir.Kind.t) =
  match lay.tokens.(k) with
  | Some t -> t
  | None -> Ir.Layout.not_a_token
;;

let joins (e : env) (st : state) ~(next : string) : join =
  let j = join ~boundary:e.boundary ~run:st.run ~next in
  say e (name_of j);
  j
;;

(* Indentation is a count of spaces clamped at zero. handsome has no primitive
   for a line at column zero, so one comes from nesting down by more than any
   document nests up. *)
let column_zero = Handsome.Utf8.nest (-1_000_000) Handsome.Utf8.hardline

(* The bytes the fold may write, and where they come from.

   Law B rests on this: a formatter prints what the tree holds. The predecessor
   left it in a comment, and its [format] wrote a delimiter the tree recorded as
   missing, then wrote one more on every pass.

   So it is a signature instead. A [t] is a token the tree carries or the
   separator a body's policy adds, and there is no third constructor. The one
   exception has a name of its own, so it is easy to find. *)
module Written : sig
  type t

  val of_token : Siesta.Green.token -> t

  (* The separator a body's policy adds after its last element. These are the
     only bytes in the output that the tree does not hold.

     It only ever adds, and only into a frame the parse closed. Removing one does
     not settle: it changes the token run, and that changes which frame the next
     parse gives the closer to. *)
  val separator : Ir.Kind.t -> string -> t
  val kind : t -> Ir.Kind.t
  val text : t -> string

  (* The token, with its own newlines in the document rather than inside a text
     node. An unterminated block comment holds them, and so does any comment
     spanning lines.

     A newline inside a text node leaves every enclosing group measuring the
     token as though it could be flat, and the column wrong after it. That is
     D6, and [Handsome.check] rejects it.

     The later lines go back at column zero. Their indentation is already in the
     token's own text, and adding more would change the bytes it matched. *)
  val doc : t -> Ir.Kind.t Handsome.Utf8.t
end = struct
  type t =
    { kind : Ir.Kind.t
    ; text : string
    }

  let of_token t = { kind = Siesta.Green.Token.kind t; text = Siesta.Green.Token.text t }
  let separator kind text = { kind; text }
  let kind t = t.kind
  let text t = t.text

  let doc t =
    let d =
      match String.split_on_char ~sep:'\n' t.text with
      | [] -> empty
      | first :: rest ->
        List.fold_left rest ~init:(Handsome.Utf8.text first) ~f:(fun acc line ->
          acc ^^ column_zero ^^ Handsome.Utf8.text line)
    in
    Handsome.Utf8.annotate t.kind d
  ;;
end

(* Every space, every line break and every byte of a token goes through here.

   Three things meet at a boundary, and they rank. A join the lexer will not
   close outranks the production's break style, because no break style is a
   solution where the bytes fuse. The gap settles the rest, and it only ever adds
   a blank the join had not already required. A token whose side is [false]
   removes the gap's space. *)
let glue (e : env) (st : state) (w : Written.t) : Ir.Kind.t Handsome.Utf8.t * state =
  let kind = Written.kind w
  and text = Written.text w in
  let lay = e.lay in
  let leaving ~clears ~(broke : bool) =
    { last =
        Some { kind; space_after = (token lay kind).space_after }
        (* A newline inside a token is not a blank this printed, and the lexer
           does not stop at one. It is a byte of a lexeme still open, which is
           what an unterminated block comment is, so the run carries on. *)
    ; run = (if clears then "" else st.run) ^ text
    ; brk = Flat
    ; gap = None
    ; held = st.held
    ; written = st.written + 1
    ; hard =
        (st.hard + (if broke then 1 else 0) + if String.contains text '\n' then 1 else 0)
    }
  in
  match st.last with
  | None -> empty, leaving ~clears:false ~broke:false
  | Some prev ->
    let spaced =
      Option.value st.gap ~default:true
      && prev.space_after
      && (token lay kind).space_before
    in
    let j = joins e st ~next:text in
    let blank = spaced || j = Blank in
    let brk =
      match j with
      | Newline | None_at_all -> stronger st.brk (Hard 1)
      | Touching | Blank -> st.brk
    in
    let d, clears, broke =
      match brk with
      | Hard n ->
        say e "break-hard";
        breaks n, true, true
      | Fit ->
        say e "break-fit";
        (if blank then Handsome.Utf8.line else Handsome.Utf8.softline), blank, false
      | Flat ->
        say e "break-flat";
        (if blank then Handsome.Utf8.text " " else empty), blank, false
    in
    d, leaving ~clears ~broke
;;

(* -- what each child is -------------------------------------------------- *)

type role =
  | Opener
  | Closer
  | Separator
  | Filled
  | Stray

type entry =
  { child : Siesta.Green.child
  ; role : role
  ; held : bool
  ; before : Ir.Layout.break
  ; gap : bool option (* [None] on the first child, which has no gap in this rule. *)
  ; starts_line : bool
    (* The grammar breaks hard in front of this child, so its line is a new one
       rather than a continuation of the rule. *)
  }

(* The rule for a kind that has none: a sequence, on the lines the source had.
   Recovery builds those nodes, and such a node's extent is whatever it
   swallowed, so a layout taken from its width would move under a reparse and
   the fixed point would go. *)
let unruled : Ir.Layout.rule =
  { name = ""
  ; kind = 0
  ; frame = Plain
  ; slots = [||]
  ; body = Flat
  ; inner = Flat
  ; indent = 0
  }
;;

let kind_of (c : Siesta.Green.child) =
  match c with
  | Siesta.Green.Node n -> Siesta.Green.kind n
  | Siesta.Green.Token t -> Siesta.Green.Token.kind t
;;

let sep_of (r : Ir.Layout.rule) =
  match r.frame with
  | Delimited { sep; _ } -> sep
  | Separated s -> Some s
  | Plain -> None
;;

let all_space (s : string) : bool =
  String.for_all s ~f:(fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r')
;;

let sep_of (r : Ir.Layout.rule) =
  match r.frame with
  | Delimited { sep; _ } -> sep
  | Separated s -> Some s
  | Plain -> None
;;

(* The slot a child fills, and whether that slot already held one. The scan runs
   forward from the slot last filled, so two slots admitting the same kind stay
   apart: an infix role's [lhs] and [rhs] are both the block's own kinds, and
   only their order separates them. *)
let slot_of (r : Ir.Layout.rule) ~(prev : int) (k : Ir.Kind.t) : (int * bool) option =
  let n = Array.length r.slots in
  let holds i = Array.exists r.slots.(i).kinds ~f:(fun x -> x = k) in
  if prev >= 0 && r.slots.(prev).repeats && holds prev
  then Some (prev, true)
  else (
    let rec go i =
      if i >= n then None else if holds i then Some (i, false) else go (i + 1)
    in
    go (prev + 1))
;;

(* Where the frame's own delimiters sit among the children. One the parse
   never read is a childless node of the delimiter's kind, so this finds it
   either way. *)
let delimiters (r : Ir.Layout.rule) (cs : Siesta.Green.child array) =
  match r.frame with
  | Delimited { open_; close; _ } ->
    let n = Array.length cs in
    let rec first i =
      if i >= n then -1 else if kind_of cs.(i) = open_ then i else first (i + 1)
    in
    let rec last i =
      if i < 0 then -1 else if kind_of cs.(i) = close then i else last (i - 1)
    in
    let o = first 0 in
    let c = last (n - 1) in
    o, if c > o then c else -1
  | Plain | Separated _ -> -1, -1
;;

(* Every child the document gets, in the order the tree holds them, with what
   the boundary in front of each one does to the line.

   One thing comes out: whitespace whose trivia is [Reformat], because the
   boundaries write the spacing back. Nothing else leaves. *)
let entries
      (e : env)
      (r : Ir.Layout.rule)
      ~hold_all
      ~prev_held
      (cs : Siesta.Green.child array)
  =
  let lay = e.lay in
  let o, c = delimiters r cs in
  let sep_kind =
    match sep_of r with
    | Some s -> Some s.sep_kind
    | None -> None
  in
  let kept = ref [] in
  (* How many line breaks the whitespace dropped in front of a child held. *)
  let nls = ref 0 in
  let prev_slot = ref (-1) in
  let body_started = ref false in
  let seen_open = ref false in
  let elements = ref 0 in
  Array.iteri cs ~f:(fun i ch ->
    let k = kind_of ch in
    let tok = token lay k in
    let spacing =
      match tok.trivia, ch with
      | Some Reformat, Siesta.Green.Token t -> Some (Siesta.Green.Token.text t)
      | _ -> None
    in
    match spacing with
    | Some s when all_space s ->
      nls
      := !nls + String.fold_left s ~init:0 ~f:(fun n c -> if c = '\n' then n + 1 else n)
    | Some _ | None ->
      let recovered =
        match ch with
        | Siesta.Green.Node n -> lay.of_kind.(k) < 0 && Siesta.Green.num_children n > 0
        | Siesta.Green.Token _ -> false
      in
      let held = hold_all || tok.trivia <> None || recovered in
      let again = ref false in
      let role =
        if i = o
        then (
          seen_open := true;
          Opener)
        else if i = c
        then Closer
        else if Some k = sep_kind && i > o && (c < 0 || i < c)
        then Separator
        else (
          match slot_of r ~prev:!prev_slot k with
          | Some (s, ag) ->
            prev_slot := s;
            again := ag;
            incr elements;
            if ag then say e "repeat";
            Filled
          | None ->
            say e "stray";
            Stray)
      in
      let was_held =
        match !kept with
        | e :: _ -> e.held
        | [] -> prev_held
      in
      (* A blank line the source has is kept, one at most, where the boundary
         breaks hard anyway: between two statements, two members, or in front
         of a comment on a line of its own. A blank line straight inside a
         delimiter goes. The output then has a blank line where the source did,
         so the next format keeps it too. *)
      let keep_blank (b : Ir.Layout.break) : Ir.Layout.break =
        let inside =
          match !kept with
          | [] -> false
          | { role = Opener; _ } :: _ -> false
          | _ :: _ -> role <> Closer
        in
        match b with
        | Hard n when !nls >= 2 && inside ->
          say e "blank-kept";
          Hard (if n > 2 then n else 2)
        | Flat | Fit | Hard _ -> b
      in
      let before : Ir.Layout.break =
        keep_blank
        @@
        if held || was_held
        then (
          say e (if !nls > 0 then "held-line" else "held-same");
          if !nls > 0 then Hard 1 else Flat)
        else (
          match role with
          | Opener | Separator -> Flat
          | Closer ->
            (match !kept, ch with
             (* A frame with nothing in it has nothing to break around. *)
             | { role = Opener; _ } :: _, _ -> Flat
             (* A closer the parse never found writes nothing, so there is
                nothing to break in front of. Leaving the rule's break there
                cuts the run, and the group that settles the line then stops
                short of the tokens the caller writes on it. *)
             | _, Siesta.Green.Node n when Siesta.Green.num_children n = 0 -> Flat
             | _, _ -> r.inner)
          | Stray -> r.body
          | Filled ->
            if !seen_open && not !body_started
            then r.inner
            else if !again
            then r.slots.(!prev_slot).between
            else r.slots.(!prev_slot).before)
      in
      (* The rule's slots set the gap between two of its children, and its
         frame sets the gaps just inside the delimiters, whatever sits there.
         A separator takes a space on both sides, and [,] removes the one in
         front of it with its own flag. [→] keeps both. *)
      let gap =
        match !kept with
        | [] -> None
        | prev :: _ ->
          Some
            (match prev.role, role with
             | _, Opener ->
               (match r.frame with
                | Delimited { open_space; _ } -> open_space
                | Plain | Separated _ -> true)
             | Opener, _ | _, Closer -> false
             | _, (Separator | Stray) | Separator, Filled -> true
             | (Closer | Filled | Stray), Filled -> r.slots.(!prev_slot).space)
      in
      (match role with
       | Filled -> body_started := true
       | Opener | Closer | Separator | Stray -> ());
      nls := 0;
      let starts_line =
        (not (held || was_held))
        && role = Filled
        &&
        match before with
        | Hard _ -> true
        | Flat | Fit -> false
      in
      kept := { child = ch; role; held; before; gap; starts_line } :: !kept);
  let es = Array.of_list (List.rev !kept) in
  let n = Array.length es in
  let index role =
    let rec go i = if i >= n then -1 else if es.(i).role = role then i else go (i + 1) in
    go 0
  in
  es, index Opener, index Closer
;;

(* -- the walk ------------------------------------------------------------ *)

(* A child with no bytes: a hole, a delimiter the parse looked for and did not
   find, a token recovery inserted. {!node} writes nothing for one and puts its
   boundary back, so it is the same test spelled in both places. *)
let silent (it : entry) : bool =
  match it.child with
  | Siesta.Green.Node n -> Array.length (Siesta.Green.children_array n) = 0
  | Siesta.Green.Token t -> Siesta.Green.Token.text t = ""
;;

(* Where the run of children sharing one line ends. A boundary that cannot end
   the line reads [Flat], and the layout settles that on its own, so a run's
   extent is known before anything is rendered. What a boundary prints is
   settled later, from the bytes.

   A child that writes nothing does not end the run, whatever its boundary
   says. The boundary goes back where the child writes nothing, so what follows
   lands on the line before it. *)
let rec flat_end (es : entry array) (i : int) (stop : int) : int =
  if i >= stop
  then i
  else if silent es.(i)
  then flat_end es (i + 1) stop
  else if es.(i).before <> Ir.Layout.Flat
  then i
  else flat_end es (i + 1) stop
;;

(* [indent] is what a boundary of this rule does to the lines after it. A rule
   with one child has no boundary, so nesting there would add its indent to
   every rule that wraps a single child on the way down. *)
let nest_of (r : Ir.Layout.rule) (children : int) =
  if children > 1 then Handsome.Utf8.nest r.indent else Fun.id
;;

(* [walk env es i stop] is the children in [i, stop), one after another.

   Each child hands back the glue in front of it apart from its own document,
   so the caller can put that glue outside the group it wraps the child in. The
   first child's goes back to this walk's caller for the same reason; the rest
   are written where they fall. *)
let rec walk (e : env) (es : entry array) (i : int) (stop : int) (st : state) =
  if i >= stop
  then empty, empty, st
  else (
    let it = es.(i) in
    (* The break this boundary carries, and what stood before it. A child that
       writes nothing had no boundary in front of it, so the request goes back:
       carried on, it would reach a token in some other frame and break a
       boundary outside that frame. *)
    let stood = st.brk
    and stood_gap = st.gap in
    let st = { st with brk = stronger st.brk it.before } in
    let st =
      match it.gap with
      | Some _ as g -> { st with gap = g }
      | None -> st
    in
    let lead, d, st = child e it ~stood ~stood_gap st in
    let l, rest, st = walk e es (i + 1) stop st in
    lead, d ^^ l ^^ rest, st)

(* [split env es i stop ~nest] is {!walk} over a plain rule's children.

   A plain sequence breaks from its last gap backwards. A group decides each
   gap, and it holds everything to the gap's left and nothing to its right, so
   the outermost decides the last gap and an earlier one breaks only where its
   own group still does not fit. [def f() = match (x) {] at a narrow ruler
   breaks in front of [match] and keeps [def f() =] together.

   Each child is indented from the line it starts on, and [nest] goes on the
   gaps alone, so a line a gap breaks onto is a continuation and steps in.
   A block that opens on the header's last line is then indented from that
   line, whether the header broke or not.

   A child that breaks across lines on its own stays out of every group deciding
   a gap. A group holding a hard line break lays out broken, and the child's own
   lines would otherwise break the gaps on both sides of it. json's
   [{"a": {"b": 1}}] put ["a"], [:] and [{] on three lines when they did. So
   the gap in front of the child closes the groups so far, and the gaps after it
   are decided from the child's last line on.

   Where the grammar breaks hard in front of a child, the line is a new one
   rather than a continuation, so that gap takes no [nest]: effekt's
   definition after its doc comment starts where the comment does.

   A delimited or separated body keeps one group, so a list breaks all its
   separators or none. *)
and split
      (e : env)
      (es : entry array)
      (i : int)
      (stop : int)
      ~(nest : Ir.Kind.t Handsome.Utf8.t -> Ir.Kind.t Handsome.Utf8.t)
      (st : state)
  =
  let group = Handsome.Utf8.group
  and from_line = Handsome.Utf8.from_line in
  let enter (it : entry) (st : state) =
    let st = { st with brk = stronger st.brk it.before } in
    match it.gap with
    | Some _ as g -> { st with gap = g }
    | None -> st
  in
  (* [out] is what is settled: everything up to the last child that broke on
     its own. [acc] is what follows it, for the next gap's group to hold. *)
  let rec go
            (i : int)
            ~(out : Ir.Kind.t Handsome.Utf8.t)
            (acc : Ir.Kind.t Handsome.Utf8.t)
            (st : state)
    =
    if i >= stop
    then out ^^ group acc, st
    else (
      let it = es.(i) in
      let stood = st.brk
      and stood_gap = st.gap in
      let st = enter it st in
      let lead, d, after = child e it ~stood ~stood_gap st in
      let gap =
        if it.starts_line
        then (
          say e "new-line";
          lead)
        else nest lead
      in
      if after.hard > st.hard
      then (
        say e "split";
        go (i + 1) ~out:(out ^^ group (group acc ^^ gap) ^^ from_line d) empty after)
      else go (i + 1) ~out (group acc ^^ gap ^^ from_line d) after)
  in
  if i >= stop
  then empty, empty, st
  else (
    (* The first child's glue goes back to the caller, as {!walk}'s does. *)
    let it = es.(i) in
    let stood = st.brk
    and stood_gap = st.gap in
    let before = st.hard in
    let lead, d, st = child e it ~stood ~stood_gap (enter it st) in
    let d, st =
      if st.hard > before
      then go (i + 1) ~out:(from_line d) empty st
      else go (i + 1) ~out:empty (from_line d) st
    in
    lead, d, st)

and child
      (e : env)
      (it : entry)
      ~(stood : Ir.Layout.break)
      ~(stood_gap : bool option)
      (st : state)
  =
  match it.child with
  | Siesta.Green.Token t ->
    let w = Written.of_token t in
    let lead, st = glue e st w in
    lead, Written.doc w, { st with held = it.held }
  | Siesta.Green.Node n -> node e n ~stood ~stood_gap st

and node
      (e : env)
      (n : Siesta.Green.node)
      ~(stood : Ir.Layout.break)
      ~(stood_gap : bool option)
      (st : state)
  =
  let entry = st.written in
  (* [undo] belongs to the boundary in front of this node, which is the enclosing
     rule's, so it traces against the [e] this was called with. *)
  let undo st =
    if st.written = entry
    then (
      if st.brk <> stood || st.gap <> stood_gap then say e "break-back";
      { st with brk = stood; gap = stood_gap })
    else st
  in
  let k = Siesta.Green.kind n in
  let e = { e with kind = k } in
  let cs = Siesta.Green.children_array n in
  (* A node with no children stands in for something the parse never read: a
     hole, or a delimiter it looked for and did not find. It has no bytes, so
     nothing is written for it.

     Writing the delimiter back would be repair. The bytes are not in the tree,
     which is why [Green.to_source] gives the input back, so writing one would
     put bytes in the output the source never had. The predecessor wrote them,
     and its output grew one delimiter per pass. *)
  if Array.length cs = 0
  then (
    say e "absent";
    empty, empty, undo st)
  else (
    let ri = e.lay.of_kind.(k) in
    let ruled = ri >= 0 in
    let r = if ruled then e.lay.rules.(ri) else unruled in
    if not ruled then say e "unruled";
    let es, o, c = entries e r ~hold_all:(not ruled) ~prev_held:st.held cs in
    let last = Array.length es in
    (* What the body does about a separator after its last element.

       [closed] is the parse having found the frame's closer. A separator written
       into a frame it did not close lets the next parse take a token from
       outside the frame as an element, so the body grows by one on every pass.

       [tail_sep] is a separator the source already has there. Under [On_break]
       that is a request for a broken body, and it is the only way a grammar's
       user can make one directly. Under [Always] the separator is a terminator
       and carries no request. Either way it stays, and no second one is
       written. *)
    let closed =
      c >= 0
      &&
      match es.(c).child with
      | Siesta.Green.Token _ -> true
      | Siesta.Green.Node _ -> false
    in
    let body_end = if c >= 0 then c else last in
    let elt =
      let rec go i =
        if i <= o then -1 else if es.(i).role = Filled then i else go (i - 1)
      in
      go (body_end - 1)
    in
    (* Whether the source already has a separator after the last element, and
       where a lone one sits. Only a lone one is a byte the policy may take
       back: a body holding two keeps both, because the fold writes every token
       the tree holds and one of the two is not a policy's to give. *)
    let tail_sep, lone_sep =
      let rec go (i : int) (n : int) (at : int) =
        if i >= body_end
        then n, at
        else if es.(i).role = Separator
        then go (i + 1) (n + 1) (if n = 0 then i else at)
        else go (i + 1) n at
      in
      let n, at = if elt < 0 then 0, -1 else go (elt + 1) 0 (-1) in
      n > 0, if n = 1 then at else -1
    in
    (* Whether the last byte the body writes came out of a node the layout does
       not rule. That is an error node, which is where recovery puts the tokens
       it could not place.

       A separator written after one of those is swept into it by the next
       parse. This fold reads its own children for a separator already there,
       an error node's contents are not among them, and so it writes another --
       and the body gains one on every pass without ever settling. rust's
       [match]{e(}] is the case: the arm ends in an error node holding [(], and
       [e(] becomes [e(,] becomes [e(,,].

       An inner frame the parse never closed does the same: it is still taking
       tokens where the body ends, so the separator becomes its trailing one
       rather than this body's. rust's [match]{e\te(!}] is that case, where the
       call [e(] has no [)].

       Reading the body's last token instead of its children does not work. An
       element can end in a token of the separator's own kind -- a [Let] in
       grammars/shapes_grammar.ml ends in the [;] that is also its body's
       separator -- and then there is no telling whose it is. *)
    let swept_from =
      let rec last (inside : bool) (c : Siesta.Green.child) : bool option =
        match c with
        | Siesta.Green.Token t ->
          let k = Siesta.Green.Token.kind t in
          if Siesta.Green.Token.text t = "" || (token e.lay k).trivia <> None
          then None
          else Some inside
        | Siesta.Green.Node n ->
          let k = Siesta.Green.kind n in
          let cs = Siesta.Green.children_array n in
          let ri =
            if k < 0 || k >= Array.length e.lay.of_kind then -1 else e.lay.of_kind.(k)
          in
          (* Unruled is an error node. A delimited rule whose closer the parse
             never found is the same thing under another name: both are still
             taking tokens where the body ends, so both take the separator. *)
          let open_frame =
            ri < 0
            ||
            match e.lay.rules.(ri).frame with
            | Ir.Layout.Delimited { close; _ } ->
              not
                (Array.exists cs ~f:(function
                   | Siesta.Green.Token t ->
                     Siesta.Green.Token.kind t = close && Siesta.Green.Token.text t <> ""
                   | Siesta.Green.Node _ -> false))
            | Ir.Layout.Plain | Ir.Layout.Separated _ -> false
          in
          let inside = inside || open_frame in
          let rec back i =
            if i < 0
            then None
            else (
              match last inside cs.(i) with
              | Some _ as found -> found
              | None -> back (i - 1))
          in
          back (Array.length cs - 1)
      in
      let rec body_back i =
        if i <= o
        then false
        else (
          match last false es.(i).child with
          | Some swept -> swept
          | None -> body_back (i - 1))
      in
      body_back
    in
    (* Asked of the body as it ends today, which is what an added separator
       would follow. *)
    let swept_tail = swept_from (body_end - 1) in
    (* Asked of the body without its trailing separator, which is what taking
       one back leaves behind. The two differ, because a separator already
       there is the last token the first one reaches, so it never sees the
       element in front of it. [\[\[j,,\]] is the case: the outer body's
       separator hides an inner frame the parse never closed, and dropping it
       hands that frame the outer's closer on the next pass. *)
    let swept_elt = swept_from elt in
    let policy =
      match sep_of r with
      | Some s when s.text <> "" && elt >= 0 -> Some s
      | Some _ | None -> None
    in
    (* What the body's tail does. It has to be one shape whether the separator
       came from the source or from here: splitting the last run only when the
       fold adds one makes the document's shape depend on that, and the glue then
       lands elsewhere on the pass after the one that added it. *)
    let tail_policy =
      match policy with
      | None -> `Plain
      | Some s ->
        if (not closed) || swept_tail
        then if tail_sep then `Present s else `Plain
        else (
          match s.trailing, tail_sep with
          (* The separator the source has is the one this would have written,
             so it goes under the same conditional rather than being written
             flat. That is what keeps the two passes building one document:
             the separator stops saying anything about how the body was laid
             out, so reading it back cannot move a decision. *)
          | On_break, true when lone_sep >= 0 && not swept_elt -> `Held lone_sep
          | (Never | Always | On_break), true -> `Present s
          | Never, false -> `Plain
          | Always, false -> `Add s
          | On_break, false -> `Maybe s)
    in
    (* Where the body segment ends, which is not always where the body does.
       The closer normally starts its own line, so it sits outside the nest. A
       recovery node beside it can hold it to the body's last line instead, and
       then it belongs to that line. Nesting it changes nothing there, because
       no break is taken. *)
    let body_stop =
      if c >= 0 && es.(c).before = Ir.Layout.Flat
      then flat_end es (c + 1) last
      else body_end
    in
    (* Where the body's own segment starts. Slot zero's boundary is always
       [Flat], so the body's first run lands on the opener's line. *)
    let body_from = flat_end es (o + 1) body_stop in
    (* The separator a policy adds, straight after the last element.

       After the last element because a comment can sit between it and the
       closer, and a separator written past the comment is on the wrong side of
       it.

       [On_break] turns on whether the body broke, which is this frame's group
       and not the element's. {!Handsome.Utf8.framed} is what separates the
       two: the byte stays where it is printed, inside the group that settles
       its line, and the conditional follows the group that decides however
       many groups lie between them.

       What is folded twice is everything after the last element, and only
       that. The separator changes the run, so a token following it on the same
       line glues differently with it and without -- a held comment does, and
       the spacing of one is the difference between a format that settles and
       one that alternates. The element itself is folded once and stays where
       the head put it.

       One shape either way. A separator the source already has is the flat
       branch and one this adds is the broken branch, so the document does not
       change shape on the pass after the one that added it. *)
    let body
          ~(alt :
             Ir.Kind.t Handsome.Utf8.t
             -> Ir.Kind.t Handsome.Utf8.t
             -> Ir.Kind.t Handsome.Utf8.t)
          (first : int)
          (stop : int)
          (st : state)
      =
      match tail_policy with
      (* No policy, so no split: cutting the walk at the last element would
         truncate every run that crosses it. *)
      | `Plain ->
        (match r.frame with
         | Plain -> split e es first stop ~nest:(nest_of r last) st
         | Delimited _ | Separated _ -> walk e es first stop st)
      (* The separator is a child here, so the broken branch walks it and the
         flat branch steps over it. Nothing is written that the tree does not
         hold, and one token of it goes unwritten. *)
      | `Held at ->
        if elt < first || elt >= stop
        then walk e es first stop st
        else (
          let rest ~(sep : bool) (st : state) =
            if sep
            then (
              let l, d, st = walk e es (elt + 1) stop st in
              l ^^ d, st)
            else (
              say e "sep-dropped";
              let l, d, st = walk e es (elt + 1) at st in
              let l2, d2, st = walk e es (at + 1) stop st in
              l ^^ d ^^ l2 ^^ d2, st)
          in
          let lead, head, st = walk e es first (elt + 1) st in
          let without, after_without = rest ~sep:false st in
          let with_sep, _ = rest ~sep:true st in
          lead, head ^^ alt without with_sep, after_without)
      | (`Present s | `Add s | `Maybe s) as p ->
        if elt < first || elt >= stop
        then walk e es first stop st
        else (
          let written = Written.separator s.sep_kind s.text in
          let rest ~(sep : bool) (st : state) =
            let d1, st =
              if not sep
              then empty, st
              else (
                say
                  e
                  (match s.trailing with
                   | Always -> "sep-always"
                   | On_break -> "sep-on-break"
                   | Never -> "sep-never");
                let lead, st = glue e { st with gap = Some true } written in
                lead ^^ Written.doc written, st)
            in
            let l, d, st = walk e es (elt + 1) stop st in
            d1 ^^ l ^^ d, st
          in
          let lead, head, st = walk e es first (elt + 1) st in
          let d, st =
            match p with
            | `Present _ -> rest ~sep:false st
            | `Add _ -> rest ~sep:true st
            | `Maybe _ ->
              (* Both foldings start from the state the element left. Taking
                 the state the first one leaves would put the glue of the
                 second in the wrong place. *)
              let without, after_without = rest ~sep:false st in
              let with_sep, _ = rest ~sep:true st in
              alt without with_sep, after_without
          in
          lead, head ^^ d, st)
    in
    let build ~alt =
      if o < 0
      then (
        let lead, d, st = body ~alt 0 last st in
        (* {!split} nests a plain rule's own pieces. *)
        let d =
          match r.frame with
          | Plain -> d
          | Delimited _ | Separated _ -> nest_of r last d
        in
        lead, d, st)
      else (
        let lead, head, st = walk e es 0 (o + 1) st in
        let opened_lead, opened, st = walk e es (o + 1) body_from st in
        let inner_lead, inner, st = body ~alt body_from body_stop st in
        let close_lead, rest, st = walk e es body_stop last st in
        ( lead
        , head
          ^^ Handsome.Utf8.nest r.indent (opened_lead ^^ opened ^^ inner_lead ^^ inner)
          ^^ close_lead
          ^^ rest
        , st ))
    in
    (* [framed] builds its body inside the callback, because the conditional it
       hands out is only in scope there. The fold's state comes back out beside
       the document, which the callback has no room for, so it is put down as
       the body is built. [framed] calls the body once.

       Only [On_break] needs one, whether the separator comes from here or
       from the tree. The other policies write theirs either way, so there is
       nothing for a conditional to turn on, and a plain group is what they
       take. *)
    match tail_policy with
    | `Plain | `Present _ | `Add _ ->
      let lead, d, st = build ~alt:(fun flat _ -> flat) in
      lead, Handsome.Utf8.group (Handsome.Utf8.annotate k d), undo st
    | `Maybe _ | `Held _ ->
      let out = ref None in
      let d =
        Handsome.Utf8.framed (fun alt ->
          let lead, d, st = build ~alt in
          out := Some (lead, st);
          Handsome.Utf8.annotate k d)
      in
      (match !out with
       | Some (lead, st) -> lead, d, undo st
       | None -> invalid_arg "Layout.node: framed did not build its body"))
;;

let fit : Handsome.fit = Handsome.Line

let doc
      ?(trace = fun ~step:(_ : string) ~kind:(_ : Ir.Kind.t) -> ())
      (lay : Ir.Layout.t)
      ~boundary
      n
  =
  let kind = Siesta.Green.kind n in
  let lead, d, _ =
    node { lay; boundary; trace; kind } n ~stood:Ir.Layout.Flat ~stood_gap:None start
  in
  lead ^^ d
;;

let format
      ?trace
      (lay : Ir.Layout.t)
      ~(boundary : string -> int -> bool)
      ~(width : int)
      (n : Siesta.Green.node)
  : string
  =
  Handsome.Utf8.to_string
    (fst (Handsome.Utf8.render ~fit ~width (doc ?trace lay ~boundary n)))
;;
