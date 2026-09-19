open StdLabels

let ( ^^ ) = Handsome.Ascii.( ^^ )
let empty = Handsome.Ascii.empty

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
      }
  | Separated of sep

type slot = Ir.Layout.slot =
  { kinds : Ir.Kind.t array
  ; repeats : bool
  ; before : break
  ; between : break
  }

type rule = Ir.Layout.rule =
  { name : string
  ; kind : Ir.Kind.t
  ; frame : frame
  ; slots : slot array
  ; body : break
  ; inner : break
  ; indent : int
  ; edge_before : bool option
  ; edge_after : bool option
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

(* The last token written, and whether a space may follow it. A rule's
   [edge_after] replaces that flag, so it sits on the edge rather than being
   looked up again at the boundary. *)
type edge =
  { kind : Ir.Kind.t
  ; space_after : bool
  }

(* [run] is every byte written since the last blank. That is the left-hand side
   of the max-munch test.

   [brk] holds the strongest break any boundary crossed since the last token,
   and [space_before] an override of the next token's leading flag. Both are
   requests, and a token discharges them. A child that writes nothing discharges
   nothing, so {!node} puts them back rather than letting them reach a token in
   the frame above. *)
type state =
  { last : edge option
  ; run : string
  ; brk : Ir.Layout.break
  ; space_before : bool option
  ; held : bool (* The last token written keeps the line the source gave it. *)
  ; written : int (* How many tokens have gone into the document. *)
  }

let start =
  { last = None
  ; run = ""
  ; brk = Ir.Layout.Flat
  ; space_before = None
  ; held = false
  ; written = 0
  }
;;

let stronger (a : Ir.Layout.break) (b : Ir.Layout.break) : Ir.Layout.break =
  match a, b with
  | Hard m, Hard n -> Hard (if m > n then m else n)
  | (Hard _ as h), _ | _, (Hard _ as h) -> h
  | Fit, _ | _, Fit -> Fit
  | Flat, Flat -> Flat
;;

let rec breaks n =
  if n <= 1 then Handsome.Ascii.hardline else Handsome.Ascii.hardline ^^ breaks (n - 1)
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

let join ~boundary ~run ~next =
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

let name_of j =
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
  ; trace : string -> unit
  }

(* Only a token's spacing flags are read at a boundary, so a kind that is not a
   token gives the neutral entry. [trivia] is read of any child. *)
let token (lay : Ir.Layout.t) (k : Ir.Kind.t) =
  match lay.tokens.(k) with
  | Some t -> t
  | None -> Ir.Layout.not_a_token
;;

let joins (e : env) (st : state) ~next =
  let j = join ~boundary:e.boundary ~run:st.run ~next in
  e.trace (name_of j);
  j
;;

(* Indentation is a count of spaces clamped at zero. handsome has no primitive
   for a line at column zero, so one comes from nesting down by more than any
   document nests up. *)
let column_zero = Handsome.Ascii.nest (-1_000_000) Handsome.Ascii.hardline

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
  val doc : t -> Ir.Kind.t Handsome.Ascii.t
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
        List.fold_left rest ~init:(Handsome.Ascii.text first) ~f:(fun acc line ->
          acc ^^ column_zero ^^ Handsome.Ascii.text line)
    in
    Handsome.Ascii.annotate t.kind d
  ;;
end

(* Every space, every line break and every byte of a token goes through here.

   Three things meet at a boundary, and they rank. A join the lexer will not
   close outranks the production's break style, because no break style is a
   solution where the bytes fuse. The spacing preference settles the rest, and it
   only ever adds a blank the join had not already required. *)
let glue (e : env) (st : state) (w : Written.t) : Ir.Kind.t Handsome.Ascii.t * state =
  let kind = Written.kind w
  and text = Written.text w in
  let lay = e.lay in
  let leaving ~clears =
    { last =
        Some { kind; space_after = (token lay kind).space_after }
        (* A newline inside a token is not a blank this printed, and the lexer
           does not stop at one. It is a byte of a lexeme still open, which is
           what an unterminated block comment is, so the run carries on. *)
    ; run = (if clears then "" else st.run) ^ text
    ; brk = Flat
    ; space_before = None
    ; held = st.held
    ; written = st.written + 1
    }
  in
  match st.last with
  | None -> empty, leaving ~clears:false
  | Some prev ->
    let spaced =
      prev.space_after
      &&
      match st.space_before with
      | Some b -> b
      | None -> (token lay kind).space_before
    in
    let j = joins e st ~next:text in
    let blank = spaced || j = Blank in
    let brk =
      match j with
      | Newline | None_at_all -> stronger st.brk (Hard 1)
      | Touching | Blank -> st.brk
    in
    let d, clears =
      match brk with
      | Hard n ->
        e.trace "break-hard";
        breaks n, true
      | Fit ->
        e.trace "break-fit";
        (if blank then Handsome.Ascii.line else Handsome.Ascii.softline), blank
      | Flat ->
        e.trace "break-flat";
        (if blank then Handsome.Ascii.text " " else empty), blank
    in
    d, leaving ~clears
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
  ; edge_before = None
  ; edge_after = None
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

let all_space s =
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
let slot_of (r : Ir.Layout.rule) ~prev k =
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
  let nl = ref false in
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
    | Some s when all_space s -> if String.contains s '\n' then nl := true
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
            if ag then e.trace "repeat";
            Filled
          | None ->
            e.trace "stray";
            Stray)
      in
      let was_held =
        match !kept with
        | e :: _ -> e.held
        | [] -> prev_held
      in
      let before : Ir.Layout.break =
        if held || was_held
        then (
          e.trace (if !nl then "held-line" else "held-same");
          if !nl then Hard 1 else Flat)
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
      (match role with
       | Filled -> body_started := true
       | Opener | Closer | Separator | Stray -> ());
      nl := false;
      kept := { child = ch; role; held; before } :: !kept);
  let es = Array.of_list (List.rev !kept) in
  let n = Array.length es in
  let index role =
    let rec go i = if i >= n then -1 else if es.(i).role = role then i else go (i + 1) in
    go 0
  in
  es, index Opener, index Closer
;;

(* -- the walk ------------------------------------------------------------ *)

(* Where the run of children sharing one line ends. A boundary that cannot end
   the line reads [Flat], and the layout settles that on its own, so a run's
   extent is known before anything is rendered. What a boundary prints is
   settled later, from the bytes. *)
let rec flat_end es i stop =
  if i >= stop || es.(i).before <> Ir.Layout.Flat then i else flat_end es (i + 1) stop
;;

let nothing st = empty, st

(* [walk env es i stop ~after] is the children in [i, stop) and then [after].

   Each child is handed the children sharing its last line as its tail, so its
   group measures them. Without that a group renders flat, a separator lands
   after it on the same line, and the two together run past the ruler with no
   break left. *)
let rec walk (e : env) es i stop ~after st =
  if i >= stop
  then (
    let d, st = after st in
    empty, d, st)
  else (
    let it = es.(i) in
    let j = flat_end es (i + 1) stop in
    (* The break this boundary asks for, and what stood before it. A child that
       writes nothing had no boundary in front of it, so the request goes back:
       carried on, it would reach a token in some other frame and break a
       boundary that frame knows nothing of. *)
    let stood = st.brk in
    let st = { st with brk = stronger st.brk it.before } in
    (* [after] lands on this child's line, so it goes in the tail rather than
       beside it. That carries a parent's separator and closer down the rightmost
       spine, and every group on the way down measures them. *)
    let ends_here = j >= stop in
    let tail st =
      let l, d, st =
        walk e es (i + 1) j ~after:(if ends_here then after else nothing) st
      in
      l ^^ d, st
    in
    let lead, d, st = child e it ~tail ~stood st in
    if ends_here
    then lead, d, st
    else (
      let l2, d2, st = walk e es j stop ~after st in
      lead, d ^^ l2 ^^ d2, st))

and child (e : env) it ~tail ~stood st =
  match it.child with
  | Siesta.Green.Token t ->
    let w = Written.of_token t in
    let lead, st = glue e st w in
    let d, st = tail { st with held = it.held } in
    lead, Written.doc w ^^ d, st
  | Siesta.Green.Node n -> node e n ~tail ~stood st

and node (e : env) (n : Siesta.Green.node) ~tail ~stood st =
  let entry = st.written
  and stood_space = st.space_before in
  let undo st =
    if st.written = entry then { st with brk = stood; space_before = stood_space } else st
  in
  let tail st = tail (undo st) in
  let k = Siesta.Green.kind n in
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
    let d, st = tail st in
    empty, d, undo st)
  else (
    let ri = e.lay.of_kind.(k) in
    let ruled = ri >= 0 in
    let r = if ruled then e.lay.rules.(ri) else unruled in
    if not ruled then e.trace "unruled";
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
    let tail_sep =
      let rec go i = i < body_end && (es.(i).role = Separator || go (i + 1)) in
      elt >= 0 && go (elt + 1)
    in
    let policy =
      match sep_of r with
      | Some s when s.text <> "" && elt >= 0 -> Some s
      | Some _ | None -> None
    in
    let magic =
      match policy with
      | Some { trailing = On_break; _ } -> tail_sep
      | Some _ | None -> false
    in
    (* A body the source requested broken, breaks. Its own boundaries read
       [Fit], and the request outranks that. *)
    let es =
      if not magic
      then es
      else
        Array.mapi es ~f:(fun i it ->
          if i > o && i <= body_end && it.before = Ir.Layout.Fit
          then { it with before = Ir.Layout.Hard 1 }
          else it)
    in
    (* What the body's tail does. It has to be one shape whether the separator
       came from the source or from here: splitting the last run only when the
       fold adds one makes the document's shape depend on that, and the glue then
       lands elsewhere on the pass after the one that added it. *)
    let tail_policy =
      match policy with
      | None -> `Plain
      | Some s ->
        if tail_sep
        then `Present s
        else if not closed
        then `Plain
        else (
          match s.trailing with
          | Never -> `Plain
          | Always -> `Add s
          | On_break -> `Maybe s)
    in
    let st =
      match r.edge_before with
      | Some _ as b -> { st with space_before = b }
      | None -> st
    in
    let close st =
      let st =
        match r.edge_after, st.last with
        | Some b, Some edge -> { st with last = Some { edge with space_after = b } }
        | (Some _ | None), _ -> st
      in
      tail st
    in
    (* Where the body segment ends, which is not always where the body does.
       The closer normally starts its own line, so it sits outside the nest and
       outside the last element's group. A recovery node beside it can hold it to
       the body's last line instead. It then belongs to that line's run, or the
       group that settles the line will not have measured it. Nesting it changes
       nothing there, because no break is taken. *)
    let body_stop =
      if c >= 0 && es.(c).before = Ir.Layout.Flat
      then flat_end es (c + 1) last
      else body_end
    in
    (* The separator the policy adds goes straight after the last element, and
       outside that element's group.

       After the last element because a comment can sit between it and the
       closer, and a separator written past the comment is on the wrong side of
       it.

       Outside the group because [On_break] turns on whether the body broke, and
       a [flat_alt] resolves against the group it sits directly inside. In the
       element's tail it would resolve against that element, which is flat on its
       own line whenever a broken body fits one element per line.

       [Always] needs none of this. It is written either way, so it can sit in
       the element's tail where that group measures it. [On_break] cannot, so the
       run the last element's line begins with is folded twice, once with the
       separator and once without, and the frame chooses. Everything before that
       run is folded once and shared. *)
    let body first stop ~after st =
      match tail_policy with
      | `Plain -> walk e es first stop ~after st
      | (`Present s | `Add s | `Maybe s) as p ->
        let written = Written.separator s.sep_kind s.text in
        let tail_of ~sep st =
          let d1, st =
            if not sep
            then nothing st
            else (
              e.trace
                (match s.trailing with
                 | Always -> "sep-always"
                 | On_break -> "sep-on-break"
                 | Never -> "sep-never");
              (* Glued like any token the tree holds. Forcing [Flat] here would
                 put the separator on a different line from the one a separator
                 the source already had lands on, and the two have to agree. *)
              let lead, st = glue e st written in
              lead ^^ Written.doc written, st)
          in
          let l, d, st = walk e es (elt + 1) stop ~after st in
          d1 ^^ l ^^ d, st
        in
        let start =
          let rec back i =
            if i <= first || es.(i).before <> Ir.Layout.Flat then i else back (i - 1)
          in
          back elt
        in
        let run ~sep st = walk e es start (elt + 1) ~after:(tail_of ~sep) st in
        let lead, head, st = walk e es first start ~after:nothing st in
        let joined choice st =
          if start > first then lead, head ^^ choice, st else empty, choice, st
        in
        (match p with
         | `Present _ ->
           let l1, d1, st = run ~sep:false st in
           joined (l1 ^^ d1) st
         | `Add _ ->
           let l2, d2, st = run ~sep:true st in
           joined (l2 ^^ d2) st
         | `Maybe _ ->
           (* Both foldings start from the state the run reached. Taking the
              state the first one left puts the glue of the second in the wrong
              place. *)
           let l1, d1, without = run ~sep:false st in
           let l2, d2, _ = run ~sep:true st in
           joined (Handsome.Ascii.flat_alt (l1 ^^ d1) (l2 ^^ d2)) without)
    in
    let lead, d, st =
      if o < 0
      then (
        let lead, d, st = body 0 last ~after:close st in
        (* [indent] is what a boundary of this rule does to the lines after it.
           A rule with one child has no boundary, so nesting there would add its
           indent to every rule that wraps a single child on the way down. *)
        lead, (if last > 1 then Handsome.Ascii.nest r.indent d else d), st)
      else (
        let ends_body = body_stop >= last in
        let lead, head, st = walk e es 0 (o + 1) ~after:nothing st in
        let inner_lead, inner, st =
          body (o + 1) body_stop ~after:(if ends_body then close else nothing) st
        in
        let close_lead, rest, st =
          if ends_body then empty, empty, st else walk e es body_stop last ~after:close st
        in
        ( lead
        , head ^^ Handsome.Ascii.nest r.indent (inner_lead ^^ inner) ^^ close_lead ^^ rest
        , st ))
    in
    lead, Handsome.Ascii.group (Handsome.Ascii.annotate k d), undo st)
;;

let doc ?(trace = fun (_ : string) -> ()) (lay : Ir.Layout.t) ~boundary n =
  let lead, d, _ =
    node { lay; boundary; trace } n ~tail:nothing ~stood:Ir.Layout.Flat start
  in
  lead ^^ d
;;

let format ?trace (lay : Ir.Layout.t) ~boundary ~width n =
  Handsome.Ascii.to_string
    (fst (Handsome.Ascii.render ~width (doc ?trace lay ~boundary n)))
;;
