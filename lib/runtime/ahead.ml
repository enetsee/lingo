open StdLabels

type point = Ir.Residual.Table.point =
  { first : Ir.Kind.t array
  ; may_end : bool
  ; on : (Ir.Kind.t array * int) array
  }

type t =
  { points : point array array
  ; of_kind : int array
  ; trivia : Ir.Kind.t array
  ; error_kind : Ir.Kind.t
  }

let holds (set : Ir.Kind.t array) (kind : Ir.Kind.t) : bool =
  Array.exists set ~f:(Int.equal kind)
;;

(* Every index below comes out of the tables, so a bad one is a bad table. The
   emitter writes them, and a caller can write one by hand. *)
let within (what : string) (count : int) (index : int) : int =
  if index < 0 || index >= count
  then
    invalid_arg (Printf.sprintf "Ahead.at: no %s %d; the tables hold %d" what index count);
  index
;;

let table (t : t) (kind : Ir.Kind.t) : point array option =
  if kind < 0 || kind >= Array.length t.of_kind
  then None
  else (
    let at = t.of_kind.(kind) in
    if at < 0 then None else Some t.points.(within "automaton" (Array.length t.points) at))
;;

let child_kind (child : Siesta.Green.child) : Ir.Kind.t =
  match child with
  | Siesta.Green.Node node -> Siesta.Green.kind node
  | Siesta.Green.Token token -> Siesta.Green.Token.kind token
;;

let is_error (t : t) (child : Siesta.Green.child) : bool =
  match child with
  | Siesta.Green.Node node -> Int.equal (Siesta.Green.kind node) t.error_kind
  | Siesta.Green.Token _ -> false
;;

(* Trivia is no child of a rule. A node of no length is a gap: one before
   [byte] holds the place of the child it stands for, and one at [byte] is what
   the parse wrote on reading this very token, so it is not there yet.

   An error node counts. A loop that recovers starts again at its entry, and
   the table carries the edge that says so. *)
let counts (t : t) (child : Siesta.Green.child) ~(at : int) ~(byte : int) : bool =
  match child with
  | Siesta.Green.Node node -> Siesta.Green.text_len node > 0 || at < byte
  | Siesta.Green.Token token -> not (holds t.trivia (Siesta.Green.Token.kind token))
;;

(* A child with no transition leaves the walk where it is. The tree holds what
   the parse built, and a parse that recovered built something the rule does
   not name. *)
let drive (points : point array) (taken : Ir.Kind.t list) : int =
  let count = Array.length points in
  List.fold_left taken ~init:(within "point" count 0) ~f:(fun at kind ->
    let point = points.(at) in
    match
      Array.find_map point.on ~f:(fun (on, dest) ->
        if holds on kind then Some dest else None)
    with
    | Some dest -> within "point" count dest
    | None -> at)
;;

(* The start byte of the first meaningful token at or after [offset], or the
   end of the input where there is none.

   A child that ends at or before [offset] holds no such token, so the walk
   steps over it without reading inside. *)
let target (t : t) (root : Siesta.Green.node) ~(offset : int) : int =
  let rec go (node : Siesta.Green.node) (from : int) : int option =
    let children = Siesta.Green.children_array node in
    let rec next (index : int) (at : int) : int option =
      if index >= Array.length children
      then None
      else (
        let child = children.(index) in
        let len = Siesta.Green.child_text_len child in
        let onward () = next (index + 1) (at + len) in
        if at + len <= offset
        then onward ()
        else (
          match child with
          | Siesta.Green.Token token ->
            if at >= offset && not (holds t.trivia (Siesta.Green.Token.kind token))
            then Some at
            else onward ()
          | Siesta.Green.Node inner ->
            (match go inner at with
             | Some found -> Some found
             | None -> onward ())))
    in
    next 0 from
  in
  match go root 0 with
  | Some at -> at
  | None -> Siesta.Green.text_len root
;;

let at (t : t) (root : Siesta.Green.node) ~(offset : int) : Ir.Kind.t array =
  let byte = target t root ~offset in
  let seek (node : Siesta.Green.node) (from : int)
    : Ir.Kind.t list
      * (Siesta.Green.child * int) option
      * (Siesta.Green.node * int) option
    =
    let children = Siesta.Green.children_array node in
    let rec go index at taken behind =
      if index >= Array.length children
      then taken, None, behind
      else (
        let child = children.(index) in
        let len = Siesta.Green.child_text_len child in
        if byte < at + len
        then taken, Some (child, at), behind
        else (
          let counted = counts t child ~at ~byte in
          (* Only the child immediately before can still be open. A token or
             an error node between them put the parse back in this node. A gap
             did not: the parse wrote it on reading this very token. *)
          let behind =
            match child, counted with
            | _ when is_error t child -> None
            | Siesta.Green.Node inner, true -> Some (inner, at)
            | Siesta.Green.Node _, false -> behind
            | Siesta.Green.Token _, true -> None
            | Siesta.Green.Token _, false -> behind
          in
          go
            (index + 1)
            (at + len)
            (if counted then child_kind child :: taken else taken)
            behind))
    in
    go 0 from [] None
  in
  (* Down to the deepest frame the parse still had open when the token came
     into view, because that frame read it first. The list comes back innermost
     last.

     A token that begins a child was read from inside the child before it,
     which the parse had not yet left: it leaves a child only by looking at
     what follows. *)
  let rec descend (node : Siesta.Green.node) (from : int) : (point array * int) list =
    match table t (Siesta.Green.kind node) with
    | None -> []
    | Some points ->
      let taken, here, behind = seek node from in
      let inside =
        match here with
        | Some (Siesta.Green.Node inner, at) when byte > at -> Some (inner, at)
        | Some _ | None -> behind
      in
      let taken =
        match here, inside with
        | Some (child, at), Some _ when byte > at ->
          if counts t child ~at ~byte then child_kind child :: taken else taken
        | _ -> taken
      in
      let mine = points, drive points (List.rev taken) in
      (match inside with
       | Some (inner, at) when table t (Siesta.Green.kind inner) <> None ->
         mine :: descend inner at
       | Some _ | None -> [ mine ])
  in
  let rec union (frames : (point array * int) list) : Ir.Kind.t list =
    match frames with
    | [] -> []
    | (points, at) :: outer ->
      let point = points.(at) in
      let first = Array.to_list point.first in
      if point.may_end then first @ union outer else first
  in
  Array.of_list (List.sort_uniq ~cmp:Int.compare (union (List.rev (descend root 0))))
;;
