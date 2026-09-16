open StdLabels

type t =
  { num_states : int
  ; num_classes : int
  ; segments : int array
  ; segment_class : int array
  ; next : int array
  ; accept : Kind.t option array
  }

let initial : int = 0

(* The classes are disjoint so their intervals sorted by least codepoint are
   the segments. Where two intervals leave a gap, the gap takes an entry of
   its own with class [-1]; without it a codepoint in the gap would read as
   the class before it. redfa's partition leaves one gap, at the surrogate
   block. *)
let flatten (classes : Ucharset.t array) : int array * int array =
  let runs = ref [] in
  Array.iteri classes ~f:(fun (klass : int) (set : Ucharset.t) ->
    Ucharset.iter_intervals
      (fun (lo : int) (hi : int) -> runs := (lo, hi, klass) :: !runs)
      set);
  let runs =
    List.sort
      ~cmp:(fun ((left, _, _) : int * int * int) ((right, _, _) : int * int * int) ->
        Int.compare left right)
      !runs
  in
  let out = ref [] in
  let push (lo : int) (klass : int) : unit = out := (lo, klass) :: !out in
  let after =
    List.fold_left
      runs
      ~init:0
      ~f:(fun (expected : int) ((lo, hi, klass) : int * int * int) ->
        if lo > expected then push expected (-1);
        push lo klass;
        hi + 1)
  in
  if after <= Ucharset.max_codepoint then push after (-1);
  let segments = Array.of_list (List.rev !out) in
  Array.map segments ~f:fst, Array.map segments ~f:snd
;;

let of_facts (facts : Facts.t) : t =
  let dfa = facts.lexer in
  let table = Redfa.Dfa.table dfa in
  let segments, segment_class = flatten table.Redfa.Dfa.classes in
  (* [accepts] comes back in ascending case id, and a token's case id is its
     position in the grammar. So the head is the token declared first, and
     declaration order is priority order. *)
  let accept =
    Array.init (Redfa.Dfa.num_states dfa) ~f:(fun (state : int) ->
      match Redfa.Dfa.accepts dfa state with
      | [] -> None
      | token_id :: _ -> Some (Facts.token facts token_id).Token.kind)
  in
  { num_states = Redfa.Dfa.num_states dfa
  ; num_classes = Array.length table.Redfa.Dfa.classes
  ; segments
  ; segment_class
  ; next = table.Redfa.Dfa.next
  ; accept
  }
;;

let class_of (t : t) (codepoint : int) : int =
  if codepoint < 0 || codepoint > Ucharset.max_codepoint
  then -1
  else (
    (* The last segment starting at or below [codepoint]. *)
    let rec search (lo : int) (hi : int) : int =
      if lo >= hi
      then lo
      else (
        let mid = (lo + hi + 1) / 2 in
        if t.segments.(mid) <= codepoint then search mid hi else search lo (mid - 1))
    in
    t.segment_class.(search 0 (Array.length t.segments - 1)))
;;

let step (t : t) ~(state : int) ~(klass : int) : int =
  if klass < 0 then -1 else t.next.((state * t.num_classes) + klass)
;;

(* -- printing -------------------------------------------------------------- *)

(* A cell is one character wide wherever it can be, so a row of a wide table
   still fits a line in a diff. [.] stands for no class, no transition and no
   accept. *)
let cell (value : int) : string = if value < 0 then "." else string_of_int value

let pp (formatter : Format.formatter) (t : t) : unit =
  Format.fprintf
    formatter
    "states %d  classes %d  cells %d@\n"
    t.num_states
    t.num_classes
    (Array.length t.next);
  Format.fprintf formatter "@\n; the codespace, by class@\n";
  Array.iteri t.segments ~f:(fun (index : int) (lo : int) ->
    let hi =
      if index + 1 < Array.length t.segments
      then t.segments.(index + 1) - 1
      else Ucharset.max_codepoint
    in
    Format.fprintf formatter "%06X..%06X %s@\n" lo hi (cell t.segment_class.(index)));
  Format.fprintf
    formatter
    "@\n; per state: the kind it accepts, then a destination per class@\n";
  Array.iteri t.accept ~f:(fun (state : int) (accept : Kind.t option) ->
    Format.fprintf
      formatter
      "%4d %4s "
      state
      (match accept with
       | None -> "."
       | Some kind -> string_of_int (Kind.to_int kind));
    for klass = 0 to t.num_classes - 1 do
      Format.fprintf formatter " %s" (cell t.next.((state * t.num_classes) + klass))
    done;
    Format.fprintf formatter "@\n")
;;
