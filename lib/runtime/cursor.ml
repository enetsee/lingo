open StdLabels

type t =
  { tokens : Token.t array
  ; mutable pos : int
  ; mutable offset : int
  ; trivia_lookup : bool array
  ; next_meaningful : int array
  ; builder : Siesta.Builder.t
  ; diags : Diagnostic.t Dynarray.t
  ; mutable depth : int
  }

(* [next_meaningful.(i)] is the smallest [j >= i] that is either the end of
   the array or a meaningful token. Precomputing it makes a peek O(1). Walking
   the trivia instead costs the run's length on every peek, and a peek happens
   at every dispatch.

   Built right to left so each entry reads its successor. *)
let create ?(cache = Siesta.Cache.create ()) ~trivia_kinds tokens =
  let max_kind = List.fold_left trivia_kinds ~init:(-1) ~f:max in
  let trivia_lookup = Array.make (max_kind + 1) false in
  List.iter trivia_kinds ~f:(fun k -> if k >= 0 then trivia_lookup.(k) <- true);
  let n = Array.length tokens in
  let is_trivia_at i =
    let k = (tokens.(i) : Token.t).kind in
    k >= 0 && k < Array.length trivia_lookup && trivia_lookup.(k)
  in
  let next_meaningful = Array.make (n + 1) n in
  for i = n - 1 downto 0 do
    next_meaningful.(i) <- (if is_trivia_at i then next_meaningful.(i + 1) else i)
  done;
  { tokens
  ; pos = 0
  ; offset = 0
  ; trivia_lookup
  ; next_meaningful
  ; builder = Siesta.Builder.create ~cache ()
  ; diags = Dynarray.create ()
  ; depth = 0
  }
;;

let position (c : t) = c.pos

let is_trivia (c : t) (k : Ir.Kind.t) =
  k >= 0 && k < Array.length c.trivia_lookup && c.trivia_lookup.(k)
;;

let peek_idx (c : t) = c.next_meaningful.(c.pos)

let current (c : t) : Ir.Kind.t =
  let i = peek_idx c in
  if i >= Array.length c.tokens then Ir.Kind.none else c.tokens.(i).kind
;;

let eof (c : t) = peek_idx c >= Array.length c.tokens
let at (c : t) (k : Ir.Kind.t) = current c = k

let peek_meaningful_at (c : t) ~(n : int) : Ir.Kind.t =
  let total = Array.length c.tokens in
  let j = ref (peek_idx c) in
  let remaining = ref n in
  while !remaining > 0 && !j < total do
    j := c.next_meaningful.(!j + 1);
    decr remaining
  done;
  if !j >= total then Ir.Kind.none else c.tokens.(!j).kind
;;

let range (c : t) =
  let i = peek_idx c in
  let trivia_bytes = ref 0 in
  for j = c.pos to i - 1 do
    trivia_bytes := !trivia_bytes + String.length c.tokens.(j).text
  done;
  let start = c.offset + !trivia_bytes in
  if i >= Array.length c.tokens
  then start, start
  else start, start + String.length c.tokens.(i).text
;;

let emit_token (c : t) (t : Token.t) =
  Siesta.Builder.token c.builder t.kind t.text;
  c.pos <- c.pos + 1;
  c.offset <- c.offset + String.length t.text
;;

let skip_trivia (c : t) =
  let n = Array.length c.tokens in
  while c.pos < n && is_trivia c c.tokens.(c.pos).kind do
    emit_token c c.tokens.(c.pos)
  done
;;

let bump (c : t) =
  skip_trivia c;
  if c.pos < Array.length c.tokens then emit_token c c.tokens.(c.pos)
;;

(* A Dynarray appends in amortised constant time. Consing and reversing at the
   end is quadratic on input that reports a diagnostic per token, and a fuzz
   corpus is full of that input. *)
let report (c : t) (kind : Diagnostic.kind) =
  Dynarray.add_last c.diags { Diagnostic.range = range c; kind }
;;

let offset (c : t) = c.offset

let report_at (c : t) (range : int * int) (kind : Diagnostic.kind) =
  Dynarray.add_last c.diags { Diagnostic.range; kind }
;;

let report_id (c : t) (kind : Diagnostic.kind) =
  let r = range c in
  let n = Dynarray.length c.diags in
  let append () =
    Dynarray.add_last c.diags { Diagnostic.range = r; kind };
    Dynarray.length c.diags
  in
  match kind with
  | Diagnostic.Missing _ when n > 0 ->
    let prev = Dynarray.get c.diags (n - 1) in
    (match prev.Diagnostic.kind with
     | Diagnostic.Missing _ when prev.Diagnostic.range = r -> n
     | _ -> append ())
  | _ -> append ()
;;

let while_progress (c : t) cond body =
  let continue = ref true in
  while !continue && cond () do
    let before = c.pos in
    body ();
    if c.pos = before then continue := false
  done
;;

let builder (c : t) = c.builder
let depth (c : t) = c.depth
let entered (c : t) = c.depth <- c.depth + 1
let left (c : t) = c.depth <- c.depth - 1
let diagnostics (c : t) = Dynarray.to_list c.diags
