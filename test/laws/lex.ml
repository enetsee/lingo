(* -- a lexer over the facts' automaton -----------------------------------------

      The facts carry the DFA every token's regex compiles to, so a law can
      lex a grammar it knows nothing about. Without this each law would carry
      a hand-written lexer per grammar, and a law that only runs on the
      grammars someone wrote a lexer for is a law with a hole in it.

      This is not the lexer lingo emits. Max-munch, the intern table and
      error-token synthesis are lingo's rather than redfa's, and the emitter
      writes its own. This is the smallest thing that turns source into the
      tokens a parse needs.
   -------------------------------------------------------------------------- *)

(* The token with the lowest id among those a state accepts. Ids follow
   declaration order, so the grammar's own order breaks a tie. *)
let best (f : Core.Facts.t) (dfa : Redfa.Dfa.t) (state : int) =
  match Redfa.Dfa.accepts dfa state with
  | [] -> None
  | ids -> Some (Core.Facts.token f (List.fold_left min (List.hd ids) ids))
;;

let step (dfa : Redfa.Dfa.t) (state : int) (code : int) =
  List.find_map
    (fun (set, dest) -> if Ucharset.mem set code then Some dest else None)
    (Redfa.Dfa.transitions dfa state)
;;

(* The automaton reads codepoints and a token carries bytes, so the input is
   decoded on the way in. Stepping it a byte at a time instead would take the
   two bytes of [Ã¯] as two codepoints, and no set holding U+00EF would
   ever match.

   Invalid UTF-8 decodes to U+FFFD over one byte, so a bad byte still moves
   the scan along. *)
let uchar_at (s : string) (i : int) =
  let d = String.get_utf_8_uchar s i in
  Uchar.to_int (Uchar.utf_decode_uchar d), Uchar.utf_decode_length d
;;

(* Longest match from [pos], and the token it accepted. [None] where no token
   starts here at all. *)
let longest (f : Core.Facts.t) (dfa : Redfa.Dfa.t) (s : string) (pos : int) =
  let n = String.length s in
  let rec go i state found =
    let found =
      match best f dfa state with
      | Some t when i > pos -> Some (t, i)
      | Some _ | None -> found
    in
    if i >= n
    then found
    else (
      let code, width = uchar_at s i in
      match step dfa state code with
      | None -> found
      | Some dest -> go (i + width) dest found)
  in
  go pos (Redfa.Dfa.initial dfa) None
;;

(* Every byte of the input lands in exactly one token, so the tree can rebuild
   it. A byte no token starts with becomes an error token of its own, which is
   what keeps that true on input the grammar does not describe. *)
let run (f : Core.Facts.t) (s : string) : Lingo_runtime.Token.t array =
  let out = Dynarray.create () in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    match longest f f.lexer s !i with
    | Some ((t : Core.Token.def), stop) ->
      Dynarray.add_last
        out
        { Lingo_runtime.Token.kind = Core.Kind.to_int t.kind
        ; text = String.sub s !i (stop - !i)
        };
      i := stop
    | None ->
      (* One codepoint, not one byte. Splitting a character across error
         tokens would put bytes in the tree that the input never had as
         separate characters. *)
      let _, width = uchar_at s !i in
      Dynarray.add_last
        out
        { Lingo_runtime.Token.kind = Core.Kind.to_int f.error_token_kind
        ; text = String.sub s !i width
        };
      i := !i + width
  done;
  Dynarray.to_array out
;;
