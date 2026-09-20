let mark (c : Cursor.t) =
  Cursor.skip_trivia c;
  Siesta.Builder.checkpoint (Cursor.builder c)
;;

(* Trivia under the cursor goes into the frame that is already open, so the
   node starting here begins at its first meaningful token.

   The root is the exception, and has to be: nothing is open yet, so leading
   trivia has nowhere to go but inside the root. *)
let start_node ?payload (c : Cursor.t) (k : Ir.Kind.t) =
  if Cursor.depth c > 0 then Cursor.skip_trivia c;
  Siesta.Builder.start_node ?payload (Cursor.builder c) k;
  Cursor.entered c
;;

(* No skip here. The checkpoint came from {!mark}, which took the trivia
   before it, so the node this opens already starts where it should. *)
let start_node_at ?payload (c : Cursor.t) (cp : Siesta.Builder.checkpoint) (k : Ir.Kind.t)
  : unit
  =
  Siesta.Builder.start_node_at ?payload (Cursor.builder c) cp k;
  Cursor.entered c
;;

let finish_node (c : Cursor.t) =
  Siesta.Builder.finish_node (Cursor.builder c);
  Cursor.left c
;;

let missing_node ?payload (c : Cursor.t) (k : Ir.Kind.t) =
  start_node ?payload c k;
  finish_node c
;;

let finish (c : Cursor.t) =
  let root = Siesta.Builder.finish (Cursor.builder c) in
  root, Cursor.diagnostics c
;;
