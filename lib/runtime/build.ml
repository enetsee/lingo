let mark (c : Cursor.t) =
  Cursor.skip_trivia c;
  Siesta.Builder.checkpoint (Cursor.builder c)
;;

let start_node ?payload (c : Cursor.t) (k : Ir.Kind.t) =
  Siesta.Builder.start_node ?payload (Cursor.builder c) k
;;

let start_node_at ?payload (c : Cursor.t) cp (k : Ir.Kind.t) =
  Siesta.Builder.start_node_at ?payload (Cursor.builder c) cp k
;;

let finish_node (c : Cursor.t) = Siesta.Builder.finish_node (Cursor.builder c)

let missing_node ?payload (c : Cursor.t) (k : Ir.Kind.t) =
  start_node ?payload c k;
  finish_node c
;;

let finish (c : Cursor.t) =
  let root = Siesta.Builder.finish (Cursor.builder c) in
  root, Cursor.diagnostics c
;;
