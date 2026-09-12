(** Node bracketing over siesta's builder.

    A parser opens a node, emits tokens into it through {!Cursor.bump}, and
    closes it. Everything here works on the builder inside a {!Cursor.t}, so
    the tree and the position stay in step.

    A node's byte length comes from its children, so nothing here takes a
    range. *)

(** The write position in the open frame's child list, for
    {!start_node_at}.

    It skips trivia first. A checkpoint taken before the trivia would put
    that trivia inside the node {!start_node_at} opens, and leading trivia
    belongs to the frame that was already open. *)
val mark : Cursor.t -> Siesta.Builder.checkpoint

(** Opens a node. [?payload] stamps a diagnostic id on it; see
    {!Cursor.report_id}. *)
val start_node : ?payload:int -> Cursor.t -> Kind.t -> unit

(** Opens a node around everything emitted into the open frame since the
    checkpoint. A left-associative operator wraps its left side this way,
    once the operator has been read. *)
val start_node_at
  :  ?payload:int
  -> Cursor.t
  -> Siesta.Builder.checkpoint
  -> Kind.t
  -> unit

val finish_node : Cursor.t -> unit

(** A childless node of that kind, so the gap is in the tree and not only in
    the diagnostics.

    A production that lost its closing delimiter to recovery otherwise looks
    the same as one that has it, and every consumer has to work the
    difference out again. *)
val missing_node : ?payload:int -> Cursor.t -> Kind.t -> unit

(** The root and the diagnostics, in the order they were reported. A 1-based
    id from {!Cursor.report_id} is that diagnostic's place in the list.

    Raises [Failure] if a frame is still open, or if no node was ever
    started. *)
val finish : Cursor.t -> Siesta.Green.node * Diagnostic.t list
