(** How a character finds its transition.

    [Table] indexes one array. The codespace is partitioned into classes, and
    a row holds one cell per class. [Match] writes a function per state and
    dispatches each on a pattern match over the byte, which the compiler
    turns into a decision tree; a state consuming a run of its own characters
    consumes it in a loop.

    Both represent the same automaton and answer with the same tokens.

    [Table] is the default. *)
type shape =
  | Table
  | Match

val generate : ?shape:shape -> Core.Facts.t -> Emit.item list

(** The interface {!generate}'s module satisfies. Every grammar's is the
    same. *)
val signature : Emit.sig_item list
