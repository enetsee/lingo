(** The layout table, as OCaml source.

    A grammar's layout comes from its facts and is written out as a constant, so
    the module that ships reads no facts and carries no fold of its own. What
    folds it is {!Lingo_runtime.Layout}, which is grammar-agnostic.

    The lexer arrives as an argument rather than by name. An emitted parser
    takes its tokens the same way. So neither generated module depends on the
    other.

    Takes a layout [Layout.Check] accepts. *)

(** [layout], and [format] over it. *)
val generate : Ir.Layout.t -> Emit.item list

(** The interface {!generate}'s module satisfies. Every grammar's is the same. *)
val signature : Emit.sig_item list
