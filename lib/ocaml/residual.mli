(** The residual tables, as OCaml source.

    A grammar's points come from its plan and are written out as constants, so
    the module that ships reads no plan and carries no walk of one. What walks
    them is {!Lingo_runtime.Ahead}, which is grammar-agnostic.

    The parse pays nothing for this. A parser records no position as it runs;
    the tables are read off the tree it left, and only when something calls
    [at].

    Takes a plan [Plan.Check] accepts. *)

(** [tables], and [at] over it. *)
val generate : Ir.Plan.t -> Emit.item list

val signature : Ir.Plan.t -> Emit.sig_item list
