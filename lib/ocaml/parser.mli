(** The parser, as OCaml source.

    One [parse_<rule>] per rule in a single [let rec] cluster, an expression
    block's two functions beside them, and an entry point per root. The
    emitted module links [Lingo_runtime] and nothing else.

    Takes a plan [Plan.Check] accepts. An index in a plan goes unchecked, so a
    bad one raises from the array access. *)

(** The whole module: [add_all] and [skip], the [let rec] cluster, then the
    entry points. Every rule gets a binding, including one nothing calls, so a
    [Call] carries any index. *)
val generate : Ir.Plan.t -> Emit.item list

(** [parse_tokens_<root>] per root, and [parse_tokens] for the first. *)
val signature : Ir.Plan.t -> Emit.sig_item list
