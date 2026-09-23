(** The names the emitted JSON uses for itself.

    A TextMate grammar is a flat object of repository entries keyed by
    string, and a pattern reaches another by [#key]. The keys come from rule
    names, and they are lowercase: [StructBody] is [struct_body].

    A scope built from a rule name goes the other way and keeps the dots:
    [meta.struct.body], not [meta.struct_body]. A theme matches a prefix, so
    a rule written for [meta.struct] fires on the first and not on the
    second. *)

(** The repository key. *)
val of_rule : Core.Grammar.Name.Rule.t -> string

(** The body of a [meta.*] scope, with the segments dotted. *)
val dotted : Core.Grammar.Name.Rule.t -> string
