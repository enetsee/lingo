(** How a name the author writes becomes a name a backend emits.

    Grammar names are loosely UpperCamelCase. OCaml takes a different shape
    in each place one lands. Values and fields take [snake_case]. Modules take
    [Upper_first]. A few names land on a keyword and take a trailing
    underscore.

    Nothing here reads a grammar. {!Manifest} applies these functions to one,
    and owns the namespaces that come out. *)

(** ["FooBar"] gives ["foo_bar"]. An uppercase letter after the first
    position takes an underscore before it, and every letter is lowered.

    Two names can share an image. ["Match"] and ["Match_"] both reach
    ["match_"] once {!escape_reserved} has run. That is why {!Manifest}
    groups by the emitted string. *)
val snake_case : string -> string

(** Splits a run of capitals before its last letter, so ["URLPattern"] gives
    ["url_pattern"]. Use it for a name that becomes a prefix-matched dotted
    scope. An OCaml binding takes {!snake_case}. *)
val snake_case_acronym : string -> string

val ocaml_reserved : string list

(** ["match"] gives ["match_"]. Any string that is not a keyword comes back
    unchanged. *)
val escape_reserved : string -> string

(** [escape_reserved (snake_case s)]. Every emitted value binding and record
    field goes through it. *)
val safe_snake : string -> string

(** ["FooBar"] gives ["Foo_bar"]. This is {!snake_case} with the first
    letter raised, which is the form an OCaml module name takes. *)
val upper_first : string -> string

(** ["FooBar"] gives ["FOOBAR"]. It uppercases the name as written, so
    ["FooBar"] does not become ["FOO_BAR"]. Kind names such as [N_FOOBAR] are
    built with it.

    Two names can collide here too, and they need not be the same two that
    {!safe_snake} merges. That is why {!Manifest} checks both namespaces. *)
val screaming_snake : string -> string

(** [None] when the string is a valid OCaml identifier,
    [\[A-Za-z_\]\[A-Za-z0-9_'\]*]. [Some reason] when it is not.

    A grammar name is mangled into a constructor, a binding or an accessor,
    and then emitted as written. So it has to be an identifier to start with.
    A [.] inside a name would scope as a qualified path. *)
val ident_error : string -> string option

val is_ident : string -> bool
