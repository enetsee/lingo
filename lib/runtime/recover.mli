(** Reporting a token that is not there.

    {!expect} is the half of recovery that no grammar changes. Look for one
    kind, take it where it is there, and report it missing where it is not.

    A balanced skip is the other half. It reads the grammar's delimiter pairs
    and its error kind, so the OCaml emitter writes it with those as
    constants and the interpreter brings its own. The two are then separate
    implementations and a differential test compares them. One shared copy
    would put recovery outside that comparison, and recovery is where this
    project's hard bugs have lived. *)

(** [expect ?at_child ?hole_kind ?placeholder c k id] consumes a token of
    kind [k] where the cursor is on one. Otherwise it reports [k] missing and
    leaves the cursor where it is.

    [?at_child] and [?hole_kind] go into the diagnostic, so a consumer can
    attribute the gap to a child position and lower it to a typed hole.

    [?placeholder] records the absence in the tree as well, as a childless
    node of that kind carrying the diagnostic's id. Pass the missing token's
    own kind at a delimited production's close. The node then doubles as the
    token-typed hole a formatter materialises.

    Pass it only where the grammar closes the construct for the author. A
    materialised stray semicolon would be bytes the source never had. *)
val expect
  :  ?at_child:string
  -> ?hole_kind:Ir.Kind.t
  -> ?placeholder:Ir.Kind.t
  -> Cursor.t
  -> Ir.Kind.t
  -> Ir.Message.id
  -> unit
