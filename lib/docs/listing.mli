(** The grammar, written out the way its author wrote it.

    This reads {!Core.Grammar.t}. A reader of a language's manual needs the
    productions the author declared, under the names they chose, with the
    expression block still a block. The facts have desugared all of that,
    which is right for a parser and wrong for a page.

    The notation on the page:

    {v
      Struct = 'struct' name:ident body:StructBody
      Group  = '(' elt:Sexp* ')'
      Value  = kind:( 'true' | 'false' | number | Object )
    v}

    A slot's name comes first, then what goes in it. A modifier follows: [?]
    for one that may be absent, [*] for any number, [+] for one or more. A
    matched pair and a separator are shown where the production has them,
    because a reader has to type both. *)

(** One production, ready to lay out. *)
val production : Core.Grammar.t -> Core.Grammar.production -> Render.document

(** One expression block: its atoms, then its operators by kind, each with
    its binding power. *)
val block : Core.Grammar.t -> Core.Grammar.expr_def -> Render.document
