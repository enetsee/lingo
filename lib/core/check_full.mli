(** The checks that read the fixpoints and the lexer automaton.

    One check needs FIRST. It reads whether a block's prefix operator token
    appears in FIRST of one of its atoms. Reading the atom's leading child
    instead comes out wrong wherever those children are nullable, and that is
    the case the check exists for.

    One check needs the automaton. Token reachability follows from the token
    declarations alone. The automaton is built at this stage because
    {!Facts.t} keeps it, so the check reads it from there. *)

val run : Stage.shape -> Fixpoint.tables -> Redfa.Dfa.t -> Error.t list
