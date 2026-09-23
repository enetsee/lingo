(** What a law prints, and the count it exits on.

    A law is an oracle with no baseline. It prints a line per finding and the
    run is green where it printed none, so a law that finds a thousand prints
    a thousand. Nothing here caps or samples them: the counts a falsification
    record quotes are what a law printed. *)

(** Records a finding and prints it under [FAIL]. *)
val fail : ('a, Format.formatter, unit, unit) format4 -> 'a

(** Prints a claim that held, under [PASS].

    Most carry the count they ran over. A count that drops to zero is how a
    corpus says it has stopped reaching something, and several laws fail on
    exactly that. *)
val pass : ('a, Format.formatter, unit, unit) format4 -> 'a

(** How many findings {!fail} has recorded. A part reads it before and after
    its own checks to say whether that part alone was clean. *)
val failures : unit -> int

(** Prints [<name>: 0 failures], or the count and then exits 1. *)
val summarise : string -> unit

(** Exits 1 where there were findings, and prints nothing where there were
    none. Three laws end this way: their last part already prints the count
    it ran over, so a name and a zero beside it would say it twice. *)
val exit_on_failure : unit -> unit
