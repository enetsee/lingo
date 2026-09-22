(** Classes, not rates.

    A count says how often. A class says which construct, and those are
    different facts: the predecessor's summary read "20% of one grammar's
    samples are skipped", and grouping by cause instead of counting showed two
    unrelated classes underneath it -- 3,984 samples that were legitimately
    empty, and 17 real violations at 0.1% that the percentage had been hiding.

    So everything reported here is keyed, and the shortest input that reached a
    key is kept beside its count. A class with a short witness is a class
    somebody can reduce; a rate is a class nobody can. *)

type t

val create : unit -> t

(** [note t key ~witness] records one occurrence. The witness kept is the
    shortest seen for that key, and ties go to the first. *)
val note : t -> string -> witness:string -> unit

type klass =
  { key : string
  ; count : int
  ; witness : string
  }

(** Commonest first, and by key where two are equally common, so two runs of
    the same corpus print the same order. *)
val classes : t -> klass list

(** Occurrences, over every class. *)
val total : t -> int

(** {1 Skip classes}

    An iteration that skips reaches no oracle. Counting the skips and printing
    them is what both of the predecessor's suites did for ten issues without
    asserting anything, so a mutator that fired on every call and emitted bytes
    outside the language read zero failures on every oracle and every reach
    counter: the iteration was skipped, the class was printed, and the number
    was nobody's obligation.

    The claim has to be a ceiling per class rather than "skips are zero". Most
    classes here are obligations of laws stated elsewhere and read zero; one is
    legitimately non-zero, and a rate that drifts with no one in a position to
    notice is what this is for. *)

(** [per_million] is the rate a class may reach, and [0] means any occurrence
    is a failure. [min_iterations] is the depth the rate was measured at: below
    it the class is printed rather than judged, because at two thousand
    iterations one legitimate hit is five hundred per million. *)
type ceiling =
  { why : string
  ; min_iterations : int
  ; per_million : int
  }

(** Stated once. A class that is not in it fails at any depth, which is what
    stops a new skip class arriving unbounded. *)
val ceilings : ceiling list

(** One message per bound the run broke, and nothing where every class is
    inside its own. *)
val check_skips : t -> iterations:int -> string list
