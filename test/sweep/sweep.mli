(** A corpus of inputs generated from seeds.

    Every input is a seed's own tokens, edited: dropped, duplicated, swapped,
    replaced, truncated, or drawn at random from the pool the seeds hold. So it
    stays near the language rather than drifting into bytes a parse refuses at
    the first token.

    The generator is a linear congruential one with a fixed seed, and the state
    is global. So the corpus is the same on every run, and a count in a
    falsification record is reproducible as long as the calls stay in the same
    order. *)

(** [inputs facts seeds] is the generated corpus. The seeds are not in it; a
    caller needing them appends them.

    [depth] multiplies how many inputs each seed and each grammar contribute,
    and defaults to 1, which is about 14,000 per grammar. A deeper sweep is a
    different corpus rather than a longer one: the state carries, so input
    [n] at one depth is not input [n] at another.

    Depth is what the predecessor's idempotence defect needed. It read zero at
    100,000 for months and only appeared once the sweep ran to a million, so a
    law that runs at depth 1 in the suite has to be run deep by hand before it
    means anything. *)
val inputs : ?depth:int -> Core.Facts.t -> string list -> string list
