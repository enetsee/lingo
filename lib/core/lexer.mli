(** The token automaton as arrays for an emitter to write out.

    {!Facts.t} holds the DFA. Generated code links [lingo_runtime] and
    nothing else of ours, and redfa is a generator's dependency only, so an
    emitter takes the automaton from here as integers.

    A character reaches a transition through its {e class}, the coarsest
    partition of the codespace that every state's transitions respect. Two
    characters in one class behave alike from every state, so a row of
    {!t.next} holds one cell per class rather than one per character.

    A grammar usually gives far fewer classes than states. One whose states
    share no structure gives a class per state, and {!t.next} is then their
    product, so an emitter can count them before writing the table out.

    Max munch, the intern table and the error and unterminated tokens belong
    to the lexer, and the emitter writes them. *)

type t = private
  { num_states : int
  ; num_classes : int
  ; segments : int array
    (** The least codepoint of each segment of the codespace, ascending. The
          first is [0], so every codepoint is in a segment. *)
  ; segment_class : int array
    (** The class of every codepoint in the segment at the same index, or
          [-1] where that segment is in no class. It is as long as
          {!t.segments}. *)
  ; next : int array
    (** [num_states * num_classes] cells, row major: where a character of
          that class takes that state, and [-1] where the state stops. *)
  ; accept : Kind.t option array
    (** Indexed by state: the token kind the state accepts. A state that
          accepts two takes the one whose token is declared first. *)
  }

(** The state a token starts in. *)
val initial : int

val of_facts : Facts.t -> t

(** The class of a codepoint, or [-1] where it is in none. Total on any int:
    a surrogate and a value outside the codespace both give [-1]. *)
val class_of : t -> int -> int

(** Where [state] goes on a character of [klass]. [-1] where the state stops,
    and for a [klass] of [-1]. *)
val step : t -> state:int -> klass:int -> int

(** A stable dump: the counts, the codespace by class, and one row per state.
    Kinds print as integers; a caller wanting names prints a legend above it.
    The same grammar gives the same bytes. *)
val pp : Format.formatter -> t -> unit
