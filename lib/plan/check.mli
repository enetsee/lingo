(** The plan's invariants, in code.

    A walk over a plan relies on every one of these. Checking them once, in a
    pass that names where it looked, turns a defect in the lowering into a
    report instead of an exception somewhere downstream. *)

(** What {!run} found wrong. [at] names where: the rule or block, then the
    path inside it. *)
type problem =
  | Rule_out_of_range of
      { at : string
      ; id : int
      }
  | Block_out_of_range of
      { at : string
      ; id : int
      }
  | Kinds_unordered of
      { at : string
      ; kinds : Ir.Kind.t list
      }
  (** A set that is not ascending, or that repeats a kind. The printed
          form follows from the order, so this is what keeps it canonical. *)
  | Negative_kind of
      { at : string
      ; kind : Ir.Kind.t
      }
  | Empty_alt of { at : string } (** An [Alt] with no arms, which can go nowhere. *)
  | Empty_arm of { at : string } (** An arm no kind can take. *)
  | Kind_taken_twice of
      { at : string
      ; kind : Ir.Kind.t
      }
  (** A dispatch lists a kind an earlier entry of the same dispatch
          already takes. The first entry that holds the kind is the one that
          runs, so this one never sees it. *)
  | Loop_state_out_of_range of
      { at : string
      ; state : int
      }
  | Pairs_unordered of { at : string }
  (** The delimiter pairs are not ascending, or one appears twice. A balanced
      skip reads them as a table, and the printed form follows from their
      order. *)
  | Empty_resume of { at : string }
  (** A [resume] holding no kinds means what [resume = None] means, and two
      spellings of one thing would stop the printer being canonical. *)
  | Unbalanced of
      { at : string
      ; depth : int
      }
  (** A branch opens more nodes than it closes, or the other way about. Every
      branch has to balance on its own, or the tree's shape would depend on
      which one ran. *)
  | Close_without_open of { at : string }
  (** A branch closes a node it never opened. Its opens and its closes can
      still come to nothing overall, so counting them is not enough. The
      depth has to stay above zero the whole way through. *)
  | Commit_matches_nothing of { at : string }
  (** A [Commit] whose body can finish without taking a token. A commit
      reports its child missing where nothing in [first] is under the cursor.
      A child that can match nothing is never missing, so that report would
      be wrong on valid input. *)
  | Takes_what_follows of
      { at : string
      ; kinds : Ir.Kind.t list
      }
  (** A choice the plan makes without taking a token takes these kinds, and
      they can also come after it. There are two such choices: an [Alt] whose
      [otherwise] can take nothing, and a loop state that may exit. Either
      way the parse takes the kind, and what comes after never sees it. *)

val pp_problem : Format.formatter -> problem -> unit

(** Findings come back in the order the walk meets them, and
    [Takes_what_follows] comes last.

    [~template:true] leaves [Takes_what_follows] out. A template grammar's
    metavariable can start two things, so its plan overlaps on purpose. *)
val run : ?template:bool -> Ir.Plan.t -> (unit, problem list) result
