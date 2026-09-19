(** The layout's invariants, in code.

    The fold relies on every one of these. Checking them once, in a pass that
    names where it looked, turns a defect in the lowering into a report
    instead of a document nobody can read. *)

(** What {!run} found wrong. [at] names the rule, then the place inside it. *)
type problem =
  | Rule_out_of_range of
      { at : string
      ; id : int
      }
  | Kind_out_of_range of
      { at : string
      ; kind : int
      }
  | Kind_not_its_rule of
      { at : string
      ; kind : int
      }
  (** A rule's kind does not index back to that rule. The fold reaches a
          rule through the kind on the node, so a rule it cannot be reached by
          is a rule that never runs. *)
  | Kinds_unordered of
      { at : string
      ; kinds : int list
      }
  (** A slot's kinds are not ascending, or one repeats. The printed form
          follows from the order, so this is what keeps it canonical. *)
  | Lead_slot_breaks of { at : string }
  (** The first slot names a break at a boundary that is not there. Nothing of
          the rule precedes its first child, so that boundary can only stand for
          the caller's. *)
  | Single_slot_between of { at : string }
  (** A slot that holds one child names a break for the boundary between
          two of them. No child pair can reach it, so the two spellings of one
          layout would print differently and say the same thing. *)
  | Empty_break of
      { at : string
      ; lines : int
      }
  (** A [Hard] break that ends the line no times, which is what [Flat]
          means. *)
  | Negative_indent of
      { at : string
      ; indent : int
      }
  | Rule_kind_is_a_token of
      { at : string
      ; kind : int
      }
  (** A rule's kind also carries a token entry. A node and a token cannot
          share a kind, and the fold would read the wrong one at a boundary. *)

val pp_problem : Format.formatter -> problem -> unit

(** One pass. Findings come back in the order the walk meets them. *)
val run : Ir.Layout.t -> (unit, problem list) result
