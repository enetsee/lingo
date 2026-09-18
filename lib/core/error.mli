(** What a check reports.

    A finding holds the data the check found. Nothing in it is rendered.
    {!message} and {!hint} build the sentences from that data. A consumer
    that wants the parts reads {!type-detail}, and never has to take a
    rendered sentence back apart.

    {!code} is the stable string a consumer keys off. Renaming one is a
    breaking change. The mapping in {!code} is the definition, and the code
    literals appear nowhere else.

    {!codes} lists every code the checker can emit. It is a value so that a
    test can range over it. Each code must have a witness grammar that
    provokes it alone. A code with no witness is either untested or
    unreachable. *)

type severity =
  | Fatal
  | Warning

(** Where in the grammar the finding sits, in the author's own names. *)
type where =
  | At_grammar
  | At_token of Grammar.Name.Token.t
  | At_production of Grammar.Name.Rule.t
  | At_child of
      { production : Grammar.Name.Rule.t
      ; child : Grammar.Name.Child.t
      }
  | At_block of Grammar.Name.Rule.t
  | At_operator of
      { block : Grammar.Name.Rule.t
      ; token : Grammar.Name.Token.t
      }

(** A kind travels with the name it prints as. The check that raises a
    finding holds the kind table. A caller that got an error back from
    {!Facts.of_grammar} holds no facts, so it has no table, and it could not
    resolve the name itself. *)
type kind_ref =
  { kind : Kind.t
  ; name : Kind.Name.t
  }

(** Reads the names off the table, for a check building one of the details
    below. *)
val kind_refs : Kind.Table.t -> Kind.t list -> kind_ref list

(** Why a production's resync anchors reach nothing. Only a repeated body
    inside a matched pair has something for one to end. *)
type resync_body =
  | Repeats_nothing
  | Ends_at_its_separator

type token_unreachable_reason =
  | Empty_language
  | Subsumed_by of Grammar.Name.Token.t

type detail =
  (* -- declarations and the kind table ------------------------------------- *)
  | Empty_grammar
  | Invalid_name of
      { what : string
      ; name : string
      ; reason : string
      }
  | Invalid_token_literal of { reason : string }
  | Dup_kind_name of
      { sources : string list
      ; emitted : string
      }
  | Reserved_name of
      { sources : string list
      ; emitted : string
      ; scope : Manifest.Scope.t
      }
  | Name_collision of
      { sources : string list
      ; emitted : string
      ; scope : Manifest.Scope.t
      }
  | Dup_child_name of
      { sources : string list
      ; emitted : string
      ; view_module : string
      }
  | Unknown_rule of { name : Grammar.Name.Rule.t }
  | Unknown_token of { name : Grammar.Name.Token.t }
  | Unknown_op_token of { name : Grammar.Name.Token.t }
  | Unknown_delimiter_token of { name : Grammar.Name.Token.t }
  | Unknown_recover_to_token of { name : Grammar.Name.Token.t }
  | Unknown_resync_anchor of { name : Grammar.Name.Token.t }
  | Unknown_identity_child of { name : Grammar.Name.Child.t }
  | Unknown_message_child of { name : Grammar.Name.Child.t }
  | Unused_message_child of { name : Grammar.Name.Child.t }
  | Unused_recover_to of { name : Grammar.Name.Child.t }
  | Empty_alternatives
  | No_roots
  | Root_is_block of { name : Grammar.Name.Rule.t }
  | Unknown_root of { name : Grammar.Name.Rule.t }
  | Dup_root of { name : Grammar.Name.Rule.t }
  | Postfix_suffix_missing of
      { index : int
      ; total : int
      }
  | Postfix_suffix_duplicate of { suffix : string }
  | Nullable_token
  | Empty_pratt_atoms
  | Dup_pratt_op of
      { token : Grammar.Name.Token.t
      ; category : string
      }
  | Mixed_pratt_role of { token : Grammar.Name.Token.t }
  | Mixed_assoc_at_bp of
      { bp : int
      ; ops : (Grammar.Name.Token.t * Grammar.assoc) list
      }
  (* -- resolved children, normalised framing -------------------------------- *)
  | Delimited_arity of { children : int }
  | Separated_arity of { children : int }
  | Unused_resync_anchors of { body : resync_body }
  | Repeated_vs_single of { children : Grammar.Name.Child.t list }
  | Repeated_vs_single_kinds of
      { repeated : Grammar.Name.Child.t
      ; single : Grammar.Name.Child.t
      ; shared : kind_ref list
      }
  | Ambiguous_same_kind_child of { children : Grammar.Name.Child.t list }
  | Overlapping_single_kinds of
      { a : Grammar.Name.Child.t
      ; a_kinds : kind_ref list
      ; b : Grammar.Name.Child.t
      ; b_kinds : kind_ref list
      }
  (* -- the fixpoints and the lexer automaton -------------------------------- *)
  | First_first_conflict of { common : kind_ref list }
  | First_follow_conflict of { common : kind_ref list }
  | Left_recursion of { members : Grammar.Name.Rule.t list }
  | Nullable_repeated of { rule : Grammar.Name.Rule.t }
  | Nullable_pratt_atom of { atom : string }
  | Nullable_separated_element of { element : string }
  | Empty_first_set of { referenced_from : string list }
  | Pratt_atom_conflict of { common : kind_ref list }
  | Prefix_atom_conflict of { atom : string }
  | Token_unreachable of { reason : token_unreachable_reason }

type t =
  { detail : detail
  ; where : where
  }

val make : detail:detail -> where -> t

(** {1 Reading a finding} *)

val code : t -> string
val message : t -> string
val hint : t -> string option
val severity : t -> severity

(** {1 The codes}

    Grouped by the derivation stage that can answer them; {!Facts} describes
    the staging. {!codes} concatenates the three lists below. *)

val names_stage_codes : string list
val shape_stage_codes : string list
val full_stage_codes : string list
val codes_by_stage : (string * string list) list
val codes : string list

(** {1 Printing} *)

val pp_where : Format.formatter -> where -> unit
val pp : Format.formatter -> t -> unit
val pp_list : Format.formatter -> t list -> unit
val to_string : t -> string
val compare : t -> t -> int
