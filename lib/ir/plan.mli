(** The recursive-descent machine, as data.

    A plan says what a parser does without saying how it is written. Nothing 
    here mentions a grammar or a target language.

    {1 Sets of kinds}

    Every set here is a [Kind.t array] in ascending order with no repeats. 

    {1 Recovery sets are the local half}

    [recover] on a {!Commit} is what the position contributes on its own. It
    is not the whole set a parser resumes on.

    A parser threads the frames it has open down its own call stack. At a
    {!Call} it passes what it was given, plus the closers in {!rule.adds} of
    the rule it is in. The callee unions that with the local half at each of
    its own positions.

    A rule whose {!rule.boundary} is set drops what it was given and passes
    an empty set down instead. A failure inside it then resumes on its own
    delimiters rather than on a caller's, so recovery cannot leave a scope it
    was never in.

    The static alternative is one set per position, holding the closers of
    every frame the rule is ever inside. That is coarser, and a rule reached
    from two different frames then stops on both at both sites. *)

type instr =
  | Seq of instr array
  | Open of Kind.t (** Start a node of that kind. *)
  | Close (** Finish the open node. *)
  | Trivia (** Put the trivia under the cursor into the open node. *)
  | Bump (** Take one token. *)
  | Expect of
      { tok : Kind.t
      ; message : Message.id
      ; at_child : string option
      ; hole : Kind.t option
      ; placeholder : Kind.t option
      }
  (** Take a token of that kind, or report it missing. The three
          optional fields are the arguments of [Recover.expect]. *)
  | Call of int (** Parse the rule at that index in {!t.rules}. *)
  | Pratt of
      { block : int
      ; min_bp : int
      }
  (** Parse an expression from the block at that index, taking
          operators that bind at least as tightly as [min_bp]. *)
  | Alt of { arms : (Kind.t array * instr) array }
  (** Take the first arm whose set holds the kind under the cursor. A
          kind in no arm takes none of them. The order decides an overlap,
          so the checker rejects a grammar where a later arm is
          unreachable rather than leaving it to be discovered here. *)
  | Commit of
      { first : Kind.t array (** What can start the child. *)
      ; recover : Kind.t array (** The local half; see above. *)
      ; at_child : string
      ; message : Message.id
      ; expected : Kind.t array (** What the diagnostic reports. *)
      ; hole : Kind.t option
      ; placeholder : Kind.t (** The node that stands in for the child. *)
      ; resume : Kind.t array option
        (** What a later child can start with. The parser skips recovery
              where the cursor is already on one of these, because the rest
              of the rule can take it. [None] always recovers. *)
      ; body : instr
      } (** A required child of a production that keeps its errors inside. *)
  | Loop of
      { states : loop_state array
      ; entry : int
      }
  (** A body of repeated elements, as an automaton.

          A delimited body is three positions and they admit different
          things. After the opener an element or the closer, never the
          separator. After an element the closer or the separator, never an
          element. After a separator an element alone. One record with one
          continuation gets two of those three wrong, so the states are
          written out. *)
  | Drain
  (** Sweep what is left of the input into the open node, with one
          [Extra] diagnostic where any of it was meaningful. The root uses
          this, and nothing else does. *)

and loop_state =
  { accepts : (Kind.t array * int) array
    (** On a kind in the set, run {!loop_state.emits} and move to that
          state. *)
  ; can_exit : bool (** Whether the loop may end here. *)
  ; emits : instr (** What a transition out of this state runs. *)
  }

(** What an expression block builds on top of an atom. *)
type postfix_body =
  | Nothing (** [x?] *)
  | Then of Kind.t array (** [x.f], and these are what [f] can start with. *)
  | Enclosed of
      { close : Kind.t
      ; body : instr
      }
  (** [x\[i\]], [x { b }] and [x(a, b)]. The lead token opened the
          pair, [body] reads what is inside, and [close] shuts it. *)

type postfix =
  { lead : Kind.t
  ; bp : int
  ; kind : Kind.t (** The node this operator builds. *)
  ; body : postfix_body
  }

(** What an atom is, once the dispatch has chosen it. *)
type atom =
  | Atom_token (** Take the token and wrap it in {!block.base_kind}. *)
  | Atom_rule of int (** Parse that rule, and wrap nothing. *)

type block =
  { infix : (Kind.t * (int * int)) array
    (** Operator to its left and right binding powers. Associativity is
          the encoding: [Left] is [(bp, bp + 1)] and [Right] is [(bp, bp)],
          and one [left_bp >= min_bp] test reads both. *)
  ; prefix : (Kind.t * int) array (** Operator to its right binding power. *)
  ; postfix : postfix array
  ; atoms : (Kind.t array * atom) array (** An ordered dispatch cascade. *)
  ; base_kind : Kind.t
  ; prefix_kind : Kind.t option
  ; infix_kind : Kind.t option
  ; hole_kind : Kind.t
  ; expected : Kind.t array (** What a diagnostic reports at an atom. *)
  ; message : Message.id
  }

type rule =
  { name : string (** The author's name for it, for a dump and a diagnostic. *)
  ; kind : Kind.t
  ; first : Kind.t array
  ; adds : Kind.t array
    (** What this rule's own frame contributes to the recovery set it
          passes down. Its closer, and its separator where it has one. *)
  ; boundary : bool (** Drop the inbound set; see above. *)
  ; body : instr
  }

type t =
  { rules : rule array
  ; blocks : block array
  ; roots : int array (** Indices into {!t.rules}, in declaration order. *)
  ; pairs : (Kind.t * Kind.t) array (** Every matched pair, for a balanced skip. *)
  ; trivia : Kind.t array
  ; error_kind : Kind.t
  ; missing_kind : Kind.t
  }
