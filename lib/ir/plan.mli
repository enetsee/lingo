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
          kind in no arm takes none of them. The order settles an overlap,
          so the checker rejects a grammar where a later arm is
          unreachable rather than leaving it to be discovered here. *)
  | Commit of
      { first : Kind.t array
        (** What can start the child, and what the diagnostic reports as
              the kinds that would have satisfied the position. Those are one
              question: a set that dispatches and a set that reports would
              have to differ for a parse to take a child it then said was not
              wanted. *)
      ; recover : Kind.t array (** The local half; see above. *)
      ; at_child : string
      ; message : Message.id
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
      ; ends_on : Kind.t array option
        (** What ends the loop instead of being recovered past.

              [None] ends the loop as soon as no state accepts. A separated
              list ends that way: it runs while the cursor is on a separator,
              and anything else is the caller's business.

              [Some ks] recovers instead. A token no state accepts is swept
              into an error node and the loop carries on, and only a kind in
              [ks] or the end of the input ends it. A delimited body works
              this way, so a stray token inside one costs a diagnostic rather
              than the rest of the body: [\[1 : 2\]] keeps the [2].

              [ks] holds the closer, and the resync anchors the grammar
              declared. An anchor is a token that ought to end the body
              rather than be recovered past, such as the keyword that starts
              the next declaration.

              [Some \[||\]] is not [None]. It recovers to the end of the
              input, which is what a root takes. *)
      }
  (** A body of repeated elements, as an automaton.

          A delimited body is three positions and they admit different
          things. After the opener an element or the closer, never the
          separator. After an element the closer or the separator, never an
          element. After a separator an element alone. One record with one
          continuation gets two of those three wrong, so the states are
          written out. *)
  | Drain of Message.id
  (** Sweep what is left of the input into the open node, and report one
          [Extra] over it where any of it was meaningful. The root uses this,
          and nothing else does.

          Trivia past the last child would otherwise fall off the end of the
          input and be lost, and a meaningful token still there would be
          dropped with it. *)

(** One position in a body, and everything that position settles.

    Three questions, and a state holds all three. What continues the body
    from here, in {!loop_state.accepts}. What ending here reports, in
    {!loop_state.exit}. What this position wanted, where the body carries on
    without it, in {!loop_state.when_missing}.

    Whether the body ends at all is the loop's question rather than a state's,
    and [Loop]'s [ends_on] holds it. *)
and loop_state =
  { accepts : (Kind.t array * int) array
    (** On a kind in the set, run {!loop_state.emits} and move to that
          state. *)
  ; exit : exit_policy
  ; when_missing : missing option
  ; emits : instr (** What a transition out of this state runs. *)
  }

(** What a position wanted, where nothing it accepts is under the cursor and
    the body carries on anyway.

    A separated body is the case. After an element the body takes a separator,
    and [a b] has none. The separator is reported missing and the body carries
    on at {!missing.goto}, which is where taking one would have led, so the
    [b] is read as an element rather than swept away as junk.

    A parser reaches for this only where some state of the loop could accept
    what is under the cursor. Where none can, the input is junk rather than a
    gap, and the body sweeps it up instead. *)
and missing =
  { tok : Kind.t (** The token this position wanted. *)
  ; message : Message.id
  ; goto : int (** Carry on in that state, as though the token had been there. *)
  }

(** What ending the loop at a state reports.

    Every state may end a loop. A body that needs an element before it can end
    does not say so here: that element is a {!Commit} in front of the loop,
    and a commit already carries what to report where its child is missing.

    A body can end at its separator even where the author forbade a trailing
    one. The parser still takes the separator, because it is bytes the source
    had, and it reports the diagnostic to say the separator does not belong.

    The report hangs off the exit rather than off a transition. A separator is
    trailing only in the light of the next token ending the body. *)
and exit_policy =
  | May_exit
  | May_exit_reporting of Message.id
  (** End here, with one [Extra] diagnostic over what the last transition
          took. *)

type postfix =
  { lead : Kind.t
  ; bp : int
  ; kind : Kind.t (** The node this operator builds. *)
  ; body : instr
    (** What runs after the lead token. Empty for [x?], where the lead is
          the whole operator.

          The five familiar forms differ only in this, so they are one shape
          here. [x.f] is a {!Commit} over the child that follows. [x\[i\]],
          [x { b }] and [x(a, b)] each take their opener as the lead, then run
          their body and expect their closer, which is what a delimited
          production does once its opener is read. *)
  }

(** What an atom is, once the dispatch has chosen it. *)
type atom =
  | Atom_token (** Take the token and wrap it in {!block.base_kind}. *)
  | Atom_rule of int (** Parse that rule, and wrap nothing. *)

type block =
  { name : string
    (** The author's name for the rule whose body this block is. A dump
          names it, and an emitter names the bindings it writes after it. *)
  ; infix : (Kind.t * (int * int)) array
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
