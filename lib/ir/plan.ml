(* Add a new constructor here to law_plan's plan as well. The round trip runs
   over that plan, so the printer and the reader are never checked on an
   instruction it does not hold. *)
type instr =
  | Seq of instr array
  | Open of Kind.t
  | Close
  | Trivia
  | Bump
  | Expect of
      { tok : Kind.t
      ; message : Message.id
      ; at_child : string option
      ; hole : Kind.t option
      ; placeholder : Kind.t option
      }
  | Call of int
  | Pratt of
      { block : int
      ; min_bp : int
      }
  | Alt of { arms : (Kind.t array * instr) array }
  | Commit of
      { first : Kind.t array
      ; recover : Kind.t array
      ; at_child : string
      ; message : Message.id
      ; hole : Kind.t option
      ; placeholder : Kind.t
      ; resume : Kind.t array option
      ; body : instr
      }
  | Loop of
      { states : loop_state array
      ; entry : int
      ; ends_on : Kind.t array option
      }
  | Drain of Message.id

and loop_state =
  { accepts : (Kind.t array * int) array
  ; exit : exit_policy
  ; when_missing : missing option
  ; emits : instr
  }

and missing =
  { tok : Kind.t
  ; message : Message.id
  ; goto : int
  }

and exit_policy =
  | May_exit
  | May_exit_reporting of Message.id

type postfix =
  { lead : Kind.t
  ; bp : int
  ; kind : Kind.t
  ; body : instr
  }

type atom =
  | Atom_token
  | Atom_rule of int

type block =
  { infix : (Kind.t * (int * int)) array
  ; prefix : (Kind.t * int) array
  ; postfix : postfix array
  ; atoms : (Kind.t array * atom) array
  ; base_kind : Kind.t
  ; prefix_kind : Kind.t option
  ; infix_kind : Kind.t option
  ; hole_kind : Kind.t
  ; expected : Kind.t array
  ; message : Message.id
  }

type rule =
  { name : string
  ; kind : Kind.t
  ; first : Kind.t array
  ; adds : Kind.t array
  ; boundary : bool
  ; body : instr
  }

type t =
  { rules : rule array
  ; blocks : block array
  ; roots : int array
  ; pairs : (Kind.t * Kind.t) array
  ; trivia : Kind.t array
  ; error_kind : Kind.t
  ; missing_kind : Kind.t
  }
