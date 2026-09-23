(** What this backend rejects, over and above what a grammar has already
    passed.

    A {!Scopes.t} carries a {!Core.Facts.t}, so every name has resolved and
    every check in [lingo.core] has run. A TextMate grammar then adds
    constraints of its own. Its repository is a JSON object, so its keys
    have to differ. Its patterns are tried in order, so two that share an
    opening token make the second unreachable. Its regexes are Oniguruma,
    and not every term can be written in that dialect.

    Every problem here is fatal. A TextMate grammar that loads and colours
    the wrong thing is worse than one that does not load, because nothing
    reports it. *)

type problem =
  | No_oniguruma of
      { token : Core.Grammar.Name.Token.t
      ; reason : string
      }
  (** The token's regex has no Oniguruma form. Set
        {!Core.Grammar.pattern_spec.textmate} to write one by hand. *)
  | Key_collision of
      { key : string
      ; sources : string list
      }
  (** Two repository entries claim one key. A JSON object holds one value
        per key, so one entry would be dropped and every reference to it
        would land on the other. *)
  | Shared_opener of
      { rule : Core.Grammar.Name.Rule.t
      ; token : Core.Grammar.Name.Token.t
      ; children : Core.Grammar.Name.Child.t list
      }
  (** Two children of one rule point at matched pairs that open on the
        same token. A pattern list is tried in order, so every occurrence of
        that token fires the first, and the rest are unreachable. *)
  | Shared_postfix_lead of
      { block : Core.Grammar.Name.Rule.t
      ; token : Core.Grammar.Name.Token.t
      } (** The same, among one block's postfix operators. *)
  | Forwarding_cycle of { rules : Core.Grammar.Name.Rule.t list }
  (** Each rule in the cycle forwards to the next and adds nothing, so
        every one of them would be dropped and every reference left
        dangling. *)
  | Unattachable_scope of { rule : Core.Grammar.Name.Rule.t }
  (** A scope was set on a rule whose emission has nowhere to carry it.

        A region and a single match both have a field for the span's own
        name. A list of patterns does not: it splices into whichever list
        includes it, and there is no enclosing object to name. The same goes
        for an expression block, and for a postfix operator whose whole body
        is its lead token. *)
  | Bad_raw_pattern of
      { rule : Core.Grammar.Name.Rule.t
      ; reason : string
      }
  | Bad_setting of
      { field : string
      ; value : string
      ; reason : string
      }

val pp_problem : Format.formatter -> problem -> unit
val problem_to_string : problem -> string

(** Runs every check, and parses the hand-written patterns on the way
    through. A pattern is checked by parsing it, so it is parsed here once
    and the result is kept. Parsing it twice would let the two readings
    differ.

    Those patterns come back indexed by {!Core.Rule.type-id}, ready for the
    emitter. Problems come back sorted, and all of them. *)
val run
  :  Scopes.t
  -> language:string
  -> scope_prefix:string
  -> file_types:string list
  -> raw:(Core.Grammar.Name.Rule.t * string) list
  -> (Yojson.Basic.t list array, problem list) result
