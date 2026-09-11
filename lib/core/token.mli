(** A {!Grammar.token_def} with its kind assigned and its bytes lowered to a
    regex.

    The lowering happens here. A keyword and a punctuation literal become 
    regexes over the same codespace that a pattern uses. A token spelled ["λ"] 
    lowers the way one spelled ["x"] does. *)

(** An index into {!Facts.tokens}. *)
type id = int

type def =
  { id : id
  ; kind : Kind.t
  ; name : Grammar.Name.Token.t
  ; klass : Grammar.token_class
  ; format : Grammar.token_format
  ; trivia : Grammar.trivia_class option
  ; regex : Redfa.Regex.t
  }

val is_trivia : def -> bool

(** The matched bytes of a keyword or punctuation token. A pattern gives [None], 
    since its text is whatever it happened to match. *)
val text : def -> string option

(** The lowering. Two callers share it: the check that rejects a bad literal,
    and the stage that lowers a good one. *)
val lower : Grammar.token_class -> (Redfa.Regex.t, string) result
