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

let is_trivia d = d.trivia <> None

let text (d : def) : string option =
  match d.klass with
  | Grammar.Keyword s | Grammar.Punctuation s -> Some s
  | Grammar.Pattern _ -> None
;;

let lower (k : Grammar.token_class) =
  match k with
  | Grammar.Pattern { lexer; _ } -> Ok lexer
  | Grammar.Keyword s | Grammar.Punctuation s ->
    (* [Regex.str] decodes UTF-8 and raises on malformed input. It could have
       denoted U+FFFD silently instead. Raising is the right call, and it leaves 
       the bad-bytes case to the caller. *)
    (try Ok (Redfa.Regex.str s) with
     | Invalid_argument _ -> Error "literal is not valid UTF-8")
;;
