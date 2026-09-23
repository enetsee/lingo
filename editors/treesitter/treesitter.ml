open StdLabels
module Js = Js
module Node = Node
module Grammar_js = Grammar_js
module Queries = Queries
module Check = Check

type output =
  { grammar_js : string
  ; highlights : string
  ; folds : string
  ; locals : string
  }

(* Whether the token's language holds exactly this text.

   redfa settles it, by meeting the two languages. Comparing the spellings
   would settle nothing: [\bstruct\b] and an identifier pattern say nothing
   about each other until they are met. The budget is there because the
   decision is unbounded in general. A token regex settles in a handful of
   states, and a budget that runs out is read as no. *)
let matches (token : Core.Token.def) (text : string) : bool =
  match token.klass with
  | Core.Grammar.Keyword _ | Core.Grammar.Punctuation _ -> false
  | Core.Grammar.Pattern { lexer; _ } ->
    (match Redfa.Regex.str text with
     | exception Invalid_argument _ -> false
     | literal ->
       (match
          Redfa.Regex.is_empty_language_within
            ~max_states:2000
            (Redfa.Regex.inter literal lexer)
        with
        | Some empty -> not empty
        | None -> false))
;;

let word_token (facts : Core.Facts.t) : Core.Token.def option =
  let tokens = Array.to_list Core.Facts.(facts.tokens) in
  let keywords =
    List.filter_map tokens ~f:(fun (token : Core.Token.def) ->
      match token.klass with
      | Core.Grammar.Keyword text -> Some text
      | _ -> None)
  in
  match keywords with
  | [] -> None
  | keywords ->
    let holds_them_all (token : Core.Token.def) : bool =
      (not (Core.Token.is_trivia token)) && List.for_all keywords ~f:(matches token)
    in
    (match List.filter tokens ~f:holds_them_all with
     | [ only ] -> Some only
     | _ -> None)
;;

let generate (scopes : Scopes.t) ~(language : string) ()
  : (output, Check.problem list) result
  =
  match Check.run scopes ~language with
  | Error problems -> Error problems
  | Ok () ->
    let facts = Scopes.facts scopes in
    let word = word_token facts in
    (match Grammar_js.emit facts ~language ~word with
     | Error reason ->
       (* [Check] has already walked every token's regex, so the only way
          here is a shape the emitter declines for a reason of its own. *)
       Error
         [ Check.Bad_setting
             { field = "the grammar"
             ; value = language
             ; reason = "cannot be emitted: " ^ reason
             }
         ]
     | Ok grammar_js ->
       Ok
         { grammar_js
         ; highlights = Queries.highlights scopes
         ; folds = Queries.folds scopes
         ; locals = Queries.locals scopes ~word
         })
;;
