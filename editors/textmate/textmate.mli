(** A TextMate grammar, from a lingo grammar's facts and its scopes.

    {v
      Grammar.t --> Facts.t --> Scopes.t --> generate --> .tmLanguage.json
    v}

    This is a backend, in the same sense the OCaml emitter is one. It takes
    what every backend takes and re-derives nothing: no FIRST set, no kind
    number, no emitted name. It adds one thing of its own: how a rule
    becomes a pattern.

    {2 No configuration}

    The predecessor needed a call per production to say which ones were
    regions, and what their begin and end regexes were. lingo's
    {!Core.Rule.frame} already says it: [Delimited] is a matched pair, and
    [Committed] is a production that contains its own errors, which is the
    same claim. {!Shape} derives the region and its anchors from that.

    So the only input beside the scopes is the language's name, and a
    hand-written pattern where a grammar embeds another language. *)

module Key = Key
module Oniguruma = Oniguruma
module Shape = Shape
module Emit = Emit
module Check = Check

(** The [.tmLanguage.json] text, ending in a newline.

    [language] is the last segment of every scope and of the grammar's own
    scope name, so [~language:"rust"] gives [source.rust] and
    [keyword.other.let.rust]. [scope_prefix] defaults to ["source"];
    ["text"] is the other conventional one, for a markup language.

    [name] is the label an editor shows in its language menu, and defaults
    to [language] capitalised. [file_types] are bare extensions, without the
    dot.

    [raw] splices a hand-written pattern into a production's body. It is the
    way out for something the grammar cannot say: another language embedded
    in a string, or a comment that nests. The JSON is checked for shape and
    then spliced as written. *)
val generate
  :  Scopes.t
  -> ?name:string
  -> ?file_types:string list
  -> ?scope_prefix:string
  -> ?raw:(Core.Grammar.Name.Rule.t * string) list
  -> language:string
  -> unit
  -> (string, Check.problem list) result
