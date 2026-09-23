(** The little bit of JavaScript a [grammar.js] is made of.

    A tree-sitter grammar is a JavaScript file that calls [seq], [choice],
    [repeat] and their friends. Everything this backend emits is one of
    those calls, a string, or a regex, so that is the whole vocabulary here.

    A rule body nests, and a grammar file is read by people. The calls go in
    as a tree and the layout happens at the end. A long body then breaks one
    argument to a line, a short one stays on one line, and no call site has
    to choose. *)

type t

val atom : string -> t

(** [call "seq" args] is [seq(a, b, c)]. A call with one argument is still a
    call. [seq(x)] and [x] are the same to tree-sitter, so a caller may
    collapse one. *)
val call : string -> t list -> t

(** A JavaScript string literal, single quoted. *)
val string : string -> t

(** A regex literal, or the reason the term has none.

    tree-sitter reads the source of a JavaScript regex and hands it to a
    second engine written in Rust, so what is emitted has to parse the same
    way in both. That rules out [\u{...}], which needs a flag in JavaScript
    that the source does not carry. A codepoint above ASCII goes out as
    itself, in UTF-8. *)
val regex : Redfa.Regex.t -> (t, string) result

(** Lays the tree out to fit [width], breaking a call that does not.

    [column] is where the text starts on its line, and it settles whether
    the text fits. [indent] is where a broken call's closing bracket goes,
    and its arguments go two further in. The two differ because a rule is
    written [name: $ => body], and the body's continuation lines belong
    under the rule rather than under the arrow. *)
val render : width:int -> column:int -> indent:int -> t -> string
