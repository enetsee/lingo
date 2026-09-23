(** Folding a rendered document into HTML.

    handsome hands back a stream: the text with its measured width, the line
    breaks with their indentation, and the annotations as pushes and pops.
    One walk over that stream gives the markup. *)

type document = Mark.t Handsome.Utf8.t

(** The document laid out at [width] and folded into markup, ready to go
    inside a [<pre>]. Text is escaped. An annotated region becomes a
    [<span>] carrying {!Mark.classes}, wrapped in an [<a>] where
    {!Mark.link} gives one. *)
val html : width:int -> document -> string

(** The same document as plain text, for a caller with no page to put it
    in. *)
val text : width:int -> document -> string

(** Escapes the five characters that mean something in markup. *)
val escape : string -> string
