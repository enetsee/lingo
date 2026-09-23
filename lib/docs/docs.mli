(** A language's reference manual, from its grammar.

    A toolkit that takes a grammar and emits a parser, a formatter and two
    editor integrations should emit the manual too. The alternative is a
    second hand-written source, and a second source drifts.

    {v
      Grammar.t --> Facts.t --> Scopes.t --.
            |                               |
            '-------------------------------+--> generate --> a page
    v}

    {1 The one backend that needs the grammar}

    Every other consumer takes {!Core.Facts.t}: desugared, normalised,
    kind-numbered. A manual shows the grammar {e as the author wrote it}.
    Their production names, their expression block still a block rather than
    the rules it desugars into, and their own ordering.

    So {!Core.Facts.of_grammar} returning a {!Core.Facts.t} leaves the
    surface intact. The caller keeps what they passed in, and hands both
    here.

    The facts arrive inside the {!Scopes.t} rather than beside it. Taking
    them separately would take a claim on trust, since nothing would tie
    the two to one grammar.

    {1 What is on the page}

    {v
      a table of contents   every production, by name
      production listings   the grammar
      railroad diagrams     the structure of each production
      operator precedence   the expression block's binding powers
      token table           the facts' lexer, with each regex printed back
      first and follow      the facts' fixpoints
      code examples         lexed and coloured through the scopes
    v}

    Nothing here is declared twice. Every one of those is something the
    toolkit already computed for another reason.

    {1 Why it needs handsome's annotations}

    A listing has to be laid out to a width and then marked up, and those
    are two passes over one document. handsome renders to a stream rather
    than to a string, so the second pass reads the first's output and no
    part of the layout mentions HTML. Without that, a page would need a
    renderer of its own. *)

module Mark = Mark
module Render = Render
module Listing = Listing
module Railroad = Railroad
module Tables = Tables
module Highlight = Highlight

type output =
  { page : string (** A whole document, stylesheet inside it, ready to open. *)
  ; body : string (** The same content with no page around it, for embedding. *)
  ; stylesheet : string (** What [body] needs, for a caller with its own page. *)
  ; diagrams : (string * string) list
    (** Each production's railroad diagram, by the author's name for it, for
          a caller putting one somewhere else. *)
  }

(** [title] defaults to the first root's name. [examples] are pairs of a
    caption and some source. Each is lexed and coloured, and its lines are
    kept as they came. [width] is the column a listing is laid out to, and
    defaults to 76. *)
val generate
  :  Core.Grammar.t
  -> Scopes.t
  -> ?title:string
  -> ?examples:(string * string) list
  -> ?width:int
  -> unit
  -> output
