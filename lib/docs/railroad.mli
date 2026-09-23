(** A production as a railroad diagram, in SVG.

    A railroad diagram is the production's structure drawn as a track.
    Follow any path from left to right and you have written something the
    production accepts. It reads without a key, so a manual carries one
    beside the listing.

    {1 The shapes}

    The shapes follow "Automatic Layout of Railroad Diagrams"
    (arXiv:2509.15834). A station is a box. A sequence runs stations along
    one line. A stack puts rows one above another and joins them at both
    ends.

    A stack carries a polarity, and that one flag separates a choice from a
    loop. Under {!Forward} every row runs left to right, so the rows are
    alternatives. Under {!Back} the lower row runs right to left, so the
    track returns along it and repeats.

    {!optional} and {!any} are the same two rows under the two polarities:
    an empty row on the line and the body beneath it. The track dips into
    the body once, or any number of times. The paper writes the second
    [(- () d)] and calls it [d*].

    {!repeat} puts the body on the line and a return row under it. A
    separator rides that return row, which is where a reader types it.

    A choice and a loop are the same shape under different polarities, so
    one routine lays every row. The rails into and out of a row are laid the
    same way whatever the row holds, and they stop at the row's edges, so no
    rail crosses a station.

    {1 Where the track enters}

    A stack's entry and exit sit on one of its rows, named by {!Logical}.
    Every diagram built here enters on row 0. A tip naming row 1 would run
    the track through the body and arch the empty row over it. Other
    drawings use that convention.

    The paper has two further tip forms, and neither is built. A vertical
    tip collapses a stack into the one containing it. A physical tip places
    the entry proportionally between two rows. An n-ary {!Stack} stands for
    the paper's nested binary stacks.

    {1 Wrapping}

    A sequence wider than the page breaks across rows, and the rows come out
    even in width. A rail joins one row to the next: out to the margin, back
    along the gap, and down into the next row.

    A single item wider than the page stays whole, and the diagram comes out
    wider than the page. *)

(** How a stack's rows are read. *)
type polarity =
  | Forward (** Each row runs left to right, so the rows are alternatives. *)
  | Back
  (** Every row but the entry row runs right to left, so the track returns
          along it and repeats. *)

(** Which row the track enters and leaves a stack on. A row outside the
    stack clamps to the nearest one. *)
type tip = Logical of int

type t =
  | Station of
      { label : string
      ; terminal : bool (** The label is text the reader types, such as a keyword. *)
      ; link : string option
        (** Where a click goes. A station naming a rule links to that rule's
              section, and a station naming a token links to the token's. *)
      }
  | Nothing
  (** A row with nothing on it. A skip and a loop each carry one, and so
        does a repeat with no separator. *)
  | Seq of t list
  | Stack of
      { polarity : polarity
      ; tip : tip
      ; rows : t list
      }
  | Labelled of
      { name : string
      ; body : t
      } (** A slot name, drawn above the shape it names. *)
  | Wrapped of t list
  (** Rows of a sequence too wide for the page. Only {!place} builds
        these. *)

(** {2 Building one} *)

val station : ?link:string -> ?terminal:bool -> string -> t

(** The rows as alternatives. A single row is given back as it stands. *)
val choice : t list -> t

(** An empty row on the line with [t] under it. The track runs straight past
    [t], or dips into it once. *)
val optional : t -> t

(** The same two rows as {!optional}, read the other way, so the track may
    dip into [t] any number of times. *)
val any : t -> t

(** [t] on the line, then a return row carrying [sep], so [t] repeats one or
    more times. With no [sep] the return row is empty. *)
val repeat : ?sep:t -> t -> t

val of_production : Core.Grammar.t -> Core.Grammar.production -> t
val of_block : Core.Grammar.t -> Core.Grammar.expr_def -> t

(** {1 The geometry, before it is markup}

    Placing and drawing are separate passes. Placing gives coordinates and
    nothing else, so a test reads the geometry without parsing SVG, and
    {!check} settles whether it holds together. *)

(** Which way the track runs. A grammar is directed. A loop is drawn by
    reading one row right to left, so the arrowheads on that row point
    back. *)
type dir =
  | Ltr
  | Rtl

type placed =
  | Box of
      { x : int
      ; y : int
      ; width : int
      ; height : int
      ; terminal : bool
      ; label : string
      ; link : string option
      }
  | Rail of
      { points : (int * int) list (** A run of joined horizontal and vertical segments. *)
      ; dir : dir
      }
  | Caption of
      { x : int
      ; y : int
      ; text : string
      }

type extent =
  { width : int
  ; height : int
  }

(** Wraps the shape to [width], then places it. [width] defaults to 760,
    which is a comfortable column of text.

    A sequence wider than that breaks across rows. A single item wider than
    that stays whole, and the extent comes back wider than [width]. *)
val place : ?width:int -> t -> placed list * extent

(** A fault in a placed diagram.

    {!Rail_through_station} matters most. A rail that crosses the inside of
    a box draws a line through a word, and every coordinate that produced it
    was locally sensible. The paper states the same thing over bounding
    boxes: a sublayout's box may not overlap an unrelated one. *)
type fault =
  | Rail_through_station of
      { label : string
      ; at : int * int
      }
  | Outside_extent of { at : int * int }

val pp_fault : Format.formatter -> fault -> unit

(** Every fault in a placement. Each rail segment is tested against each
    station, and every point against the extent. *)
val check : placed list -> extent -> fault list

(** {1 Drawing} *)

(** The SVG element, with its own width and height.

    It stands alone. Every shape in it carries its own fill, stroke and font
    as attributes, so the diagram is right in a page with no stylesheet. A
    stylesheet rule beats a presentation attribute, so a page that themes
    these still gets its own colours through the classes.

    [title] is read out to anyone who cannot see the picture. [width] is the
    page the diagram is wrapped to. *)
val svg : ?title:string -> ?width:int -> t -> string
