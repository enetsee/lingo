(** Where a formatter may end a line, as data.

    A layout holds two things about a node: which boundaries inside it may end
    the line, and how far its body is indented. Nothing here mentions a grammar
    or a target language.

    Every set is a [Kind.t array] in ascending order with no repeats.

    {1 Spacing is not here}

    A layout never holds "a space goes here". What falls between two tokens is
    settled at the boundary, from the two tokens' own flags and from what the
    grammar's lexer makes of the bytes on either side. The fold in
    [lingo_runtime] settles it once per boundary, and nothing else touches it.

    The two live apart because they are settled at different times. Whether the
    line may end at a boundary follows from the layout alone, so a walk has it
    before anything is rendered. How many blanks the boundary needs follows from
    the bytes that reach it, so it is settled as they are written.

    {1 The walk is over the tree}

    A recovering parse builds trees the grammar does not describe: a child
    missing, a child twice, an error node holding whatever was swept up. So the
    fold walks the children the tree has and reads {!rule.slots} for what each
    one is. It does not walk the slots and reach for children.

    That is what makes losing a token unwritable rather than forbidden. *)

(** What a boundary does to the line.

    [Hard] carries how many line breaks, so a blank line between two items is
    [Hard 2]. *)
type break =
  | Flat (** The line does not end here. *)
  | Fit (** It ends here when what follows does not fit. *)
  | Hard of int

(** What a body does about a separator after its last element.

    The formatter only ever writes one. It never takes one away: dropping
    changes the token run, which changes which frame the next parse gives the
    closer to, so the tail loses one separator per pass and never settles.

    - [Never]: the parser reports one, and the formatter writes none.
    - [On_break]: the formatter writes one where the body breaks across lines.
      A separator the source already had is a request for a broken body, and it
      is the only way a grammar's user can make one directly. It stays, and the
      body breaks.
    - [Always]: the formatter writes one whether the body breaks or not. This is
      a terminator rather than a separator. It is the [;] after every statement
      in a block, the last one included, so a separator the source had carries no
      request and the body still lays out by width. *)
type trailing =
  | Never
  | On_break
  | Always

(** The separator of a delimited body or a separated list.

    The fold reads {!sep.sep_kind} to recognise a separator it meets in the
    tree: the boundary in front of one does not end the line, so [a,] never
    becomes [a\n,]. *)
type sep =
  { sep_kind : Kind.t
  ; text : string (** Its spelling, which is the only byte string the fold writes. *)
  ; trailing : trailing
  }

(** How a body is bracketed.

    A production's delimited framing and an expression block's enclosed
    postfix body are the same shape, and the facts have already made them the
    same value. So one frame covers both and there is no second walk for
    the second spelling. *)
type frame =
  | Plain
  | Delimited of
      { open_ : Kind.t
      ; close : Kind.t
      ; sep : sep option
      }
  | Separated of sep

(** A declared child position.

    The fold matches a tree child against the slots left to right. A slot that
    takes more than one child keeps taking them, which is what separates a second
    element of a list from the first child of the next position. *)
type slot =
  { kinds : Kind.t array (** What may fill it. *)
  ; repeats : bool (** Whether it takes more than one child. *)
  ; before : break (** At the boundary in front of the slot's first child. *)
  ; between : break (** At the boundary between two children of the slot. *)
  }

type rule =
  { name : string (** The author's name for it, for a dump and a diagnostic. *)
  ; kind : Kind.t
  ; frame : frame
  ; slots : slot array (** In declaration order. *)
  ; body : break
    (** At a boundary the slots do not account for. Recovery puts children
          in a node that no slot admits, and they are laid out with this. *)
  ; inner : break (** Just inside a frame: after the opener, before the closer. *)
  ; indent : int (** How far the body is nested. *)
  ; edge_before : bool option
    (** Replaces the leading spacing flag, which otherwise comes from this
          rule's first token. *)
  ; edge_after : bool option (** The same on the trailing edge. *)
  }

(** What the formatter does with a trivia token. *)
type trivia =
  | Reformat (** Drop it and let the boundary re-emit the spacing. *)
  | Preserve (** Write it as it stands, on the line the source put it on. *)

(** A token's own contribution to a boundary.

    There is no spelling here. The fold writes the tokens the tree holds, and
    their bytes come with them, so a layout that carried a token's text would be
    carrying bytes for the fold to write of its own accord. It has none to
    write. *)
type token =
  { space_before : bool
  ; space_after : bool
  ; trivia : trivia option
  }

(** What a kind that is not a token holds: a space on either side and no trivia.
    Only a token's flags are read at a boundary, so the spacing here is dead.
    [trivia] is read of every child, so it is not. *)
val not_a_token : token

type t =
  { rules : rule array
  ; of_kind : int array
    (** Kind to an index into {!t.rules}, or [-1]. A kind with no rule is
          laid out by the fold's own account of it: a hole writes its token's
          text, an error node writes its children where the source had them. *)
  ; tokens : token option array
    (** Indexed by kind. [None] where the kind is a node's, which is what
          separates a token with the default flags from a kind that is not a
          token at all. *)
  }
