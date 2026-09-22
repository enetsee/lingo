(** The laws, asked of one input.

    Seven, and each is a property with no baseline: a count of anything here is
    a count of defects. They are asked independently and all of them are
    reported, because a fix that turns failures of one into failures of another
    is not a fix and the residue is the third law nobody stated.

    {1 What each one says}

    - {b A} -- the parse rebuilds its input, byte for byte. It holds on
      malformed input too: recovery may add structure and may never lose text.
    - {b B} -- [format] is a fixed point. Its own output formats to itself.
    - {b C} -- the output holds the tree's tokens, in order, and at most one
      separator more per frame the parse closed. Read through the reparse.
    - {b D} -- and the lexer agrees. The same comparison read through the
      lexer rather than the parse, which is the one that sees two tokens fused
      at a boundary: a fusion changes what the reparse holds only when losing
      the pair also changes what the next pass lays out.
    - {b E} -- the output parses back to the same tree. Not the same bytes,
      which is B, and not the same tokens, which is C: the same shape. A
      formatter can be a fixed point that holds every token and still write
      bytes the parser reads into a different tree, and a recovered tree is
      where that happens.
    - {b F} -- every line a break was declined on is inside the ruler. A
      declined break is the engine saying it could have ended the line there;
      a line past the ruler with one on it is bytes placed by a fit decision
      that left them out.
    - {b W} -- which tokens come out does not depend on the width. Width
      selects layout and must not select content, the trailing separator a
      policy owns aside.

    Beside them sit the document's own two checks. No text node holds a
    newline. A
    raw newline inside a text node is invisible to the column model, so the
    group measures as able to fit a line it ends, and F would be silent about
    every byte after it.

    {1 Soundness}

    Each of these is a forbidding law, so the way one fails badly is by firing
    on an input that is fine. Nothing here can see that; what sees it is
    running the whole set over a corpus that is known good and reading zero.
    test/laws/law_fuzz.ml does that first, before any mutation, and it is the
    oldest hole in the project closed. *)

type finding =
  | Lossy of { rebuilt : string } (** A. What the tree serialises back to. *)
  | Unstable of
      { width : int
      ; once : string
      ; twice : string
      } (** B. *)
  | Lost of
      { width : int
      ; once : string
      ; at : string (** Which token, and what came back in its place. *)
      } (** C. *)
  | Refused of
      { width : int
      ; once : string
      ; at : string
      } (** D. The output does not lex back into the tokens it was written from. *)
  | Recast of
      { width : int
      ; once : string
      ; before : string
      ; after : string (** The two shapes, as kind trees. *)
      } (** E. *)
  | Uncounted of
      { width : int
      ; once : string
      ; line : int
      ; reached : int (** How wide that line ran. *)
      } (** F. *)
  | Varies of
      { widths : int * int
      ; narrow : string
      ; wide : string
      } (** W. The two token runs, and the widths they came out at. *)
  | Wrapped of { newlines : int } (** A text node holds a newline. *)
  | Unframed of { conditionals : int }
  (** A conditional from [Handsome.Utf8.framed] used outside the body it
          was handed to. The fold builds one per frame and never lets it out of
          the callback, so this is the fold, not the document. *)
  | Raised of { message : string } (** The oracle itself raised. *)

(** Which law, as a single letter, or ["doc"] and ["raised"]. A report groups
    by this so a law that reads zero still has a row. *)
val law : finding -> string

(** Every value {!law} returns, so a report shows the zeros too. *)
val laws : string list

(** One line naming the law and what broke, without the inputs. A class key. *)
val describe : finding -> string

(** Every law, over one input, at each width.

    The parse happens once. Each width is laid out, rendered, reparsed and
    reformatted, so the cost is linear in the widths and a caller measuring
    volume passes one. *)
val run : Harness.t -> widths:int list -> string -> finding list

(** The tree as its kinds, with every token left out. Two strings holding the
    same meaningful tokens must give the same one, or a formatter cannot
    settle: it writes the same tokens every pass and the spacing is all it may
    change. Exposed because E is stated over it. *)
val shape : Siesta.Green.node -> string
