(** A TextMate scope name, as a typed vocabulary.

    A scope is a dotted path, and a theme matches a prefix of it. A rule
    written for [keyword.control] fires on [keyword.control.let.rust] too.
    So the paths are spelled out here rather than passed as strings. A
    misspelled [keyword.controll] then fails to compile. A string would
    reach an editor instead, and the editor colours nothing and reports
    nothing.

    TextMate renders these into [.tmLanguage.json], tree-sitter into
    [highlights.scm], and the documentation backend into the class on a
    [<span>]. The vocabulary is TextMate's, because the other two borrow
    from it already.

    {!Custom} takes a path this list does not cover, and is spliced as
    written. {!check} rejects a malformed one. *)

(** Which bracket a {!Punctuation_section} is about. *)
type section =
  | Parens
  | Brackets
  | Braces

(** A scope leaf. An arm carrying [string option] has an optional trailing
    segment, so [Keyword_control None] is [keyword.control] and
    [Keyword_control (Some "let")] is [keyword.control.let]. *)
type t =
  (* keyword.* *)
  | Keyword_control of string option
  | Keyword_other of string option
  | Keyword_operator_arithmetic
  | Keyword_operator_comparison
  | Keyword_operator_logical
  | Keyword_operator_assignment
  | Keyword_operator_bitwise
  | Keyword_operator_other of string
  (* storage.* *)
  | Storage_type of string option
  | Storage_modifier
  (* entity.name.* *)
  | Entity_name_function
  | Entity_name_type_struct
  | Entity_name_type_enum
  | Entity_name_type_trait
  | Entity_name_type_class
  | Entity_name_type of string option
  (* variable.* *)
  | Variable_parameter
  | Variable_language
  | Variable_other
  | Variable_other_member
  (* string.* *)
  | String_quoted_single
  | String_quoted_double
  | String_quoted_triple
  | String_quoted_other of string
  (* comment.* *)
  | Comment_line_slashes
  | Comment_line_hash
  | Comment_line_other of string
  | Comment_block
  (* constant.* *)
  | Constant_numeric_integer
  | Constant_numeric_float
  | Constant_numeric_hex
  | Constant_numeric_other
  | Constant_language
  (* punctuation.* *)
  | Punctuation_separator
  | Punctuation_accessor
  | Punctuation_definition of string
  | Punctuation_section of section
  (** The same scope on the opener and the closer, for a theme that styles
        the pair alike. *)
  | Punctuation_section_begin of section
  | Punctuation_section_end of section
  (* the rest *)
  | Meta of string
  | Custom of string (** A path this list does not name, spliced as written. *)

(** The dotted path, with [language] appended as the last segment. Leaving
    [language] off gives the bare path. Leave it off to compare two
    scopes. *)
val to_string : ?language:string -> t -> string

(** The reason the scope would render a malformed path, or [None].

    A path holds [A-Za-z0-9._-] and nothing else, and no segment of it is
    empty. A [Custom] carrying a quote renders JSON that loads and matches
    nothing, and an empty segment renders a stray dot. Both are caught
    here, where the name of the override that carries them is still to
    hand. *)
val check : t -> string option

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
