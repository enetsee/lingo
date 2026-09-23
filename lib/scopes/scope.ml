open StdLabels

type section =
  | Parens
  | Brackets
  | Braces

type t =
  | Keyword_control of string option
  | Keyword_other of string option
  | Keyword_operator_arithmetic
  | Keyword_operator_comparison
  | Keyword_operator_logical
  | Keyword_operator_assignment
  | Keyword_operator_bitwise
  | Keyword_operator_other of string
  | Storage_type of string option
  | Storage_modifier
  | Entity_name_function
  | Entity_name_type_struct
  | Entity_name_type_enum
  | Entity_name_type_trait
  | Entity_name_type_class
  | Entity_name_type of string option
  | Variable_parameter
  | Variable_language
  | Variable_other
  | Variable_other_member
  | String_quoted_single
  | String_quoted_double
  | String_quoted_triple
  | String_quoted_other of string
  | Comment_line_slashes
  | Comment_line_hash
  | Comment_line_other of string
  | Comment_block
  | Constant_numeric_integer
  | Constant_numeric_float
  | Constant_numeric_hex
  | Constant_numeric_other
  | Constant_language
  | Punctuation_separator
  | Punctuation_accessor
  | Punctuation_definition of string
  | Punctuation_section of section
  | Punctuation_section_begin of section
  | Punctuation_section_end of section
  | Meta of string
  | Custom of string

let section_string (section : section) : string =
  match section with
  | Parens -> "parens"
  | Brackets -> "brackets"
  | Braces -> "braces"
;;

(* The path without the language. Every arm is a literal, so a standard
   scope has one spelling, and two scopes compare by their paths. *)
let bare (t : t) : string =
  match t with
  | Keyword_control None -> "keyword.control"
  | Keyword_control (Some leaf) -> "keyword.control." ^ leaf
  | Keyword_other None -> "keyword.other"
  | Keyword_other (Some leaf) -> "keyword.other." ^ leaf
  | Keyword_operator_arithmetic -> "keyword.operator.arithmetic"
  | Keyword_operator_comparison -> "keyword.operator.comparison"
  | Keyword_operator_logical -> "keyword.operator.logical"
  | Keyword_operator_assignment -> "keyword.operator.assignment"
  | Keyword_operator_bitwise -> "keyword.operator.bitwise"
  | Keyword_operator_other leaf -> "keyword.operator." ^ leaf
  | Storage_type None -> "storage.type"
  | Storage_type (Some leaf) -> "storage.type." ^ leaf
  | Storage_modifier -> "storage.modifier"
  | Entity_name_function -> "entity.name.function"
  | Entity_name_type_struct -> "entity.name.type.struct"
  | Entity_name_type_enum -> "entity.name.type.enum"
  | Entity_name_type_trait -> "entity.name.type.trait"
  | Entity_name_type_class -> "entity.name.type.class"
  | Entity_name_type None -> "entity.name.type"
  | Entity_name_type (Some leaf) -> "entity.name.type." ^ leaf
  | Variable_parameter -> "variable.parameter"
  | Variable_language -> "variable.language"
  | Variable_other -> "variable.other"
  | Variable_other_member -> "variable.other.member"
  | String_quoted_single -> "string.quoted.single"
  | String_quoted_double -> "string.quoted.double"
  | String_quoted_triple -> "string.quoted.triple"
  | String_quoted_other leaf -> "string.quoted." ^ leaf
  | Comment_line_slashes -> "comment.line.double-slash"
  | Comment_line_hash -> "comment.line.number-sign"
  | Comment_line_other leaf -> "comment.line." ^ leaf
  | Comment_block -> "comment.block"
  | Constant_numeric_integer -> "constant.numeric.integer"
  | Constant_numeric_float -> "constant.numeric.float"
  | Constant_numeric_hex -> "constant.numeric.hex"
  | Constant_numeric_other -> "constant.numeric"
  | Constant_language -> "constant.language"
  | Punctuation_separator -> "punctuation.separator"
  | Punctuation_accessor -> "punctuation.accessor"
  | Punctuation_definition leaf -> "punctuation.definition." ^ leaf
  | Punctuation_section section -> "punctuation.section." ^ section_string section
  | Punctuation_section_begin section ->
    "punctuation.section." ^ section_string section ^ ".begin"
  | Punctuation_section_end section ->
    "punctuation.section." ^ section_string section ^ ".end"
  | Meta leaf -> "meta." ^ leaf
  | Custom path -> path
;;

let to_string ?language (t : t) : string =
  match language with
  | None -> bare t
  | Some language -> bare t ^ "." ^ language
;;

(* The segment a constructor carries. Only these come from the caller. The
   rest of the path is a literal above. *)
let payload (t : t) : string option =
  match t with
  | Keyword_control (Some leaf)
  | Keyword_other (Some leaf)
  | Keyword_operator_other leaf
  | Storage_type (Some leaf)
  | Entity_name_type (Some leaf)
  | String_quoted_other leaf
  | Comment_line_other leaf
  | Punctuation_definition leaf
  | Meta leaf
  | Custom leaf -> Some leaf
  | _ -> None
;;

let valid_char (c : char) : bool =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
  | _ -> false
;;

let check (t : t) : string option =
  match payload t with
  | None -> None
  | Some "" -> Some "the segment is empty, which renders a stray dot"
  | Some segment ->
    let bad = ref None in
    String.iter segment ~f:(fun c ->
      if !bad = None && not (valid_char c)
      then
        bad
        := Some
             (Printf.sprintf
                "the segment %S holds %C, and a scope path holds [A-Za-z0-9._-] alone"
                segment
                c));
    !bad
;;

(* Two scopes are equal when their paths are. [Custom "meta.x"] and
   [Meta "x"] name one scope, and a check that drops a redundant emission
   has to treat them as one. *)
let equal (a : t) (b : t) : bool = String.equal (bare a) (bare b)
let compare (a : t) (b : t) : int = String.compare (bare a) (bare b)
let pp (fmt : Format.formatter) (t : t) : unit = Format.pp_print_string fmt (bare t)
