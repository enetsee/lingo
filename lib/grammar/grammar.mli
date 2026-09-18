(** The grammar a user writes. *)

(** The namespaces a grammar's names live in. Re-exported so a consumer can
    name the types the records below carry. *)
module Name = Name

(** {1 Types} *)

(** How many times a child occurs. *)
type modifier =
  | Required (** Exactly one. Absence emits a diagnostic and a hole. *)
  | Optional (** Zero or one. Absence is silent. *)
  | Repeated (** Zero or more. *)

(** What a child binds to. The payloads are strings since the data constructor 
    already says which namespace it is in. The checker lifts to either  
    {!Name.Rule.t} or {!Name.Token.t} when it resolves it. *)
type symbol =
  | Token of string
  | Rule of string

val is_token : symbol -> bool
val is_rule : symbol -> bool

(** A child's right-hand side. [Alternatives] lets one parent kind hold any of
    several shapes and keep a single accessor. The alternative is a production 
    per shape, which is more kinds to match on. The parser dispatches on FIRST 
    sets. *)
type child_sym =
  | Single of symbol
  | Alternatives of symbol list

(** Parser overrides for one child.

    {2 [recover_to]}

    When the parser fails partway through a production it skips forward until
    it reaches a token it can start again on. [recover_to] lists those tokens 
    for this child position, in place of the ones lingo works out.

    Left alone, the computed list holds whatever could legally come next:

    - the tokens that can start the children after this one;
    - this production's own closing delimiter and separator, if it has them;
    - the tokens that can follow the production itself, but only when
      nothing after this child is required.

    Set [recover_to] when that leaves the parser resuming somewhere
    unhelpful. A statement inside a block is the usual case. Naming the
    statement terminator stops a broken statement at the next [;], instead
    of running on to whatever follows the block.

    Either way, the closing delimiters of the enclosing productions go on
    top. Skipping one of those leaves the outer production waiting for a
    delimiter that has already been consumed, and one error becomes two.

    {2 [greedy]}

    An optional or repeated child can start with the same token as whatever
    follows it. The parser cannot tell which one it is looking at, so the
    grammar is rejected as [first-follow-conflict]. [greedy] says to take the
    child.

    The dangling [else] is the standard case. An [else] after a nested [if]
    could attach to either [if], and greedy attaches it to the nearer one. *)
type child_parse =
  { recover_to : Name.Token.t list option
  ; greedy : bool
  }

(** A named slot in a production. [name] becomes the accessor in the typed
    view. *)
type child =
  { name : Name.Child.t
  ; sym : child_sym
  ; modifier : modifier
  ; c_parse : child_parse
  }

(** How the formatter lays out a production's children. *)
type break_style =
  | Fit (** One line where it fits, broken where it doesn't. *)
  | Always (** Broken, with a hard line between children. *)
  | Never (** One line even where it doesn't fit. *)

(** What happens to a separator after the last element.

    - [Never]: the parser rejects one and the formatter emits none.
    - [On_break]: the parser accepts one; the formatter strips whatever the
      source had and emits one when the body breaks across lines.
    - [Always]: the parser accepts one; the formatter strips whatever the
      source had and emits one. *)
type trailing_sep =
  | Never
  | On_break
  | Always

(** The separator in a delimited body or an enclosed postfix. Carries the
    separator and its trailing policy together, so a trailing policy always
    has a separator to apply to. *)
type sep_policy =
  | No_sep
  | With_sep of
      { sep : Name.Token.t
      ; trailing : trailing_sep
      }

(** A recovery lookahead count in [1, max_recovery_lookahead]. Built with 
    {!val-lookahead_n}, which raises outside that range. *)
type lookahead_n = private int

(** How a production recovers. *)
type recovery_strategy =
  | Insert_only (** Insert the missing token and carry on. *)
  | Lookahead of lookahead_n
  (** Also peek up to [n] tokens ahead for a place to resume. *)

(** How a production's body is bracketed, and what recovery inside it does.

    A committed production contains its own errors. A missing required child
    emits a hole, and the parse resumes at the local recovery set.
    [Delimited] is committed for the same reason: the opener has already been
    consumed, so recovery resumes at the closer.

    - [Plain]: a sequence of children. The caller decides what a failure
      means.
    - [Committed]: errors stay inside. [boundary] decides what descendants
      recover on. With [false] they inherit the caller's recovery set. With
      [true] they start from empty, which stops a child resuming past this
      production's edge.
    - [Delimited]: a matched pair around the body, with an optional
      separator.
    - [Separated]: a [sep]-separated list with nothing around it. This models
      a brace-less comma list. *)
type framing =
  | Plain
  | Committed of { boundary : bool }
  | Delimited of
      { open_tok : Name.Token.t
      ; close_tok : Name.Token.t
      ; sep_policy : sep_policy
      ; boundary : bool
      }
  | Separated of
      { sep : Name.Token.t
      ; trailing : trailing_sep
      ; boundary : bool
      }

(** A record with one field. A later recovery setting can then be added
    without every production literal having to change. *)
type recovery_spec = { strategy : recovery_strategy }

(** Layout hints for a production. Set through {!prod}. *)
type production_format =
  { break_style : break_style
  ; indent_width : int
  ; separator_lines : int
  }

(** A production.

    Each pair in [error_messages] names a child, and the catalogue entry to
    use for that child in place of the default.

    [resync_anchors] adds to the tokens that end this production's body loop
    early. See {!with_resync_to}. *)
type production =
  { kind_name : Name.Rule.t
  ; children : child list
  ; identity_child : Name.Child.t option
    (** The child whose text names the production in a diagnostic. *)
  ; framing : framing
  ; recovery : recovery_spec
  ; error_messages : (Name.Child.t * string) list
  ; format : production_format
  ; has_hole : bool (** Whether a paired [<KIND>_HOLE] kind is emitted. *)
  ; edge_space_before : bool option
    (** Overrides the leading spacing flag. Left alone, that flag comes from
          the production's first token. *)
  ; edge_space_after : bool option (** The same on the trailing edge. *)
  ; resync_anchors : Name.Token.t list
  }

type assoc =
  | Left
  | Right

(** A prefix or infix operator. [bp] is the binding power, and higher binds
    tighter.

    Associativity picks the pair of powers the parser climbs with. [Left]
    gives [(bp, bp + 1)] and [Right] gives [(bp, bp)]. *)
type operator =
  { op_token : Name.Token.t
  ; bp : int
  ; op_assoc : assoc
  }

(** What a postfix operator takes after its lead token.

    - [Nothing] for [x?].
    - [Then] for [x.f], where one symbol follows the lead.
    - [Enclosed] where the lead opens a matched pair, as in [x\[i\]],
      [x(a, b)] and [x { b }].

    An [Enclosed] body has the same open, close and separator that a
    production's [Delimited] framing has. It lowers to the same loop and the
    same layout frame. *)
type postfix_body =
  | Nothing
  | Then of symbol
  | Enclosed of
      { close : Name.Token.t
      ; content : content
      }

(** What sits inside an [Enclosed] postfix. Use [One] for an index or a
    brace body, and [Many] for an argument list. *)
and content =
  | One of symbol
  | Many of
      { elem : symbol
      ; sep : sep_policy
      }

(** A postfix operator.

    [kind_suffix] keeps the generated identifiers apart. A block with one
    postfix operator can leave it empty. A block with more than one has to
    give each operator its own non-empty suffix.

    A non-empty suffix has to be an identifier. It is spliced into a kind
    constructor, a view module and a formatter binding. It is checked as
    written, before any of those three mangle it. Every other name in a
    grammar obeys the same rule, and a breach is [invalid-name]. *)
and postfix_op =
  { bp : int
  ; lead : Name.Token.t (** The token that opens it. *)
  ; body : postfix_body
  ; kind_suffix : string
  }

(** Where the operator goes when an infix expression breaks across lines. *)
type operator_position =
  | Op_before (** At the start of the next line. *)
  | Op_after (** At the end of the previous line. *)

type expr_format =
  { operator_position : operator_position
  ; continuation_indent : int
  }

(** An expression block. It holds atoms and an operator table, and is parsed
    by precedence climbing. It is desugared once, at the [Facts] boundary.

    Productions reference the block by [rule_name]. That name also
    namespaces every identifier generated for the block. *)
type expr_def =
  { rule_name : Name.Rule.t
  ; atoms : symbol list
  ; prefix_ops : operator list
  ; infix_ops : operator list
  ; postfix : postfix_op list
  ; infix_recovery : bool
  ; e_format : expr_format
  }

(** What the formatter does with a trivia token. *)
type trivia_class =
  | Reformat (** Drop it and re-emit the spacing. *)
  | Preserve (** Keep the matched text as it stands, for a comment. *)

(** [lexer] drives the lexer automaton and, by default, TextMate emission.
    [textmate] supplies Oniguruma source for the features that need it. *)
type pattern_spec =
  { lexer : Redfa.Regex.t
  ; textmate : string option
  }

(** How a token's bytes are matched. *)
type token_class =
  | Keyword of string
  | Punctuation of string
  | Pattern of pattern_spec

(** Spacing the formatter puts around a token. Both sides default to [true].
    Punctuation usually wants [false] on both. *)
type token_format =
  { space_before : bool
  ; space_after : bool
  }

(** A token.

    [trivia = None] is a token that productions reference by name. The
    parser skips the other two.

    - [Some Reformat]: the formatter re-emits the spacing.
    - [Some Preserve]: the formatter keeps the matched text as it stands.
      This is what a comment wants.

    The tree records trivia either way. That is how it stays lossless while
    productions say nothing about whitespace.

    A token is a lexeme, and line termination belongs to the formatter. Weld
    a newline into a comment token and the layout engine's column model comes
    out a line short, on the path recovery takes. *)
type token_def =
  { token_name : Name.Token.t
  ; token_class : token_class
  ; t_format : token_format
  ; trivia : trivia_class option
  }

(** A grammar. [roots] are the entry productions. The list must be
    non-empty, must hold no duplicates, and every name in it must be a
    production. *)
type t =
  { productions : production list
  ; expr : expr_def list
  ; tokens : token_def list
  ; roots : Name.Rule.t list
  }

(** [?expr] defaults to the empty list. *)
val create
  :  ?expr:expr_def list
  -> tokens:token_def list
  -> roots:string list
  -> production list
  -> t

(** {1 Expression blocks} *)

(** [assoc] defaults to [Left]. *)
val infix : ?assoc:assoc -> token:string -> bp:int -> unit -> operator

(** [assoc] defaults to [Right]. *)
val prefix : ?assoc:assoc -> token:string -> bp:int -> unit -> operator

(** A separator for a delimited body or an enclosed postfix. Use it when you
    are building a {!type-sep_policy} yourself, as {!postfix_call} needs. The
    [with_] functions take a plain [~sep] instead. [trailing] defaults to
    [Never]. *)
val with_sep : ?trailing:trailing_sep -> string -> sep_policy

val expr_block
  :  rule_name:string
  -> atoms:symbol list
  -> ?prefix_ops:operator list
  -> ?infix_ops:operator list
  -> ?postfix:postfix_op list
  -> ?infix_recovery:bool
  -> ?operator_position:operator_position
  -> ?continuation_indent:int
  -> unit
  -> expr_def

(** {2 Postfix operators}

    Five names for the five familiar forms. Each builds one
    {!type-postfix_op}.

    {v
      x?        postfix_simple   lead = token,     body = Nothing
      x.f       postfix_access   lead = token,     body = Then rhs
      x[i]      postfix_index    lead = open_tok,  body = Enclosed (One …)
      x { b }   postfix_brace    lead = open_tok,  body = Enclosed (One …)
      x(a, b)   postfix_call     lead = open_tok,  body = Enclosed (Many …)
    v} *)

val postfix_simple : ?kind_suffix:string -> token:string -> bp:int -> unit -> postfix_op

val postfix_access
  :  ?kind_suffix:string
  -> token:string
  -> rhs:symbol
  -> bp:int
  -> unit
  -> postfix_op

val postfix_index
  :  ?kind_suffix:string
  -> open_tok:string
  -> close_tok:string
  -> index:symbol
  -> bp:int
  -> unit
  -> postfix_op

val postfix_brace
  :  ?kind_suffix:string
  -> open_tok:string
  -> close_tok:string
  -> body:symbol
  -> bp:int
  -> unit
  -> postfix_op

(** [sep_policy] defaults to [No_sep]. *)
val postfix_call
  :  ?kind_suffix:string
  -> open_tok:string
  -> close_tok:string
  -> elem:symbol
  -> ?sep_policy:sep_policy
  -> bp:int
  -> unit
  -> postfix_op

(** {1 Children} *)

val child
  :  ?recover_to:string list
  -> ?greedy:bool
  -> modifier:modifier
  -> string
  -> symbol
  -> child

val child_req : ?recover_to:string list -> string -> symbol -> child
val child_opt : ?recover_to:string list -> ?greedy:bool -> string -> symbol -> child
val child_rep : ?recover_to:string list -> ?greedy:bool -> string -> symbol -> child

val child_alt
  :  ?recover_to:string list
  -> modifier:modifier
  -> string
  -> symbol list
  -> child

(** {!child_alt} where every alternative is a rule. *)
val child_alt_rules
  :  ?recover_to:string list
  -> modifier:modifier
  -> string
  -> string list
  -> child

(** {1 Tokens} *)

(** A reserved word. The positional argument is the literal and, by default,
    the token name.

    Pass [~name] where the literal is not an identifier. [kw "foo-bar"] is
    rejected as [invalid-name]. [kw ~name:"foo_bar" "foo-bar"] is the same
    token under a name that can reach a kind constructor and a parser
    binding. *)
val kw : ?name:string -> ?trivia:trivia_class -> string -> token_def

(** Punctuation. [~name] is required, since a literal such as ["("] cannot
    double as an identifier. Spacing defaults to [true] on both sides. *)
val punct
  :  ?space_before:bool
  -> ?space_after:bool
  -> ?trivia:trivia_class
  -> name:string
  -> string
  -> token_def

(** {!punct} with no spacing on either side. For commas, brackets, dots. *)
val punct_tight : ?trivia:trivia_class -> name:string -> string -> token_def

(** A token matched by a regex. *)
val pat : ?textmate:string -> ?trivia:trivia_class -> string -> Redfa.Regex.t -> token_def

(** {1 Productions} *)

val prod
  :  ?break_style:break_style
  -> ?indent_width:int
  -> ?separator_lines:int
  -> string
  -> child list
  -> production

(** Gives named children their own catalogue entries. Without one, a child
    gets the default "expected X". *)
val with_messages : (string * string) list -> production -> production

(** Wraps the body in a matched pair.

    A delimited production takes a different parser shape from a plain
    sequence of children. The body loop runs until it reaches the close
    token, and never consults element FIRST sets. The recovery set inside
    the body gains the closer, so a broken child resumes at the bracket. *)
val with_delimited
  :  open_tok:string
  -> close_tok:string
  -> ?boundary:bool
  -> production
  -> production

(** {!with_delimited} with a separator between elements. *)
val with_delimited_sep
  :  open_tok:string
  -> close_tok:string
  -> sep:string
  -> ?trailing_sep:trailing_sep
  -> ?boundary:bool
  -> production
  -> production

(** A [sep]-separated list with nothing around it. The first element is
    required, whatever modifier the child slot carries. To allow an empty
    list, put this production behind an optional child in its parent. *)
val with_separator
  :  sep:string
  -> ?trailing_sep:trailing_sep
  -> ?boundary:bool
  -> production
  -> production

val with_committed : ?boundary:bool -> production -> production
val with_identity : string -> production -> production
val with_no_hole : production -> production
val with_leading_space : bool -> production -> production
val with_trailing_space : bool -> production -> production

(** Ends this production's body loop when the cursor reaches any of [toks].
    The closer and end of input still end the loop; these are extra exits. An
    empty list is the default, and does nothing.

    It stops a broken body from swallowing what belongs to an outer scope.
    Take a production holding a function body. Declaring
    [with_resync_to \[ "def" \]] means a malformed expression inside it
    leaves alone the [def] that starts the next declaration, so one
    diagnostic stays one.

    Only a repeated child inside a matched pair gives a loop for an anchor to
    end. A production that repeats nothing has none, and a separated list has
    already ended wherever an anchor could sit, because its loop runs while the
    cursor is on the separator. Anchors on either are rejected as
    [unused-resync-anchors]. *)
val with_resync_to : string list -> production -> production

val with_recovery_strategy : recovery_strategy -> production -> production
val max_recovery_lookahead : int

(** Raises [Invalid_argument] outside [1, max_recovery_lookahead]. *)
val lookahead_n : int -> lookahead_n
