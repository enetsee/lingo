(** The grammar a user writes. *)

(** The namespaces a grammar's names live in. Re-exported so a consumer can
    name the types the records below carry. *)
module Name = Name

(** {1 Types} *)

(** A count of line breaks, one or more. Build one with {!always}, which
    raises outside that range. Zero breaks is [Never]. *)
type lines = private int

(** What the formatter does at one boundary.

    A boundary is the join between two things the formatter writes, and a
    production with [n] children has [n - 1] of them. Each takes its own
    break, so a declaration can hold its header on one line and break before
    its body.

    A single setting for a whole production is what [Fn]'s header used to
    have, and it is why [fn main() -> int \{] came out over six lines: a body
    that always breaks leaves the group around it broken, and every boundary
    in that group then opens together. Say [Never] at the boundaries that are
    a space and the rest is free to break. *)
type break_style =
  | Never (** A space. The boundary never becomes a line break. *)
  | Fit (** A line break where the line does not fit, and a space where it does. *)
  | Always of lines (** That many line breaks, whatever fits. *)

(** How many times a child occurs.

    The two repeating forms carry the break between two elements, because
    that boundary exists only where a child repeats. It is separate from the
    break in front of the child itself: a file whose items are one to a line
    with a blank between them takes [Always (lines 1)] in front of the list
    and [Always (lines 2)] between its elements. *)
type modifier =
  | Exactly_one (** Absence emits a diagnostic and a hole. *)
  | Zero_or_one (** Absence is silent. *)
  | Zero_or_more of break_style
  (** Absence is silent, and the loop ends where no element starts. *)
  | One_or_more of break_style
  (** The first reports the way {!Exactly_one} does, and the rest repeat. *)

(** What a child binds to. The payloads are strings since the data constructor 
    already says which namespace it is in. The checker lifts to either  
    {!Name.Rule.t} or {!Name.Token.t} when it resolves it. *)
type symbol =
  | Token of string
  | Rule of string

val is_token : symbol -> bool
val is_rule : symbol -> bool

(** [always n] is [n] line breaks at a boundary. Raises [Invalid_argument]
    below one, where {!Never} is what is meant. *)
val always : int -> break_style

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
    follows it. The parser cannot separate the two, so the
    grammar is rejected as [first-follow-conflict]. [greedy] says to take the
    child.

    The dangling [else] is the standard case. An [else] after a nested [if]
    could attach to either [if], and greedy attaches it to the nearer one. *)
type child_parse =
  { recover_to : Name.Token.t list option
  ; greedy : bool
  }

(** A named slot in a production. [name] becomes the accessor in the typed
    view, [modifier] says how many nodes the slot holds, and [head] with
    [rest] are the symbols it admits.

    The symbols are a head and the rest rather than a list, so a child that
    admits nothing cannot be written. Several of them let one parent kind hold
    any of a few shapes and keep a single accessor. The alternative is a
    production per shape, which is more kinds to match on. The parser
    dispatches on FIRST sets, taking the first whose set admits the cursor. *)
type child =
  { name : Name.Child.t
  ; c_break : break_style
    (** The boundary in front of this child. On the first child that is the
          boundary against whatever encloses the children: the opener of a
          frame, and the one a child recovery put where no slot admits it
          takes. *)
  ; c_space : bool
    (** Whether a space goes in front of this child, and between two of its
          elements. On the first child there is nothing of this production in
          front of it, and the value is unread. *)
  ; head : symbol
  ; rest : symbol list
  ; modifier : modifier
  ; c_parse : child_parse
  }

(** Where an infix operator or a separator goes when what holds it breaks
    across lines. *)
type operator_position =
  | Op_before (** At the start of the next line. *)
  | Op_after (** At the end of the previous line. *)

(** What the formatter does with a separator the grammar allows at one end of a
    body: in front of the first element, or after the last.

    The parser takes one at either end whatever the policy, because it is bytes
    the source had. The policy says whether it belongs there.

    - [Never]: it does not belong. The parser reports it, and the formatter
      writes none.
    - [On_break]: the formatter writes one where the body breaks across lines,
      and none where it lies flat.
    - [Always]: the formatter writes one whether the body breaks or not. *)
type optional_sep =
  | Never
  | On_break
  | Always

(** The separator in a delimited body or an enclosed postfix. Carries the
    separator and its two end policies together, so a policy always has a
    separator to apply to. *)
type sep_policy =
  | No_sep
  | With_sep of
      { sep : Name.Token.t
      ; leading : optional_sep (** In front of the first element. *)
      ; trailing : optional_sep (** After the last element. *)
      ; position : operator_position
        (** Where it goes when the body breaks: [Op_after] ends a line with
              it, as [a,] does, and [Op_before] starts the next with it, as
              [| B] does. *)
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

    - [Plain]: a sequence of children. What a failure means is the caller's.
    - [Committed]: errors stay inside. [boundary] sets what descendants
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
      ; pad : bool
        (** Whether a space goes just inside each delimiter of a body with
              something in it: [{ a }] rather than [{a}]. *)
      ; boundary : bool
      }
  | Separated of
      { sep : Name.Token.t
      ; leading : optional_sep
      ; trailing : optional_sep
      ; position : operator_position
      ; boundary : bool
      }

(** A record with one field. A later recovery setting can then be added
    without every production literal having to change. *)
type recovery_spec = { strategy : recovery_strategy }

(** Layout hints for a production. Set through {!prod}. The breaks are on
    the children, because a break is a property of one boundary. *)
type production_format = { indent_width : int }

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
  ; binders : Name.Child.Set.t
    (** The children whose text introduces a name. See {!with_binder}. *)
  ; opens_scope : bool
    (** Whether a name introduced inside this production belongs to it. See
          {!with_scope}. *)
  ; framing : framing
  ; recovery : recovery_spec
  ; error_messages : string Name.Child.Map.t
  ; format : production_format
  ; has_hole : bool (** Whether a paired [<KIND>_HOLE] kind is emitted. *)
  ; resync_anchors : Name.Token.Set.t
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
  ; space : bool
    (** Whether a space goes between the operand and the lead token. Every
          builder defaults it to [false]. *)
  ; pad : bool
    (** Whether a space goes just inside the pair of an enclosed body, as a
          delimited production's [pad] does. Every builder defaults it to
          [false]. *)
  }

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

(** [lexer] drives the lexer automaton and, by default, both backends'
    emission.

    A term neither backend's dialect has, such as a complement or an
    intersection over anything but character classes, is written out by hand:
    [textmate] carries the Oniguruma spelling and [treesitter] the JavaScript
    one. The two dialects fail on the same terms, so a token that needs one
    needs the other, and a token that sets only [textmate] is one the
    tree-sitter backend rejects. *)
type pattern_spec =
  { lexer : Redfa.Regex.t
  ; textmate : string option
  ; treesitter : string option
  }

(** How a token's bytes are matched. *)
type token_class =
  | Keyword of string
  | Punctuation of string
  | Pattern of pattern_spec

(** What one side of a token does to the space there.

    The production holding a boundary sets whether a space goes there, and a
    token's side outranks it. Both are for how a language writes the token
    everywhere it appears. Where the two tokens of a boundary disagree, [Hug]
    wins. *)
type side =
  | Hug (** Never a space: [,] and [;] before, [(] after, [.] on both. *)
  | Free (** The production holding the boundary decides. *)
  | Space (** Always a space: rust's [{] before. *)

(** Both sides default to [Free]. *)
type token_format =
  { space_before : side
  ; space_after : side
  }

(** A token.

    [trivia = None] is a token that productions reference by name. The
    parser skips the other two.

    - [Some Reformat]: the formatter re-emits the spacing.
    - [Some Preserve]: the formatter keeps the matched text as it stands.
      This is what a comment takes.

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

(** The two tokens a template writes where a child goes, such as [$x] and
    [$$xs]. [single] stands for one child. [sequence] stands for a run of the
    elements of a repeated child, from none upwards.

    They are not tokens of the language. The language's lexer leaves them out,
    and only the template lexer reads them. Neither may match or start any
    string another token matches, so a template lexes the way the same text
    with real children in it would. *)
type metavariables =
  { single : token_def
  ; sequence : token_def
  }

(** A grammar. [roots] are the entry productions. The list must be
    non-empty, must hold no duplicates, and every name in it must be a
    production. A grammar with no [metavariables] has no templates. *)
type t =
  { productions : production list
  ; expr : expr_def list
  ; tokens : token_def list
  ; roots : Name.Rule.t list
  ; metavariables : metavariables option
  }

(** [?expr] defaults to the empty list. *)
val create
  :  ?expr:expr_def list
  -> ?metavariables:metavariables
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
    [with_] functions take a plain [~sep] instead. [leading] and [trailing]
    default to [Never], and [position] to [Op_after]. *)
val with_sep
  :  ?leading:optional_sep
  -> ?trailing:optional_sep
  -> ?position:operator_position
  -> string
  -> sep_policy

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
    v}

    A postfix operator is written against its operand, so [space] defaults to
    [false]. A lead token whose side is [Space] still takes one, which is how
    effekt's trailing block comes out as [f(x) { s; }]. *)

val postfix_simple
  :  ?kind_suffix:string
  -> ?space:bool
  -> token:string
  -> bp:int
  -> unit
  -> postfix_op

val postfix_access
  :  ?kind_suffix:string
  -> ?space:bool
  -> token:string
  -> rhs:symbol
  -> bp:int
  -> unit
  -> postfix_op

val postfix_index
  :  ?kind_suffix:string
  -> ?space:bool
  -> ?pad:bool
  -> open_tok:string
  -> close_tok:string
  -> index:symbol
  -> bp:int
  -> unit
  -> postfix_op

val postfix_brace
  :  ?kind_suffix:string
  -> ?space:bool
  -> ?pad:bool
  -> open_tok:string
  -> close_tok:string
  -> body:symbol
  -> bp:int
  -> unit
  -> postfix_op

(** [sep_policy] defaults to [No_sep]. *)
val postfix_call
  :  ?kind_suffix:string
  -> ?space:bool
  -> ?pad:bool
  -> open_tok:string
  -> close_tok:string
  -> elem:symbol
  -> ?sep_policy:sep_policy
  -> bp:int
  -> unit
  -> postfix_op

(** {1 Children} *)

(** [break] is the boundary in front of the child and defaults to {!Fit}.
    [between], on the repeating builders, is the boundary between two of its
    elements and defaults to the same.

    [space] is whether a space goes in front of the child and between its
    elements, and defaults to [true]. Two children of a production are
    separated by a space unless the grammar says otherwise. effekt's [def f(x)]
    has a name followed by a parameter list, and gives the list
    [~space:false]. *)
val child
  :  ?recover_to:string list
  -> ?greedy:bool
  -> ?break:break_style
  -> ?space:bool
  -> modifier:modifier
  -> string
  -> symbol
  -> child

val child_req
  :  ?recover_to:string list
  -> ?break:break_style
  -> ?space:bool
  -> string
  -> symbol
  -> child

val child_opt
  :  ?recover_to:string list
  -> ?greedy:bool
  -> ?break:break_style
  -> ?space:bool
  -> string
  -> symbol
  -> child

val child_rep
  :  ?recover_to:string list
  -> ?greedy:bool
  -> ?break:break_style
  -> ?space:bool
  -> ?between:break_style
  -> string
  -> symbol
  -> child

(** One or more, where [child_rep] is zero or more. A list with nothing
    around it usually takes this: an empty one is not syntax anybody wrote. *)
val child_rep1
  :  ?recover_to:string list
  -> ?greedy:bool
  -> ?break:break_style
  -> ?space:bool
  -> ?between:break_style
  -> string
  -> symbol
  -> child

(** Raises [Invalid_argument] on an empty list. A child admitting no symbol
    is not a shape a grammar can mean. *)
val child_alt
  :  ?recover_to:string list
  -> ?break:break_style
  -> ?space:bool
  -> modifier:modifier
  -> string
  -> symbol list
  -> child

(** {!child_alt} where every alternative is a rule. *)
val child_alt_rules
  :  ?recover_to:string list
  -> ?break:break_style
  -> ?space:bool
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
    double as an identifier. Both sides default to [Free]. *)
val punct
  :  ?space_before:side
  -> ?space_after:side
  -> ?trivia:trivia_class
  -> name:string
  -> string
  -> token_def

(** {!punct} with no space on either side, wherever it appears. For [.] and [::]. *)
val punct_tight : ?trivia:trivia_class -> name:string -> string -> token_def

(** A token matched by a regex. *)
val pat
  :  ?textmate:string
  -> ?treesitter:string
  -> ?trivia:trivia_class
  -> string
  -> Redfa.Regex.t
  -> token_def

(** {1 Productions} *)

val prod : ?indent_width:int -> string -> child list -> production

(** Gives named children their own catalogue entries. Without one, a child
    gets the default "expected X". *)
val with_messages : (string * string) list -> production -> production

(** Wraps the body in a matched pair.

    A delimited production takes a different parser shape from a plain
    sequence of children. The body loop runs until it reaches the close
    token, and never consults element FIRST sets. The recovery set inside
    the body gains the closer, so a broken child resumes at the bracket.

    [pad] puts a space just inside each delimiter when the body has something
    in it and lies on one line, and defaults to [false]. *)
val with_delimited
  :  open_tok:string
  -> close_tok:string
  -> ?pad:bool
  -> ?boundary:bool
  -> production
  -> production

(** {!with_delimited} with a separator between elements. [leading_sep] and
    [trailing_sep] default to [Never] and [sep_position] to [Op_after]; see
    {!type-sep_policy}. *)
val with_delimited_sep
  :  open_tok:string
  -> close_tok:string
  -> sep:string
  -> ?leading_sep:optional_sep
  -> ?trailing_sep:optional_sep
  -> ?sep_position:operator_position
  -> ?pad:bool
  -> ?boundary:bool
  -> production
  -> production

(** A [sep]-separated list with nothing around it. The child's modifier says
    whether it can be empty, and {!child_rep1} is usually what one of these
    takes: an empty list with nothing around it is no syntax at all. The
    separator arguments are {!with_delimited_sep}'s. *)
val with_separator
  :  sep:string
  -> ?leading_sep:optional_sep
  -> ?trailing_sep:optional_sep
  -> ?sep_position:operator_position
  -> ?boundary:bool
  -> production
  -> production

val with_committed : ?boundary:bool -> production -> production

(** A name introduced inside this production belongs to it, and is out of use
    after it ends. A block is one of these. So is a function whose parameters
    are its own.

    Nothing derives this. The grammar accepts the same language either way,
    and only an editor reads it. *)
val with_scope : production -> production

val with_identity : string -> production -> production

(** Names a child whose text introduces a name. The [x] in [let x = 1] is one.

    An editor reads binders to take a reader from a use of a name to where it
    was introduced, to rename one, and to grey out one that is shadowed. A
    production may name more than one child, and each call adds one.

    The child has to hold a single pattern token, because the child's own text
    is the name. A child holding a rule spans a whole subtree, and a child
    holding a keyword or a punctuation literal has the same text wherever it
    appears. Both are rejected as [binder-not-pattern-token].

    {!with_identity} is close to this and says something else. It names the
    child a diagnostic calls the production by. In [grammars/rust_grammar.ml]
    every identity child is also a binder, and [Let.name] is a binder that is
    not the identity. *)
val with_binder : string -> production -> production

val with_no_hole : production -> production

(** Ends this production's body loop when the cursor reaches any of [toks].
    The closer and end of input still end the loop; these are extra exits. An
    empty list is the default, and does nothing.

    It stops a broken body from swallowing what belongs to an outer scope.
    Take a production holding a function body. Declaring
    [with_resync_to \[ "def" \]] means a malformed expression inside it
    leaves alone the [def] that starts the next declaration, so one
    diagnostic stays one.

    Pick a token no element of the body starts with. An anchor an element
    could start with ends the body wherever one could begin, so the body never
    takes one, and that is rejected as [resync-anchor-conflict].

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
