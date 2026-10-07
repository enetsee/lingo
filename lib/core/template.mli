(** The grammar a template is parsed with.

    A template is text in the language's own syntax with metavariables where
    children go, such as [$x + 0]. It is parsed with a grammar derived from
    the language's.

    There are three kinds of metavariable. The plain one, [$x], stands for a
    token, or for an expression. A typed one, such as [$x:Member], stands for
    a whole node of that rule. The sequence one, [$$xs], stands for a run of
    the elements of a repeated child.

    - A child that can be a pattern token also takes the plain
      metavariable. A token with fixed text is the grammar's own structure,
      as an operator is, and no metavariable stands for one.
    - A child that names a rule also takes that rule's typed one. Inside the
      rule the plain one fills the first child that takes it, so [$x: 1] is
      a member whose key is [$x], and [$x:Member] is a whole member.
    - A repeated child also takes the sequence metavariable, as its first
      alternative, so a run is read as a run where the element's own rule
      could begin with the same token.
    - An expression block takes the plain metavariable, the sequence one and
      its own typed one as atoms. The sequence one is how [f($$xs)] reads,
      since a postfix body names one symbol and takes no alternatives.
    - A metavariable fills the first child that takes it. An optional child
      that can begin with it takes it, and the child after it does not.
    - Every rule, production or block, gets a root of its own that holds one
      of it, so a template can be a fragment. The rules themselves are not
      roots, since a root takes whatever input is left.

    The metavariables are the derived grammar's last tokens. Its kinds come
    out in another order from the language's, so a tree parsed with it is
    relabelled by name before it is compared with the language's trees. *)

(** The derived grammar, or [None] for a grammar with no metavariables. *)
val grammar : Grammar.t -> Grammar.t option

(** [entry g rule] is the root the derived grammar has for the production
    or block named [rule]. It holds one of it. *)
val entry : Grammar.t -> string -> string

(** [typed_name m rule] is the name of the token the derived grammar gives
    [rule]'s typed metavariable. *)
val typed_name : Grammar.metavariables -> string -> string
