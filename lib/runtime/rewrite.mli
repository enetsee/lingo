(** Strategic rewriting over siesta trees.

    A rule takes a cursor and gives a green node, or a reason it failed. The
    combinators build rules from rules, and the traversals move a rule through
    a tree. They are Stratego's, and nothing here reads a grammar. The
    congruences and constructors that do are generated per grammar.

    {1 How a rewrite moves through a tree}

    A rule reads a red cursor, so it can look at parents, siblings and byte
    ranges. It gives a green node, which has no position. A node with the
    input's tag means nothing changed, and comparing tags is O(1).

    {!all}, {!one} and {!some} run their rule on each child's cursor in the
    current tree, so every rule sees its real ancestors. They rebuild the
    parent once, and only when some child's tag changed. A node left alone
    comes back as the same green node.

    {!seq} runs its second rule on a cursor over what the first one gave.
    Where the first changed the node, {!Siesta.Syntax.replace} puts the result
    in the tree, which rebuilds the spine up to the root. Where it changed
    nothing, the cursor is reused. So a change costs the depth of the tree,
    and a node left alone costs nothing.

    {1 Failure}

    A failed rule gives a reason. The reason separates a rule that had
    nothing to do from one that is broken. By convention it is a constant
    string, so a {!choice} that moves on to its second rule allocates
    nothing.

    A combinator that fails because a rule failed gives that rule's reason.
    Where it tried several, it gives the last one's. *)

(** What every rule in one rewrite shares. ['s] is the language's own
    semantics, such as its scopes and types. Nothing here reads it. *)
module Ctx : sig
  type 's t

  (** [create cache root semantics].

      [cache] must be the one the parse used. A node a rule rebuilds from
      unchanged children is then the node the parse built, with its tag. With
      any other cache it is a fresh record, and the rewrite reports a change
      where there was none. *)
  val create : Siesta.Cache.t -> Siesta.Syntax.t -> 's -> 's t

  val cache : 's t -> Siesta.Cache.t

  (** The tree the rewrite started from, which is the tree the semantics
      describe. After an edit, a rule's cursor is in a new tree.
      {!Siesta.Syntax.same_tree} with [root] is false for it, and a query
      about it has to fail. *)
  val root : 's t -> Siesta.Syntax.t

  val semantics : 's t -> 's
end

(** A rule. Giving back the cursor's own green node is success with no
    change. *)
type 's t = 's Ctx.t -> Siesta.Syntax.t -> (Siesta.Green.node, string) result

(** {1 Combinators} *)

val id : 's t
val fail : string -> 's t

(** [seq s1 s2] runs [s2] on what [s1] gave. *)
val seq : 's t -> 's t -> 's t

(** [choice s1 s2] runs [s2] where [s1] fails. Once [s1] succeeds the choice
    is made, and a later failure in an enclosing {!seq} does not reach
    [s2]. *)
val choice : 's t -> 's t -> 's t

(** [choice s id]. It never fails. *)
val try_ : 's t -> 's t

(** [repeat ?fuel s] applies [s] until it fails, and gives the last node it
    reached. Each application uses one unit of [fuel], which is 100_000 by
    default.

    Running out of fuel fails. A rule set that succeeds forever is broken,
    and the tree it had reached when the fuel ran out is an arbitrary one. A
    rule that succeeds without changing anything succeeds forever, so
    [repeat (try_ s)] always runs out. *)
val repeat : ?fuel:int -> 's t -> 's t

(** [where_ s1 s2] runs [s2] where [s1] succeeds. Both run on the same node,
    and [s1]'s result is dropped. *)
val where_ : 's t -> 's t -> 's t

(** [kind k s] runs [s] on a node of kind [k], and fails on any other. *)
val kind : Ir.Kind.t -> 's t -> 's t

(** {1 One level down}

    These three visit child nodes only. Trivia, delimiters and separators are
    tokens, so they pass through as they were. A rewrite reaches a token
    through the generated congruences, which know which token is which
    child.

    {!one} and {!some} fail on a node with no child nodes. *)

(** [all s] runs [s] on every child node, and fails where any of them fails.
    A node with no child nodes comes back unchanged. *)
val all : 's t -> 's t

(** [one s] runs [s] on the child nodes in order, and stops at the first
    where it succeeds. *)
val one : 's t -> 's t

(** [some s] runs [s] on every child node, and keeps what it gave where it
    succeeded. It fails where it succeeded on none. *)
val some : 's t -> 's t

(** {1 Traversals}

    Each is defined from the combinators, as in Stratego's library. The
    definition is written beside each one. *)

(** [seq s (all (topdown s))]. *)
val topdown : 's t -> 's t

(** [seq (all (bottomup s)) s]. *)
val bottomup : 's t -> 's t

(** [seq s (seq (all (downup s)) s)]. *)
val downup : 's t -> 's t

(** [choice s (all (alltd s))]. [s] runs at the outermost nodes where it
    succeeds, and nothing below them is visited. It never fails. *)
val alltd : 's t -> 's t

(** [choice s (one (oncetd s))]. [s] runs at the first node in preorder
    where it succeeds. *)
val oncetd : 's t -> 's t

(** [choice (one (oncebu s)) s]. [s] runs at the first node in postorder
    where it succeeds. *)
val oncebu : 's t -> 's t

(** [choice s (some (sometd s))]. {!alltd}, except that it fails where [s]
    succeeded nowhere. *)
val sometd : 's t -> 's t

(** [bottomup (try_ (seq s (innermost s)))]. It rewrites to a normal form,
    innermost first. It does not terminate for a rule set that does not. *)
val innermost : 's t -> 's t

(** [repeat ?fuel (oncetd s)]. *)
val outermost : ?fuel:int -> 's t -> 's t

(** [seq s (choice (where_ stop id) (all (topdown_stop ~stop s)))].
    {!topdown}, except that it does not descend below a node where [stop]
    succeeds. [stop] runs on what [s] gave. *)
val topdown_stop : stop:'s t -> 's t -> 's t

(** [seq (choice (where_ stop id) (all (bottomup_stop ~stop s))) s].
    {!bottomup}, except that it does not descend below a node where [stop]
    succeeds. [stop] runs before the children are visited, and [s] still
    runs at the node it stopped at. *)
val bottomup_stop : stop:'s t -> 's t -> 's t

(** [collect s ctx node] finds the nodes {!alltd} would run [s] at, and
    lists each with what [s] gave there, in source order. It rebuilds
    nothing. *)
val collect
  :  's t
  -> 's Ctx.t
  -> Siesta.Syntax.t
  -> (Siesta.Syntax.t * Siesta.Green.node) list

(** {1 Congruences}

    A congruence runs a rule on each named child of a node, and rebuilds the
    node once. The generated [<lang>_rewrite.ml] has one per production. It
    finds the children with the views' slot walk and calls {!congruence}.

    A child rule runs on the elements of its slot, on cursors in the original
    tree. It runs on the elements of its own shape. A token rule passes a node
    through as it was, and a node rule a token. That is how {!all} treats
    tokens, and it covers a placeholder that recovery left in a token's
    slot. *)

(** A rule for a token. *)
module Token : sig
  type 's t =
    's Ctx.t -> Siesta.Syntax.token_cursor -> (Siesta.Green.token, string) result

  val id : 's t

  (** [text s] gives the token the text [s] and keeps its kind. *)
  val text : string -> 's t

  (** [make k s] gives a token of kind [k] and text [s]. *)
  val make : Ir.Kind.t -> string -> 's t
end

(** A rule for a child that holds a token or a node, depending on which of its
    symbols the parse found. *)
module Elem : sig
  type 's rule := 's t
  type 's t

  (** [make ?node ?token ()] runs [node] on a node and [token] on a token. An
      omitted rule leaves its element as it was. *)
  val make : ?node:'s rule -> ?token:'s Token.t -> unit -> 's t
end

(** A rule for a repeated child. ['r] is the rule each element takes: a {!t},
    a {!Token.t} or an {!Elem.t}, by what the child holds. These mirror the
    one-level traversals. *)
module Elems : sig
  type 'r t

  (** [all r] runs [r] on every element, and fails where any fails. *)
  val all : 'r -> 'r t

  (** [one r] runs [r] on the elements in order, and stops at the first where
      it succeeds. It fails where it succeeds on none. *)
  val one : 'r -> 'r t

  (** [some r] runs [r] on every element, and keeps what it gave where it
      succeeded. It fails where it succeeded on none. *)
  val some : 'r -> 'r t

  (** [nth i r] runs [r] on element [i], counting from 0. It fails where the
      child has no element [i]. *)
  val nth : int -> 'r -> 'r t
end

(** One child's rule, as {!congruence} takes it. Generated code builds these. *)
module Slot : sig
  type 's rule := 's t
  type 's t

  val node : 's rule -> 's t
  val token : 's Token.t -> 's t
  val elem : 's Elem.t -> 's t
  val nodes : 's rule Elems.t -> 's t
  val tokens : 's Token.t Elems.t -> 's t
  val elems : 's Elem.t Elems.t -> 's t
end

(** [congruence k slots rules] runs on a node of kind [k], and fails on any
    other. [slots] sorts the node's children into its slots. The rule at
    index [i] of [rules] runs on slot [i], and [None] leaves the slot as it
    was. A slot with no elements, such as an absent optional child, succeeds.

    The node is rebuilt once, and only where some element's tag changed. *)
val congruence
  :  Ir.Kind.t
  -> (Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
  -> 's Slot.t option array
  -> 's t

(** {1 Constructors}

    What a generated [make] calls. A [make] lists a node's parts in order:
    the frame's delimiters, and the elements of each of the production's
    children. These build the pieces and the node. They write no whitespace.
    The formatter writes all of it.

    {2 Comments}

    Given the node a [make] replaces, {!finish} keeps the comments that sit
    directly between that node's children. Comments inside a child go with
    the child. Each comment goes back in front of what it was in front of:

    - an element of a child, by its child and its place among that child's
      elements;
    - the separator after an element, which puts it straight after that
      element;
    - the closer, or the end of the node.

    Where the new node has no such element, because a list got shorter, the
    comment goes after the last element of the same child. Whitespace is
    left out, since the formatter writes all of it. *)
module Construct : sig
  type part

  val token : Siesta.Cache.t -> Ir.Kind.t -> string -> Siesta.Green.child

  (** The green node under a view. *)
  val node : Siesta.Syntax.t -> Siesta.Green.child

  (** A delimiter. *)
  val frame : Siesta.Green.child -> part

  (** [slot i elements] is the production's child [i]. Each element is one
      green child. *)
  val slot : int -> Siesta.Green.child list -> part

  (** [separated i ~leading ~trailing sep elements] is child [i] with [sep]
      between its elements. [leading] and [trailing] add one in front of the
      first and after the last, where there is an element at all. *)
  val separated
    :  int
    -> leading:bool
    -> trailing:bool
    -> Siesta.Green.child
    -> Siesta.Green.child list
    -> part

  (** [takes ~slots ~trivia ~open_after node next] holds where [node], put in
      front of a token of kind [next], would read that token as its own. The
      parse would then give the token to [node], and the text would not mean
      the tree. An [if] with no [else], put in front of an [else], is the
      case.

      [open_after k j next] says whether a node of kind [k], whose last
      filled slot is [j], takes [next] at its end: an optional or repeated
      child after [j] that begins with [next], more of [j] where it repeats,
      a separator, or an operator where [j] holds an expression. [j] is [-1]
      where no slot is filled. Where nothing in [node] follows the last
      element of [j], the question goes on to that element. *)
  val takes
    :  slots:(Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
    -> trivia:(Ir.Kind.t -> bool)
    -> open_after:(Ir.Kind.t -> int -> Ir.Kind.t -> bool)
    -> Siesta.Syntax.t
    -> Ir.Kind.t
    -> bool

  (** [finish ?replacing ~slots ~trivia ~comment ~takes cache k cast parts]
      builds a node of kind [k] from the parts, and gives it as a view over a
      cursor of its own. [cast] is the view module's. [slots] is the views
      module's slot walk, and [trivia] and [comment] say which kinds are
      trivia and which of those are comments.

      It fails where a child would take the token after it, by [takes]. A
      role gives a [takes] that never holds, since its operands are settled
      by parentheses. *)
  val finish
    :  ?replacing:Siesta.Syntax.t
    -> slots:(Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
    -> trivia:(Ir.Kind.t -> bool)
    -> comment:(Ir.Kind.t -> bool)
    -> takes:(Siesta.Syntax.t -> Ir.Kind.t -> bool)
    -> Siesta.Cache.t
    -> Ir.Kind.t
    -> (Siesta.Syntax.t -> 'view option)
    -> part list
    -> ('view, string) result
end

(** {1 Parentheses}

    What a generated block module calls to settle whether an operand needs
    parentheses. The binding powers are the block's, and the generated code
    holds them. *)
module Parens : sig
  (** [ends_open ~trivia ~expression node] holds where the last thing in
      [node] is an expression. A slot that refers to a block parses its
      expression from binding power 0, so that expression takes in any
      operator that follows it. ml's [if c then a else b] is the case: an
      operator after [b] belongs to [b].

      It reads the last child that is not trivia. A token ends the node. A
      node whose kind [expression] holds is an expression. Any other node is
      read the same way, so a production that ends in another production
      ends however that one does. *)
  val ends_open
    :  trivia:(Ir.Kind.t -> bool)
    -> expression:(Ir.Kind.t -> bool)
    -> Siesta.Syntax.t
    -> bool
end

(** {1 List edits}

    What a generated [insert_<child>] and [delete_<child>] call. Both work on
    one repeated child of a node, by its slot, and splice the node's own
    children. So whitespace and comments around the other elements stay where
    they were, and the formatter settles the rest.

    [sep] is the frame's separator, with the text to write, and [opener] the
    kind of its opening delimiter. Generated code gives both as constants. *)
module Edit : sig
  (** [insert ~kind ~slots ~trivia ~slot ~sep ~opener ~at element] puts
      [element] in front of element [at] of the child, or after the last where
      [at] is the number of elements.

      With a separator it goes straight after the separator in front of
      element [at], or after the opener, so the comments in front of element
      [at] stay with it. A list with a separator after its last element keeps
      one there. It fails on a node of any other kind and where [at] is past
      the end. *)
  val insert
    :  kind:Ir.Kind.t
    -> slots:(Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
    -> trivia:(Ir.Kind.t -> bool)
    -> slot:int
    -> sep:(Ir.Kind.t * string) option
    -> opener:Ir.Kind.t option
    -> at:int
    -> Siesta.Green.child
    -> 's t

  (** [delete ~kind ~slots ~trivia ~slot ~sep ~required ~at] takes element
      [at] out of the child, with the comments directly in front of it.

      With a separator it takes one separator too: the one after the
      element, or for the last element the one before it. A separator after
      the last element stays where the list had one. It fails on a node of
      any other kind, where there is no element [at], and where [required] is
      set and [at] is the only element. *)
  val delete
    :  kind:Ir.Kind.t
    -> slots:(Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
    -> trivia:(Ir.Kind.t -> bool)
    -> slot:int
    -> sep:(Ir.Kind.t * string) option
    -> required:bool
    -> at:int
    -> 's t
end

(** {1 Applying a result}

    A rewrite gives a new tree. [apply] turns it into text edits on the old
    source, one top-level item at a time, so the bytes of every item the
    rewrite left alone stay as the author wrote them. *)

(** Replace the bytes in [range], half open, with [text]. Ranges are offsets
    in the source the old tree was parsed from. *)
type splice =
  { range : int * int
  ; text : string
  }

(** [apply ~format ~items ~between ~trivia ~comment ~before ~after] gives
    the splices that turn the source of [before] into a text whose parse is
    [after].

    [items] gives the items of a root: the nodes in its repeated slot, or
    [None] where it has none. [between] is the text that goes between two
    items. A generated module gives both from the grammar.

    Old and new items are matched by tag, in order, so an item the rewrite
    left alone is the same node and keeps its bytes.

    - A changed item is formatted on its own and spliced over its range.
      Its range is its own text, so the comments above it stay.
    - A deleted item takes with it the run of comments directly above it,
      with no blank line in between, and the whitespace after it.
    - A moved item takes its bytes and that run of comments to its new place.
    - A new item is formatted and put after the item before it, with
      [between] in front of it.

    Where [items] gives [None], or where anything in the root outside its
    items changed, there is one splice, the whole new tree formatted. The
    splices are in source order and do not overlap. *)
val apply
  :  format:(Siesta.Green.node -> string)
  -> items:(Siesta.Syntax.t -> Siesta.Syntax.t list option)
  -> between:string
  -> trivia:(Ir.Kind.t -> bool)
  -> comment:(Ir.Kind.t -> bool)
  -> before:Siesta.Green.node
  -> after:Siesta.Green.node
  -> splice list

(** [splice text splices] makes the edits. *)
val splice : string -> splice list -> string

(** {1 Templates}

    A template is a tree parsed from text in the language's own syntax, with
    metavariables where children go. A generated module parses it and gives
    it the language's kinds. These match one against a tree and build one
    from what a match bound.

    A metavariable is a token. The single one stands for one child: a node
    or a token. The sequence one stands for a run of children of a repeated
    child, separators included, from none upwards.

    A node that holds only a single metavariable stands for whatever is in
    its place, so the base node an expression's metavariable is wrapped in
    stands for any expression. A node that holds only a sequence metavariable
    is a list whose elements are the run. A block's base node is the one
    exception, since that is how [f($$xs)] reads: an argument list names an
    expression, and the run stands for its arguments.

    A metavariable is named by its text up to any [:], so [$x] and [$x:expr]
    are one. A name that occurs twice must bind the same text, trivia left
    out. *)
module Template : sig
  type binding =
    | One of Siesta.Green.child
    | Run of Siesta.Green.child list

  (** What a template needs to know of the language's kinds. *)
  type kinds =
    { trivia : Ir.Kind.t -> bool
    ; single : Ir.Kind.t -> bool (** The plain metavariable and every typed one. *)
    ; sequence : Ir.Kind.t -> bool
    ; base : Ir.Kind.t -> bool (** A block's base node. *)
    }

  (** [relabel cache map tree] gives every node and token of [tree] the kind
      [map] gives for its own. *)
  val relabel
    :  Siesta.Cache.t
    -> (Ir.Kind.t -> Ir.Kind.t)
    -> Siesta.Green.node
    -> Siesta.Green.node

  (** The one node a template's root holds. A template is parsed at a root
      made for the rule it is written in, which holds one of that rule. *)
  val fragment : kinds -> Siesta.Green.node -> Siesta.Green.node option

  (** [matches kinds template node] gives what each metavariable bound, or
      [None] where [template] does not match [node]. Tokens match by kind and
      text, and trivia is left out on both sides. *)
  val matches
    :  kinds
    -> Siesta.Green.node
    -> Siesta.Syntax.t
    -> (string * binding) list option

  (** [instantiate cache kinds template bindings] is [template] with every
      metavariable replaced by what it bound. It fails for a metavariable
      with no binding, and where a sequence metavariable's run stands where a
      single one goes. *)
  val instantiate
    :  Siesta.Cache.t
    -> kinds
    -> Siesta.Green.node
    -> (string * binding) list
    -> (Siesta.Green.node, string) result
end

(** {1 Binders}

    Names, read the way a grammar declares them: a binder child holds a
    token whose text introduces a name, and a production that opens a scope
    keeps the names introduced inside it, its own binders included. The root
    is a scope too. A reference is any token of a binder's kind that is not
    a binder itself.

    A reference resolves the way tree-sitter's locals do: to the nearest
    binder of the same text whose scope holds the reference and which comes
    before it. *)
module Binders : sig
  type 's rule := 's t

  (** What a grammar says about its names. A generated module gives one. *)
  type t =
    { slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option
    ; trivia : Ir.Kind.t -> bool
    ; scope : Ir.Kind.t -> bool
    ; binders : Ir.Kind.t -> int list (** The slots of a kind that hold binders. *)
    ; reference : Ir.Kind.t -> bool (** A token kind a binder can have. *)
    ; base : Ir.Kind.t -> bool (** A block's base node. *)
    }

  (** Every binder whose scope holds [at] and which comes before it, with
      its name, nearest first. A shadowed binder is in the list, after the
      one that shadows it. *)
  val visible : t -> Siesta.Syntax.t -> (string * Siesta.Syntax.token_cursor) list

  (** [fresh t at ~base] is [base], or [base] followed by the smallest
      number, that no binder visible at [at] has. *)
  val fresh : t -> Siesta.Syntax.t -> base:string -> string

  (** The binder a reference resolves to, or [None] for a name nothing in
      the tree binds. A binder resolves to itself. *)
  val resolve : t -> Siesta.Syntax.token_cursor -> Siesta.Syntax.token_cursor option

  (** [rename t root binder ~to_] gives [root]'s tree with [binder] and every
      reference that resolves to it spelled [to_]. It fails where a binder
      named [to_] would take one of those references, or where [to_] is
      visible at [binder] already. *)
  val rename
    :  t
    -> Siesta.Cache.t
    -> Siesta.Syntax.token_cursor
    -> to_:string
    -> (Siesta.Green.node, string) result

  (** [substitute t ~name ~by] is a rule that replaces every free use of
      [name] in a node with [by]. A use is free where it resolves to nothing
      inside the node. A use is replaced where it stands as an expression:
      the only token of a block's base node.

      It avoids capture. A binder inside the node that would take a name
      [by] uses, at a place [by] goes, is renamed with {!fresh} first, with
      its references. *)
  val substitute : t -> name:string -> by:Siesta.Green.node -> 's rule
end
