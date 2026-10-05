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
