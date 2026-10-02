# A diagnostic should say which node it was reported in

## What exists

```ocaml
type t =
  { range : int * int   (* byte offsets into the input, half open *)
  ; kind  : kind
  }
```

`diagnostic.mli` is explicit that this is a choice, not an oversight: "A
diagnostic is anchored by a byte range over the input, so a consumer places one
without walking the tree." That is right, and this request does not propose
changing it.

One link between a diagnostic and the tree already exists, in one direction: a
recovery node carries its diagnostic's 1-based id as its green payload, so
**node → diagnostic** is one step (`cursor.mli`, `report_id`). There is no
**diagnostic → node**.

## What we need, and why a byte range cannot give it

We are building an incremental daemon. Two questions come up constantly and both
need a diagnostic related to structure rather than to a file position:

**"Is this the same error as before, or a new one?"** An agent rewrites a file and
asks what it broke. The useful answer separates errors it caused from errors that
were already there. With byte ranges alone, inserting a line at the top of a file
changes the range of every diagnostic below it, so every error looks new. The
errors are the same errors; only the file moved.

**"Which part of the program is this error in?"** To group errors by definition, or
to re-check only the definitions whose errors changed, we need the subtree the
error sits in — and its identity, not its position. siesta gives us that identity
for free: green nodes are hash-consed, so an unchanged subtree is physically the
same node across parses.

A byte range cannot answer either, because it is a fact about the file rather than
about the program.

## The ask

Have the cursor record the node a diagnostic was reported inside, which it already
knows — the open frame at the moment of `report_at` / `report_id`:

```ocaml
type t =
  { range : int * int
  ; kind  : kind
  ; in_node : (int * int) option
      (** The byte range of the innermost node open when this was reported.
          [None] where nothing was open. *)
  }
```

The range of the enclosing node is enough. A consumer holding the tree can go from
that range to the node, and the node's hash-cons identity is then the stable thing:
"the same error, in a subtree that did not change" becomes decidable, and so does
"this error is inside that definition".

A relative offset (`at`, within `in_node`) would serve equally and may read better.
Either is fine; the information is the same.

## Why lingo rather than each consumer

A consumer can recover this by searching the tree for the innermost node containing
the diagnostic's range. We can write that. But:

- it is O(tree) per diagnostic, where the cursor has the answer for free at report
  time;
- it is ambiguous at boundaries — a zero-width or adjacent range can sit in two
  nodes, and the consumer has to guess which the parser meant;
- every consumer that relates diagnostics to structure writes the same search, and
  each one guesses at the boundaries differently;
- and the parser's own answer is the correct one by construction. It knows which
  node it was building when it gave up.

The existing recovery-node payload shows the link is wanted; this is the other half
of it, and it covers the diagnostics that have no recovery node (`Extra`,
`Unexpected`) as well.

## What this does not solve

**Hash-cons identity is not a position.** Two structurally identical subtrees are
the *same* green node, so node identity alone cannot locate a diagnostic — only say
whether the code it is about changed. That is exactly what we want for "is this the
same error", and it is not a substitute for the byte range, which stays.

**"Which definition" is a language concept, not a grammar one.** lingo has no notion
of a top-level named item, so mapping an error to `add` rather than to a node
remains ours. `in_node` is what makes that mapping cheap and unambiguous rather than
a search.

## Cost and risk

Small, as far as we can see: one more field, populated from state the cursor already
holds. The risks we can see are both about *which* node, and both are decisions
rather than difficulties:

- the innermost open node is most precise and changes most often; the enclosing
  item would be more stable but lingo cannot know what an item is. Innermost is the
  right choice — a consumer can walk outwards, and cannot walk in.
- a diagnostic reported after a node has finished (`report_at` exists for exactly
  that case — a trailing separator is only trailing in the light of what follows)
  has no open node, hence the `option`.

## Priority

Higher than the lexing request in `lexing.md`, and for a different reason. That one
is a performance optimisation we have not yet earned with a measurement. This one is
a capability: without it, telling an agent which errors its change caused is
guesswork, and that feature is the main reason the daemon is worth building for a
command-line client rather than only for an editor.
