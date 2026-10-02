# An entry point that lexes from an offset

## What is missing

The emitted lexer has one entry point:

```ocaml
val lex : string -> Lingo_runtime.Token.t array
```

Whole file in, whole token array out. There is no way to lex from a position, and
no way to stop early. So every edit costs a full relex, however small the edit.

## Why we need it

We are building an incremental daemon on lingo, siesta and `incr`. For an editor,
a keystroke changes one character and we want the work after it to be proportional
to the edit rather than to the file.

siesta already gives us most of that. Green nodes are hash-consed, so an unchanged
subtree comes back physically equal and everything above it is spared — measured,
with one definition changed in a ten-definition file re-checking one definition.
But that saving starts *after* the tree is built. Building it still costs a full
relex plus a full reparse, because:

- the lexer takes the whole string, and
- `Cursor.create` takes a complete `Token.t array`.

So the floor on an edit is O(file), and this request is about the first half of
that floor.

## Why it should be cheap

The algorithm is already restartable. In the reference lexer
(`test/lex/lex.ml:45-62`), `longest` begins at `Redfa.Dfa.initial dfa` for **every**
token, and the loop in `run` carries nothing between tokens — no mode stack, no
interpolation state, no indentation state. Each token is an independent maximal
munch from a byte offset.

That is a property of lingo's lexer model rather than an accident of the reference
implementation: a grammar's tokens are regexes compiled to one DFA, and nothing in
`token_def` can express a mode.

So the capability exists and the emitted interface does not expose it.

## What we would like

The primitive, which is enough:

```ocaml
(** [next_token text pos] is the token starting at [pos] and the offset after it,
    or [None] at the end of input. Byte offsets. *)
val next_token : string -> int -> (Lingo_runtime.Token.t * int) option
```

With that we can build re-synchronisation ourselves: lex forward from a token
boundary at or before the edit, and stop when a produced token matches the old
array's token at the adjusted offset, then splice the old tail.

A convenience that would be better placed in lingo than in us, because the
knowledge about token boundaries lives here:

```ocaml
(** [relex ~old ~text ~edit:(start, old_len, new_len)] is the token array for
    [text], reusing the tail of [old] beyond the point where the two streams
    re-synchronise. *)
val relex :
  old:Lingo_runtime.Token.t array ->
  text:string ->
  edit:int * int * int ->
  Lingo_runtime.Token.t array
```

If only one of the two is wanted, `next_token` is the one — everything else can be
built on it.

## The acceptance criterion

Differential, and it should exist before any caller trusts it:

> For any text, any edit, and the token array of the text before that edit,
> `relex` must produce exactly `lex` of the text after it.

Over generated edits, including edits inside a token, edits that join two tokens,
edits that split one, edits at the very start and very end, and edits inside
trivia. This is the same shape as the laws already in `test/` and the one that
matters most, because a lexer that is subtly wrong after an edit produces a tree
that is subtly wrong, and the error surfaces somewhere else entirely.

## Priority

**Not urgent, and we have not earned it with a measurement yet.** The number that
would justify it is lingo's real parse time — lex plus parse — on the largest file
a user would open, against a keystroke interval of roughly 100ms. Our own
measurement of 1.66ms per 20,000 tokens was taken with a hand-written builder and
our own cache, not with lingo's lexer and plan, so it bounds nothing here.

rust-analyzer does a full file reparse for most edits and is fine, which suggests
this may stay unnecessary. We will measure before asking for it to be built.

What makes it worth writing down now rather than later: the agentic workload does
not need this at all — an agent rewrites whole files, so there is no unchanged
prefix to reuse — but the editor workload might, and the same daemon serves both.

## What this is not

Not a request for incremental *parsing*. That needs a way to push an existing
`Siesta.Green.node` into a builder as a child, and a reuse check in the generated
parser with a condition on unchanged lookahead. Those are separate and larger, and
this request is useful without them: a faster lexer helps every parse, incremental
or not.
