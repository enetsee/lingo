(** One parse's state: the tokens, the position in them, the tree being
    built, and the diagnostics so far.

    Every primitive a generated parser calls takes one of these. It is named
    for the cursor, because a parser spends most of its time reading the
    token stream.

    {1 Trivia}

    The kinds given to {!create} as trivia are hidden from the cursor's view.
    {!current}, {!at}, {!eof}, {!peek_meaningful_at} and {!range} all look
    past them. None of the five moves the cursor or puts anything in the
    tree.

    Trivia enters the tree when the parser consumes input or freezes a
    position. That means {!bump}, {!skip_trivia}, {!Recover.expect} and
    {!Build.mark}. It lands in whichever frame is open at the time, so trivia
    before a child attaches to the parent. {!Build.mark} skips trivia before
    it takes its checkpoint, so a node built at that checkpoint starts after
    the trivia rather than inside it.

    A parser that looks and then declines to consume leaves the trivia alone.
    Whatever consumes next takes it. *)

type t

(** [create ?cache ~trivia_kinds tokens] starts a parse over [tokens].

    [?cache] decides how siesta interns nodes. The default shares identical
    subtrees between parses. An editor re-parsing on every keystroke wants
    that. A tool that parses once and drops the tree should pass
    [Siesta.Cache.create_plain ()] and skip the hashing. *)
val create : ?cache:Siesta.Cache.t -> trivia_kinds:Ir.Kind.t list -> Token.t array -> t

(** {1 Reading} *)

(** The index into the token array. It counts trivia. *)
val position : t -> int

(** The kind under the cursor, past any trivia. {!Ir.Kind.none} at the end of
    the input. *)
val current : t -> Ir.Kind.t

val at : t -> Ir.Kind.t -> bool
val eof : t -> bool

(** [peek_meaningful_at c ~n] is the kind [n] meaningful tokens further on.
    [n = 0] is {!current}. {!Ir.Kind.none} past the end of the input. *)
val peek_meaningful_at : t -> n:int -> Ir.Kind.t

(** The byte range of the token {!current} answers for, half open. Leading
    trivia sits outside it, so a diagnostic points at the token rather than
    at the whitespace before it. Both ends are the end of the input once
    there is no token left. *)
val range : t -> int * int

val is_trivia : t -> Ir.Kind.t -> bool

(** {1 Consuming} *)

(** Puts the trivia under the cursor into the open frame and stops at the
    next meaningful token. *)
val skip_trivia : t -> unit

(** Skips trivia, then puts one token into the open frame and advances. Does
    nothing at the end of the input. *)
val bump : t -> unit

(** {1 Diagnostics} *)

(** Records a diagnostic over {!range}. *)
val report : t -> Diagnostic.kind -> unit

(** The byte offset just past everything the parse has taken. Read it before
    and after consuming, and the two offsets bracket what was read. *)
val offset : t -> int

(** Records a diagnostic over a range the caller worked out, rather than over
    {!range}.

    Use it to report on input the parse has already taken, because {!range}
    answers for the token under the cursor and the cursor has moved past it. A
    trailing separator is the case that needs this: a parse knows the
    separator was trailing only once it has read what follows. *)
val report_at : t -> int * int -> Diagnostic.kind -> unit

(** The same, answering with the diagnostic's 1-based id. Stamp that id on
    the recovery node as its payload, and a consumer walking the tree gets
    from a node to its diagnostic in one step.

    Two [Missing] diagnostics over one range fold into one, and both callers
    get its id. A committed production whose leading required children all
    fail at the same cursor asks for one diagnostic per child. The tree keeps
    a hole per child and the list keeps one entry. *)
val report_id : t -> Diagnostic.kind -> int

(** {1 The builder and the diagnostics}

    {!Build} brackets nodes around what the cursor emits, so it works on the
    same builder and the same list.

    Anything else holding the builder can put tokens in the tree that the
    cursor never read. The tree and the position then disagree, and nothing
    reports it. *)

val builder : t -> Siesta.Builder.t
val diagnostics : t -> Diagnostic.t list

(** How many frames are open. It is [0] before the first node is started, and
    again once the last one closes.

    {!Build.start_node} reads it to tell the root from every other node. At
    the root nothing is open yet, so leading trivia has nowhere to go but
    inside the root itself.

    {!Build} keeps it, through {!entered} and {!left}. Nothing else should
    call those two. *)
val depth : t -> int

val entered : t -> unit
val left : t -> unit
