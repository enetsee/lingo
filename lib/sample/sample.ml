open StdLabels

type token =
  { kind : Core.Kind.t
  ; text : string
  }

type map =
  { facts : Core.Facts.t (* The facts this was built from. *)
  ; nts : token list Bolts.nonterminal option array (* Indexed by rule id. *)
  ; lexer : Core.Lexer.t
  }

let nonterminal (map : map) (rule : Core.Rule.id) : token list Bolts.nonterminal option =
  map.nts.(rule)
;;

(* -- a lexeme is a walk over the automaton --------------------------------- *)

(* No accepting state for this token is reachable from here. *)
let unreachable = max_int / 2

(* The codepoints a class stands for, at most sixteen of them. Printable ASCII
   first, so a sample reads back and survives being logged. A class holding
   none of it falls back to the least codepoint of each of its segments.

   Both lists ascend, so index zero is the plainest character the class has,
   and a shrinking draw walks towards it. *)
let class_chars (lexer : Core.Lexer.t) (klass : int) : int array =
  let keep = ref [] in
  let count = ref 0 in
  let add (codepoint : int) : unit =
    if
      !count < 16
      && Uchar.is_valid codepoint
      && Core.Lexer.class_of lexer codepoint = klass
    then (
      keep := codepoint :: !keep;
      incr count)
  in
  for codepoint = 33 to 126 do
    add codepoint
  done;
  if !count = 0 then Array.iter lexer.segments ~f:add;
  Array.of_list (List.rev !keep)
;;

(* Where each state goes, by class, leaving out a class no character spells.
   The walk and the distance table read the same array, so a move one of them
   allows is a move the other counted. *)
let transitions (lexer : Core.Lexer.t) (chars : int array array) : (int * int) array array
  =
  Array.init lexer.num_states ~f:(fun state ->
    let moves = ref [] in
    for klass = lexer.num_classes - 1 downto 0 do
      let dest = Core.Lexer.step lexer ~state ~klass in
      if dest >= 0 && Array.length chars.(klass) > 0 then moves := (klass, dest) :: !moves
    done;
    Array.of_list !moves)
;;

(* Steps from each state to one that accepts this kind, breadth first over the
   reversed transitions. A walk that has spent its budget follows this down, so
   it reaches an accepting state and the lexeme ends. *)
let distances
      (lexer : Core.Lexer.t)
      (edges : (int * int) array array)
      (kind : Core.Kind.t)
  : int array
  =
  let incoming = Array.make lexer.num_states [] in
  Array.iteri edges ~f:(fun state moves ->
    Array.iter moves ~f:(fun (_, dest) -> incoming.(dest) <- state :: incoming.(dest)));
  let distance = Array.make lexer.num_states unreachable in
  let queue = Queue.create () in
  Array.iteri lexer.accept ~f:(fun state accepted ->
    match accepted with
    | Some accepted when Core.Kind.equal accepted kind ->
      distance.(state) <- 0;
      Queue.add state queue
    | Some _ | None -> ());
  while not (Queue.is_empty queue) do
    let state = Queue.pop queue in
    List.iter incoming.(state) ~f:(fun source ->
      if distance.(source) = unreachable
      then (
        distance.(source) <- distance.(state) + 1;
        Queue.add source queue))
  done;
  distance
;;

(* One lexeme for a pattern token. The walk takes a random admissible move
   until its budget runs out, then follows [distance] down to a state that
   accepts the token.

   Stopping at such a state is what makes the lexeme lex back as this token.
   The scan has no bytes left to munch, so the lexeme lexes as the token whose
   accepting state the walk stopped in, and [ident = [a-z]+] cannot spell a
   keyword.

   Every draw goes through [Bolts.Source.Draw], so the bytes are in the trace
   and a shrink can reduce them. *)
let lexeme
      (chars : int array array)
      (edges : (int * int) array array)
      (accepts : bool array)
      (distance : int array)
  : unit -> string
  =
  fun () ->
  let buf = Buffer.create 8 in
  let budget = 1 + Bolts.Source.Draw.int 4 in
  let rec walk (state : int) (steps : int) : unit =
    let onward =
      Array.of_list
        (List.filter
           (Array.to_list edges.(state))
           ~f:(fun (_, dest) -> distance.(dest) < unreachable))
    in
    if (accepts.(state) && steps >= budget) || Array.length onward = 0
    then ()
    else (
      let candidates =
        if steps < budget
        then onward
        else (
          (* The budget is spent and the state does not accept, so head for one
             that does. Its distance is one less than this state's, which is
             what ends the walk. *)
          let nearest =
            Array.fold_left onward ~init:unreachable ~f:(fun acc (_, dest) ->
              min acc distance.(dest))
          in
          Array.of_list
            (List.filter (Array.to_list onward) ~f:(fun (_, dest) ->
               distance.(dest) = nearest)))
      in
      let klass, dest = candidates.(Bolts.Source.Draw.int (Array.length candidates)) in
      let codepoints = chars.(klass) in
      Buffer.add_utf_8_uchar
        buf
        (Uchar.of_int codepoints.(Bolts.Source.Draw.int (Array.length codepoints)));
      walk dest (steps + 1))
  in
  walk Core.Lexer.initial 0;
  Buffer.contents buf
;;

(* -- the system ------------------------------------------------------------ *)

(* The loop admits every binding power there is. *)
let no_ceiling = max_int

let cat (first : token list Bolts.t) (second : token list Bolts.t) : token list Bolts.t =
  Bolts.map2 first second ~f:(fun left right -> left @ right)
;;

let cats (species : token list Bolts.t list) : token list Bolts.t =
  match species with
  | [] -> Bolts.pure []
  | [ only ] -> only
  | species -> Bolts.map (Bolts.seq species) ~f:List.concat
;;

let flatten (repeated : token list list Bolts.t) : token list Bolts.t =
  Bolts.map repeated ~f:List.concat
;;

let maybe (species : token list Bolts.t) : token list Bolts.t =
  Bolts.map (Bolts.option species) ~f:(function
    | None -> []
    | Some tokens -> tokens)
;;

let system_of ?(comments = 0.) (f : Core.Facts.t) : Bolts.system * map =
  let sys = Bolts.create () in
  let lexer = Core.Lexer.of_facts f in
  let chars = Array.init lexer.num_classes ~f:(class_chars lexer) in
  let edges = transitions lexer chars in
  let draw_for (kind : Core.Kind.t) : unit -> string =
    let accepts =
      Array.map lexer.accept ~f:(function
        | Some accepted -> Core.Kind.equal accepted kind
        | None -> false)
    in
    lexeme chars edges accepts (distances lexer edges kind)
  in
  (* A comment is trivia the formatter keeps, so it is a draw of size zero at
     every token boundary. Whitespace is trivia the formatter re-emits, so it
     is absent and [decode] writes it. *)
  let comment_draws =
    Array.of_list
      (List.filter_map (Array.to_list f.tokens) ~f:(fun (tok : Core.Token.def) ->
         match tok.trivia with
         | Some Core.Grammar.Preserve -> Some (tok.kind, draw_for tok.kind)
         | Some Core.Grammar.Reformat | None -> None))
  in
  let leading =
    if comments <= 0. || Array.length comment_draws = 0
    then None
    else
      Some
        (Bolts.gen0 (fun () ->
           (* An exhausted trace draws zero, and zero is no comment. A draw
              read the other way round would have a truncated trace grow more
              comments than the one it was cut from. *)
           if Bolts.Source.Draw.float () < 1. -. comments
           then []
           else (
             let kind, draw =
               comment_draws.(Bolts.Source.Draw.int (Array.length comment_draws))
             in
             [ { kind; text = draw () } ])))
  in
  let nts =
    Array.map f.rules ~f:(fun (rule : Core.Rule.def) ->
      match rule.origin with
      | Core.Rule.Pratt_role _ -> None
      | Core.Rule.User | Core.Rule.Pratt_block ->
        Some (Bolts.declare sys (Core.Grammar.Name.Rule.to_string rule.name)))
  in
  let terminals = Hashtbl.create 64 in
  let terminal (tok : Core.Token.def) : token list Bolts.t =
    match Hashtbl.find_opt terminals tok.id with
    | Some species -> species
    | None ->
      let bytes =
        match Core.Token.text tok with
        | Some text -> Bolts.atom [ { kind = tok.kind; text } ]
        | None ->
          let draw = draw_for tok.kind in
          Bolts.prim ~id:(Core.Kind.to_int tok.kind) (fun () ->
            [ { kind = tok.kind; text = draw () } ])
      in
      let species =
        match leading with
        | None -> bytes
        | Some comment -> cat comment bytes
      in
      Hashtbl.add terminals tok.id species;
      species
  in
  (* A symbol is a token or a rule, and the checks settled that before the
     facts existed. Both refusals below are for a kind no grammar can put
     here. Giving nothing back instead would build a system quietly not the
     grammar. *)
  let sym (kind : Core.Kind.t) : token list Bolts.t =
    match Core.Facts.token_of_kind f kind, Core.Facts.rule_of_kind f kind with
    | Some tok, _ -> terminal tok
    | None, Some rule ->
      (match nts.(rule.id) with
       | Some nonterminal -> Bolts.nt nonterminal
       (* A role's kind. Nothing references a role, so no child and no operator
          table holds one. *)
       | None ->
         Core.Grammar.Name.Rule.to_string rule.name
         |> Printf.sprintf "Sample.system_of: %s is a role and has no species"
         |> invalid_arg)
    | None, None ->
      Core.Kind.Name.to_string (Core.Facts.kind_name f kind)
      |> Printf.sprintf "Sample.system_of: %s is neither a token nor a rule"
      |> invalid_arg
  in
  let alts (kinds : Core.Kind.t array) : token list Bolts.t =
    match Array.to_list kinds with
    | [ only ] -> sym only
    | kinds -> Bolts.sum (List.map ~f:sym kinds)
  in
  (* Elements with a separator between them, and one after the last where the
     policy allows it. A trailing separator the policy forbids is bytes the
     parser still takes and then reports, so [Never] is the one policy that
     leaves it out. *)
  let repeat
        (element : token list Bolts.t)
        (sep : Core.Rule.sep option)
        ~(nonempty : bool)
    : token list Bolts.t
    =
    match sep with
    | None -> flatten (if nonempty then Bolts.plus element else Bolts.star element)
    | Some sep ->
      let elements = cat element (flatten (Bolts.star (cat (sym sep.sep_tok) element))) in
      let elements =
        match sep.trailing with
        | Core.Grammar.Never -> elements
        | Core.Grammar.On_break | Core.Grammar.Always ->
          cat elements (maybe (sym sep.sep_tok))
      in
      if nonempty then elements else maybe elements
  in
  let repeated (child : Core.Rule.child) : bool =
    match child.modifier with
    | Core.Grammar.Zero_or_more | Core.Grammar.One_or_more -> true
    | Core.Grammar.Exactly_one | Core.Grammar.Zero_or_one -> false
  in
  let nonempty (child : Core.Rule.child) : bool =
    match child.modifier with
    | Core.Grammar.Exactly_one | Core.Grammar.One_or_more -> true
    | Core.Grammar.Zero_or_one | Core.Grammar.Zero_or_more -> false
  in
  let child (child : Core.Rule.child) : token list Bolts.t =
    let element = alts child.alts in
    match child.modifier with
    | Core.Grammar.Exactly_one -> element
    | Core.Grammar.Zero_or_one -> maybe element
    | Core.Grammar.Zero_or_more | Core.Grammar.One_or_more ->
      repeat element None ~nonempty:(nonempty child)
  in
  (* A separator reaches a repeated child and nothing else, which is where the
     plan's lowering puts it too. *)
  let body (rule : Core.Rule.def) (sep : Core.Rule.sep option) : token list Bolts.t list =
    match sep, Core.Rule.body_children rule with
    | Some sep, [ element ] when repeated element ->
      [ repeat (alts element.alts) (Some sep) ~nonempty:(nonempty element) ]
    | _, children -> List.map ~f:child children
  in
  (* What the frame wraps, with its separator in place and without its
     delimiters. A production and a desugared postfix share it: the postfix
     operator's lead token is the frame's opener, and the Pratt loop has
     already written it. *)
  let framed (rule : Core.Rule.def) : token list Bolts.t list =
    match rule.frame with
    | Core.Rule.Plain | Core.Rule.Committed _ -> body rule None
    | Core.Rule.Separated sep ->
      body rule (Some { Core.Rule.sep_tok = sep.sep_tok; trailing = sep.trailing })
    | Core.Rule.Delimited frame -> body rule frame.sep
  in
  (* The children before the frame opens, then the frame.

     [body_from] is zero for every rule this reaches, so the first list is
     always empty. It is written because it is the other half of what [framed]
     takes, and the two have to decompose a rule's children between them. The
     half that is not empty belongs to a desugared [Enclosed] postfix, whose
     operand sits in front of its opener, and the Pratt loop supplies that
     operand itself. *)
  let production (rule : Core.Rule.def) : token list Bolts.t =
    let before = List.init ~len:rule.body_from ~f:(fun at -> child rule.children.(at)) in
    cats
      (before
       @
       match rule.frame with
       | Core.Rule.Plain | Core.Rule.Committed _ | Core.Rule.Separated _ -> framed rule
       | Core.Rule.Delimited frame ->
         (sym frame.open_ :: framed rule) @ [ sym frame.close ])
  in
  (* An expression block is its parse loop written out. [expr threshold] is
     what the loop reads on entry, and [loop threshold ceiling] is the tail
     that follows: an operator whose left binding power is at least the
     threshold and below the ceiling, then the body that operator takes, then
     the next tail.

     The ceiling is what leaves one derivation per string. An infix operator's
     right operand is read at its right binding power and the loop carries that
     number afterwards, so an operator the operand could have taken is out of
     the loop's reach. A prefix operand leaves its own binding power behind for
     the same reason. A postfix operator leaves none, because the parse goes
     back to reading everything from the threshold up.

     Both numbers come from the table, so the species a block adds are bounded
     by its size. *)
  let block (b : Core.Block.def) : unit =
    let name = Core.Grammar.Name.Rule.to_string f.rules.(b.rule_id).name in
    let ceiling_name (ceiling : int) : string =
      if ceiling = no_ceiling then "*" else string_of_int ceiling
    in
    let exprs = Hashtbl.create 8 in
    let loops = Hashtbl.create 16 in
    let infix =
      Array.map b.infix ~f:(fun (op : Core.Block.op) -> op.op_kind, Core.Block.op_bps op)
    in
    let prefix =
      Array.map b.prefix ~f:(fun (op : Core.Block.op) ->
        op.op_kind, snd (Core.Block.op_bps op))
    in
    let rec expr (threshold : int) : token list Bolts.nonterminal =
      match Hashtbl.find_opt exprs threshold with
      | Some declared -> declared
      | None ->
        let here = Bolts.declare sys (Printf.sprintf "%s.expr@%d" name threshold) in
        Hashtbl.add exprs threshold here;
        let atom (kind : Core.Kind.t) =
          cat (sym kind) (Bolts.nt (loop threshold no_ceiling))
        in
        let prefixed (kind, operand_at) =
          cats
            [ sym kind; Bolts.nt (expr operand_at); Bolts.nt (loop threshold operand_at) ]
        in
        Bolts.define
          here
          (Bolts.sum
             (List.map ~f:atom (Array.to_list b.atoms)
              @ List.map ~f:prefixed (Array.to_list prefix)));
        here
    and loop (threshold : int) (ceiling : int) : token list Bolts.nonterminal =
      match Hashtbl.find_opt loops (threshold, ceiling) with
      | Some declared -> declared
      | None ->
        let here =
          Bolts.declare
            sys
            (Printf.sprintf "%s.loop@%d<%s" name threshold (ceiling_name ceiling))
        in
        Hashtbl.add loops (threshold, ceiling) here;
        let admits (bp : int) : bool = bp >= threshold && bp < ceiling in
        let step (kind : Core.Kind.t) (body : token list Bolts.t) (leaves : int) =
          cats [ sym kind; body; Bolts.nt (loop threshold leaves) ]
        in
        let infix_step (kind, (binds_left, operand_at)) =
          if admits binds_left
          then [ step kind (Bolts.nt (expr operand_at)) operand_at ]
          else []
        in
        let postfix_step (op : Core.Block.postfix) =
          if admits op.p_bp then [ step op.p_lead (tail op) no_ceiling ] else []
        in
        Bolts.define
          here
          (Bolts.sum
             (Bolts.pure []
              :: (List.concat_map ~f:infix_step (Array.to_list infix)
                  @ List.concat_map ~f:postfix_step (Array.to_list b.postfix))));
        here
    (* What a postfix operator reads after its lead token, taken from the rule
       the desugaring built for it. The lead is the loop's own step and the
       frame's opener, so it is not here.

       The three shapes are the three the plan's lowering reads from the same
       rule: nothing at all, the rule's last child, or the frame's body and its
       closer. *)
    and tail (op : Core.Block.postfix) : token list Bolts.t =
      let rule = Core.Facts.rule f op.p_rule in
      match op.p_body with
      | Core.Block.Nothing -> Bolts.pure []
      | Core.Block.Then _ -> child rule.children.(Array.length rule.children - 1)
      | Core.Block.Enclosed enclosed -> cats (framed rule @ [ sym enclosed.close ])
    in
    match nts.(b.rule_id) with
    | Some block_nt -> Bolts.define block_nt (Bolts.nt (expr 0))
    | None -> ()
  in
  Array.iter f.rules ~f:(fun (rule : Core.Rule.def) ->
    match rule.origin, nts.(rule.id) with
    | Core.Rule.User, Some nonterminal -> Bolts.define nonterminal (production rule)
    | (Core.Rule.User | Core.Rule.Pratt_block | Core.Rule.Pratt_role _), _ -> ());
  Array.iter f.blocks ~f:block;
  sys, { facts = f; nts; lexer }
;;

(* -- back to source text --------------------------------------------------- *)

(* The codepoint at a byte offset, and how many bytes it took. A byte that is
   not valid UTF-8 decodes to U+FFFD over one byte, so a scan still moves on. *)
let uchar_at (text : string) (at : int) : int * int =
  let decoded = String.get_utf_8_uchar text at in
  Uchar.to_int (Uchar.utf_decode_uchar decoded), Uchar.utf_decode_length decoded
;;

(* Where the token starting at [from] ends, and whether the scan was still
   running when the bytes ran out. Longest match, and the two runs that match
   nothing end the way the emitted lexer's do: bytes that ran out inside a
   lexeme take the rest of the string, and a character no state moves on takes
   itself.

   A scan that stopped at a character it could not take is settled: no bytes
   appended after the string can lengthen that token. [live_suffix] is what
   reads the flag. *)
let token_end (lexer : Core.Lexer.t) (text : string) (from : int) : int * bool =
  let len = String.length text in
  let rec scan (at : int) (state : int) (longest : int) : int * int =
    let longest = if at > from && lexer.accept.(state) <> None then at else longest in
    if at >= len
    then longest, at
    else (
      let code, width = uchar_at text at in
      match Core.Lexer.step lexer ~state ~klass:(Core.Lexer.class_of lexer code) with
      | -1 -> longest, at
      | dest -> scan (at + width) dest longest)
  in
  let longest, stopped = scan from Core.Lexer.initial (-1) in
  let ends =
    if longest > from
    then longest
    else if stopped >= len
    then len
    else from + snd (uchar_at text from)
  in
  ends, stopped >= len
;;

(* Does lexing [text] put a token boundary at byte [at]? Every step moves the
   scan on, so the walk ends. *)
let boundary (lexer : Core.Lexer.t) (text : string) (at : int) : bool =
  let rec walk (from : int) : bool =
    from = at || (from < at && walk (fst (token_end lexer text from)))
  in
  walk 0
;;

(* The tail of the run that bytes still to come can change.

   Lexing left to right, a token whose scan stopped at a character it could not
   take is settled, and so is the boundary in front of it. The first token whose
   scan was still running when the run ran out is where that stops, and
   everything before it can go.

   The run would otherwise be every byte since the last joiner, and a document
   that needs no joiner keeps all of itself: json's nested brackets would take a
   boundary test over the whole output at every token. Dropping a settled prefix
   cannot move a boundary, so the bytes [decode] writes are the same either
   way. *)
let live_suffix (lexer : Core.Lexer.t) (run : string) : string =
  let len = String.length run in
  let rec first_live (from : int) : int =
    if from >= len
    then len
    else (
      let ends, alive = token_end lexer run from in
      if alive then from else first_live ends)
  in
  match first_live 0 with
  | 0 -> run
  | from -> String.sub run ~pos:from ~len:(len - from)
;;

let decode (facts : Core.Facts.t) (map : map) (tokens : token list) : string =
  (* The map holds the automaton, and an automaton from another grammar would
     join these tokens by another grammar's rules and say nothing about it. *)
  if facts != map.facts
  then invalid_arg "Sample.decode: the map was not built from these facts";
  let out = Buffer.create 256 in
  let run = ref "" in
  List.iter tokens ~f:(fun (tok : token) ->
    let joiner =
      if Buffer.length out = 0
      then ""
      else (
        let keeps (joiner : string) : bool =
          boundary
            map.lexer
            (!run ^ joiner ^ tok.text)
            (String.length !run + String.length joiner)
        in
        (* Nothing, then a space, then a line break. The first that leaves the
           lexer a boundary wins, which is the order the formatter's fold takes
           over the same automaton. A grammar where none of the three works has
           no way to write the two tokens side by side. *)
        match List.find_opt ~f:keeps [ ""; " "; "\n" ] with
        | Some joiner -> joiner
        | None -> "\n")
    in
    Buffer.add_string out joiner;
    Buffer.add_string out tok.text;
    (* A blank inside a lexeme is not one this wrote, and the lexer does not
       stop at it. A line comment's tab and a block comment's newline are both
       bytes of a lexeme, so the run carries on through them and the next token
       still has to find a boundary past the whole comment. *)
    run := live_suffix map.lexer ((if joiner = "" then !run else "") ^ tok.text));
  Buffer.contents out
;;
