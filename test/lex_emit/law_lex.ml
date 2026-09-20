(* -- the emitted lexer --------------------------------------------------------

      (a) A lex rebuilds its input, byte for byte, whatever the input.
      (b) Every token spans at least one byte, and its kind is a token kind
          the grammar has.
      (c) A token's text is in the language of the token it was taken as. No
          token's language holds a longer prefix from where it started, and
          no token declared earlier holds the same text. An error or
          unterminated token starts where no token's language holds any
          prefix at all.
      (d) A lexeme the input ended inside leaves as one unterminated token
          spanning those bytes.
      (e) The emitted lexer and test/lex agree, token for token.

      Mechanism. Seven grammars, and for each of them a list of inputs
      written here. The emitted lexers are compiled and run: dune runs the
      generator, compiles what it wrote against lingo_runtime alone, and this
      links the result.

      Both shapes come out of one [Ocaml.Lexer] and both are checked. Parts
      (a) to (d) read the table, which {!Ocaml.Lexer.generate} emits by
      default. Part (e) reads the table and the match shape against test/lex,
      so both are held to one standard.

      Part (c)'s oracle is not a lexer. It evaluates each token's regex over
      the candidate text, so it says what the grammar's tokens denote with no
      automaton in between. A regex raises on malformed UTF-8 where the
      lexers decode it to U+FFFD, so a candidate that is not valid UTF-8 is
      counted and skipped, and the count prints.

      Part (e) is the differential, and its three sides reach the same tokens
      three ways. test/lex searches a state's interval list on every
      transition. The table maps the character to a class and indexes a row.
      The match shape branches on the byte. Anything lost in flattening the
      automaton, or in writing it out as control flow, shows up as a
      disagreement.

      Coverage. Seven grammars, 74 inputs, 258 tokens, two shapes. The counts
      print beside the result. A law with no unterminated token and no error
      token to look at proves nothing about either, so both counts have to
      read above zero.

      What this says nothing about. What the parser makes of the tokens;
      test/laws/law_interp.ml reads that, over the same lexer. Nor which
      shape to emit, which test/bench/bench_lex.ml measures.

      Falsification. Every mutation was applied, run and reverted, and the
      result recorded is the one observed. A count of inputs counts distinct
      inputs: one input failing in both shapes counts once.

        M1  In [Ocaml.Lexer.cells], pack the accept of the cell's own row
            rather than the destination's.
            -> part (c), 225 extents, and part (e), 59 of 74 inputs. Each
               state records the accept of the state before it. Part (a)
               reads 1, because the bytes still reach the stream, in tokens
               whose text is wrong.
        M2  In [Ocaml.Lexer.finish_body], send a run that ended inside a
            lexeme to the error arm.
            -> part (e), 4 inputs, and the count of unterminated tokens falls
               to zero, which is the line that fails. Part (a) reads zero:
               the bytes still reach the stream, one error token a codepoint.
               The unterminated token has its own arm for that reason, and
               the count sits beside the parts for the same one.
        M3  In [Ocaml.Lexer.finish_body], restart after an error token one
            byte on rather than one codepoint.
            -> part (e), 5 inputs across sexp and unicode. A multi-byte
               character splits across error tokens, so the stream holds
               characters the input never had. Part (a) reads zero again, and
               for the same reason: the bytes are all still there.
        M4  In [Ocaml.Lexer.intern_item], cut an interned token's text to its
            first byte.
            -> part (a), 20 inputs, part (c), 64 extents, and part (e), 20.
               The stream holds the interned record, so the record's text is
               the token's text.
        M5  In [Ocaml.Lexer.scan_body], start the search one segment above
            the one holding U+0080.
            -> parts (a), (c) and (e), 1 input: unicode's "«©»". The search
               skips the segment U+00A9 is in and lands on the one after
               it, which is [«]'s own class, so the copyright sign lexes as a
               left guillemet.

               The first run of this reddened nothing, which was a finding
               about the corpus. Every input above U+007F was a character its
               grammar had no token for, and a wrong class and no class stop
               the scan in the same place. "«©»" separates them.
        M6  In [Ocaml.Lexer.table_items], build the ASCII table from
            [class_of (cp + 1)].
            -> parts (a), (c) and (e): 16, 157 and 53. Every ASCII character
               reads its neighbour's class, and the unterminated count falls
               to zero as well.
        M7  In [Ocaml.Lexer.cells], store the destination state rather than
            its row.
            -> parts (a), (c) and (e): 33, 129 and 55. The row is the
               destination times the class count, and the scan indexes with
               an add.
        M8  In [Core.Lexer.of_facts], take the last accepting case id rather
            than the first.
            -> part (c), 9 extents, part (e), 7 inputs, and part (d) of
               test/laws/law_lexer.ml, 3 states. [let] lexes as [name], so a
               keyword stops beating an identifier spelled the same way.
        M9  In [Ocaml.Lexer.take], shift the cell one bit too few.
            -> parts (a), (b), (c) and (e): 8, 6, 79 and 37. The row and the
               accept share one int, and shifting at the wrong bit mixes the
               two.
        M10 In [Ocaml.Lexer.condition], end every run one character short.
            -> nothing. [condition] writes the match shape's arms above
               U+007F, and no input here ends a token on the last character
               of one of those ranges. That is a finding about the corpus.
               Its non-ASCII inputs are guillemets, Greek and one emoji, and
               none of them sits at the top of a range the grammar names.
        M11 In [Ocaml.Lexer.pattern_of], end every run one character short.
            -> nothing here, because it does not compile. The arms stop
               covering the byte and the exhaustiveness check says so. The
               match shape has that and the table does not: the compiler
               reads the dispatch.
        M12 In the wide arm of [Ocaml.Lexer.state_body_match], advance one
            byte rather than the decoded length.
            -> part (e), 12 inputs. A multi-byte character is read as its own
               first byte and then again from the middle.
        M13 In [Ocaml.Lexer.self_arm], go to [finish] after the run rather
            than back through the state.
            -> part (e), 15 inputs. The run is consumed and nothing then reads
               what ended it, so an accepting state stops where it started
               and every run comes out as a one-character token.
   -------------------------------------------------------------------------- *)

let failures = ref 0

let fail : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt ->
  Format.kasprintf
    (fun s ->
       incr failures;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt -> Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt
;;

(* -- the corpus ------------------------------------------------------------ *)

type case =
  { name : string
  ; grammar : Core.Grammar.t
  ; lex : string -> Lingo_runtime.Token.t array (** The table shape. *)
  ; matched : string -> Lingo_runtime.Token.t array
    (** A function per state, dispatched by match. *)
  ; inputs : string list
  }

let corpus : case list =
  [ { name = "sexp"
    ; grammar = Lingo_grammars.Sexp_grammar.grammar
    ; lex = Emitted.Sexp_lexer.lex
    ; matched = Emitted.Sexp_match.lex
    ; inputs =
        [ ""
        ; "(a b)"
        ; "(a (b 12) c)"
        ; "( a  b )"
        ; "(a\n b)"
        ; "12.5 -3"
        ; "-1.5"
        ; "a-b"
        ; "<=>"
        ; "  )"
        ; "."
        ; "-."
        ; "(a ) b"
        ; "\xff"
        ; "(\xc3"
          (* Characters sexp has no token for, one two bytes wide and one
             three. An error token covers a whole character, and no other
             input here is wide enough to show it. *)
        ; "(\xc3\xa9)"
        ; "(a \xe2\x86\x92 b)"
        ]
    }
  ; { name = "json"
    ; grammar = Lingo_grammars.Json_grammar.grammar
    ; lex = Emitted.Json_lexer.lex
    ; matched = Emitted.Json_match.lex
    ; inputs =
        [ "{\"a\": 1}"
        ; "[1, 2]"
        ; "1e10"
        ; "-0.5E+3"
        ; "\"a\\\"b\""
        ; "true false null"
        ; "truer"
        ; "[1,]"
        ; "@"
        ; "0123" (* The next four end inside a lexeme, which part (d) reads. *)
        ; "\""
        ; "\"abc"
        ; "{\"a\": \"b\\"
        ; "[\"x\", \"y"
        ]
    }
  ; { name = "calc"
    ; grammar = Lingo_grammars.Calc_grammar.grammar
    ; lex = Emitted.Calc_lexer.lex
    ; matched = Emitted.Calc_match.lex
    ; inputs = [ "1+2*3"; "(1+2)*3"; "1 - 2"; "12/4"; "1+"; "$"; "1 2" ]
    }
  ; { name = "rassoc"
    ; grammar = Lingo_grammars.Rassoc_grammar.grammar
    ; lex = Emitted.Rassoc_lexer.lex
    ; matched = Emitted.Rassoc_match.lex
    ; inputs = [ "1^2^3"; "1+2+3"; "-1^2"; "1++"; "^"; "1^" ]
    }
  ; { name = "postfix"
    ; grammar = Lingo_grammars.Postfix_grammar.grammar
    ; lex = Emitted.Postfix_lexer.lex
    ; matched = Emitted.Postfix_match.lex
    ; inputs = [ "a.b[2]?+1"; "a(1, 2)"; "a{1}"; "a()"; "a."; "a[1"; "ab12"; "%" ]
    }
  ; { name = "shapes"
    ; grammar = Lingo_grammars.Shapes_grammar.grammar
    ; lex = Emitted.Shapes_lexer.lex
    ; matched = Emitted.Shapes_match.lex
    ; inputs =
        [ "let a"
        ; "let a = b, c"
        ; "{ let a; let b }"
        ; "{ let a end }"
          (* [let] and [end] are keywords and [name] takes the same letters.
             Part (c) checks both here: which token wins on equal text, and
             that no longer match was available. *)
        ; "letter"
        ; "lets end ending"
        ; "@ let a"
        ; "}"
        ; "#"
        ]
    }
  ; { name = "unicode"
    ; grammar = Lingo_grammars.Unicode_grammar.grammar
    ; lex = Emitted.Unicode_lexer.lex
    ; matched = Emitted.Unicode_match.lex
    ; inputs =
        [ "\xc2\xabhello\xc2\xbb"
        ; "\xc2\xab\xc3\xa9t\xc3\xa9\xc2\xbb"
        ; "\xc2\xab\xce\xb1\xce\xb2\xce\xb3\xc2\xbb"
        ; "\xc2\xaba \xe2\x86\x92 b\xc2\xbb"
        ; "\xc2\xab\xc2\xbb"
        ; "\xc2\xab$\xc2\xbb"
        ; "\xc2\xab"
        ; "\xe2\x86\x92"
          (* A truncated lead byte and a lone continuation byte. Both decode
             to U+FFFD, and the bytes still have to reach the stream. *)
        ; "\xc2"
        ; "\xab\xc2\xab"
          (* Hebrew is outside every range the grammar names and U+1F600 is
             past the basic plane, so the class search runs its full depth.
             U+00A9 is the first codepoint above the ASCII table, and a
             search that starts too high reads it as the character after
             it. *)
        ; "\xc2\xab\xd7\x90\xc2\xbb"
        ; "\xc2\xab\xf0\x9f\x98\x80\xc2\xbb"
        ; "\xc2\xab\xc2\xa9\xc2\xbb"
        ]
    }
  ]
;;

(* -- counters -------------------------------------------------------------- *)

let inputs = ref 0
let tokens = ref 0
let extents = ref 0
let skipped = ref 0
let unterminated = ref 0
let errors = ref 0

(* -- the oracle ------------------------------------------------------------ *)

(* The token ids whose language holds [src.\[lo .. hi)]. [None] where those
   bytes are not valid UTF-8, which a regex refuses to read. *)
let holders (asts : Redfa.Ast.t array) (src : string) (lo : int) (hi : int)
  : int list option
  =
  let text = String.sub src lo (hi - lo) in
  if not (String.is_valid_utf_8 text)
  then None
  else (
    let out = ref [] in
    Array.iteri
      (fun (token_id : int) (ast : Redfa.Ast.t) ->
         if Redfa.Ast.eval ast text then out := token_id :: !out)
      asts;
    Some (List.rev !out))
;;

(* Whether any token's language holds a prefix from [lo] ending past [from]. *)
let longer (asts : Redfa.Ast.t array) (src : string) (lo : int) (from : int) : int option =
  let rec go hi =
    if hi > String.length src
    then None
    else (
      match holders asts src lo hi with
      | Some (_ :: _) -> Some hi
      | Some [] | None -> go (hi + 1))
  in
  go (from + 1)
;;

(* -- the parts ------------------------------------------------------------- *)

let show (src : string) : string =
  if String.length src > 24 then String.sub src 0 24 ^ "…" else src
;;

let part_a (name : string) (src : string) (ts : Lingo_runtime.Token.t array) : unit =
  let rebuilt = Array.fold_left (fun b (t : Lingo_runtime.Token.t) -> b ^ t.text) "" ts in
  if not (String.equal rebuilt src)
  then fail "%s: %S rebuilds as %S" name (show src) (show rebuilt)
;;

let part_b
      (facts : Core.Facts.t)
      (name : string)
      (src : string)
      (ts : Lingo_runtime.Token.t array)
  : unit
  =
  Array.iter
    (fun (t : Lingo_runtime.Token.t) ->
       incr tokens;
       if String.length t.text = 0 then fail "%s: %S has an empty token" name (show src);
       let k = List.nth (Core.Kind.Table.kinds facts.kinds) t.kind in
       if
         Core.Facts.token_of_kind facts k = None
         && not
              (Core.Kind.equal k facts.error_token_kind
               || Core.Kind.equal k facts.unterminated_kind)
       then
         fail
           "%s: %S holds kind %s"
           name
           (show src)
           (Core.Kind.Name.to_string (Core.Facts.kind_name facts k)))
    ts
;;

let part_c
      (facts : Core.Facts.t)
      (asts : Redfa.Ast.t array)
      (name : string)
      (src : string)
      (ts : Lingo_runtime.Token.t array)
  : unit
  =
  let lo = ref 0 in
  Array.iter
    (fun (t : Lingo_runtime.Token.t) ->
       let hi = !lo + String.length t.text in
       let kinds = Core.Kind.Table.kinds facts.kinds in
       let kind = List.nth kinds t.kind in
       (match holders asts src !lo hi with
        | None -> incr skipped
        | Some ids ->
          incr extents;
          (match Core.Facts.token_of_kind facts kind with
           | None ->
             (* An error or unterminated token: nothing matched from here. *)
             (match longer asts src !lo !lo with
              | None -> ()
              | Some e ->
                fail "%s: %S has no token at %d, and one ends at %d" name (show src) !lo e)
           | Some (def : Core.Token.def) ->
             if not (List.mem def.id ids)
             then
               fail
                 "%s: %S takes %S as %s, which does not match it"
                 name
                 (show src)
                 t.text
                 (Core.Kind.Name.to_string (Core.Facts.kind_name facts kind));
             (match List.find_opt (fun (token_id : int) -> token_id < def.id) ids with
              | None -> ()
              | Some earlier ->
                fail
                  "%s: %S takes %S as token %d, and token %d also holds it"
                  name
                  (show src)
                  t.text
                  def.id
                  earlier);
             (match longer asts src !lo hi with
              | None -> ()
              | Some e ->
                fail
                  "%s: %S takes %d..%d, and a longer match ends at %d"
                  name
                  (show src)
                  !lo
                  hi
                  e)));
       lo := hi)
    ts
;;

let part_d
      (facts : Core.Facts.t)
      (name : string)
      (src : string)
      (ts : Lingo_runtime.Token.t array)
  : unit
  =
  Array.iteri
    (fun i (t : Lingo_runtime.Token.t) ->
       let kind = List.nth (Core.Kind.Table.kinds facts.kinds) t.kind in
       if Core.Kind.equal kind facts.error_token_kind then incr errors;
       if Core.Kind.equal kind facts.unterminated_kind
       then (
         incr unterminated;
         if i <> Array.length ts - 1
         then fail "%s: %S continues past an unterminated token" name (show src)))
    ts
;;

let show_kinds (ts : Lingo_runtime.Token.t array) : string =
  String.concat
    " "
    (Array.to_list
       (Array.map
          (fun (t : Lingo_runtime.Token.t) -> Printf.sprintf "%d:%S" t.kind t.text)
          ts))
;;

let part_e
      (name : string)
      (shape : string)
      (src : string)
      (emitted : Lingo_runtime.Token.t array)
      (walked : Lingo_runtime.Token.t array)
  : unit
  =
  if
    Array.length emitted <> Array.length walked
    || not
         (Array.for_all2
            (fun (a : Lingo_runtime.Token.t) (b : Lingo_runtime.Token.t) ->
               a.kind = b.kind && String.equal a.text b.text)
            emitted
            walked)
  then
    fail
      "%s/%s: %S lexes as [%s] here and [%s] over the automaton"
      name
      shape
      (show src)
      (show_kinds emitted)
      (show_kinds walked)
;;

(* -- running --------------------------------------------------------------- *)

let () =
  List.iter
    (fun (case : case) ->
       match Core.Facts.of_grammar case.grammar with
       | Error errors ->
         List.iter
           (fun (error : Core.Error.t) -> Format.printf "%a@." Core.Error.pp error)
           errors;
         fail "%s: the grammar does not check" case.name
       | Ok facts ->
         let asts =
           Array.map
             (fun (token : Core.Token.def) -> Redfa.Regex.to_ast token.regex)
             facts.tokens
         in
         let table = Core.Lexer.of_facts facts in
         Printf.printf
           "; %-8s states %d  classes %d  cells %d\n"
           case.name
           table.num_states
           table.num_classes
           (Array.length table.next);
         List.iter
           (fun (src : string) ->
              incr inputs;
              let emitted = case.lex src in
              let walked = Lex.run facts src in
              part_a case.name src emitted;
              part_b facts case.name src emitted;
              part_c facts asts case.name src emitted;
              part_d facts case.name src emitted;
              part_e case.name "table" src emitted walked;
              part_e case.name "match" src (case.matched src) walked)
           case.inputs)
    corpus
;;

let () =
  if !unterminated = 0 then fail "no input ended inside a lexeme";
  if !errors = 0 then fail "no input held a character no token starts with";
  pass
    "%d inputs, %d tokens, %d extents checked (%d skipped as malformed UTF-8)"
    !inputs
    !tokens
    !extents
    !skipped;
  pass "%d unterminated tokens, %d error tokens" !unterminated !errors;
  if !failures > 0
  then (
    Printf.printf "%d failures\n" !failures;
    exit 1)
;;
