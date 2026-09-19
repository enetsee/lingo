(* A list per law was three lists that drifted. law_parse had nine inputs the
   others lacked, law_interp two, and law_residual's was a subset of
   law_interp's missing twenty-one, so an input added for one law was not seen
   by the other two.

   Adding one here moves all three falsification records: law_parse generates
   its corpus from these, and a seed changes every input drawn after it. *)

type t =
  { good : string list
  ; broken : string list
  }

let all (t : t) : string list = t.good @ t.broken

let sexp : t =
  { good =
      [ "(a b)"; "(a (b 12) c)"; "(a (b (c)))"; "()"; "( a  b )"; "(a\n b)"; "(a b)  " ]
      (* The last three carry leading trivia in front of the token that fails,
         which is the case that put law_interp's part (e) here: the skip used
         to open its error node before taking that trivia, so the trivia
         landed inside the node rather than in the frame around it. *)
  ; broken = [ "("; "(a"; ")"; "(a ) b"; "(()"; "  )"; "( a  )  )"; "(  ]" ]
  }
;;

let json : t =
  { good =
      [ "1"
      ; "[1, 2]"
      ; "{\"a\": 1}"
      ; "[]"
      ; "{}"
      ; "[true, false, null]"
      ; "[{\"a\": [1]}]"
      ; "  [1]  "
      ]
      (* The last one ends inside a string. The lexer leaves those bytes as one
         unterminated token, so the parse has something to report. *)
  ; broken =
      [ "[1, 2,]"
      ; "[1 2]"
      ; "[1 : 2]"
      ; "{\"a\" 1}"
      ; "{\"a\": 1 \"b\": 2}"
      ; "[1"
      ; "{"
      ; "[{1 ]"
      ; "[1] junk"
      ; "{\"a\": \"b"
      ]
  }
;;

let calc : t =
  { good = [ "1"; "1+2*3"; "-1*2"; "-(1)"; "(1+2)*3"; "1-2-3"; " 1 + 2 " ]
  ; broken = [ "1+"; "1+*2"; "("; "(1"; "(1+2"; "1 2" ]
  }
;;

let rassoc : t =
  { good = [ "1"; "1^2^3"; "1^2+3^4"; "1+2+3"; "-1^2" ]; broken = [ "1^"; "^1"; "1++" ] }
;;

let postfix : t =
  { good = [ "a"; "a?"; "a.b"; "a[1]"; "a{1}"; "a(1, 2)"; "a()"; "a.b[2]?+1"; "a(1)(2)" ]
  ; broken = [ "a."; "a(1"; "a(1 2)"; "a[1"; "a(1,)"; "a["; "a.?" ]
  }
;;

(* Every token in this grammar is more than one byte in UTF-8, so a lexer that
   stepped a byte at a time would read none of them. *)
let unicode : t =
  { good =
      [ "\xc2\xabhello\xc2\xbb"
      ; "\xc2\xab\xc3\xa9t\xc3\xa9\xc2\xbb"
      ; "\xc2\xab\xce\xb1\xce\xb2\xce\xb3\xc2\xbb"
      ; "\xc2\xaba \xe2\x86\x92 b\xc2\xbb"
      ; "\xc2\xab\xc2\xbb"
      ; "\xc2\xab \xc3\xa9t\xc3\xa9 \xe2\x86\x92 \xce\xb1 \xc2\xbb"
      ]
  ; broken =
      [ "\xc2\xabhello"
      ; "hello\xc2\xbb"
      ; "\xc2\xab$\xc2\xbb"
      ; "\xc2\xab\xe2\x86\x92\xc2\xbb"
      ]
  }
;;

(* The parts of a recovery set the other grammars leave unexercised. The broken
   inputs are one per part, each at the position that reads it, and dropping
   that part changes the parse. grammars/recovery_grammar.ml says which is
   which. *)
let recovery : t =
  { good =
      [ ""
      ; "let a in end"
      ; "( let a in end )"
      ; "sig : a ; in"
      ; "sig : a ; , : b ; in"
      ; "let a in end ( let b in end )"
      ; "let a in end  "
      ]
  ; broken =
      [ (* [end] is in the commit's resume set and not in its recovery set. *)
        "let end"
      ; "let in"
      ; "let" (* The caller passes down [sig], and the boundary drops it. *)
      ; "( sig )"
      ; "( in let a in end )"
      ; "( let a in end" (* The child's [recover_to] replaces the computed set. *)
      ; "sig end in"
      ; "sig in"
      ; "sig" (* The separator reaches the element through the rule's adds. *)
      ; "sig : , : a ; in"
      ; "sig : a ; , in"
      ; ")"
      ; ","
      ; "end"
      ]
  }
;;

let shapes : t =
  { good =
      [ "let a"
      ; "let a = b"
      ; "let a = b, c"
      ; "{ let a }"
      ; "{ let a; let b }"
      ; "{ let a; }"
      ; "let a { let b }"
      ; "let a  "
      ; ""
      ]
      (* The last two turn on the resync anchor. [end] starts nothing, so a body
         without an anchor would sweep it up; [Block] declares it, so the body
         stops there instead. *)
  ; broken =
      [ "let"
      ; "{"
      ; "{ let }"
      ; "let a ="
      ; "{ let a; ; }"
      ; "}"
      ; "{ let a end"
      ; "{ let a end }"
        (* A root of repeated items recovers to the end of the input, so a stray
           token between two declarations costs a diagnostic rather than every
           declaration after it. *)
      ; "let a ; let b"
      ; "@ let a"
      ; "let a ; ; let b"
        (* The block's closer is missing, so the body's last run is what holds
           the tokens after it and has to measure them. *)
      ; "{c let a;; letlet b a}"
      ]
  }
;;

(* The formatter's inputs. Every comment here sits somewhere a boundary has to
   decide about: at the head of a body, between two elements, and at the end of
   the input with nothing to end its line. The long one is there to break, so
   the trailing separator that appears only on a break has an input that makes
   it appear. *)
let comments : t =
  { good =
      [ "[a, b]"
      ; "[a.b, 1]"
      ; "[]"
      ; "[[a], [b, c]]"
      ; "[ // first\n  a, b ]"
      ; "[a, // trailing\n  b ]"
      ; "[a /* mid */, b]"
      ; "[aaaa, bbbb, cccc, dddd]"
      ; "[a] // after"
      ; "[1 . 5]"
        (* The separator the source already has is the author asking for a broken
           body. Nothing else in a grammar can ask for that directly. *)
      ; "[a, b,]"
      ; "[[a, b,], c]"
      ]
      (* ["\[1 .5\]"] is the max-munch case: three tokens the source had apart,
         no two of which join, that are one number together. *)
  ; broken =
      [ "[1 .5]"
      ; "[a"
      ; "[a, // x"
      ; "[a,, b]"
      ; "[. a]"
      ; "[a b]"
      ; "/* open"
      ; "[a] junk"
        (* A [Field] with no name leaves a break asked for by a child that is
           not there. Carried on, the trailing separator takes it and lands on a
           line of its own. *)
      ; "[a/* mid *//* mid */.]"
      ]
  }
;;
