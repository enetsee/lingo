open StdLabels

(* The automaton is emitted as integers. Generated code links [lingo_runtime]
   and nothing else of ours, so it has no [Ucharset] to decode a class set
   with. The scan reads which class a codepoint is in, and [Core.Lexer]
   flattens the sets into that table. *)

type shape =
  | Table
  | Match

let int_array (xs : int array) : Emit.expr =
  Emit.earray (Array.to_list (Array.map xs ~f:Emit.eint))
;;

let kind_int (kind_opt : Core.Kind.t option) : int =
  Option.fold kind_opt ~none:~-1 ~some:(fun (kind : Core.Kind.t) -> Core.Kind.to_int kind)
;;

let get (array : string) (index : Emit.expr) : Emit.expr =
  Emit.ecall "Array.unsafe_get" [ Emit.evar array; index ]
;;

let add (left : Emit.expr) (right : Emit.expr) : Emit.expr =
  Emit.ecall "+" [ left; right ]
;;

(* -- what both shapes share ------------------------------------------------ *)

(* One preallocated token per kind whose text is fixed, [None] for a pattern
   token and for every kind the lexer never emits. It is indexed by kind, so
   the lookup is an array read.

   A grammar of pattern tokens alone fills the array with [None]. Nothing
   else in the emitted module gives its type, so it is annotated. *)
let intern_item (facts : Core.Facts.t) : Emit.item =
  let cells = Array.make (Core.Facts.kind_count facts) (Emit.econstruct "None" []) in
  Array.iter facts.Core.Facts.tokens ~f:(fun (token : Core.Token.def) ->
    match Core.Token.text token with
    | None -> ()
    | Some text ->
      let kind = Core.Kind.to_int token.Core.Token.kind in
      cells.(kind)
      <- Emit.econstruct
           "Some"
           [ Emit.erecord
               [ "Lingo_runtime.Token.kind", Emit.eint kind; "text", Emit.estr text ]
           ]);
  Emit.ilet
    "intern"
    (Emit.econstraint
       (Emit.earray (Array.to_list cells))
       (Emit.tcon "array" [ Emit.tcon "option" [ Emit.tcon "Lingo_runtime.Token.t" [] ] ]))
;;

(* [emit kind lo hi] puts the token spanning [lo .. hi) into the output. *)
let emit_binding : Emit.expr =
  let open Emit in
  elambda
    [ arg_typed ~arg_name:"kind" ~type_path:"int"
    ; arg_typed ~arg_name:"lo" ~type_path:"int"
    ; arg_typed ~arg_name:"hi" ~type_path:"int"
    ]
    (ematch
       (get "intern" (evar "kind"))
       [ ecase
           (pconstruct "Some" [ pvar "t" ])
           (ecall "Dynarray.add_last" [ evar "out"; evar "t" ])
       ; ecase
           (pconstruct "None" [])
           (ecall
              "Dynarray.add_last"
              [ evar "out"
              ; erecord
                  [ "Lingo_runtime.Token.kind", evar "kind"
                  ; ( "text"
                    , ecall
                        "String.sub"
                        [ evar "src"; evar "lo"; ecall "-" [ evar "hi"; evar "lo" ] ] )
                  ]
              ])
       ])
;;

let finish_args : Emit.arg list =
  [ Emit.arg_typed ~arg_name:"pos" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"i" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"kind" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"stop" ~type_path:"int"
  ]
;;

let finish_call : Emit.expr =
  Emit.ecall
    "finish"
    [ Emit.evar "pos"; Emit.evar "i"; Emit.evar "kind"; Emit.evar "stop" ]
;;

(* What a run that has ended leaves behind. The longest match, if the scan
   found one. Otherwise an error token or an unterminated one. *)
let finish_body (facts : Core.Facts.t) : Emit.expr =
  let open Emit in
  let width =
    ecall
      "Uchar.utf_decode_length"
      [ ecall "String.get_utf_8_uchar" [ evar "src"; evar "pos" ] ]
  in
  let error =
    elet
      "w"
      ~body:width
      ~rest:
        (eseq
           [ ecall
               "emit"
               [ eint (Core.Kind.to_int facts.Core.Facts.error_token_kind)
               ; evar "pos"
               ; add (evar "pos") (evar "w")
               ]
           ; ecall "start" [ add (evar "pos") (evar "w") ]
           ])
  in
  let unterminated =
    ecall
      "emit"
      [ eint (Core.Kind.to_int facts.Core.Facts.unterminated_kind); evar "pos"; evar "n" ]
  in
  eif
    ~condition:(egreater_equal ~left:(evar "kind") ~right:(eint 0))
    ~then_:
      (eseq
         [ ecall "emit" [ evar "kind"; evar "pos"; evar "stop" ]
         ; ecall "start" [ evar "stop" ]
         ])
    ~else_:
      (eif
         ~condition:(egreater_equal ~left:(evar "i") ~right:(evar "n"))
         ~then_:unterminated
         ~else_:error)
;;

(* [lex] around whichever cluster the shape gave. Everything outside the scan
   is here, so the shapes differ only in the scan. *)
let lex_item (facts : Core.Facts.t) (cluster : (string * Emit.arg list * Emit.expr) list)
  : Emit.item
  =
  let open Emit in
  let body =
    elet_rec
      (cluster @ [ "finish", finish_args, finish_body facts ])
      (eseq [ ecall "start" [ eint 0 ]; ecall "Dynarray.to_array" [ evar "out" ] ])
  in
  ilet
    ~args:[ arg_typed ~arg_name:"src" ~type_path:"string" ]
    "lex"
    (elet
       "n"
       ~body:(ecall "String.length" [ evar "src" ])
       ~rest:
         (elet
            "out"
            ~body:(ecall "Dynarray.create" [ eunit ])
            ~rest:(elet "emit" ~body:emit_binding ~rest:body)))
;;

(* -- the table shape ------------------------------------------------------- *)

(* Bits enough to hold [0 .. n]. *)
let bits_for (n : int) : int =
  let rec go (b : int) : int = if 1 lsl b > n then b else go (b + 1) in
  go 0
;;

(* One cell per state and class. It packs the row the character moves to
   together with the kind that row's state accepts, so the scan loads once and
   shifts. In two arrays the second load waits on the first, because the first
   says which accept to read.

   The row is the destination already multiplied by the class count, so the
   scan indexes with an add. The accept is stored one above the kind. That
   leaves zero for a state accepting nothing, and [-1] for a cell no
   character reaches. *)
let cells (table : Core.Lexer.t) (bits : int) : int array =
  let { Core.Lexer.num_classes; next; accept; _ } = table in
  Array.map next ~f:(fun (dest : int) ->
    if dest < 0
    then -1
    else ((dest * num_classes) lsl bits) lor (kind_int accept.(dest) + 1))
;;

(* The first 128 codepoints are one byte of UTF-8 each and index a table
   directly. Everything above searches the segments. [Core.Lexer.class_of]
   fills the table in, so the two agree. *)
let table_items (table : Core.Lexer.t) (bits : int) : Emit.item list =
  let { Core.Lexer.segments; segment_class; _ } = table in
  [ Emit.ilet "segments" (int_array segments)
  ; Emit.ilet "segment_class" (int_array segment_class)
  ; Emit.ilet
      "ascii_class"
      (int_array
         (Array.init 128 ~f:(fun (codepoint : int) -> Core.Lexer.class_of table codepoint)))
  ; Emit.ilet "step" (int_array (cells table bits))
  ]
;;

(* The segment holding U+0080. The ASCII table covers every codepoint below
   it, so the search starts here. *)
let first_high (table : Core.Lexer.t) : int =
  let found = ref 0 in
  Array.iteri table.Core.Lexer.segments ~f:(fun (index : int) (lo : int) ->
    if lo <= 128 then found := index);
  !found
;;

(* [class_search cp lo hi] is the last segment starting at or below [cp]. The
   segments cover the codespace, so the search always finds one and the
   caller checks no bounds.

   The scan reads [ascii_class] inline, so a codepoint below U+0080 costs one
   array read and no call. *)
let class_items : Emit.item list =
  let open Emit in
  let mid = ecall "/" [ add (add (evar "lo") (evar "hi")) (eint 1); eint 2 ] in
  [ ilet
      ~rec_:true
      ~args:
        [ arg_typed ~arg_name:"cp" ~type_path:"int"
        ; arg_typed ~arg_name:"lo" ~type_path:"int"
        ; arg_typed ~arg_name:"hi" ~type_path:"int"
        ]
      "class_search"
      (eif
         ~condition:(egreater_equal ~left:(evar "lo") ~right:(evar "hi"))
         ~then_:(get "segment_class" (evar "lo"))
         ~else_:
           (elet
              "mid"
              ~body:mid
              ~rest:
                (eif
                   ~condition:
                     (eless_equal ~left:(get "segments" (evar "mid")) ~right:(evar "cp"))
                   ~then_:(ecall "class_search" [ evar "cp"; evar "mid"; evar "hi" ])
                   ~else_:
                     (ecall
                        "class_search"
                        [ evar "cp"; evar "lo"; ecall "-" [ evar "mid"; eint 1 ] ]))))
  ]
;;

(* The scan reads these on every character and never writes them. As
   arguments they stay in registers across the loop's own branch. As free
   variables they cost two loads from the closure and two more through the
   module's block, on every character. *)
let carried : string list = [ "src"; "n"; "step"; "ascii_class" ]

let scan_args : Emit.arg list =
  [ Emit.arg_typed ~arg_name:"pos" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"row" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"i" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"kind" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"stop" ~type_path:"int"
  ]
  @ List.map carried ~f:Emit.arg_var
;;

(* What one character does, once its cell and its width are in hand. Both arms
   of the scan end here, and the emitter writes it into each. Sharing one
   function between them would add a call to the fast arm.

   The accept is the destination's, so a token is only recorded once a
   character has been consumed. Reading the state stepped out of would accept
   the empty string in the initial state. *)
let take (bits : int) ~(cell : Emit.expr) ~(width : Emit.expr) : Emit.expr =
  let open Emit in
  let go (kind : Emit.expr) (stop : Emit.expr) =
    ecall
      "scan"
      ([ evar "pos"; evar "dest"; evar "j"; kind; stop ] @ List.map carried ~f:evar)
  in
  elet
    "cell"
    ~body:cell
    ~rest:
      (eif
         ~condition:(eless ~left:(evar "cell") ~right:(eint 0))
         ~then_:finish_call
         ~else_:
           (elet
              "j"
              ~body:(add (evar "i") width)
              ~rest:
                (elet
                   "dest"
                   ~body:(ecall "lsr" [ evar "cell"; eint bits ])
                   ~rest:
                     (elet
                        "a"
                        ~body:
                          (ecall
                             "-"
                             [ ecall "land" [ evar "cell"; eint ((1 lsl bits) - 1) ]
                             ; eint 1
                             ])
                        ~rest:
                          (eif
                             ~condition:(eless ~left:(evar "a") ~right:(eint 0))
                             ~then_:(go (evar "kind") (evar "stop"))
                             ~else_:(go (evar "a") (evar "j")))))))
;;

(* One step of the automaton. [kind] and [stop] carry the last accepting state
   passed, which is the match taken when the run ends.

   A byte below 0x80 is a one-byte UTF-8 sequence whose codepoint is the byte
   itself. The fast arm reads the byte, indexes the ASCII class table and
   advances by one, so ASCII source never reaches the UTF-8 decoder. The slow
   arm decodes and searches. *)
let scan_body (table : Core.Lexer.t) (bits : int) : Emit.expr =
  let open Emit in
  let ascii =
    take
      bits
      ~cell:(get "step" (add (evar "row") (get "ascii_class" (evar "b"))))
      ~width:(eint 1)
  in
  let wide =
    elet
      "d"
      ~body:(ecall "String.get_utf_8_uchar" [ evar "src"; evar "i" ])
      ~rest:
        (elet
           "c"
           ~body:
             (ecall
                "class_search"
                [ ecall "Uchar.to_int" [ ecall "Uchar.utf_decode_uchar" [ evar "d" ] ]
                ; eint (first_high table)
                ; ecall "-" [ ecall "Array.length" [ evar "segments" ]; eint 1 ]
                ])
           ~rest:
             (take
                bits
                ~cell:
                  (eif
                     ~condition:(eless ~left:(evar "c") ~right:(eint 0))
                     ~then_:(eint (-1))
                     ~else_:(get "step" (add (evar "row") (evar "c"))))
                ~width:(ecall "Uchar.utf_decode_length" [ evar "d" ])))
  in
  eif
    ~condition:(egreater_equal ~left:(evar "i") ~right:(evar "n"))
    ~then_:finish_call
    ~else_:
      (elet
         "b"
         ~body:(ecall "Char.code" [ ecall "String.unsafe_get" [ evar "src"; evar "i" ] ])
         ~rest:
           (eif
              ~condition:(eless ~left:(evar "b") ~right:(eint 128))
              ~then_:ascii
              ~else_:wide))
;;

let table_cluster (table : Core.Lexer.t) (bits : int)
  : (string * Emit.arg list * Emit.expr) list
  =
  let open Emit in
  [ ( "start"
    , [ arg_typed ~arg_name:"pos" ~type_path:"int" ]
    , ewhen
        ~condition:(eless ~left:(evar "pos") ~right:(evar "n"))
        ~then_:
          (ecall
             "scan"
             ([ evar "pos"
              ; eint (Core.Lexer.initial * table.Core.Lexer.num_classes)
              ; evar "pos"
              ; eint (-1)
              ; evar "pos"
              ]
              @ List.map carried ~f:evar)) )
  ; "scan", scan_args, scan_body table bits
  ]
;;

(* -- a function per state, dispatched by pattern match ---------------------- *)

let state_name (state : int) : string = Printf.sprintf "st_%d" state

let state_args : Emit.arg list =
  [ Emit.arg_typed ~arg_name:"pos" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"i" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"kind" ~type_path:"int"
  ; Emit.arg_typed ~arg_name:"stop" ~type_path:"int"
  ]
;;

(* "[read] is in this set", as a chain of interval tests. *)
let condition (read : string) (set : Ucharset.t) : Emit.expr =
  let open Emit in
  let one (lo, hi) =
    if lo = hi
    then eequal ~left:(evar read) ~right:(eint lo)
    else
      eand
        ~left:(egreater_equal ~left:(evar read) ~right:(eint lo))
        ~right:(eless_equal ~left:(evar read) ~right:(eint hi))
  in
  match Ucharset.to_intervals set with
  | [] -> ebool false
  | first :: rest ->
    List.fold_left
      rest
      ~init:(one first)
      ~f:(fun (acc : Emit.expr) (interval : int * int) ->
        eor ~left:acc ~right:(one interval))
;;

(* The chain for one side of the split. [width] is the step: a literal one
   below U+0080, the decoded length above it. *)
let chain (read : string) (width : Emit.expr) (arms : (Ucharset.t * int) list) : Emit.expr
  =
  let open Emit in
  List.fold_right
    arms
    ~init:finish_call
    ~f:(fun ((set, dest) : Ucharset.t * int) (rest : Emit.expr) ->
      eif
        ~condition:(condition read set)
        ~then_:
          (ecall
             (state_name dest)
             [ evar "pos"; add (evar "i") width; evar "kind"; evar "stop" ])
        ~else_:rest)
;;

(* The state body splits at one byte of UTF-8 and writes a chain on each side.
   [transitions_in] clips the list to the side being written, so each arm is
   written once. *)
let clipped (dfa : Redfa.Dfa.t) (state : int) ~(lo : int) ~(hi : int)
  : (Ucharset.t * int) list
  =
  List.map
    (Redfa.Dfa.transitions_in dfa state ~lo ~hi)
    ~f:(fun ((set, dest) : Ucharset.t * int) ->
      Ucharset.inter set (Ucharset.range ~lo ~hi), dest)
;;

(* The set as a pattern: one alternative per run, and a run of one character
   as that character. *)
let pattern_of (set : Ucharset.t) : Emit.pat option =
  let one (lo, hi) =
    let c n = Char.chr n in
    if lo = hi then Emit.pchar (c lo) else Emit.pchar_range ~lo:(c lo) ~hi:(c hi)
  in
  match Ucharset.to_intervals set with
  | [] -> None
  | first :: rest -> Some (Emit.por (one first) (List.map rest ~f:one))
;;

let ascii : Ucharset.t = Ucharset.range ~lo:0 ~hi:0x7F

(* An identifier, a run of whitespace and a number all leave a state through
   an arm that comes back to it. A transition between states is a jump to a
   function's entry, which re-runs the stack check and sets up a frame, and a
   state looping to itself pays that on every character of the run.

   So the self arm consumes the whole run in a loop inside the function, and
   re-enters the state once. The character the loop stopped on is outside the
   self set, so that visit takes one of the other arms.

   The run sits inside the arm. In front of the dispatch it costs a test on
   every character of a state whose run never fires, and that measured slower
   than the re-entry it saves. *)
let run_name (state : int) : string = Printf.sprintf "st_%d_run" state

let self_set (dfa : Redfa.Dfa.t) (state : int) : Ucharset.t =
  Ucharset.union_list
    (List.filter_map
       (clipped dfa state ~lo:0 ~hi:0x7F)
       ~f:(fun ((set, dest) : Ucharset.t * int) ->
         if dest = state then Some set else None))
;;

(* [st_N_run j] is the end of the run of self characters starting at [j]. Its
   match branches straight to the next step. A version giving a boolean has to
   reach a [true] first and let the caller branch on that, and over seven runs
   the extra step showed.

   It sits in the cluster. A local [let rec] closing over [src] and [n]
   allocates a closure every time the arm is taken, which measured at twelve
   to eighteen bytes a token. *)
let run_binding (self : Ucharset.t) (state : int)
  : (string * Emit.arg list * Emit.expr) option
  =
  let open Emit in
  Option.map
    (fun (pat : Emit.pat) ->
       ( run_name state
       , [ arg_typed ~arg_name:"j" ~type_path:"int" ]
       , eif
           ~condition:(egreater_equal ~left:(evar "j") ~right:(evar "n"))
           ~then_:(evar "j")
           ~else_:
             (ematch
                (ecall "String.unsafe_get" [ evar "src"; evar "j" ])
                [ ecase pat (ecall (run_name state) [ add (evar "j") (eint 1) ])
                ; ecase pany (evar "j")
                ]) ))
    (pattern_of self)
;;

let self_arm (self : Ucharset.t) (state : int) : Emit.expr option =
  let open Emit in
  Option.map
    (fun (_ : Emit.pat) ->
       ecall
         (state_name state)
         [ evar "pos"
         ; ecall (run_name state) [ add (evar "i") (eint 1) ]
         ; evar "kind"
         ; evar "stop"
         ])
    (pattern_of self)
;;

let state_body_match (facts : Core.Facts.t) (dfa : Redfa.Dfa.t) (state : int) : Emit.expr =
  let open Emit in
  let ascii_arms = clipped dfa state ~lo:0 ~hi:0x7F in
  let self = self_set dfa state in
  let other =
    List.filter ascii_arms ~f:(fun ((_, dest) : Ucharset.t * int) -> dest <> state)
  in
  let wide_arms = clipped dfa state ~lo:0x80 ~hi:Ucharset.max_codepoint in
  let wide =
    match wide_arms with
    | [] -> finish_call
    | arms ->
      elet
        "d"
        ~body:(ecall "String.get_utf_8_uchar" [ evar "src"; evar "i" ])
        ~rest:
          (elet
             "cp"
             ~body:(ecall "Uchar.to_int" [ ecall "Uchar.utf_decode_uchar" [ evar "d" ] ])
             ~rest:(chain "cp" (ecall "Uchar.utf_decode_length" [ evar "d" ]) arms))
  in
  let arm (set, dest) =
    Option.map
      (fun (pat : Emit.pat) ->
         ecase
           pat
           (ecall
              (state_name dest)
              [ evar "pos"; add (evar "i") (eint 1); evar "kind"; evar "stop" ]))
      (pattern_of set)
  in
  (* A lead byte is never an ASCII arm, so the two never overlap. The catch-all
     goes on only where the arms leave a byte over: with all 128 spoken for it
     would be an arm nothing reaches, which does not compile. The self arms are
     not among them; the loop above has eaten those. *)
  let covered =
    Ucharset.subset ascii ~of_:(Ucharset.union_list (self :: List.map other ~f:fst))
  in
  let self_case =
    match self_arm self state, pattern_of self with
    | Some body, Some pat -> [ ecase pat body ]
    | _ -> []
  in
  let cases =
    self_case
    @ List.filter_map other ~f:arm
    @ [ ecase (pchar_range ~lo:'\128' ~hi:'\255') wide ]
    @ if covered then [] else [ ecase pany finish_call ]
  in
  let read =
    eif
      ~condition:(egreater_equal ~left:(evar "i") ~right:(evar "n"))
      ~then_:finish_call
      ~else_:(ematch (ecall "String.unsafe_get" [ evar "src"; evar "i" ]) cases)
  in
  match Redfa.Dfa.accepts dfa state with
  | [] -> read
  | id :: _ ->
    elet
      "kind"
      ~body:(eint (Core.Kind.to_int (Core.Facts.token facts id).kind))
      ~rest:(elet "stop" ~body:(evar "i") ~rest:read)
;;

let match_cluster (facts : Core.Facts.t) : (string * Emit.arg list * Emit.expr) list =
  let open Emit in
  let dfa = facts.Core.Facts.lexer in
  let states = ref [] in
  Redfa.Dfa.iter_states dfa (fun (state : int) ->
    (match run_binding (self_set dfa state) state with
     | Some binding -> states := binding :: !states
     | None -> ());
    states := (state_name state, state_args, state_body_match facts dfa state) :: !states);
  ( "start"
  , [ arg_typed ~arg_name:"pos" ~type_path:"int" ]
  , ewhen
      ~condition:(eless ~left:(evar "pos") ~right:(evar "n"))
      ~then_:
        (ecall
           (state_name (Redfa.Dfa.initial dfa))
           [ evar "pos"; evar "pos"; eint (-1); evar "pos" ]) )
  :: List.rev !states
;;

(* -- the module ------------------------------------------------------------ *)

let generate ?(shape : shape = Table) (facts : Core.Facts.t) : Emit.item list =
  match shape with
  | Table ->
    let t = Core.Lexer.of_facts facts in
    let bits = bits_for (Core.Facts.kind_count facts) in
    table_items t bits
    @ [ intern_item facts ]
    @ class_items
    @ [ lex_item facts (table_cluster t bits) ]
  | Match -> [ intern_item facts; lex_item facts (match_cluster facts) ]
;;

let signature : Emit.sig_item list =
  [ Emit.sval
      "lex"
      (Emit.tarrow
         ~domain:(Emit.tcon "string" [])
         ~codomain:(Emit.tcon "array" [ Emit.tcon "Lingo_runtime.Token.t" [] ]))
  ]
;;
