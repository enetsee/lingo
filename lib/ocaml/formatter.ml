open StdLabels

let ctor (name : string) (args : Emit.expr list) : Emit.expr =
  Emit.econstruct ("Lingo_runtime.Layout." ^ name) args
;;

let kinds (set : Ir.Kind.t array) : Emit.expr =
  Emit.earray (List.map (Array.to_list set) ~f:Emit.eint)
;;

let option (f : 'a -> Emit.expr) (o : 'a option) : Emit.expr =
  match o with
  | None -> Emit.econstruct "None" []
  | Some x -> Emit.econstruct "Some" [ f x ]
;;

let break (b : Ir.Layout.break) : Emit.expr =
  match b with
  | Flat -> ctor "Flat" []
  | Fit -> ctor "Fit" []
  | Hard lines -> ctor "Hard" [ Emit.eint lines ]
;;

let trailing (t : Ir.Layout.trailing) : Emit.expr =
  match t with
  | Never -> ctor "Never" []
  | On_break -> ctor "On_break" []
  | Always -> ctor "Always" []
;;

let sep (s : Ir.Layout.sep) : Emit.expr =
  Emit.erecord
    [ "Lingo_runtime.Layout.sep_kind", Emit.eint s.sep_kind
    ; "text", Emit.estr s.text
    ; "trailing", trailing s.trailing
    ]
;;

(* [Delimited]'s fields are inline, so the constructor resolves them and they
   carry no path. *)
let frame (f : Ir.Layout.frame) : Emit.expr =
  match f with
  | Plain -> ctor "Plain" []
  | Delimited d ->
    ctor
      "Delimited"
      [ Emit.erecord
          [ "open_", Emit.eint d.open_
          ; "close", Emit.eint d.close
          ; "sep", option sep d.sep
          ]
      ]
  | Separated s -> ctor "Separated" [ sep s ]
;;

let slot (s : Ir.Layout.slot) : Emit.expr =
  Emit.erecord
    [ "Lingo_runtime.Layout.kinds", kinds s.kinds
    ; "repeats", Emit.ebool s.repeats
    ; "before", break s.before
    ; "between", break s.between
    ]
;;

let rule (r : Ir.Layout.rule) : Emit.expr =
  Emit.erecord
    [ "Lingo_runtime.Layout.name", Emit.estr r.name
    ; "kind", Emit.eint r.kind
    ; "frame", frame r.frame
    ; "slots", Emit.earray (List.map (Array.to_list r.slots) ~f:slot)
    ; "body", break r.body
    ; "inner", break r.inner
    ; "indent", Emit.eint r.indent
    ; "edge_before", option Emit.ebool r.edge_before
    ; "edge_after", option Emit.ebool r.edge_after
    ]
;;

let trivia (t : Ir.Layout.trivia) : Emit.expr =
  match t with
  | Reformat -> ctor "Reformat" []
  | Preserve -> ctor "Preserve" []
;;

let token (t : Ir.Layout.token) : Emit.expr =
  Emit.erecord
    [ "Lingo_runtime.Layout.space_before", Emit.ebool t.space_before
    ; "space_after", Emit.ebool t.space_after
    ; "trivia", option trivia t.trivia
    ]
;;

let generate (lay : Ir.Layout.t) : Emit.item list =
  [ Emit.ilet
      "layout"
      (Emit.econstraint
         (Emit.erecord
            [ ( "Lingo_runtime.Layout.rules"
              , Emit.earray (List.map (Array.to_list lay.rules) ~f:rule) )
            ; "of_kind", Emit.earray (List.map (Array.to_list lay.of_kind) ~f:Emit.eint)
            ; ( "tokens"
              , Emit.earray (List.map (Array.to_list lay.tokens) ~f:(option token)) )
            ])
         (Emit.tcon "Lingo_runtime.Layout.t" []))
  ; Emit.ilet
      ~args:
        [ Emit.Opt ("trace", None)
        ; Emit.Named "lex"
        ; Emit.Named "width"
        ; Emit.Plain (Emit.pvar "root")
        ]
      "format"
      (Emit.eapply_labelled
         (Emit.evar "Lingo_runtime.Layout.format")
         [ Ppxlib.Optional "trace", Emit.evar "trace"
         ; Ppxlib.Nolabel, Emit.evar "layout"
         ; ( Ppxlib.Labelled "boundary"
           , Emit.eapply_labelled
               (Emit.evar "Lingo_runtime.Layout.boundary")
               [ Ppxlib.Labelled "lex", Emit.evar "lex" ] )
         ; Ppxlib.Labelled "width", Emit.evar "width"
         ; Ppxlib.Nolabel, Emit.evar "root"
         ])
  ]
;;

let signature : Emit.sig_item list =
  [ Emit.sval "layout" (Emit.tcon "Lingo_runtime.Layout.t" [])
  ; Emit.sval
      "format"
      (Emit.tarrow_optional
         "trace"
         ~domain:
           (Emit.tarrow_labelled
              "step"
              ~domain:(Emit.tcon "string" [])
              ~codomain:
                (Emit.tarrow_labelled
                   "kind"
                   ~domain:(Emit.tcon "Lingo_runtime.Kind.t" [])
                   ~codomain:(Emit.tcon "unit" [])))
         ~codomain:
           (Emit.tarrow_labelled
              "lex"
              ~domain:
                (Emit.tarrow
                   ~domain:(Emit.tcon "string" [])
                   ~codomain:(Emit.tcon "array" [ Emit.tcon "Lingo_runtime.Token.t" [] ]))
              ~codomain:
                (Emit.tarrow_labelled
                   "width"
                   ~domain:(Emit.tcon "int" [])
                   ~codomain:
                     (Emit.tarrow
                        ~domain:(Emit.tcon "Siesta.Green.node" [])
                        ~codomain:(Emit.tcon "string" [])))))
  ]
;;
