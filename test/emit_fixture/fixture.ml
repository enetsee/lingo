(* -- one structure and one signature, reaching every value in emit.mli --------

      Two things read this, and each says something the other cannot.

      test/laws/law_emit.ml renders it, parses the source back with ppxlib, and
      compares the trees. That says the printer and the parser agree.

      test/expect/emit.expected holds the source it renders to. That says what
      the source is, which a round trip cannot: a builder that builds a
      different well-formed thing round-trips just as well. [eseq] folding the
      wrong way and [elist] ending in [()] are both invisible to the law and
      both move the file.
   -------------------------------------------------------------------------- *)

(* -- one structure, reaching everything ------------------------------------ *)

let kind_type =
  Ocaml.Emit.itype_variant
    "kind"
    [ "K_a", []; "K_b", [ Ocaml.Emit.tcon "int" []; Ocaml.Emit.tcon "string" [] ] ]
;;

let token_type =
  Ocaml.Emit.itype_record
    "token"
    [ "kind", Ocaml.Emit.tcon "kind" []; "text", Ocaml.Emit.tcon "string" [] ]
;;

let id_type = Ocaml.Emit.itype_alias "id" (Ocaml.Emit.tcon "int" [])

(* Operators, a field read, and the literals. *)
let conditions =
  Ocaml.Emit.ilet
    ~args:
      [ Ocaml.Emit.arg_var "p"
      ; Ocaml.Emit.arg_any
      ; Ocaml.Emit.arg_typed ~arg_name:"n" ~type_path:"int"
      ]
    "conditions"
    (Ocaml.Emit.eand
       ~left:
         (Ocaml.Emit.eor
            ~left:
              (Ocaml.Emit.enot
                 (Ocaml.Emit.eequal
                    ~left:(Ocaml.Emit.efield (Ocaml.Emit.evar "p") "kind")
                    ~right:(Ocaml.Emit.eint 0)))
            ~right:
              (Ocaml.Emit.enot_equal
                 ~left:(Ocaml.Emit.evar "n")
                 ~right:(Ocaml.Emit.eint 1)))
       ~right:
         (Ocaml.Emit.eand
            ~left:
              (Ocaml.Emit.eand
                 ~left:
                   (Ocaml.Emit.eless
                      ~left:(Ocaml.Emit.evar "n")
                      ~right:(Ocaml.Emit.eint 2))
                 ~right:
                   (Ocaml.Emit.eless_equal
                      ~left:(Ocaml.Emit.evar "n")
                      ~right:(Ocaml.Emit.eint 3)))
            ~right:
              (Ocaml.Emit.eand
                 ~left:
                   (Ocaml.Emit.egreater
                      ~left:(Ocaml.Emit.evar "n")
                      ~right:(Ocaml.Emit.eint 4))
                 ~right:
                   (Ocaml.Emit.egreater_equal
                      ~left:(Ocaml.Emit.evar "n")
                      ~right:(Ocaml.Emit.eint 5)))))
;;

(* Control flow, and the shapes a value takes. *)
let walk =
  Ocaml.Emit.ilet_rec
    [ ( "walk"
      , [ Ocaml.Emit.arg_var "p"
        ; Named "depth"
        ; Named_pat ("seen", Ocaml.Emit.pvar "s")
        ; Opt ("loud", Some (Ocaml.Emit.ebool false))
        ]
      , Ocaml.Emit.eseq
          [ Ocaml.Emit.ewhen
              ~condition:(Ocaml.Emit.evar "loud")
              ~then_:(Ocaml.Emit.ecall "ignore" [ Ocaml.Emit.estr "noisy" ])
          ; Ocaml.Emit.ewhile
              ~condition:
                (Ocaml.Emit.egreater
                   ~left:(Ocaml.Emit.evar "depth")
                   ~right:(Ocaml.Emit.eint 0))
              ~body:
                (Ocaml.Emit.eseq
                   [ Ocaml.Emit.ecall "step" [ Ocaml.Emit.evar "p" ]
                   ; Ocaml.Emit.ecall "step" [ Ocaml.Emit.evar "s" ]
                   ])
          ; Ocaml.Emit.eif
              ~condition:(Ocaml.Emit.evar "loud")
              ~then_:
                (Ocaml.Emit.ematch
                   (Ocaml.Emit.evar "depth")
                   [ Ocaml.Emit.ecase
                       (Ocaml.Emit.por
                          (Ocaml.Emit.pint 0)
                          [ Ocaml.Emit.pint 1; Ocaml.Emit.pint 2 ])
                       (Ocaml.Emit.estr "low")
                   ; Ocaml.Emit.ecase
                       ~guard:(Ocaml.Emit.ebool true)
                       (Ocaml.Emit.pconstruct "Some" [ Ocaml.Emit.pvar "x" ])
                       (Ocaml.Emit.evar "x")
                   ; Ocaml.Emit.ecase
                       (Ocaml.Emit.ptuple [ Ocaml.Emit.pvar "a"; Ocaml.Emit.pany ])
                       (Ocaml.Emit.evar "a")
                   ; Ocaml.Emit.ecase (Ocaml.Emit.pstr "done") Ocaml.Emit.eunit
                   ; Ocaml.Emit.ecase
                       (Ocaml.Emit.por
                          (Ocaml.Emit.pchar 'a')
                          [ Ocaml.Emit.pchar_range ~lo:'0' ~hi:'9' ])
                       Ocaml.Emit.eunit
                   ; Ocaml.Emit.ecase
                       Ocaml.Emit.pany
                       (Ocaml.Emit.ecall "helper" [ Ocaml.Emit.eunit ])
                   ])
              ~else_:
                (Ocaml.Emit.elet
                   "local"
                   ~body:
                     (Ocaml.Emit.etuple
                        [ Ocaml.Emit.eint 1
                        ; Ocaml.Emit.estr "two"
                        ; Ocaml.Emit.ebool true
                        ])
                   ~rest:
                     (Ocaml.Emit.elet_rec
                        [ ( "inner"
                          , [ Ocaml.Emit.arg_var "q" ]
                          , Ocaml.Emit.eapply_labelled
                              (Ocaml.Emit.evar "f")
                              [ Labelled "at", Ocaml.Emit.evar "q" ] )
                        ]
                        (Ocaml.Emit.eapply
                           (Ocaml.Emit.evar "inner")
                           [ Ocaml.Emit.evar "local" ])))
          ] )
    ; ( "helper"
      , [ Ocaml.Emit.arg_any ]
      , Ocaml.Emit.ethunk
          (Ocaml.Emit.elist
             [ Ocaml.Emit.econstraint
                 (Ocaml.Emit.earray [ Ocaml.Emit.eint 0; Ocaml.Emit.eint 1 ])
                 (Ocaml.Emit.tcon "array" [ Ocaml.Emit.tcon "int" [] ])
             ; Ocaml.Emit.erecord [ "kind", Ocaml.Emit.evar "k" ]
             ]) )
    ]
;;

let module_item =
  Ocaml.Emit.imodule
    "Inner"
    [ Ocaml.Emit.iopen "Stdlib"
    ; Ocaml.Emit.ilet "some_value" (Ocaml.Emit.econstruct "Some" [ Ocaml.Emit.eint 42 ])
    ]
;;

let structure = [ kind_type; token_type; id_type; conditions; walk; module_item ]

(* -- one signature --------------------------------------------------------- *)

let signature =
  [ Ocaml.Emit.stype_abstract "t"
  ; Ocaml.Emit.stype_alias "id" (Ocaml.Emit.tcon "int" [])
  ; Ocaml.Emit.stype_variant
      "shape"
      [ "Leaf", []; "Node", [ Ocaml.Emit.tcon "t" []; Ocaml.Emit.tcon "t" [] ] ]
  ; Ocaml.Emit.stype_record
      "pair"
      [ "left", Ocaml.Emit.tcon "t" []
      ; "right", Ocaml.Emit.ttuple [ Ocaml.Emit.tcon "int" []; Ocaml.Emit.tcon "t" [] ]
      ]
  ; Ocaml.Emit.smodule
      "Sub"
      [ Ocaml.Emit.sval
          "make"
          (Ocaml.Emit.tarrow
             ~domain:(Ocaml.Emit.tcon "int" [])
             ~codomain:(Ocaml.Emit.tcon "t" []))
      ; Ocaml.Emit.sval
          "walk"
          (Ocaml.Emit.tarrow_optional
             "deep"
             ~domain:(Ocaml.Emit.tcon "bool" [])
             ~codomain:
               (Ocaml.Emit.tarrow_labelled
                  "at"
                  ~domain:(Ocaml.Emit.tcon "t" [])
                  ~codomain:(Ocaml.Emit.tcon "unit" [])))
      ]
  ]
;;
