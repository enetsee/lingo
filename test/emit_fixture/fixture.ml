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

open Ocaml.Emit

(* -- one structure, reaching everything ------------------------------------ *)

let kind_type =
  itype_variant "kind" [ "K_a", []; "K_b", [ tcon "int" []; tcon "string" [] ] ]
;;

let token_type = itype_record "token" [ "kind", tcon "kind" []; "text", tcon "string" [] ]
let id_type = itype_alias "id" (tcon "int" [])

(* Operators, a field read, and the literals. *)
let conditions =
  ilet
    ~args:[ arg_var "p"; arg_any; arg_typed ~arg_name:"n" ~type_path:"int" ]
    "conditions"
    (eand
       ~left:
         (eor
            ~left:(enot (eequal ~left:(efield (evar "p") "kind") ~right:(eint 0)))
            ~right:(enot_equal ~left:(evar "n") ~right:(eint 1)))
       ~right:
         (eand
            ~left:
              (eand
                 ~left:(eless ~left:(evar "n") ~right:(eint 2))
                 ~right:(eless_equal ~left:(evar "n") ~right:(eint 3)))
            ~right:
              (eand
                 ~left:(egreater ~left:(evar "n") ~right:(eint 4))
                 ~right:(egreater_equal ~left:(evar "n") ~right:(eint 5)))))
;;

(* Control flow, and the shapes a value takes. *)
let walk =
  ilet_rec
    [ ( "walk"
      , [ arg_var "p"
        ; Named "depth"
        ; Named_pat ("seen", pvar "s")
        ; Opt ("loud", Some (ebool false))
        ]
      , eseq
          [ ewhen ~condition:(evar "loud") ~then_:(ecall "ignore" [ estr "noisy" ])
          ; ewhile
              ~condition:(egreater ~left:(evar "depth") ~right:(eint 0))
              ~body:(eseq [ ecall "step" [ evar "p" ]; ecall "step" [ evar "s" ] ])
          ; eif
              ~condition:(evar "loud")
              ~then_:
                (ematch
                   (evar "depth")
                   [ ecase (por (pint 0) [ pint 1; pint 2 ]) (estr "low")
                   ; ecase ~guard:(ebool true) (pconstruct "Some" [ pvar "x" ]) (evar "x")
                   ; ecase (ptuple [ pvar "a"; pany ]) (evar "a")
                   ; ecase (pstr "done") eunit
                   ; ecase pany (ecall "helper" [ eunit ])
                   ])
              ~else_:
                (elet
                   "local"
                   ~body:(etuple [ eint 1; estr "two"; ebool true ])
                   ~rest:
                     (elet_rec
                        [ ( "inner"
                          , [ arg_var "q" ]
                          , eapply_labelled (evar "f") [ Labelled "at", evar "q" ] )
                        ]
                        (eapply (evar "inner") [ evar "local" ])))
          ] )
    ; ( "helper"
      , [ arg_any ]
      , ethunk (elist [ earray [ eint 0; eint 1 ]; erecord [ "kind", evar "k" ] ]) )
    ]
;;

let module_item =
  imodule "Inner" [ iopen "Stdlib"; ilet "answer" (econstruct "Some" [ eint 42 ]) ]
;;

let structure = [ kind_type; token_type; id_type; conditions; walk; module_item ]

(* -- one signature --------------------------------------------------------- *)

let signature =
  [ stype_abstract "t"
  ; stype_alias "id" (tcon "int" [])
  ; stype_variant "shape" [ "Leaf", []; "Node", [ tcon "t" []; tcon "t" [] ] ]
  ; stype_record
      "pair"
      [ "left", tcon "t" []; "right", ttuple [ tcon "int" []; tcon "t" [] ] ]
  ; smodule
      "Sub"
      [ sval "make" (tarrow ~domain:(tcon "int" []) ~codomain:(tcon "t" []))
      ; sval
          "walk"
          (tarrow_optional
             "deep"
             ~domain:(tcon "bool" [])
             ~codomain:
               (tarrow_labelled "at" ~domain:(tcon "t" []) ~codomain:(tcon "unit" [])))
      ]
  ]
;;
