open StdLabels

let kinds (set : Ir.Kind.t array) : Emit.expr =
  Emit.earray (List.map (Array.to_list set) ~f:Emit.eint)
;;

let point (p : Ir.Residual.Table.point) : Emit.expr =
  Emit.erecord
    [ "Lingo_runtime.Ahead.first", kinds p.first
    ; "may_end", Emit.ebool p.may_end
    ; ( "on"
      , Emit.earray
          (List.map (Array.to_list p.on) ~f:(fun (on, dest) ->
             Emit.etuple [ kinds on; Emit.eint dest ])) )
    ]
;;

(* [of_kind] is indexed by node kind, so it runs to the highest kind that has a
   table. A kind past the end has none, and the walk reads that as a node it
   holds no position for. *)
let generate (plan : Ir.Plan.t) : Emit.item list =
  let entries = Ir.Residual.Table.of_plan plan in
  let size =
    List.fold_left entries ~init:0 ~f:(fun acc (kind, _) -> max acc (kind + 1))
  in
  let of_kind = Array.make size (-1) in
  List.iteri entries ~f:(fun index (kind, _) -> of_kind.(kind) <- index);
  [ Emit.ilet
      "tables"
      (Emit.econstraint
         (Emit.erecord
            [ ( "Lingo_runtime.Ahead.points"
              , Emit.earray
                  (List.map entries ~f:(fun (_, points) ->
                     Emit.earray (List.map (Array.to_list points) ~f:point))) )
            ; "of_kind", Emit.earray (List.map (Array.to_list of_kind) ~f:Emit.eint)
            ; "trivia", kinds plan.trivia
            ; "error_kind", Emit.eint plan.error_kind
            ])
         (Emit.tcon "Lingo_runtime.Ahead.t" []))
  ; Emit.ilet
      ~args:[ Emit.Plain (Emit.pvar "root"); Emit.Named "offset" ]
      "at"
      (Emit.eapply_labelled
         (Emit.evar "Lingo_runtime.Ahead.at")
         [ Ppxlib.Nolabel, Emit.evar "tables"
         ; Ppxlib.Nolabel, Emit.evar "root"
         ; Ppxlib.Labelled "offset", Emit.evar "offset"
         ])
  ]
;;

let signature (_plan : Ir.Plan.t) : Emit.sig_item list =
  [ Emit.sval "tables" (Emit.tcon "Lingo_runtime.Ahead.t" [])
  ; Emit.sval
      "at"
      (Emit.tarrow
         ~domain:(Emit.tcon "Siesta.Green.node" [])
         ~codomain:
           (Emit.tarrow_labelled
              "offset"
              ~domain:(Emit.tcon "int" [])
              ~codomain:(Emit.tcon "array" [ Emit.tcon "int" [] ])))
  ]
;;
