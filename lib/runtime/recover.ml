(* [expect] skips trivia before it looks. The test peeks past trivia on its
   own, so the skip is there for the failing case. It puts the trivia in the
   open frame first, and the hole then sits after the trivia. *)
let expect
      ?at_child
      ?hole_kind
      ?placeholder
      (c : Cursor.t)
      (k : Ir.Kind.t)
      (expected : Ir.Message.id)
  : unit
  =
  Cursor.skip_trivia c;
  if Cursor.at c k
  then Cursor.bump c
  else (
    let d =
      Diagnostic.Missing { at_child; expected; expected_kinds = [ k ]; hole_kind }
    in
    match placeholder with
    | None -> Cursor.report c d
    | Some pk -> Build.missing_node ~payload:(Cursor.report_id c d) c pk)
;;
