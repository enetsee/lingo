(* The source Emit renders. A round trip says the printer and the parser
   agree; this says what the printer wrote, which is the half a round trip
   cannot see. *)

let () =
  print_string (Ocaml.Emit.render Fixture.structure);
  print_newline ();
  print_string (Ocaml.Emit.render_signature Fixture.signature)
;;
