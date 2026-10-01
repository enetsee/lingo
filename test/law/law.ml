let count = ref 0

let fail : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt ->
  Format.kasprintf
    (fun s ->
       incr count;
       print_endline ("FAIL " ^ s))
    fmt
;;

let pass : type a. (a, Format.formatter, unit, unit) format4 -> a =
  fun fmt -> Format.kasprintf (fun s -> print_endline ("PASS " ^ s)) fmt
;;

let failures () : int = !count

let summarise (name : string) : unit =
  if !count = 0
  then print_endline (name ^ ": 0 failures")
  else (
    Printf.printf "%s: %d failures\n" name !count;
    exit 1)
;;

let exit_on_failure () : unit =
  if !count > 0
  then (
    Printf.printf "%d failures\n" !count;
    exit 1)
;;

(* -- a module the build generated ------------------------------------------ *)

(* Beside the executable rather than beside the cwd. A law runs from its own
   build directory under [dune test] and from the root under [dune exec], and
   the generated modules sit next to the executable either way. *)
let beside (file : string) : string =
  Filename.concat (Filename.dirname Sys.executable_name) file
;;

let first_difference (built : string) (now : string) : string =
  let rec walk (n : int) (a : string list) (b : string list) : string =
    match a, b with
    | x :: xs, y :: ys when String.equal x y -> walk (n + 1) xs ys
    | x :: _, y :: _ -> Printf.sprintf "line %d, %S for %S" n y x
    | [], y :: _ -> Printf.sprintf "line %d, %S for nothing" n y
    | x :: _, [] -> Printf.sprintf "line %d, nothing for %S" n x
    | [], [] -> "nowhere"
  in
  walk 1 (String.split_on_char '\n' built) (String.split_on_char '\n' now)
;;

let generated ~(file : string) (now : string) : unit =
  let built = In_channel.with_open_bin (beside file) In_channel.input_all in
  if not (String.equal built now)
  then
    fail
      "%s: the emitter writes something else now, at %s"
      file
      (first_difference built now)
;;
