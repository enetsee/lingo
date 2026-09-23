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
