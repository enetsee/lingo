open StdLabels

type klass =
  { key : string
  ; count : int
  ; witness : string
  ; at : Core.Rule.id option
  }

type t = (string, klass) Hashtbl.t

let create () : t = Hashtbl.create 32

let note (t : t) ?(at : Core.Rule.id option) (key : string) ~(witness : string) : unit =
  match Hashtbl.find_opt t key with
  | None -> Hashtbl.replace t key { key; count = 1; witness; at }
  | Some held ->
    let shorter =
      if String.length witness < String.length held.witness
      then { held with witness; at }
      else held
    in
    Hashtbl.replace t key { shorter with count = held.count + 1 }
;;

let classes (t : t) : klass list =
  Hashtbl.fold (fun _ klass acc -> klass :: acc) t []
  |> List.sort ~cmp:(fun a b ->
    if a.count = b.count then compare a.key b.key else compare b.count a.count)
;;

let total (t : t) : int = Hashtbl.fold (fun _ klass sum -> sum + klass.count) t 0

(* -- skip classes ---------------------------------------------------------- *)

type ceiling =
  { why : string
  ; min_iterations : int
  ; per_million : int
  }

let ceilings : ceiling list =
  [ { why = "every mutator declined"; min_iterations = 5_000; per_million = 20_000 } ]
;;

let check_skips (t : t) ~(iterations : int) : string list =
  classes t
  |> List.filter_map ~f:(fun (k : klass) ->
    match List.find_opt ceilings ~f:(fun c -> String.equal c.why k.key) with
    | None ->
      Some
        (Printf.sprintf
           "the skip class %S is in no ceiling, and reached %d"
           k.key
           k.count)
    | Some c ->
      if iterations < c.min_iterations
      then None
      else (
        let rate = k.count * 1_000_000 / iterations in
        if rate <= c.per_million
        then None
        else
          Some
            (Printf.sprintf
               "the skip class %S reached %d per million, over a ceiling of %d"
               k.key
               rate
               c.per_million)))
;;
