(* -- tokens as the formatter settles them -----------------------------------------

      The laws that format a tree and compare its tokens with another's share
      this. A separator at either end of a body is left out, because its
      policy lets the formatter write one or leave it out, and whether it
      does turns on where lines break. Separators between elements are
      kept.
   -------------------------------------------------------------------------- *)

open StdLabels

let meaningful ~(comments : bool) (f : Core.Facts.t) (tree : Siesta.Green.node)
  : (int * string) list
  =
  let trivia (k : int) : bool =
    Array.exists f.tokens ~f:(fun (t : Core.Token.def) ->
      Core.Kind.to_int t.kind = k
      &&
      match t.trivia with
      | Some Core.Grammar.Reformat -> true
      | Some Core.Grammar.Preserve -> not comments
      | None -> false)
  in
  let rule (k : int) : Core.Rule.def option =
    Array.find_opt f.rules ~f:(fun (d : Core.Rule.def) -> Core.Kind.to_int d.kind = k)
  in
  let is_sep (sep : int) (item : (int * string) list) : bool =
    match item with
    | [ (k, _) ] -> k = sep
    | _ -> false
  in
  (* The body lies between [first] and [last], items counted from each end. *)
  let trim (sep : int) ~(first : int) ~(last : int) (items : (int * string) list list) =
    let n = List.length items in
    List.filteri items ~f:(fun (i : int) (item : (int * string) list) ->
      not (is_sep sep item && (i = first || i = n - 1 - last)))
  in
  let rec go (node : Siesta.Green.node) : (int * string) list =
    let items =
      List.filter_map
        (Array.to_list (Siesta.Green.children_array node))
        ~f:(fun (child : Siesta.Green.child) ->
          match child with
          | Siesta.Green.Node node -> Some (go node)
          | Siesta.Green.Token token ->
            let k = Siesta.Green.Token.kind token in
            if trivia k then None else Some [ k, Siesta.Green.Token.text token ])
    in
    let items =
      match rule (Siesta.Green.kind node) with
      | Some { frame = Core.Rule.Delimited { sep = Some sep; _ }; body_from; _ } ->
        trim (Core.Kind.to_int sep.sep_tok) ~first:(body_from + 1) ~last:1 items
      | Some { frame = Core.Rule.Separated { sep_tok; _ }; _ } ->
        trim (Core.Kind.to_int sep_tok) ~first:0 ~last:0 items
      | Some _ | None -> items
    in
    List.concat items
  in
  go tree
;;
