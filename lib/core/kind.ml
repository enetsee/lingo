open StdLabels

type t = int

let to_int (k : t) = k
let equal (a : t) (b : t) = a = b
let compare (a : t) (b : t) = Int.compare a b
let hash (k : t) = k
let pp fmt (k : t) = Format.pp_print_int fmt k

(* -- sets ------------------------------------------------------------------ *)

module Set = struct
  (* [Sys.int_size - 1] bits per word, so 62 on a 64-bit host. Spending one
     bit per word keeps every word positive. *)
  let word_bits = Sys.int_size - 1

  type elt = t
  type t = int array

  let empty : t = [||]

  let is_empty (s : t) =
    let rec go i = i >= Array.length s || (s.(i) = 0 && go (i + 1)) in
    go 0
  ;;

  let mem (s : t) (k : elt) =
    let w = k / word_bits in
    w < Array.length s && s.(w) land (1 lsl (k mod word_bits)) <> 0
  ;;

  let add (k : elt) (s : t) : t =
    let w = k / word_bits in
    let n = max (Array.length s) (w + 1) in
    let out = Array.make n 0 in
    Array.blit ~src:s ~src_pos:0 ~dst:out ~dst_pos:0 ~len:(Array.length s);
    out.(w) <- out.(w) lor (1 lsl (k mod word_bits));
    out
  ;;

  let remove (k : elt) (s : t) : t =
    let w = k / word_bits in
    if w >= Array.length s
    then s
    else (
      let out = Array.copy s in
      out.(w) <- out.(w) land lnot (1 lsl (k mod word_bits));
      out)
  ;;

  let singleton (k : elt) = add k empty
  let of_list ks = List.fold_left ks ~init:empty ~f:(fun acc k -> add k acc)

  let binop f (a : t) (b : t) : t =
    let na = Array.length a
    and nb = Array.length b in
    let n = max na nb in
    Array.init n ~f:(fun i ->
      f (if i < na then a.(i) else 0) (if i < nb then b.(i) else 0))
  ;;

  let union a b = binop ( lor ) a b
  let inter a b = binop ( land ) a b
  let diff a b = binop (fun x y -> x land lnot y) a b
  let unions l = List.fold_left l ~init:empty ~f:union
  let subset a b = is_empty (diff a b)

  let equal (a : t) (b : t) =
    let n = max (Array.length a) (Array.length b) in
    let get s i = if i < Array.length s then s.(i) else 0 in
    let rec go i = i >= n || (get a i = get b i && go (i + 1)) in
    go 0
  ;;

  let compare (a : t) (b : t) =
    let n = max (Array.length a) (Array.length b) in
    let get s i = if i < Array.length s then s.(i) else 0 in
    let rec go i =
      if i >= n
      then 0
      else (
        match Int.compare (get a i) (get b i) with
        | 0 -> go (i + 1)
        | c -> c)
    in
    go 0
  ;;

  let fold f (s : t) init =
    let acc = ref init in
    Array.iteri
      ~f:(fun w bits ->
        if bits <> 0
        then
          for b = 0 to word_bits - 1 do
            if bits land (1 lsl b) <> 0 then acc := f ((w * word_bits) + b) !acc
          done)
      s;
    !acc
  ;;

  let iter f s = fold (fun k () -> f k) s ()
  let elements s = List.rev (fold (fun k acc -> k :: acc) s [])
  let cardinal s = fold (fun _ n -> n + 1) s 0
  let exists p s = fold (fun k acc -> acc || p k) s false
  let for_all p s = fold (fun k acc -> acc && p k) s true
  let filter p s = fold (fun k acc -> if p k then add k acc else acc) s empty

  let pp fmt s =
    Format.fprintf
      fmt
      "@[<h>{%a}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt k -> Format.pp_print_int fmt k))
      (elements s)
  ;;
end

(* -- names ----------------------------------------------------------------- *)

module Name = struct
  type t = string

  let to_string (n : t) : string = n
  let node (s : string) : t = "N_" ^ Mangle.screaming_snake s
  let token (s : string) : t = "T_" ^ Mangle.screaming_snake s
  let hole (n : t) : t = n ^ "_HOLE"
  let error : t = "N_ERROR"
  let missing : t = "N_MISSING"
  let error_token : t = "T_ERROR"
  let unterminated : t = "T_UNTERMINATED"
  let equal (a : t) (b : t) : bool = String.equal a b
  let compare (a : t) (b : t) : int = String.compare a b
  let pp (fmt : Format.formatter) (n : t) : unit = Format.pp_print_string fmt n
end

(* -- the table ------------------------------------------------------------- *)

module Table = struct
  type kind = Set.elt

  type t =
    { by_index : Name.t array
    ; by_name : (Name.t, int) Hashtbl.t
    }

  let of_names (names : Name.t list) : t =
    let by_index = Array.of_list names in
    let by_name = Hashtbl.create (Array.length by_index * 2) in
    (* First occurrence wins, so [find] stays total on a list that holds a
       duplicate. A check reports the duplicate as [dup-kind-name]. *)
    Array.iteri
      ~f:(fun i n -> if not (Hashtbl.mem by_name n) then Hashtbl.add by_name n i)
      by_index;
    { by_index; by_name }
  ;;

  let count (t : t) : int = Array.length t.by_index
  let name (t : t) (k : kind) : Name.t = t.by_index.(k)
  let find (t : t) (n : Name.t) : kind option = Hashtbl.find_opt t.by_name n
  let mem_name (t : t) (n : Name.t) : bool = Hashtbl.mem t.by_name n
  let kinds (t : t) : kind list = List.init ~len:(Array.length t.by_index) ~f:(fun i -> i)
  let names (t : t) : Name.t list = Array.to_list t.by_index

  let pp_set (tbl : t) fmt (s : Set.t) : unit =
    Format.fprintf
      fmt
      "@[<h>{%a}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt k -> Name.pp fmt (name tbl k)))
      (Set.elements s)
  ;;
end
