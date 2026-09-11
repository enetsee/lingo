module type S = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module Make () = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string (n : t) : string = n
  let equal (a : t) (b : t) : bool = String.equal a b
  let compare (a : t) (b : t) : int = String.compare a b
  let pp (fmt : Format.formatter) (n : t) : unit = Format.pp_print_string fmt n
end

module Rule = Make ()
module Token = Make ()
module Child = Make ()
