type t =
  { kind : Ir.Kind.t
  ; text : string
  }

type boundary =
  | Absent
  | Open
  | Settled
