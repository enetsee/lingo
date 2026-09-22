type point = Ir.Residual.State.t

type t =
  { points : (point, unit) Hashtbl.t
  ; edges : (point * point, unit) Hashtbl.t
  ; mutable previous : point option
  ; mutable fresh : bool
  }

let create () : t =
  { points = Hashtbl.create 256
  ; edges = Hashtbl.create 1024
  ; previous = None
  ; fresh = false
  }
;;

let walk (t : t) : unit =
  t.previous <- None;
  t.fresh <- false
;;

let at (t : t) (state : Ir.Residual.State.t) : unit =
  let point = Ir.Residual.State.site state in
  if not (Hashtbl.mem t.points point) then Hashtbl.replace t.points point ();
  (match t.previous with
   | None -> ()
   | Some before ->
     let edge = before, point in
     if not (Hashtbl.mem t.edges edge)
     then (
       Hashtbl.replace t.edges edge ();
       t.fresh <- true));
  t.previous <- Some point
;;

let points (t : t) : int = Hashtbl.length t.points
let edges (t : t) : int = Hashtbl.length t.edges
let fresh (t : t) : bool = t.fresh
