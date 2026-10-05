open StdLabels

module Ctx = struct
  type 's t =
    { cache : Siesta.Cache.t
    ; root : Siesta.Syntax.t
    ; semantics : 's
    }

  let create (cache : Siesta.Cache.t) (root : Siesta.Syntax.t) (semantics : 's) : 's t =
    { cache; root; semantics }
  ;;

  let cache (ctx : 's t) : Siesta.Cache.t = ctx.cache
  let root (ctx : 's t) : Siesta.Syntax.t = ctx.root
  let semantics (ctx : 's t) : 's = ctx.semantics
end

type 's t = 's Ctx.t -> Siesta.Syntax.t -> (Siesta.Green.node, string) result

let unchanged (node : Siesta.Syntax.t) (green : Siesta.Green.node) : bool =
  Siesta.Green.equal green (Siesta.Syntax.green node)
;;

let after (ctx : 's Ctx.t) (node : Siesta.Syntax.t) (green : Siesta.Green.node)
  : Siesta.Syntax.t
  =
  if unchanged node green
  then node
  else (Siesta.Syntax.replace (Ctx.cache ctx) node green).self
;;

(* {1 Combinators} *)

let id : 's t = fun _ node -> Ok (Siesta.Syntax.green node)
let fail (reason : string) : 's t = fun _ _ -> Error reason

let seq (s1 : 's t) (s2 : 's t) : 's t =
  fun ctx node ->
  match s1 ctx node with
  | Error _ as failed -> failed
  | Ok green -> s2 ctx (after ctx node green)
;;

let choice (s1 : 's t) (s2 : 's t) : 's t =
  fun ctx node ->
  match s1 ctx node with
  | Ok _ as succeeded -> succeeded
  | Error _ -> s2 ctx node
;;

let try_ (s : 's t) : 's t = choice s id

let repeat ?(fuel : int = 100_000) (s : 's t) : 's t =
  fun ctx node ->
  let rec go (remaining : int) (node : Siesta.Syntax.t) =
    match s ctx node with
    | Error _ -> Ok (Siesta.Syntax.green node)
    | Ok _ when remaining <= 0 -> Error "repeat: out of fuel"
    | Ok green -> go (remaining - 1) (after ctx node green)
  in
  go fuel node
;;

let where_ (s1 : 's t) (s2 : 's t) : 's t =
  fun ctx node ->
  match s1 ctx node with
  | Error _ as failed -> failed
  | Ok _ -> s2 ctx node
;;

let kind (k : Ir.Kind.t) (s : 's t) : 's t =
  fun ctx node -> if Siesta.Syntax.kind node = k then s ctx node else Error "another kind"
;;

(* {1 One level down} *)

(* A parent's children as [all], [one] and [some] change them. The green
   array is copied at the first child whose tag changed. A parent where
   nothing changed allocates nothing and comes back as the same node, which is
   what lets an enclosing rule see that nothing happened. *)
module Rebuild = struct
  type t =
    { parent : Siesta.Syntax.t
    ; mutable children : Siesta.Green.child array option
    }

  let start (parent : Siesta.Syntax.t) : t = { parent; children = None }

  let set (t : t) (index : int) (child : Siesta.Syntax.t) (green : Siesta.Green.node)
    : unit
    =
    if not (unchanged child green)
    then (
      let children =
        match t.children with
        | Some children -> children
        | None ->
          let children = Siesta.Green.children_array (Siesta.Syntax.green t.parent) in
          t.children <- Some children;
          children
      in
      children.(index) <- Siesta.Green.Node green)
  ;;

  (* The payload is the parent's diagnostic id, and the parent is still the
     node that diagnostic was about. *)
  let finish (ctx : 's Ctx.t) (t : t) : Siesta.Green.node =
    let green = Siesta.Syntax.green t.parent in
    match t.children with
    | None -> green
    | Some children ->
      Siesta.Green.mk_node
        (Ctx.cache ctx)
        ~kind:(Siesta.Green.kind green)
        ~payload:(Siesta.Green.payload green)
        ~children
        ()
  ;;
end

let no_child_node = "no child node"

let all (s : 's t) : 's t =
  fun ctx node ->
  let elems = Siesta.Syntax.children_array node in
  let rebuild = Rebuild.start node in
  let rec go (index : int) =
    if index = Array.length elems
    then Ok (Rebuild.finish ctx rebuild)
    else (
      match elems.(index) with
      | Siesta.Syntax.Token _ -> go (index + 1)
      | Siesta.Syntax.Node child ->
        (match s ctx child with
         | Error _ as failed -> failed
         | Ok green ->
           Rebuild.set rebuild index child green;
           go (index + 1)))
  in
  go 0
;;

(* A child's reason is carried along, so the last one to fail gives it. In
   [oncetd fold] that is the rule's own reason from the last leaf it tried. *)
let one (s : 's t) : 's t =
  fun ctx node ->
  let elems = Siesta.Syntax.children_array node in
  let rec go (index : int) (reason : string) =
    if index = Array.length elems
    then Error reason
    else (
      match elems.(index) with
      | Siesta.Syntax.Token _ -> go (index + 1) reason
      | Siesta.Syntax.Node child ->
        (match s ctx child with
         | Error reason -> go (index + 1) reason
         | Ok green ->
           let rebuild = Rebuild.start node in
           Rebuild.set rebuild index child green;
           Ok (Rebuild.finish ctx rebuild)))
  in
  go 0 no_child_node
;;

let some (s : 's t) : 's t =
  fun ctx node ->
  let elems = Siesta.Syntax.children_array node in
  let rebuild = Rebuild.start node in
  let rec go (index : int) (succeeded : bool) (reason : string) =
    if index = Array.length elems
    then if succeeded then Ok (Rebuild.finish ctx rebuild) else Error reason
    else (
      match elems.(index) with
      | Siesta.Syntax.Token _ -> go (index + 1) succeeded reason
      | Siesta.Syntax.Node child ->
        (match s ctx child with
         | Error reason -> go (index + 1) succeeded reason
         | Ok green ->
           Rebuild.set rebuild index child green;
           go (index + 1) true reason))
  in
  go 0 false no_child_node
;;

(* {1 Traversals} *)

let topdown (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> seq s (all x) ctx node in
  x
;;

let bottomup (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> seq (all x) s ctx node in
  x
;;

let downup (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> seq s (seq (all x) s) ctx node in
  x
;;

let alltd (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> choice s (all x) ctx node in
  x
;;

let oncetd (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> choice s (one x) ctx node in
  x
;;

let oncebu (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> choice (one x) s ctx node in
  x
;;

let sometd (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> choice s (some x) ctx node in
  x
;;

(* After a rule fires, everything it moved is normalised again in full.
   Stratego's [innermost-tagged] skips subterms already in normal form, and a
   green tag would serve as that mark here. It waits for a client whose trees
   make the cost show. *)
let innermost (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> bottomup (try_ (seq s x)) ctx node in
  x
;;

let outermost ?(fuel : int option) (s : 's t) : 's t = repeat ?fuel (oncetd s)

let topdown_stop ~(stop : 's t) (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> seq s (choice (where_ stop id) (all x)) ctx node in
  x
;;

let bottomup_stop ~(stop : 's t) (s : 's t) : 's t =
  let rec x : 's t = fun ctx node -> seq (choice (where_ stop id) (all x)) s ctx node in
  x
;;

let collect (s : 's t) (ctx : 's Ctx.t) (node : Siesta.Syntax.t)
  : (Siesta.Syntax.t * Siesta.Green.node) list
  =
  let found = ref [] in
  Siesta.Syntax.preorder node ~f:(fun (node : Siesta.Syntax.t) ->
    match s ctx node with
    | Ok green ->
      found := (node, green) :: !found;
      Siesta.Syntax.Skip
    | Error _ -> Siesta.Syntax.Descend);
  List.rev !found
;;
