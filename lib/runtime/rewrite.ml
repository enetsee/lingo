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

let original (elem : Siesta.Syntax.elem) : Siesta.Green.child =
  match elem with
  | Siesta.Syntax.Node node -> Siesta.Green.Node (Siesta.Syntax.green node)
  | Siesta.Syntax.Token token -> Siesta.Green.Token (Siesta.Syntax.Token.green token)
;;

let same (elem : Siesta.Syntax.elem) (child : Siesta.Green.child) : bool =
  match elem, child with
  | Siesta.Syntax.Node node, Siesta.Green.Node green -> unchanged node green
  | Siesta.Syntax.Token token, Siesta.Green.Token green ->
    Siesta.Green.Token.equal green (Siesta.Syntax.Token.green token)
  | Siesta.Syntax.Node _, Siesta.Green.Token _
  | Siesta.Syntax.Token _, Siesta.Green.Node _ -> false
;;

let index (elem : Siesta.Syntax.elem) : int =
  match elem with
  | Siesta.Syntax.Node node -> Siesta.Syntax.index_in_parent node
  | Siesta.Syntax.Token token -> Siesta.Syntax.Token.index_in_parent token
;;

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

  let set (t : t) (elem : Siesta.Syntax.elem) (child : Siesta.Green.child) : unit =
    if not (same elem child)
    then (
      let children =
        match t.children with
        | Some children -> children
        | None ->
          let children = Siesta.Green.children_array (Siesta.Syntax.green t.parent) in
          t.children <- Some children;
          children
      in
      children.(index elem) <- child)
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
           Rebuild.set rebuild (Siesta.Syntax.Node child) (Siesta.Green.Node green);
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
           Rebuild.set rebuild (Siesta.Syntax.Node child) (Siesta.Green.Node green);
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
           Rebuild.set rebuild (Siesta.Syntax.Node child) (Siesta.Green.Node green);
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

(* {1 Congruences} *)

module Token = struct
  type 's t =
    's Ctx.t -> Siesta.Syntax.token_cursor -> (Siesta.Green.token, string) result

  let id : 's t = fun _ token -> Ok (Siesta.Syntax.Token.green token)

  let text (text : string) : 's t =
    fun ctx token ->
    Ok
      (Siesta.Green.mk_token (Ctx.cache ctx) ~kind:(Siesta.Syntax.Token.kind token) ~text)
  ;;

  let make (k : Ir.Kind.t) (text : string) : 's t =
    fun ctx _ -> Ok (Siesta.Green.mk_token (Ctx.cache ctx) ~kind:k ~text)
  ;;
end

(* What a rule gives for one element of a slot. [None] is an element of the
   other shape, which the rule passes through. [Elems.one] and [Elems.some]
   do not count it as a success. *)
type 's element =
  's Ctx.t -> Siesta.Syntax.elem -> (Siesta.Green.child, string) result option

let node_element (s : 's t) : 's element =
  fun ctx elem ->
  match elem with
  | Siesta.Syntax.Node node ->
    Some (Result.map (fun green -> Siesta.Green.Node green) (s ctx node))
  | Siesta.Syntax.Token _ -> None
;;

let token_element (s : 's Token.t) : 's element =
  fun ctx elem ->
  match elem with
  | Siesta.Syntax.Token token ->
    Some (Result.map (fun green -> Siesta.Green.Token green) (s ctx token))
  | Siesta.Syntax.Node _ -> None
;;

module Elem = struct
  type 's rule = 's t

  type 's t =
    { node : 's rule option
    ; token : 's Token.t option
    }

  let make ?(node : 's rule option) ?(token : 's Token.t option) () : 's t =
    { node; token }
  ;;
end

(* An omitted rule leaves its element as it was, and that counts as a success. *)
let elem_element (e : 's Elem.t) : 's element =
  fun ctx elem ->
  match elem, e.node, e.token with
  | Siesta.Syntax.Node _, Some s, _ -> node_element s ctx elem
  | Siesta.Syntax.Token _, _, Some s -> token_element s ctx elem
  | Siesta.Syntax.Node _, None, _ | Siesta.Syntax.Token _, _, None ->
    Some (Ok (original elem))
;;

module Elems = struct
  type 'r t =
    | All of 'r
    | One of 'r
    | Some_of of 'r
    | Nth of int * 'r

  let all (r : 'r) : 'r t = All r
  let one (r : 'r) : 'r t = One r
  let some (r : 'r) : 'r t = Some_of r
  let nth (index : int) (r : 'r) : 'r t = Nth (index, r)
end

let no_element = "no element"

(* What one slot becomes, element for element. *)
let run_all (element : 's element) (ctx : 's Ctx.t) (elems : Siesta.Syntax.elem list)
  : (Siesta.Green.child list, string) result
  =
  let rec go (elems : Siesta.Syntax.elem list) (acc : Siesta.Green.child list) =
    match elems with
    | [] -> Ok (List.rev acc)
    | elem :: rest ->
      (match element ctx elem with
       | None -> go rest (original elem :: acc)
       | Some (Error _ as failed) -> failed
       | Some (Ok child) -> go rest (child :: acc))
  in
  go elems []
;;

let run_one (element : 's element) (ctx : 's Ctx.t) (elems : Siesta.Syntax.elem list)
  : (Siesta.Green.child list, string) result
  =
  let rec go
            (elems : Siesta.Syntax.elem list)
            (acc : Siesta.Green.child list)
            (reason : string)
    =
    match elems with
    | [] -> Error reason
    | elem :: rest ->
      (match element ctx elem with
       | None -> go rest (original elem :: acc) reason
       | Some (Error reason) -> go rest (original elem :: acc) reason
       | Some (Ok child) -> Ok (List.rev_append acc (child :: List.map rest ~f:original)))
  in
  go elems [] no_element
;;

let run_some (element : 's element) (ctx : 's Ctx.t) (elems : Siesta.Syntax.elem list)
  : (Siesta.Green.child list, string) result
  =
  let rec go
            (elems : Siesta.Syntax.elem list)
            (acc : Siesta.Green.child list)
            (succeeded : bool)
            (reason : string)
    =
    match elems with
    | [] -> if succeeded then Ok (List.rev acc) else Error reason
    | elem :: rest ->
      (match element ctx elem with
       | None -> go rest (original elem :: acc) succeeded reason
       | Some (Error reason) -> go rest (original elem :: acc) succeeded reason
       | Some (Ok child) -> go rest (child :: acc) true reason)
  in
  go elems [] false no_element
;;

let run_nth
      (index : int)
      (element : 's element)
      (ctx : 's Ctx.t)
      (elems : Siesta.Syntax.elem list)
  : (Siesta.Green.child list, string) result
  =
  match List.nth_opt elems index with
  | None -> Error no_element
  | Some elem ->
    (match element ctx elem with
     | None -> Error "another shape"
     | Some (Error _ as failed) -> failed
     | Some (Ok child) ->
       Ok (List.mapi elems ~f:(fun i elem -> if i = index then child else original elem)))
;;

let run_elems (element : 'r -> 's element) (es : 'r Elems.t)
  : 's Ctx.t -> Siesta.Syntax.elem list -> (Siesta.Green.child list, string) result
  =
  match es with
  | Elems.All r -> run_all (element r)
  | Elems.One r -> run_one (element r)
  | Elems.Some_of r -> run_some (element r)
  | Elems.Nth (index, r) -> run_nth index (element r)
;;

module Slot = struct
  type 's rule = 's t

  type 's t =
    's Ctx.t -> Siesta.Syntax.elem list -> (Siesta.Green.child list, string) result

  let node (s : 's rule) : 's t = run_all (node_element s)
  let token (s : 's Token.t) : 's t = run_all (token_element s)
  let elem (e : 's Elem.t) : 's t = run_all (elem_element e)
  let nodes (es : 's rule Elems.t) : 's t = run_elems node_element es
  let tokens (es : 's Token.t Elems.t) : 's t = run_elems token_element es
  let elems (es : 's Elem.t Elems.t) : 's t = run_elems elem_element es
end

let congruence
      (k : Ir.Kind.t)
      (slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
      (rules : 's Slot.t option array)
  : 's t
  =
  fun ctx node ->
  match slots node with
  | Some elems when Siesta.Syntax.kind node = k ->
    let rebuild = Rebuild.start node in
    let rec go (slot : int) =
      if slot = Array.length rules
      then Ok (Rebuild.finish ctx rebuild)
      else (
        match rules.(slot) with
        | None -> go (slot + 1)
        | Some rule ->
          let elems = if slot < Array.length elems then elems.(slot) else [] in
          (match rule ctx elems with
           | Error _ as failed -> failed
           | Ok children ->
             List.iter2 elems children ~f:(Rebuild.set rebuild);
             go (slot + 1)))
    in
    go 0
  | Some _ | None -> Error "another kind"
;;

(* {1 Constructors} *)

module Construct = struct
  type part =
    | Frame of Siesta.Green.child
    | Slot of int * Siesta.Green.child list
    | Separated of
        { slot : int
        ; leading : bool
        ; trailing : bool
        ; sep : Siesta.Green.child
        ; elements : Siesta.Green.child list
        }

  let token (cache : Siesta.Cache.t) (k : Ir.Kind.t) (text : string) : Siesta.Green.child =
    Siesta.Green.Token (Siesta.Green.mk_token cache ~kind:k ~text)
  ;;

  let node (syntax : Siesta.Syntax.t) : Siesta.Green.child =
    Siesta.Green.Node (Siesta.Syntax.green syntax)
  ;;

  let frame (child : Siesta.Green.child) : part = Frame child

  let slot (index : int) (elements : Siesta.Green.child list) : part =
    Slot (index, elements)
  ;;

  let separated
        (index : int)
        ~(leading : bool)
        ~(trailing : bool)
        (sep : Siesta.Green.child)
        (elements : Siesta.Green.child list)
    : part
    =
    Separated { slot = index; leading; trailing; sep; elements }
  ;;

  (* Where a comment goes: in front of element [j] of child [i], straight
     after it, or at the end of the node. *)
  type anchor =
    | Before of int * int
    | After of int * int
    | End

  (* The comments directly inside [node], each with its anchor, in order. A
     comment in front of a frame token anchors after the element before it.
     One with no element before it waits for the next element. *)
  let comments
        (node : Siesta.Syntax.t)
        ~(slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
        ~(trivia : Ir.Kind.t -> bool)
        ~(comment : Ir.Kind.t -> bool)
    : (anchor * Siesta.Green.child) list
    =
    let places = Hashtbl.create 16 in
    Array.iteri
      (Option.value (slots node) ~default:[||])
      ~f:(fun (i : int) (elems : Siesta.Syntax.elem list) ->
        List.iteri elems ~f:(fun (j : int) (elem : Siesta.Syntax.elem) ->
          Hashtbl.replace places (index elem) (i, j)));
    let found = ref [] in
    let pending = ref [] in
    let last = ref None in
    let flush (anchor : anchor) =
      List.iter (List.rev !pending) ~f:(fun child -> found := (anchor, child) :: !found);
      pending := []
    in
    Array.iter (Siesta.Syntax.children_array node) ~f:(fun (elem : Siesta.Syntax.elem) ->
      let k = Siesta.Syntax.elem_kind elem in
      match Hashtbl.find_opt places (index elem), elem with
      | Some (i, j), _ ->
        flush (Before (i, j));
        last := Some (i, j)
      | None, Siesta.Syntax.Token token when comment k ->
        pending := Siesta.Green.Token (Siesta.Syntax.Token.green token) :: !pending
      | None, Siesta.Syntax.Token _ when trivia k -> ()
      | None, (Siesta.Syntax.Token _ | Siesta.Syntax.Node _) ->
        (match !last with
         | Some (i, j) -> flush (After (i, j))
         | None -> ()));
    flush End;
    List.rev !found
  ;;

  (* The children in order, with each comment put back by its anchor. *)
  let place (comments : (anchor * Siesta.Green.child) list) (parts : part list)
    : Siesta.Green.child list
    =
    let taken = Hashtbl.create 16 in
    let take (wanted : anchor -> bool) : Siesta.Green.child list =
      List.filteri comments ~f:(fun (n : int) ((anchor : anchor), _) ->
        if (not (Hashtbl.mem taken n)) && wanted anchor
        then (
          Hashtbl.replace taken n ();
          true)
        else false)
      |> List.map ~f:snd
    in
    let at (anchor : anchor) : Siesta.Green.child list =
      take (fun (a : anchor) -> a = anchor)
    in
    (* An element the new node no longer has leaves its comments at the end
       of the child. *)
    let beyond (slot : int) (count : int) : Siesta.Green.child list =
      take (fun (a : anchor) ->
        match a with
        | Before (i, j) | After (i, j) -> i = slot && j >= count
        | End -> false)
    in
    let elements
          (slot : int)
          (sep : Siesta.Green.child option)
          (elements : Siesta.Green.child list)
      : Siesta.Green.child list
      =
      let count = List.length elements in
      List.concat
        (List.mapi elements ~f:(fun (j : int) (element : Siesta.Green.child) ->
           let after =
             match sep with
             | Some sep when j < count - 1 -> [ sep ]
             | Some _ | None -> []
           in
           at (Before (slot, j)) @ (element :: at (After (slot, j))) @ after))
      @ beyond slot count
    in
    let last = List.length parts - 1 in
    let built =
      List.concat
        (List.mapi parts ~f:(fun (n : int) (part : part) ->
           match part with
           | Frame child when n = last -> at End @ [ child ]
           | Frame child -> [ child ]
           | Slot (slot, children) -> elements slot None children
           | Separated { slot; leading; trailing; sep; elements = children } ->
             let body = elements slot (Some sep) children in
             if children = []
             then body
             else
               (if leading then [ sep ] else []) @ body @ if trailing then [ sep ] else []))
    in
    built @ take (fun (_ : anchor) -> true)
  ;;

  let finish
        ?(replacing : Siesta.Syntax.t option)
        ~(slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
        ~(trivia : Ir.Kind.t -> bool)
        ~(comment : Ir.Kind.t -> bool)
        (cache : Siesta.Cache.t)
        (k : Ir.Kind.t)
        (cast : Siesta.Syntax.t -> 'view option)
        (parts : part list)
    : ('view, string) result
    =
    let comments =
      match replacing with
      | None -> []
      | Some node -> comments node ~slots ~trivia ~comment
    in
    let green =
      Siesta.Green.mk_node
        cache
        ~kind:k
        ~children:(Array.of_list (place comments parts))
        ()
    in
    match cast (Siesta.Syntax.of_root green) with
    | Some view -> Ok view
    | None -> Error "the view does not take its own kind"
  ;;
end
