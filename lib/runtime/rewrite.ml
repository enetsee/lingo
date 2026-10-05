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

  (* The last child that is not trivia. *)
  let last_meaningful ~(trivia : Ir.Kind.t -> bool) (node : Siesta.Syntax.t)
    : Siesta.Syntax.elem option
    =
    Array.fold_left (Siesta.Syntax.children_array node) ~init:None ~f:(fun last elem ->
      if trivia (Siesta.Syntax.elem_kind elem) then last else Some elem)
  ;;

  let rec takes
            ~(slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
            ~(trivia : Ir.Kind.t -> bool)
            ~(open_after : Ir.Kind.t -> int -> Ir.Kind.t -> bool)
            (node : Siesta.Syntax.t)
            (next : Ir.Kind.t)
    : bool
    =
    match slots node with
    | None -> false
    | Some filled ->
      let j = ref (-1) in
      Array.iteri filled ~f:(fun (i : int) (elems : Siesta.Syntax.elem list) ->
        if elems <> [] then j := i);
      open_after (Siesta.Syntax.kind node) !j next
      ||
        (match !j, last_meaningful ~trivia node with
        | -1, _ | _, None -> false
        | j, Some last ->
          (match List.rev filled.(j) with
           | Siesta.Syntax.Node child :: _
             when Siesta.Syntax.index_in_parent child = index last ->
             takes ~slots ~trivia ~open_after child next
           | _ -> false))
  ;;

  (* The kind of the first token in [child] that is not trivia. *)
  let rec first_token ~(trivia : Ir.Kind.t -> bool) (child : Siesta.Green.child)
    : Ir.Kind.t option
    =
    match child with
    | Siesta.Green.Token token ->
      let k = Siesta.Green.Token.kind token in
      if trivia k then None else Some k
    | Siesta.Green.Node node ->
      Array.fold_left (Siesta.Green.children_array node) ~init:None ~f:(fun found child ->
        match found with
        | Some _ -> found
        | None -> first_token ~trivia child)
  ;;

  (* Whether some node child would take the first token after it. *)
  let clashes
        ~(trivia : Ir.Kind.t -> bool)
        ~(takes : Siesta.Syntax.t -> Ir.Kind.t -> bool)
        (children : Siesta.Green.child list)
    : bool
    =
    let rec go (children : Siesta.Green.child list) =
      match children with
      | [] -> false
      | Siesta.Green.Node node :: rest ->
        (match List.find_map rest ~f:(first_token ~trivia) with
         | Some next when takes (Siesta.Syntax.of_root node) next -> true
         | Some _ | None -> go rest)
      | Siesta.Green.Token _ :: rest -> go rest
    in
    go children
  ;;

  let finish
        ?(replacing : Siesta.Syntax.t option)
        ~(slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
        ~(trivia : Ir.Kind.t -> bool)
        ~(comment : Ir.Kind.t -> bool)
        ~(takes : Siesta.Syntax.t -> Ir.Kind.t -> bool)
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
    let children = place comments parts in
    if clashes ~trivia ~takes children
    then Error "a child would take the token after it"
    else (
      let green =
        Siesta.Green.mk_node cache ~kind:k ~children:(Array.of_list children) ()
      in
      match cast (Siesta.Syntax.of_root green) with
      | Some view -> Ok view
      | None -> Error "the view does not take its own kind")
  ;;
end

(* {1 Parentheses} *)

module Parens = struct
  let rec ends_open
            ~(trivia : Ir.Kind.t -> bool)
            ~(expression : Ir.Kind.t -> bool)
            (node : Siesta.Syntax.t)
    : bool
    =
    let last =
      Array.fold_left (Siesta.Syntax.children_array node) ~init:None ~f:(fun last elem ->
        if trivia (Siesta.Syntax.elem_kind elem) then last else Some elem)
    in
    match last with
    | None | Some (Siesta.Syntax.Token _) -> false
    | Some (Siesta.Syntax.Node child) ->
      expression (Siesta.Syntax.kind child) || ends_open ~trivia ~expression child
  ;;
end

(* {1 List edits} *)

module Edit = struct
  (* The raw index of the first token of kind [k] after [from], or [None]
     where a meaningful child that is not one comes first. *)
  let next_of
        ~(trivia : Ir.Kind.t -> bool)
        (children : Siesta.Syntax.elem array)
        (from : int)
        (k : Ir.Kind.t)
    : int option
    =
    let rec go (i : int) =
      if i >= Array.length children
      then None
      else (
        let kind = Siesta.Syntax.elem_kind children.(i) in
        if trivia kind
        then go (i + 1)
        else (
          match children.(i) with
          | Siesta.Syntax.Token _ when kind = k -> Some i
          | Siesta.Syntax.Token _ | Siesta.Syntax.Node _ -> None))
    in
    go (from + 1)
  ;;

  (* The same, looking back from [from]. *)
  let previous_of
        ~(trivia : Ir.Kind.t -> bool)
        (children : Siesta.Syntax.elem array)
        (from : int)
        (k : Ir.Kind.t)
    : int option
    =
    let rec go (i : int) =
      if i < 0
      then None
      else (
        let kind = Siesta.Syntax.elem_kind children.(i) in
        if trivia kind
        then go (i - 1)
        else (
          match children.(i) with
          | Siesta.Syntax.Token _ when kind = k -> Some i
          | Siesta.Syntax.Token _ | Siesta.Syntax.Node _ -> None))
    in
    go (from - 1)
  ;;

  (* The first raw index with a token of kind [k]. *)
  let find (children : Siesta.Syntax.elem array) (k : Ir.Kind.t) : int option =
    let found = ref None in
    Array.iteri children ~f:(fun (i : int) (elem : Siesta.Syntax.elem) ->
      match !found, elem with
      | None, Siesta.Syntax.Token token when Siesta.Syntax.Token.kind token = k ->
        found := Some i
      | _ -> ());
    !found
  ;;

  let splice
        (ctx : 's Ctx.t)
        (node : Siesta.Syntax.t)
        ~(from : int)
        ~(remove : int)
        (inserts : Siesta.Green.child list)
    : Siesta.Green.node
    =
    let green = Siesta.Syntax.green node in
    let children = Siesta.Green.children_array green in
    let kept_before = Array.to_list (Array.sub children ~pos:0 ~len:from) in
    let kept_after =
      Array.to_list
        (Array.sub
           children
           ~pos:(from + remove)
           ~len:(Array.length children - from - remove))
    in
    Siesta.Green.mk_node
      (Ctx.cache ctx)
      ~kind:(Siesta.Green.kind green)
      ~payload:(Siesta.Green.payload green)
      ~children:(Array.of_list (kept_before @ inserts @ kept_after))
      ()
  ;;

  (* Where an empty child's first element goes: after the opener, or after
     the last element of an earlier child, or at the start. *)
  let empty_place
        (children : Siesta.Syntax.elem array)
        (filled : Siesta.Syntax.elem list array)
        ~(slot : int)
        ~(opener : Ir.Kind.t option)
    : int
    =
    let earlier =
      Array.to_list (Array.sub filled ~pos:0 ~len:(min slot (Array.length filled)))
      |> List.concat
      |> List.map ~f:index
      |> List.fold_left ~init:(-1) ~f:max
    in
    match Option.bind opener (find children) with
    | Some o when o > earlier -> o + 1
    | Some _ | None -> earlier + 1
  ;;

  let elements
        ~(kind : Ir.Kind.t)
        ~(slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
        ~(slot : int)
        (node : Siesta.Syntax.t)
    : (Siesta.Syntax.elem list array * Siesta.Syntax.elem array, string) result
    =
    match slots node with
    | Some filled when Siesta.Syntax.kind node = kind && slot < Array.length filled ->
      Ok (filled, Array.of_list (List.map filled.(slot) ~f:(fun e -> e)))
    | Some _ | None -> Error "another kind"
  ;;

  let insert
        ~(kind : Ir.Kind.t)
        ~(slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
        ~(trivia : Ir.Kind.t -> bool)
        ~(slot : int)
        ~(sep : (Ir.Kind.t * string) option)
        ~(opener : Ir.Kind.t option)
        ~(at : int)
        (element : Siesta.Green.child)
    : 's t
    =
    fun ctx node ->
    match elements ~kind ~slots ~slot node with
    | Error _ as failed -> failed
    | Ok (_, elems) when at < 0 || at > Array.length elems -> Error "no such place"
    | Ok (filled, elems) ->
      let children = Siesta.Syntax.children_array node in
      let n = Array.length elems in
      let separator () : Siesta.Green.child =
        match sep with
        | Some (k, text) -> Construct.token (Ctx.cache ctx) k text
        | None -> invalid_arg "Edit.insert: no separator"
      in
      let put (from : int) (inserts : Siesta.Green.child list) =
        Ok (splice ctx node ~from ~remove:0 inserts)
      in
      (* Straight after the last meaningful child in front of element [at],
         so the comments in front of it stay its own. That is the separator
         before it, a leading separator, the opener, or the element before
         it where the child has no separator. *)
      let before (at : int) : int =
        let rec back (i : int) =
          if i < 0 || not (trivia (Siesta.Syntax.elem_kind children.(i)))
          then i + 1
          else back (i - 1)
        in
        back (index elems.(at) - 1)
      in
      (match sep with
       | Some (k, _) when n = 0 ->
         (* A lone separator in an empty body stays in front of the element. *)
         let place = empty_place children filled ~slot ~opener in
         (match next_of ~trivia children (place - 1) k with
          | Some lone -> put (lone + 1) [ element ]
          | None -> put place [ element ])
       | None when n = 0 -> put (empty_place children filled ~slot ~opener) [ element ]
       | None when at < n -> put (before at) [ element ]
       | None -> put (index elems.(n - 1) + 1) [ element ]
       | Some (k, _) when at = n ->
         let last = index elems.(n - 1) in
         (match next_of ~trivia children last k with
          | Some trailing -> put (trailing + 1) [ element; separator () ]
          | None -> put (last + 1) [ separator (); element ])
       | Some _ -> put (before at) [ element; separator () ])
  ;;

  let delete
        ~(kind : Ir.Kind.t)
        ~(slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option)
        ~(trivia : Ir.Kind.t -> bool)
        ~(slot : int)
        ~(sep : (Ir.Kind.t * string) option)
        ~(required : bool)
        ~(at : int)
    : 's t
    =
    fun ctx node ->
    match elements ~kind ~slots ~slot node with
    | Error _ as failed -> failed
    | Ok (_, elems) when at < 0 || at >= Array.length elems -> Error "no such element"
    | Ok (_, elems) when required && Array.length elems = 1 ->
      Error "at least one is required"
    | Ok (_, elems) ->
      let children = Siesta.Syntax.children_array node in
      let n = Array.length elems in
      let here = index elems.(at) in
      (* Everything after the meaningful child in front of the element, so
         the comments directly in front of it go with it. *)
      let start =
        let rec back (i : int) =
          if i < 0 || not (trivia (Siesta.Syntax.elem_kind children.(i)))
          then i + 1
          else back (i - 1)
        in
        back (here - 1)
      in
      let remove (from : int) (last : int) =
        Ok (splice ctx node ~from ~remove:(last - from + 1) [])
      in
      (match sep with
       | None -> remove start here
       | Some (k, _) ->
         (match next_of ~trivia children here k with
          | Some after when at < n - 1 -> remove start after
          | Some trailing when n = 1 -> remove start trailing
          | Some _ | None ->
            (match previous_of ~trivia children here k with
             | Some before when at > 0 -> remove before here
             | Some _ | None -> remove start here)))
  ;;
end

(* {1 Applying a result} *)

type splice =
  { range : int * int
  ; text : string
  }

let splice (text : string) (splices : splice list) : string =
  let ordered =
    List.stable_sort splices ~cmp:(fun (a : splice) (b : splice) ->
      compare a.range b.range)
  in
  let buffer = Buffer.create (String.length text) in
  let pos =
    List.fold_left ordered ~init:0 ~f:(fun (pos : int) (s : splice) ->
      let start, stop = s.range in
      Buffer.add_string buffer (String.sub text ~pos ~len:(start - pos));
      Buffer.add_string buffer s.text;
      stop)
  in
  Buffer.add_string buffer (String.sub text ~pos ~len:(String.length text - pos));
  Buffer.contents buffer
;;

(* The longest common subsequence of two tag arrays, as index pairs in
   order. *)
let common (olds : int array) (news : int array) : (int * int) list =
  let n = Array.length olds in
  let m = Array.length news in
  let table = Array.make_matrix ~dimx:(n + 1) ~dimy:(m + 1) 0 in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      table.(i).(j)
      <- (if olds.(i) = news.(j)
          then table.(i + 1).(j + 1) + 1
          else max table.(i + 1).(j) table.(i).(j + 1))
    done
  done;
  let rec walk (i : int) (j : int) (acc : (int * int) list) =
    if i = n || j = m
    then List.rev acc
    else if olds.(i) = news.(j)
    then walk (i + 1) (j + 1) ((i, j) :: acc)
    else if table.(i + 1).(j) >= table.(i).(j + 1)
    then walk (i + 1) j acc
    else walk i (j + 1) acc
  in
  walk 0 0 []
;;

let tag_of (node : Siesta.Syntax.t) : int = Siesta.Green.tag (Siesta.Syntax.green node)

let apply
      ~(format : Siesta.Green.node -> string)
      ~(items : Siesta.Syntax.t -> Siesta.Syntax.t list option)
      ~(between : string)
      ~(trivia : Ir.Kind.t -> bool)
      ~(comment : Ir.Kind.t -> bool)
      ~(before : Siesta.Green.node)
      ~(after : Siesta.Green.node)
  : splice list
  =
  let old_root = Siesta.Syntax.of_root before in
  let new_root = Siesta.Syntax.of_root after in
  let whole () = [ { range = 0, Siesta.Green.text_len before; text = format after } ] in
  (* The root's children that are neither trivia nor items, by tag. *)
  let outside (root : Siesta.Syntax.t) (items : Siesta.Syntax.t list) : int list =
    let inside = List.map items ~f:Siesta.Syntax.index_in_parent in
    Array.to_list (Siesta.Syntax.children_array root)
    |> List.filter_map ~f:(fun (elem : Siesta.Syntax.elem) ->
      match elem with
      | Siesta.Syntax.Node node
        when not (List.mem (Siesta.Syntax.index_in_parent node) ~set:inside) ->
        Some (tag_of node)
      | Siesta.Syntax.Token token when not (trivia (Siesta.Syntax.Token.kind token)) ->
        Some (Siesta.Green.Token.tag (Siesta.Syntax.Token.green token))
      | Siesta.Syntax.Node _ | Siesta.Syntax.Token _ -> None)
  in
  if Siesta.Green.equal before after
  then []
  else (
    match items old_root, items new_root with
    | Some olds, Some news when outside old_root olds = outside new_root news ->
      let olds = Array.of_list olds in
      let news = Array.of_list news in
      let children = Siesta.Syntax.children_array old_root in
      let source = Siesta.Green.to_source before in
      let is_space (elem : Siesta.Syntax.elem) : bool =
        let k = Siesta.Syntax.elem_kind elem in
        trivia k && not (comment k)
      in
      let start_of (elem : Siesta.Syntax.elem) : int =
        fst (Siesta.Syntax.elem_text_range elem)
      in
      (* Where an item's run of comments starts: the comments directly
         above it, with no blank line between any two of them or between the
         last and the item. *)
      let comments_start (item : Siesta.Syntax.t) : int =
        let rec back (i : int) (start : int) =
          if i < 0
          then start
          else (
            let elem = children.(i) in
            let k = Siesta.Syntax.elem_kind elem in
            if comment k
            then back (i - 1) (start_of elem)
            else if is_space elem
            then (
              let a, b = Siesta.Syntax.elem_text_range elem in
              let lines =
                String.fold_left
                  (String.sub source ~pos:a ~len:(b - a))
                  ~init:0
                  ~f:(fun n ch -> if ch = '\n' then n + 1 else n)
              in
              if lines >= 2 then start else back (i - 1) start)
            else start)
        in
        back
          (Siesta.Syntax.index_in_parent item - 1)
          (fst (Siesta.Syntax.text_range item))
      in
      (* An item with its comments, and the whitespace after it where
         something follows, or before it where nothing does. *)
      let deleted (item : Siesta.Syntax.t) : int * int =
        let start = comments_start item in
        let stop = snd (Siesta.Syntax.text_range item) in
        let rec forward (i : int) =
          if i >= Array.length children
          then None
          else if is_space children.(i)
          then forward (i + 1)
          else Some (start_of children.(i))
        in
        match forward (Siesta.Syntax.index_in_parent item + 1) with
        | Some next -> start, next
        | None ->
          let rec backward (i : int) =
            if i < 0
            then 0
            else if is_space children.(i)
            then backward (i - 1)
            else snd (Siesta.Syntax.elem_text_range children.(i))
          in
          (* The comments' own index is not tracked, so walk back from the
             first child that starts at or after [start]. *)
          let first =
            let rec find (i : int) =
              if i >= Array.length children || start_of children.(i) >= start
              then i
              else find (i + 1)
            in
            find 0
          in
          backward (first - 1), stop
      in
      let old_tags = Array.map olds ~f:tag_of in
      let new_tags = Array.map news ~f:tag_of in
      let pairs = common old_tags new_tags in
      (* An old item the rewrite took out, by tag, for a new one to move. *)
      let gone = Hashtbl.create 8 in
      let replaced = Array.make (Array.length olds) false in
      let kept = Array.make (Array.length olds) false in
      List.iter pairs ~f:(fun (i, _) -> kept.(i) <- true);
      let splices = ref [] in
      let add (s : splice) = splices := s :: !splices in
      let inserts = ref [] in
      (* Walk the gaps between matched items. *)
      let rec gaps (prev_i : int) (prev_j : int) (pairs : (int * int) list) =
        let next_i, next_j, rest =
          match pairs with
          | (i, j) :: rest -> i, j, Some rest
          | [] -> Array.length olds, Array.length news, None
        in
        let old_gap = List.init ~len:(next_i - prev_i - 1) ~f:(fun t -> prev_i + 1 + t) in
        let new_gap = List.init ~len:(next_j - prev_j - 1) ~f:(fun t -> prev_j + 1 + t) in
        let rec pair (olds_left : int list) (news_left : int list) (last : int) =
          match olds_left, news_left with
          | i :: olds_rest, j :: news_rest ->
            replaced.(i) <- true;
            add
              { range = Siesta.Syntax.text_range olds.(i)
              ; text = format (Siesta.Syntax.green news.(j))
              };
            pair olds_rest news_rest i
          | i :: olds_rest, [] ->
            Hashtbl.add gone old_tags.(i) i;
            pair olds_rest [] last
          | [], news_left -> inserts := (last, next_i, news_left) :: !inserts
        in
        pair old_gap new_gap prev_i;
        match rest with
        | Some rest -> gaps next_i next_j rest
        | None -> ()
      in
      gaps (-1) (-1) pairs;
      (* A deleted item that a new one carries on as is moves, and keeps its
         bytes and its comments. *)
      let text_of (j : int) : string =
        match Hashtbl.find_opt gone new_tags.(j) with
        | Some i ->
          Hashtbl.remove gone new_tags.(j);
          let a = comments_start olds.(i) in
          let b = snd (Siesta.Syntax.text_range olds.(i)) in
          String.sub source ~pos:a ~len:(b - a)
        | None -> format (Siesta.Syntax.green news.(j))
      in
      List.iter
        (List.rev !inserts)
        ~f:(fun ((last : int), (next : int), (js : int list)) ->
          match js with
          | [] -> ()
          | js ->
            let texts = List.map js ~f:text_of in
            if last >= 0
            then
              add
                { range =
                    (let e = snd (Siesta.Syntax.text_range olds.(last)) in
                     e, e)
                ; text = String.concat ~sep:"" (List.map texts ~f:(fun t -> between ^ t))
                }
            else if next < Array.length olds
            then (
              let s = comments_start olds.(next) in
              add
                { range = s, s
                ; text = String.concat ~sep:"" (List.map texts ~f:(fun t -> t ^ between))
                })
            else (
              let e = Siesta.Green.text_len before in
              let lead = if e > 0 && source.[e - 1] <> '\n' then "\n" else "" in
              add { range = e, e; text = lead ^ String.concat ~sep:between texts }));
      (* Every old item neither kept nor replaced is deleted. *)
      Array.iteri olds ~f:(fun (i : int) (item : Siesta.Syntax.t) ->
        if not (kept.(i) || replaced.(i)) then add { range = deleted item; text = "" });
      List.stable_sort !splices ~cmp:(fun (a : splice) (b : splice) ->
        compare a.range b.range)
    | Some _, Some _ | None, _ | _, None -> whole ())
;;

(* {1 Templates} *)

module Template = struct
  type binding =
    | One of Siesta.Green.child
    | Run of Siesta.Green.child list

  type kinds =
    { trivia : Ir.Kind.t -> bool
    ; single : Ir.Kind.t -> bool
    ; sequence : Ir.Kind.t -> bool
    ; base : Ir.Kind.t -> bool
    }

  let rec relabel
            (cache : Siesta.Cache.t)
            (map : Ir.Kind.t -> Ir.Kind.t)
            (tree : Siesta.Green.node)
    : Siesta.Green.node
    =
    let children =
      Array.map (Siesta.Green.children_array tree) ~f:(fun (child : Siesta.Green.child) ->
        match child with
        | Siesta.Green.Node node -> Siesta.Green.Node (relabel cache map node)
        | Siesta.Green.Token token ->
          Siesta.Green.Token
            (Siesta.Green.mk_token
               cache
               ~kind:(map (Siesta.Green.Token.kind token))
               ~text:(Siesta.Green.Token.text token)))
    in
    Siesta.Green.mk_node
      cache
      ~kind:(map (Siesta.Green.kind tree))
      ~payload:(Siesta.Green.payload tree)
      ~children
      ()
  ;;

  let name (text : string) : string =
    match String.index_opt text ':' with
    | Some i -> String.sub text ~pos:0 ~len:i
    | None -> text
  ;;

  let meaningful (kinds : kinds) (node : Siesta.Green.node) : Siesta.Green.child list =
    List.filter
      (Array.to_list (Siesta.Green.children_array node))
      ~f:(fun child ->
        match child with
        | Siesta.Green.Token token -> not (kinds.trivia (Siesta.Green.Token.kind token))
        | Siesta.Green.Node _ -> true)
  ;;

  (* The metavariable a template child is, by its kind. A node holding only
     a single metavariable is one too. A node holding only a sequence one is
     a list, unless it is a block's base node, which is how [f($$xs)] reads. *)
  let rec metavariable (kinds : kinds) (child : Siesta.Green.child)
    : (string * bool) option
    =
    match child with
    | Siesta.Green.Token token ->
      let k = Siesta.Green.Token.kind token in
      if kinds.single k
      then Some (name (Siesta.Green.Token.text token), false)
      else if kinds.sequence k
      then Some (name (Siesta.Green.Token.text token), true)
      else None
    | Siesta.Green.Node node ->
      (match meaningful kinds node with
       | [ (Siesta.Green.Token _ as only) ] ->
         (match metavariable kinds only with
          | Some (_, false) as single -> single
          | Some (_, true) as run when kinds.base (Siesta.Green.kind node) -> run
          | Some (_, true) | None -> None)
       | _ -> None)
  ;;

  (* Two children are the same text, trivia left out. *)
  let rec same (kinds : kinds) (a : Siesta.Green.child) (b : Siesta.Green.child) : bool =
    match a, b with
    | Siesta.Green.Token x, Siesta.Green.Token y ->
      Siesta.Green.Token.kind x = Siesta.Green.Token.kind y
      && String.equal (Siesta.Green.Token.text x) (Siesta.Green.Token.text y)
    | Siesta.Green.Node x, Siesta.Green.Node y ->
      Siesta.Green.kind x = Siesta.Green.kind y
      &&
      let xs = meaningful kinds x in
      let ys = meaningful kinds y in
      List.length xs = List.length ys && List.for_all2 xs ys ~f:(same kinds)
    | Siesta.Green.Token _, Siesta.Green.Node _
    | Siesta.Green.Node _, Siesta.Green.Token _ -> false
  ;;

  let consistent (kinds : kinds) (a : binding) (b : binding) : bool =
    match a, b with
    | One x, One y -> same kinds x y
    | Run xs, Run ys ->
      List.length xs = List.length ys && List.for_all2 xs ys ~f:(same kinds)
    | One _, Run _ | Run _, One _ -> false
  ;;

  let bind (kinds : kinds) (bound : (string * binding) list) (name : string) (b : binding)
    : (string * binding) list option
    =
    match List.assoc_opt name bound with
    | Some earlier -> if consistent kinds earlier b then Some bound else None
    | None -> Some ((name, b) :: bound)
  ;;

  (* Matching is a walk over the two lists of children. A sequence
     metavariable tries every run, shortest first. *)
  let rec children
            (kinds : kinds)
            (bound : (string * binding) list)
            (template : Siesta.Green.child list)
            (input : Siesta.Green.child list)
    : (string * binding) list option
    =
    match template, input with
    | [], [] -> Some bound
    | [], _ :: _ -> None
    | t :: template_rest, _ ->
      (match metavariable kinds t, input with
       | Some (name, true), _ ->
         let rec runs (taken : Siesta.Green.child list) (rest : Siesta.Green.child list) =
           match
             Option.bind
               (bind kinds bound name (Run (List.rev taken)))
               (fun bound -> children kinds bound template_rest rest)
           with
           | Some _ as found -> found
           | None ->
             (match rest with
              | [] -> None
              | next :: rest -> runs (next :: taken) rest)
         in
         runs [] input
       | Some (name, false), i :: input_rest ->
         Option.bind (bind kinds bound name (One i)) (fun bound ->
           children kinds bound template_rest input_rest)
       | None, i :: input_rest ->
         Option.bind (child kinds bound t i) (fun bound ->
           children kinds bound template_rest input_rest)
       | _, [] -> None)

  and child
        (kinds : kinds)
        (bound : (string * binding) list)
        (t : Siesta.Green.child)
        (i : Siesta.Green.child)
    : (string * binding) list option
    =
    match t, i with
    | Siesta.Green.Token x, Siesta.Green.Token y ->
      if
        Siesta.Green.Token.kind x = Siesta.Green.Token.kind y
        && String.equal (Siesta.Green.Token.text x) (Siesta.Green.Token.text y)
      then Some bound
      else None
    | Siesta.Green.Node x, Siesta.Green.Node y
      when Siesta.Green.kind x = Siesta.Green.kind y ->
      children kinds bound (meaningful kinds x) (meaningful kinds y)
    | Siesta.Green.Node _, _ | Siesta.Green.Token _, Siesta.Green.Node _ -> None
  ;;

  let fragment (kinds : kinds) (root : Siesta.Green.node) : Siesta.Green.node option =
    match meaningful kinds root with
    | [ Siesta.Green.Node node ] -> Some node
    | _ -> None
  ;;

  let matches (kinds : kinds) (template : Siesta.Green.node) (node : Siesta.Syntax.t)
    : (string * binding) list option
    =
    let input = Siesta.Green.Node (Siesta.Syntax.green node) in
    match metavariable kinds (Siesta.Green.Node template) with
    | Some (name, false) -> bind kinds [] name (One input)
    | Some (name, true) -> bind kinds [] name (Run [ input ])
    | None -> Option.map List.rev (child kinds [] (Siesta.Green.Node template) input)
  ;;

  let instantiate
        (cache : Siesta.Cache.t)
        (kinds : kinds)
        (template : Siesta.Green.node)
        (bound : (string * binding) list)
    : (Siesta.Green.node, string) result
    =
    let exception Unbound of string in
    let rec build (node : Siesta.Green.node) : Siesta.Green.node =
      let children =
        List.concat_map
          (Array.to_list (Siesta.Green.children_array node))
          ~f:(fun child ->
            match metavariable kinds child with
            | None ->
              (match child with
               | Siesta.Green.Node inner -> [ Siesta.Green.Node (build inner) ]
               | Siesta.Green.Token _ -> [ child ])
            | Some (name, _) ->
              (match List.assoc_opt name bound with
               | Some (One bound) -> [ bound ]
               | Some (Run run) -> run
               | None -> raise_notrace (Unbound name)))
      in
      Siesta.Green.mk_node
        cache
        ~kind:(Siesta.Green.kind node)
        ~payload:(Siesta.Green.payload node)
        ~children:(Array.of_list children)
        ()
    in
    match metavariable kinds (Siesta.Green.Node template) with
    | Some (name, false) ->
      (match List.assoc_opt name bound with
       | Some (One (Siesta.Green.Node node)) -> Ok node
       | Some (One (Siesta.Green.Token _)) ->
         Error (name ^ " is a token, and a template is a node")
       | Some (Run _) -> Error (name ^ " is a run, where one child goes")
       | None -> Error (name ^ " is not bound"))
    | Some (_, true) | None ->
      (try Ok (build template) with
       | Unbound name -> Error (name ^ " is not bound"))
  ;;
end

(* {1 Binders} *)

module Binders = struct
  type 's rule = 's t

  type t =
    { slots : Siesta.Syntax.t -> Siesta.Syntax.elem list array option
    ; trivia : Ir.Kind.t -> bool
    ; scope : Ir.Kind.t -> bool
    ; binders : Ir.Kind.t -> int list
    ; reference : Ir.Kind.t -> bool
    ; base : Ir.Kind.t -> bool
    }

  let start_of (token : Siesta.Syntax.token_cursor) : int =
    fst (Siesta.Syntax.Token.text_range token)
  ;;

  (* The binder tokens a node holds itself. *)
  let own (t : t) (node : Siesta.Syntax.t) : Siesta.Syntax.token_cursor list =
    match t.binders (Siesta.Syntax.kind node) with
    | [] -> []
    | indices ->
      (match t.slots node with
       | None -> []
       | Some filled ->
         List.concat_map indices ~f:(fun (i : int) ->
           if i >= Array.length filled
           then []
           else
             List.filter_map filled.(i) ~f:(fun (elem : Siesta.Syntax.elem) ->
               match elem with
               | Siesta.Syntax.Token token -> Some token
               | Siesta.Syntax.Node _ -> None)))
  ;;

  let is_binder (t : t) (token : Siesta.Syntax.token_cursor) : bool =
    List.exists
      (own t (Siesta.Syntax.Token.parent token))
      ~f:(Siesta.Syntax.Token.equal token)
  ;;

  (* Whether [outer] is [inner] or one of its ancestors. *)
  let holds (outer : Siesta.Syntax.t) (inner : Siesta.Syntax.t) : bool =
    Seq.exists
      (fun (a : Siesta.Syntax.t) -> Siesta.Syntax.equal a outer)
      (Siesta.Syntax.ancestors inner)
  ;;

  (* The scope a binder belongs to: the nearest scope at or above the node
     that holds it, or the root. *)
  let scope_of (t : t) (token : Siesta.Syntax.token_cursor) : Siesta.Syntax.t =
    let rec up (node : Siesta.Syntax.t) =
      if t.scope (Siesta.Syntax.kind node)
      then node
      else (
        match Siesta.Syntax.parent node with
        | Some parent -> up parent
        | None -> node)
    in
    up (Siesta.Syntax.Token.parent token)
  ;;

  let root_of (node : Siesta.Syntax.t) : Siesta.Syntax.t =
    Seq.fold_left
      (fun (_ : Siesta.Syntax.t) (a : Siesta.Syntax.t) -> a)
      node
      (Siesta.Syntax.ancestors node)
  ;;

  (* The binders visible inside [at] at offset [position]. A scope that does
     not hold [at] is not walked, and nor is anything that starts at or after
     [position]. *)
  let visible_at (t : t) (at : Siesta.Syntax.t) (position : int)
    : (string * Siesta.Syntax.token_cursor) list
    =
    let found = ref [] in
    Siesta.Syntax.preorder (root_of at) ~f:(fun (node : Siesta.Syntax.t) ->
      if fst (Siesta.Syntax.text_range node) >= position && not (holds node at)
      then Siesta.Syntax.Skip
      else if t.scope (Siesta.Syntax.kind node) && not (holds node at)
      then Siesta.Syntax.Skip
      else (
        List.iter (own t node) ~f:(fun (binder : Siesta.Syntax.token_cursor) ->
          if start_of binder < position
          then (
            let scope = scope_of t binder in
            if holds scope at
            then found := (Seq.length (Siesta.Syntax.ancestors scope), binder) :: !found));
        Siesta.Syntax.Descend));
    List.stable_sort !found ~cmp:(fun ((d1 : int), b1) ((d2 : int), b2) ->
      match compare d2 d1 with
      | 0 -> compare (start_of b2) (start_of b1)
      | c -> c)
    |> List.map ~f:(fun ((_ : int), (binder : Siesta.Syntax.token_cursor)) ->
      Siesta.Syntax.Token.text binder, binder)
  ;;

  let visible (t : t) (at : Siesta.Syntax.t) : (string * Siesta.Syntax.token_cursor) list =
    visible_at t at (fst (Siesta.Syntax.text_range at))
  ;;

  let fresh_avoiding (taken : string list) ~(base : string) : string =
    if not (List.mem base ~set:taken)
    then base
    else (
      let rec go (i : int) =
        let name = base ^ string_of_int i in
        if List.mem name ~set:taken then go (i + 1) else name
      in
      go 1)
  ;;

  let fresh (t : t) (at : Siesta.Syntax.t) ~(base : string) : string =
    fresh_avoiding (List.map (visible t at) ~f:fst) ~base
  ;;

  let resolve (t : t) (token : Siesta.Syntax.token_cursor)
    : Siesta.Syntax.token_cursor option
    =
    if is_binder t token
    then Some token
    else
      List.assoc_opt
        (Siesta.Syntax.Token.text token)
        (visible_at t (Siesta.Syntax.Token.parent token) (start_of token))
  ;;

  (* Every token of a reference kind under [node], in order. *)
  let tokens (t : t) (node : Siesta.Syntax.t) : Siesta.Syntax.token_cursor list =
    let found = ref [] in
    Siesta.Syntax.preorder node ~f:(fun (n : Siesta.Syntax.t) ->
      Array.iter (Siesta.Syntax.children_array n) ~f:(fun (elem : Siesta.Syntax.elem) ->
        match elem with
        | Siesta.Syntax.Token token when t.reference (Siesta.Syntax.Token.kind token) ->
          found := token :: !found
        | Siesta.Syntax.Token _ | Siesta.Syntax.Node _ -> ());
      Siesta.Syntax.Descend);
    List.rev !found
  ;;

  type edit =
    | Spell of string
    | Put of Siesta.Green.node

  (* [node]'s tree with the tokens and nodes [edits] names, by their range,
     edited. A node with no edit inside it is kept as it is. *)
  let rec rebuild
            (cache : Siesta.Cache.t)
            (edits : (int * int * Ir.Kind.t, edit) Hashtbl.t)
            (node : Siesta.Syntax.t)
    : Siesta.Green.node
    =
    let green = Siesta.Syntax.green node in
    let a, b = Siesta.Syntax.text_range node in
    let inside =
      Hashtbl.fold
        (fun ((x : int), (y : int), _) _ found -> found || (a <= x && y <= b))
        edits
        false
    in
    if not inside
    then green
    else (
      let children =
        Array.map
          (Siesta.Syntax.children_array node)
          ~f:(fun (elem : Siesta.Syntax.elem) ->
            let x, y = Siesta.Syntax.elem_text_range elem in
            match elem with
            | Siesta.Syntax.Token token ->
              (match Hashtbl.find_opt edits (x, y, Siesta.Syntax.Token.kind token) with
               | Some (Spell text) ->
                 Siesta.Green.Token
                   (Siesta.Green.mk_token
                      cache
                      ~kind:(Siesta.Syntax.Token.kind token)
                      ~text)
               | Some (Put _) | None ->
                 Siesta.Green.Token (Siesta.Syntax.Token.green token))
            | Siesta.Syntax.Node child ->
              (match Hashtbl.find_opt edits (x, y, Siesta.Syntax.kind child) with
               | Some (Put replacement) -> Siesta.Green.Node replacement
               | Some (Spell _) | None -> Siesta.Green.Node (rebuild cache edits child)))
      in
      Siesta.Green.mk_node
        cache
        ~kind:(Siesta.Green.kind green)
        ~payload:(Siesta.Green.payload green)
        ~children
        ())
  ;;

  let spell
        (edits : (int * int * Ir.Kind.t, edit) Hashtbl.t)
        (token : Siesta.Syntax.token_cursor)
        (text : string)
    : unit
    =
    let x, y = Siesta.Syntax.Token.text_range token in
    Hashtbl.replace edits (x, y, Siesta.Syntax.Token.kind token) (Spell text)
  ;;

  (* The references under [within] that resolve to [binder]. *)
  let uses (t : t) (within : Siesta.Syntax.t) (binder : Siesta.Syntax.token_cursor)
    : Siesta.Syntax.token_cursor list
    =
    List.filter (tokens t within) ~f:(fun (token : Siesta.Syntax.token_cursor) ->
      (not (Siesta.Syntax.Token.equal token binder))
      &&
      match resolve t token with
      | Some found -> Siesta.Syntax.Token.equal found binder
      | None -> false)
  ;;

  (* Whether [to_] would take [use] from [binder]: a binder of that name is
     visible at [use] and nearer than [binder]. *)
  let taken
        (t : t)
        (use : Siesta.Syntax.token_cursor)
        (binder : Siesta.Syntax.token_cursor)
        ~(to_ : string)
    : bool
    =
    let rec nearer (l : (string * Siesta.Syntax.token_cursor) list) =
      match l with
      | [] -> false
      | (_, b) :: _ when Siesta.Syntax.Token.equal b binder -> false
      | (name, _) :: rest -> String.equal name to_ || nearer rest
    in
    nearer (visible_at t (Siesta.Syntax.Token.parent use) (start_of use))
  ;;

  let rename
        (t : t)
        (cache : Siesta.Cache.t)
        (binder : Siesta.Syntax.token_cursor)
        ~(to_ : string)
    : (Siesta.Green.node, string) result
    =
    let root = root_of (Siesta.Syntax.Token.parent binder) in
    let refs = uses t root binder in
    if
      List.mem_assoc
        to_
        ~map:(visible_at t (Siesta.Syntax.Token.parent binder) (start_of binder))
    then Error "the new name is visible where the binder is"
    else if List.exists refs ~f:(fun use -> taken t use binder ~to_)
    then Error "a binder of the new name would take a use"
    else (
      let edits = Hashtbl.create 8 in
      List.iter (binder :: refs) ~f:(fun token -> spell edits token to_);
      Ok (rebuild cache edits root))
  ;;

  (* The names a detached tree uses: its tokens of a reference kind. *)
  let rec names_in (t : t) (green : Siesta.Green.node) : string list =
    List.concat_map
      (Array.to_list (Siesta.Green.children_array green))
      ~f:(fun child ->
        match child with
        | Siesta.Green.Token token when t.reference (Siesta.Green.Token.kind token) ->
          [ Siesta.Green.Token.text token ]
        | Siesta.Green.Token _ -> []
        | Siesta.Green.Node node -> names_in t node)
  ;;

  let substitute (t : t) ~(name : string) ~(by : Siesta.Green.node) : 's rule =
    fun ctx node ->
    let inside (token : Siesta.Syntax.token_cursor) =
      holds node (Siesta.Syntax.Token.parent token)
    in
    let all = tokens t node in
    (* A free use standing as an expression, as its base node. *)
    let sites =
      List.filter_map all ~f:(fun (token : Siesta.Syntax.token_cursor) ->
        let parent = Siesta.Syntax.Token.parent token in
        let alone =
          List.length
            (List.filter
               (Array.to_list (Siesta.Syntax.children_array parent))
               ~f:(fun e -> not (t.trivia (Siesta.Syntax.elem_kind e))))
          = 1
        in
        let free =
          match resolve t token with
          | None -> true
          | Some binder -> not (inside binder)
        in
        if
          String.equal (Siesta.Syntax.Token.text token) name
          && (not (is_binder t token))
          && free
          && alone
          && t.base (Siesta.Syntax.kind parent)
          && holds node parent
        then Some (token, parent)
        else None)
    in
    let wanted = names_in t by in
    (* A binder inside the node that [by] would be taken by at some site. *)
    let capturing =
      List.filter
        (List.filter all ~f:(fun token -> is_binder t token && inside token))
        ~f:(fun (binder : Siesta.Syntax.token_cursor) ->
          List.mem (Siesta.Syntax.Token.text binder) ~set:wanted
          && List.exists sites ~f:(fun ((site : Siesta.Syntax.token_cursor), _) ->
            List.exists
              (visible_at t (Siesta.Syntax.Token.parent site) (start_of site))
              ~f:(fun ((_ : string), b) -> Siesta.Syntax.Token.equal b binder)))
    in
    let edits = Hashtbl.create 8 in
    let used = List.map all ~f:Siesta.Syntax.Token.text @ wanted in
    List.iter capturing ~f:(fun (binder : Siesta.Syntax.token_cursor) ->
      let to_ =
        fresh_avoiding
          (used
           @ List.map
               (visible_at t (Siesta.Syntax.Token.parent binder) (start_of binder))
               ~f:fst)
          ~base:(Siesta.Syntax.Token.text binder)
      in
      List.iter (binder :: uses t node binder) ~f:(fun token -> spell edits token to_));
    List.iter
      sites
      ~f:(fun ((_ : Siesta.Syntax.token_cursor), (parent : Siesta.Syntax.t)) ->
        let x, y = Siesta.Syntax.text_range parent in
        Hashtbl.replace edits (x, y, Siesta.Syntax.kind parent) (Put by));
    Ok (rebuild (Ctx.cache ctx) edits node)
  ;;
end
