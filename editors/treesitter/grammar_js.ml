open StdLabels

exception Cannot of string

(* -- symbols --------------------------------------------------------------- *)

(* A keyword or a punctuation literal goes in as a string, which tree-sitter
   makes an anonymous node of. A pattern token goes in as a reference to a
   rule of its own, so that a query can name it. A query matches an anonymous
   node by its own text, and the text of a pattern token is whatever it
   matched. *)
let symbol (facts : Core.Facts.t) (kind : Core.Kind.t) : Js.t =
  match Core.Facts.token_of_kind facts kind with
  | Some token ->
    (match Core.Token.text token with
     | Some text -> Js.string text
     | None -> Js.atom ("$." ^ Node.of_token token.name))
  | None ->
    (match Core.Facts.rule_of_kind facts kind with
     | Some rule ->
       (* A reference to an expression block goes to the hidden rule that
          chooses among the shapes the block builds. The block's own node is
          narrower: a token atom produces one, and nothing else does. *)
       (match rule.origin with
        | Core.Rule.Pratt_block -> Js.atom ("$." ^ Node.dispatch rule.name)
        | Core.Rule.User | Core.Rule.Pratt_role _ ->
          Js.atom ("$." ^ Node.of_rule rule.name))
     | None -> raise (Cannot "a kind that is neither a token nor a rule"))
;;

let choice_of (facts : Core.Facts.t) (kinds : Core.Kind.t array) : Js.t =
  match Array.to_list kinds with
  | [] -> raise (Cannot "a child position that admits nothing")
  | [ one ] -> symbol facts one
  | many -> Js.call "choice" (List.map many ~f:(symbol facts))
;;

(* One element of a body, with its field. A query names the field, so it
   goes around each occurrence rather than around the repetition. A
   [repeat(field(...))] gives every element the field, and a
   [field(repeat(...))] gives it to the list. *)
let element (facts : Core.Facts.t) (child : Core.Rule.child) : Js.t =
  Js.call
    "field"
    [ Js.string (Core.Grammar.Name.Child.to_string child.child_name)
    ; choice_of facts child.alts
    ]
;;

let repeated (child : Core.Rule.child) (element : Js.t) : Js.t =
  match child.modifier with
  | Core.Grammar.Exactly_one -> element
  | Core.Grammar.Zero_or_one -> Js.call "optional" [ element ]
  | Core.Grammar.Zero_or_more -> Js.call "repeat" [ element ]
  | Core.Grammar.One_or_more -> Js.call "repeat1" [ element ]
;;

let child (facts : Core.Facts.t) (child : Core.Rule.child) : Js.t =
  repeated child (element facts child)
;;

(* A body whose elements are separated.

   [sepBy1] is written out at the foot of the emitted file. Hand-written
   tree-sitter grammars define the same helper, and spelling the sequence
   out at each site would bury the shape. *)
let separated
      (facts : Core.Facts.t)
      (element_child : Core.Rule.child)
      ~(sep : Core.Kind.t)
      ~(trailing : Core.Grammar.trailing_sep)
      ~(uses : (string, unit) Hashtbl.t)
  : Js.t
  =
  let sep = symbol facts sep in
  let element = element facts element_child in
  let note (name : string) : string =
    Hashtbl.replace uses name ();
    name
  in
  let one_or_more = Js.call (note "sepBy1") [ sep; element ] in
  let with_trailing =
    match trailing with
    | Core.Grammar.Never -> one_or_more
    | Core.Grammar.On_break | Core.Grammar.Always ->
      Js.call "seq" [ one_or_more; Js.call "optional" [ sep ] ]
  in
  match element_child.modifier with
  | Core.Grammar.One_or_more -> with_trailing
  | Core.Grammar.Zero_or_more -> Js.call "optional" [ with_trailing ]
  | Core.Grammar.Exactly_one -> element
  | Core.Grammar.Zero_or_one -> Js.call "optional" [ element ]
;;

(* -- a rule's body --------------------------------------------------------- *)

let seq_of (parts : Js.t list) : Js.t =
  match parts with
  | [ one ] -> one
  | parts -> Js.call "seq" parts
;;

(* Everything a rule matches, in order.

   [body_from] is where the frame begins. It is zero for a production, and
   one for the shape an enclosed postfix operator desugars into, where the
   operand sits before the opening bracket. One walk serves both, as one
   fold serves both in the formatter. *)
let body (facts : Core.Facts.t) (rule : Core.Rule.def) ~(uses : (string, unit) Hashtbl.t)
  : Js.t
  =
  let leading =
    List.init ~len:rule.body_from ~f:(fun index -> child facts rule.children.(index))
  in
  let framed =
    List.init
      ~len:(Array.length rule.children - rule.body_from)
      ~f:(fun index -> rule.children.(index + rule.body_from))
  in
  match rule.frame with
  | Core.Rule.Plain | Core.Rule.Committed _ ->
    seq_of (leading @ List.map framed ~f:(child facts))
  | Core.Rule.Delimited { open_; close; sep; _ } ->
    let inside =
      match framed, sep with
      | [ element_child ], Some { sep_tok; trailing } ->
        [ separated facts element_child ~sep:sep_tok ~trailing ~uses ]
      | _ -> List.map framed ~f:(child facts)
    in
    seq_of ((leading @ [ symbol facts open_ ]) @ inside @ [ symbol facts close ])
  | Core.Rule.Separated { sep_tok; trailing; _ } ->
    let inside =
      match framed with
      | [ element_child ] ->
        [ separated facts element_child ~sep:sep_tok ~trailing ~uses ]
      | _ -> List.map framed ~f:(child facts)
    in
    seq_of (leading @ inside)
;;

(* A greedy child is taken at a position where taking it and leaving it both
   parse. The dangling [else] is one. It could attach to either [if], and
   greedy attaches it to the nearer one. A right precedence reaches the same
   attachment, because shifting the [else] and attaching it to the nearer
   [if] are one and the same. *)
let is_greedy (rule : Core.Rule.def) : bool =
  Array.exists rule.children ~f:(fun (child : Core.Rule.child) -> child.greedy)
;;

let with_greedy (rule : Core.Rule.def) (body : Js.t) : Js.t =
  if is_greedy rule then Js.call "prec.right" [ Js.atom "1"; body ] else body
;;

(* -- expression blocks ----------------------------------------------------- *)

let precedence (assoc : Core.Grammar.assoc) : string =
  match assoc with
  | Core.Grammar.Left -> "prec.left"
  | Core.Grammar.Right -> "prec.right"
;;

(* The child of a desugared role that holds the operator.

   The child is found by its set of kinds. That set is exactly the block's
   operators for that role, and no other child's is. Matching on the child's
   name would tie this to a spelling [Core.Stage] owns and could change. *)
let operator_child (rule : Core.Rule.def) (ops : Core.Kind.t list) : int option =
  let table = Core.Kind.Set.of_list ops in
  let found = ref None in
  Array.iteri rule.children ~f:(fun index (child : Core.Rule.child) ->
    if Core.Kind.Set.equal child.kinds table
    then if !found = None then found := Some index else found := Some (-1));
  match !found with
  | Some index when index >= 0 -> Some index
  | _ -> None
;;

(* One alternative per operator, each at its own precedence.

   Emitting the role's body as it stands would put every operator of the
   block into one choice with no precedence, and tree-sitter would report the
   whole table as a conflict. The binding powers settle it, and they carry
   over directly. *)
let operator_alternatives
      (facts : Core.Facts.t)
      (rule : Core.Rule.def)
      (ops : Core.Block.op array)
      ~(uses : (string, unit) Hashtbl.t)
  : Js.t
  =
  let kinds = Array.to_list (Array.map ops ~f:(fun (op : Core.Block.op) -> op.op_kind)) in
  match operator_child rule kinds with
  | None -> body facts rule ~uses
  | Some at ->
    let alternative (op : Core.Block.op) : Js.t =
      let parts =
        List.init ~len:(Array.length rule.children) ~f:(fun index ->
          let this = rule.children.(index) in
          if index = at
          then
            Js.call
              "field"
              [ Js.string (Core.Grammar.Name.Child.to_string this.child_name)
              ; symbol facts op.op_kind
              ]
          else child facts this)
      in
      Js.call (precedence op.assoc) [ Js.atom (string_of_int op.bp); seq_of parts ]
    in
    (match Array.to_list ops with
     | [ one ] -> alternative one
     | many -> Js.call "choice" (List.map many ~f:alternative))
;;

let block_of (facts : Core.Facts.t) (rule : Core.Rule.id) : Core.Block.def option =
  Array.find_opt
    Core.Facts.(facts.blocks)
    ~f:(fun (block : Core.Block.def) -> block.rule_id = rule)
;;

(* What an expression is: the block's own node where a token atom makes one,
   any of the shapes its operators build, and any rule the author listed as
   an atom. *)
let block_dispatch (facts : Core.Facts.t) (rule : Core.Rule.def) (block : Core.Block.def)
  : Js.t
  =
  let base =
    if Array.length rule.children = 0
    then []
    else [ Js.atom ("$." ^ Node.of_rule rule.name) ]
  in
  let roles =
    Array.to_list Core.Facts.(facts.rules)
    |> List.filter_map ~f:(fun (role : Core.Rule.def) ->
      match role.origin with
      | Core.Rule.Pratt_role { block = owner; _ } when owner = block.rule_id ->
        Some (Js.atom ("$." ^ Node.of_rule role.name))
      | _ -> None)
  in
  let atoms =
    Array.to_list block.atoms
    |> List.filter_map ~f:(fun (kind : Core.Kind.t) ->
      match Core.Facts.rule_of_kind facts kind with
      | Some atom -> Some (Js.atom ("$." ^ Node.of_rule atom.name))
      | None -> None)
  in
  match base @ roles @ atoms with
  | [] -> raise (Cannot "an expression block that builds nothing")
  | [ one ] -> one
  | many -> Js.call "choice" many
;;

let role_body
      (facts : Core.Facts.t)
      (rule : Core.Rule.def)
      ~(block : Core.Rule.id)
      ~(role : Core.Role.t)
      ~(uses : (string, unit) Hashtbl.t)
  : Js.t
  =
  match block_of facts block with
  | None -> body facts rule ~uses
  | Some block ->
    (match role with
     | Core.Role.Base -> body facts rule ~uses
     | Core.Role.Bin -> operator_alternatives facts rule block.infix ~uses
     | Core.Role.Prefix -> operator_alternatives facts rule block.prefix ~uses
     | Core.Role.Postfix index ->
       if index >= Array.length block.postfix
       then body facts rule ~uses
       else (
         let postfix = block.postfix.(index) in
         Js.call
           "prec.left"
           [ Js.atom (string_of_int postfix.p_bp); body facts rule ~uses ]))
;;

(* -- the file -------------------------------------------------------------- *)

let regex_of (token : Core.Token.def) : Js.t =
  match token.klass with
  | Core.Grammar.Pattern { lexer; _ } ->
    (match Js.regex lexer with
     | Ok js -> js
     | Error reason ->
       raise
         (Cannot
            (Printf.sprintf
               "token %S has no JavaScript regex (%s)"
               (Core.Grammar.Name.Token.to_string token.name)
               reason)))
  | Core.Grammar.Keyword text | Core.Grammar.Punctuation text -> Js.string text
;;

let is_pattern (token : Core.Token.def) : bool =
  match token.klass with
  | Core.Grammar.Pattern _ -> true
  | Core.Grammar.Keyword _ | Core.Grammar.Punctuation _ -> false
;;

(* Trivia that the formatter re-emits is whitespace, and a node for it would
   be in every tree and of use to nobody. Trivia it preserves is a comment,
   and a query names a comment. *)
let is_named_trivia (token : Core.Token.def) : bool =
  match token.trivia with
  | Some Core.Grammar.Preserve -> true
  | Some Core.Grammar.Reformat | None -> false
;;

let token_rules (facts : Core.Facts.t) : (string * Js.t) list =
  Array.to_list Core.Facts.(facts.tokens)
  |> List.filter_map ~f:(fun (token : Core.Token.def) ->
    if is_pattern token && ((not (Core.Token.is_trivia token)) || is_named_trivia token)
    then Some (Node.of_token token.name, regex_of token)
    else None)
;;

let extras (facts : Core.Facts.t) : Js.t list =
  Array.to_list Core.Facts.(facts.tokens)
  |> List.filter_map ~f:(fun (token : Core.Token.def) ->
    if not (Core.Token.is_trivia token)
    then None
    else if is_named_trivia token
    then Some (Js.atom ("$." ^ Node.of_token token.name))
    else Some (regex_of token))
;;

let rule_entries (facts : Core.Facts.t) ~(uses : (string, unit) Hashtbl.t)
  : (string * Js.t) list
  =
  Array.to_list Core.Facts.(facts.rules)
  |> List.concat_map ~f:(fun (rule : Core.Rule.def) ->
    match rule.origin with
    | Core.Rule.User ->
      [ Node.of_rule rule.name, with_greedy rule (body facts rule ~uses) ]
    | Core.Rule.Pratt_role { block; role } ->
      [ Node.of_rule rule.name, role_body facts rule ~block ~role ~uses ]
    | Core.Rule.Pratt_block ->
      (match block_of facts rule.id with
       | None -> [ Node.of_rule rule.name, body facts rule ~uses ]
       | Some block ->
         (* Two entries: the node a token atom builds, and the hidden choice
            every reference to an expression goes through. *)
         (if Array.length rule.children = 0
          then []
          else [ Node.of_rule rule.name, body facts rule ~uses ])
         @ [ Node.dispatch rule.name, block_dispatch facts rule block ]))
;;

(* The root goes first: tree-sitter parses from the first rule in the map.

   A grammar may declare several, and tree-sitter takes one, so the rest are
   gathered under a rule made up for the purpose. This backend invents a
   node nowhere else. *)
let ordered (facts : Core.Facts.t) (entries : (string * Js.t) list) : (string * Js.t) list
  =
  let name_of (id : Core.Rule.id) : string =
    Node.of_rule (Core.Facts.rule facts id).name
  in
  match Core.Facts.(facts.roots) with
  | [] -> entries
  | [ only ] ->
    let key = name_of only in
    (match List.assoc_opt key entries with
     | None -> entries
     | Some js -> (key, js) :: List.filter entries ~f:(fun (k, _) -> k <> key))
  | roots ->
    ( Node.start
    , Js.call "choice" (List.map roots ~f:(fun id -> Js.atom ("$." ^ name_of id))) )
    :: entries
;;

let helpers =
  "\nfunction sepBy1(sep, rule) {\n  return seq(rule, repeat(seq(sep, rule)));\n}\n"
;;

let emit (facts : Core.Facts.t) ~(language : string) ~(word : Core.Token.def option)
  : (string, string) result
  =
  let uses = Hashtbl.create 4 in
  match
    let entries = ordered facts (rule_entries facts ~uses @ token_rules facts) in
    let buf = Buffer.create 4096 in
    Buffer.add_string
      buf
      "// Generated by lingo from the grammar. Do not edit by hand.\n\n\
       module.exports = grammar({\n";
    Buffer.add_string
      buf
      ("  name: " ^ Js.render ~width:96 ~column:8 ~indent:8 (Js.string language) ^ ",\n\n");
    (match extras facts with
     | [] -> ()
     | extras ->
       Buffer.add_string buf "  extras: $ => [\n";
       List.iteri extras ~f:(fun index item ->
         if index > 0 then Buffer.add_string buf ",\n";
         Buffer.add_string buf ("    " ^ Js.render ~width:96 ~column:4 ~indent:4 item));
       Buffer.add_string buf "\n  ],\n\n");
    (match word with
     | None -> ()
     | Some token ->
       Buffer.add_string
         buf
         (Printf.sprintf "  word: $ => $.%s,\n\n" (Node.of_token token.name)));
    Buffer.add_string buf "  rules: {\n";
    List.iteri entries ~f:(fun index (name, js) ->
      if index > 0 then Buffer.add_string buf ",\n\n";
      let prefix = "    " ^ name ^ ": $ => " in
      Buffer.add_string buf prefix;
      Buffer.add_string
        buf
        (Js.render ~width:96 ~column:String.(length prefix) ~indent:4 js));
    Buffer.add_string buf "\n  }\n});\n";
    if Hashtbl.mem uses "sepBy1" then Buffer.add_string buf helpers;
    Buffer.contents buf
  with
  | text -> Ok text
  | exception Cannot reason -> Error reason
;;
