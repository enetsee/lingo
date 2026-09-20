open StdLabels

type severity =
  | Fatal
  | Warning

type where =
  | At_grammar
  | At_token of Grammar.Name.Token.t
  | At_production of Grammar.Name.Rule.t
  | At_child of
      { production : Grammar.Name.Rule.t
      ; child : Grammar.Name.Child.t
      }
  | At_block of Grammar.Name.Rule.t
  | At_operator of
      { block : Grammar.Name.Rule.t
      ; token : Grammar.Name.Token.t
      }

(* A kind travels with the name it prints as. The check that raises a
   finding holds the kind table. A caller that got an error back from
   [Facts.of_grammar] holds no facts, so it has no table, and it could not
   resolve the name itself. *)
type kind_ref =
  { kind : Kind.t
  ; name : Kind.Name.t
  }

let kind_refs (tbl : Kind.Table.t) (ks : Kind.t list) : kind_ref list =
  List.map ~f:(fun k -> { kind = k; name = Kind.Table.name tbl k }) ks
;;

type resync_body =
  | Repeats_nothing
  | Ends_at_its_separator

type prefix_atom =
  | Starts_the_atom of { atom : Grammar.Name.Rule.t }
  | Is_the_atom

type token_unreachable_reason =
  | Empty_language
  | Subsumed_by of Grammar.Name.Token.t

type detail =
  (* -- declarations and the kind table ------------------------------------- *)
  | Empty_grammar
  | Invalid_name of
      { what : string
      ; name : string
      ; reason : string
      }
  | Invalid_token_literal of { reason : string }
  | Dup_kind_name of
      { sources : string list
      ; emitted : string
      }
  | Reserved_name of
      { sources : string list
      ; emitted : string
      ; scope : Manifest.Scope.t
      }
  | Name_collision of
      { sources : string list
      ; emitted : string
      ; scope : Manifest.Scope.t
      }
  | Dup_child_name of
      { sources : string list
      ; emitted : string
      ; view_module : string
      }
  | Unknown_rule of { name : Grammar.Name.Rule.t }
  | Unknown_token of { name : Grammar.Name.Token.t }
  | Unknown_op_token of { name : Grammar.Name.Token.t }
  | Unknown_delimiter_token of { name : Grammar.Name.Token.t }
  | Unknown_recover_to_token of { name : Grammar.Name.Token.t }
  | Unknown_resync_anchor of { name : Grammar.Name.Token.t }
  | Unknown_identity_child of { name : Grammar.Name.Child.t }
  | Unknown_message_child of { name : Grammar.Name.Child.t }
  | Unused_message_child of { name : Grammar.Name.Child.t }
  | Unused_recover_to of { name : Grammar.Name.Child.t }
  | Empty_alternatives
  | No_roots
  | Root_is_block of { name : Grammar.Name.Rule.t }
  | Unknown_root of { name : Grammar.Name.Rule.t }
  | Dup_root of { name : Grammar.Name.Rule.t }
  | Postfix_suffix_missing of
      { index : int
      ; total : int
      }
  | Postfix_suffix_duplicate of { suffix : string }
  | Nullable_token
  | Empty_pratt_atoms
  | Dup_pratt_op of
      { token : Grammar.Name.Token.t
      ; category : string
      }
  | Mixed_pratt_role of { token : Grammar.Name.Token.t }
  | Mixed_assoc_at_bp of
      { bp : int
      ; ops : (Grammar.Name.Token.t * Grammar.assoc) list
      }
  (* -- resolved children, normalised framing -------------------------------- *)
  | Delimited_arity of { children : int }
  | Separated_arity of { children : int }
  | Unused_resync_anchors of { body : resync_body }
  | Repeated_vs_single of { children : Grammar.Name.Child.t list }
  | Repeated_vs_single_kinds of
      { repeated : Grammar.Name.Child.t
      ; single : Grammar.Name.Child.t
      ; shared : kind_ref list
      }
  | Ambiguous_same_kind_child of { children : Grammar.Name.Child.t list }
  | Overlapping_single_kinds of
      { a : Grammar.Name.Child.t
      ; a_kinds : kind_ref list
      ; b : Grammar.Name.Child.t
      ; b_kinds : kind_ref list
      }
  (* -- the fixpoints and the lexer automaton -------------------------------- *)
  | First_first_conflict of { common : kind_ref list }
  | First_follow_conflict of { common : kind_ref list }
  | Left_recursion of { members : Grammar.Name.Rule.t list }
  | Nullable_repeated of { rule : Grammar.Name.Rule.t }
  | Nullable_pratt_atom of { atom : string }
  | Nullable_separated_element of { element : string }
  | Empty_first_set of { referenced_from : string list }
  | Pratt_atom_conflict of { common : kind_ref list }
  | Prefix_atom_conflict of { how : prefix_atom }
  | Resync_anchor_conflict of { anchor : Grammar.Name.Token.t }
  | Token_unreachable of { reason : token_unreachable_reason }

type t =
  { detail : detail
  ; where : where
  }

let make ~(detail : detail) (where : where) : t = { detail; where }

(* -- codes ----------------------------------------------------------------- *)

let code (e : t) : string =
  match e.detail with
  | Empty_grammar -> "empty-grammar"
  | Invalid_name _ -> "invalid-name"
  | Invalid_token_literal _ -> "invalid-token-literal"
  | Dup_kind_name _ -> "dup-kind-name"
  | Reserved_name _ -> "reserved-name"
  | Name_collision _ -> "name-collision"
  | Dup_child_name _ -> "dup-child-name"
  | Unknown_rule _ -> "unknown-rule"
  | Unknown_token _ -> "unknown-token"
  | Unknown_op_token _ -> "unknown-op-token"
  | Unknown_delimiter_token _ -> "unknown-delimiter-token"
  | Unknown_recover_to_token _ -> "unknown-recover-to-token"
  | Unknown_resync_anchor _ -> "unknown-resync-anchor"
  | Unknown_identity_child _ -> "unknown-identity-child"
  | Unknown_message_child _ -> "unknown-message-child"
  | Unused_message_child _ -> "unused-message-child"
  | Unused_recover_to _ -> "unused-recover-to"
  | Empty_alternatives -> "empty-alternatives"
  | No_roots -> "no-roots"
  | Root_is_block _ -> "root-is-block"
  | Unknown_root _ -> "unknown-root"
  | Dup_root _ -> "dup-root"
  | Postfix_suffix_missing _ -> "postfix-suffix-missing"
  | Postfix_suffix_duplicate _ -> "postfix-suffix-duplicate"
  | Nullable_token -> "nullable-token"
  | Empty_pratt_atoms -> "empty-pratt-atoms"
  | Dup_pratt_op _ -> "dup-pratt-op"
  | Mixed_pratt_role _ -> "mixed-pratt-role"
  | Mixed_assoc_at_bp _ -> "mixed-assoc-at-bp"
  | Delimited_arity _ -> "delimited-arity"
  | Separated_arity _ -> "separated-arity"
  | Unused_resync_anchors _ -> "unused-resync-anchors"
  | Repeated_vs_single _ -> "repeated-vs-single"
  | Repeated_vs_single_kinds _ -> "repeated-vs-single-kinds"
  | Ambiguous_same_kind_child _ -> "ambiguous-same-kind-child"
  | Overlapping_single_kinds _ -> "overlapping-single-kinds"
  | First_first_conflict _ -> "first-first-conflict"
  | First_follow_conflict _ -> "first-follow-conflict"
  | Left_recursion _ -> "left-recursion"
  | Nullable_repeated _ -> "nullable-repeated"
  | Nullable_pratt_atom _ -> "nullable-pratt-atom"
  | Nullable_separated_element _ -> "nullable-separated-element"
  | Empty_first_set _ -> "empty-first-set"
  | Pratt_atom_conflict _ -> "pratt-atom-conflict"
  | Prefix_atom_conflict _ -> "prefix-atom-conflict"
  | Resync_anchor_conflict _ -> "resync-anchor-conflict"
  | Token_unreachable _ -> "token-unreachable"
;;

let names_stage_codes =
  [ "empty-grammar"
  ; "invalid-name"
  ; "invalid-token-literal"
  ; "dup-kind-name"
  ; "reserved-name"
  ; "name-collision"
  ; "dup-child-name"
  ; "unknown-rule"
  ; "unknown-token"
  ; "unknown-op-token"
  ; "unknown-delimiter-token"
  ; "unknown-recover-to-token"
  ; "unknown-resync-anchor"
  ; "unknown-identity-child"
  ; "unknown-message-child"
  ; "unused-message-child"
  ; "unused-recover-to"
  ; "empty-alternatives"
  ; "no-roots"
  ; "root-is-block"
  ; "unknown-root"
  ; "dup-root"
  ; "postfix-suffix-missing"
  ; "postfix-suffix-duplicate"
  ; "nullable-token"
  ; "empty-pratt-atoms"
  ; "dup-pratt-op"
  ; "mixed-pratt-role"
  ; "mixed-assoc-at-bp"
  ]
;;

let shape_stage_codes =
  [ "delimited-arity"
  ; "separated-arity"
  ; "repeated-vs-single"
  ; "repeated-vs-single-kinds"
  ; "ambiguous-same-kind-child"
  ; "overlapping-single-kinds"
  ; "unused-resync-anchors"
  ]
;;

let full_stage_codes =
  [ "first-first-conflict"
  ; "first-follow-conflict"
  ; "left-recursion"
  ; "nullable-repeated"
  ; "nullable-pratt-atom"
  ; "nullable-separated-element"
  ; "empty-first-set"
  ; "pratt-atom-conflict"
  ; "prefix-atom-conflict"
  ; "resync-anchor-conflict"
  ; "token-unreachable"
  ]
;;

let codes_by_stage =
  [ "names", names_stage_codes; "shape", shape_stage_codes; "full", full_stage_codes ]
;;

let codes = List.concat_map ~f:snd codes_by_stage

(* -- severity -------------------------------------------------------------- *)

let severity (_ : t) : severity = Fatal

(* -- rendering ------------------------------------------------------------- *)

(* Renders a list for a sentence: ["a"], then ["a" and "b"], then ["a", "b"
   and "c"]. *)
let rec listed (names : string list) : string =
  match names with
  | [] -> ""
  | [ x ] -> Printf.sprintf "%S" x
  | [ x; y ] -> Printf.sprintf "%S and %S" x y
  | x :: rest -> Printf.sprintf "%S, %s" x (listed rest)
;;

let kinds (ks : kind_ref list) : string =
  "{"
  ^ String.concat ~sep:", " (List.map ~f:(fun k -> Kind.Name.to_string k.name) ks)
  ^ "}"
;;

let plain (names : string list) : string = String.concat ~sep:", " names

let hint (e : t) : string option =
  match e.detail with
  | Invalid_name _ -> Some "names become OCaml constructors, bindings and accessors"
  | Reserved_name _ ->
    Some "every grammar emits this name; pick another for the declaration that reaches it"
  | Name_collision _ -> Some "the two names differ only in case or punctuation"
  | Nullable_token -> Some "the generated lexer would loop at every position"
  | Mixed_pratt_role _ ->
    Some "the postfix trigger is checked first, so the infix form is dead"
  | Mixed_assoc_at_bp _ -> Some "give the two associativities distinct binding powers"
  | Delimited_arity _ | Separated_arity _ -> Some "wrap the body in a rule of its own"
  | Prefix_atom_conflict { how = Starts_the_atom _ } ->
    Some "give the operator a token of its own, or drop the atom"
  | Prefix_atom_conflict { how = Is_the_atom } ->
    Some "the prefix table is read before the atoms; drop the token from the atoms"
  | Resync_anchor_conflict _ ->
    Some
      "pick a token no element starts with, such as the keyword that begins whatever \
       follows the body"
  | Unused_resync_anchors { body = Repeats_nothing } ->
    Some "drop the anchors; only a delimited production has a body they can end"
  | Unused_resync_anchors { body = Ends_at_its_separator } ->
    Some "drop the anchors, or wrap the list in an opener and a closer"
  | Repeated_vs_single _ -> Some "give one of them a kind of its own"
  | Overlapping_single_kinds _ -> Some "wrap one side in a rule of its own"
  | First_follow_conflict _ ->
    Some
      "mark the child greedy if the parser's natural resolution is what the language \
       means"
  | _ -> None
;;

let message (e : t) : string =
  match e.detail with
  | Empty_grammar -> "a grammar needs at least one production"
  | Invalid_name { what; name; reason } ->
    Printf.sprintf "%s %S is not a valid identifier (%s)" what name reason
  | Invalid_token_literal { reason } ->
    Printf.sprintf "%s: the token's bytes have no regex" reason
  | Dup_kind_name { sources; emitted } ->
    Printf.sprintf "%s both define the kind %S" (listed sources) emitted
  | Reserved_name { sources; emitted; scope } ->
    Printf.sprintf
      "%s reaches the %s %S, which is emitted for every grammar"
      (listed sources)
      (Manifest.Scope.name scope)
      emitted
  | Name_collision { sources; emitted; scope } ->
    Printf.sprintf
      "%s both mangle to the %s %S"
      (listed sources)
      (Manifest.Scope.name scope)
      emitted
  | Dup_child_name { sources; emitted; view_module } ->
    Printf.sprintf
      "%s both mangle to the accessor %S in view module %s"
      (listed sources)
      emitted
      view_module
  | Unknown_rule { name } ->
    Printf.sprintf
      "no production or expression block named %S"
      (Grammar.Name.Rule.to_string name)
  | Unknown_token { name }
  | Unknown_op_token { name }
  | Unknown_delimiter_token { name }
  | Unknown_recover_to_token { name }
  | Unknown_resync_anchor { name } ->
    Printf.sprintf "no token named %S" (Grammar.Name.Token.to_string name)
  | Unknown_identity_child { name } ->
    Printf.sprintf
      "the identity child %S is not a child of this production"
      (Grammar.Name.Child.to_string name)
  | Unknown_message_child { name } ->
    Printf.sprintf
      "the message catalogue names a child %S this production does not have"
      (Grammar.Name.Child.to_string name)
  | Unused_message_child { name } ->
    Printf.sprintf
      "%S is optional or repeated, so it never reports and this wording would never be \
       read"
      (Grammar.Name.Child.to_string name)
  | Unused_recover_to { name } ->
    Printf.sprintf
      "%S is optional or repeated, so nothing recovers at it and this set would never be \
       read"
      (Grammar.Name.Child.to_string name)
  | Empty_alternatives -> "the alternative list is empty, so nothing can fill this child"
  | No_roots -> "a grammar needs at least one root production"
  | Root_is_block { name } ->
    Printf.sprintf
      "%S is an expression block; a root must be a production"
      (Grammar.Name.Rule.to_string name)
  | Unknown_root { name } ->
    Printf.sprintf "no production named %S" (Grammar.Name.Rule.to_string name)
  | Dup_root { name } ->
    Printf.sprintf "%S is listed as a root twice" (Grammar.Name.Rule.to_string name)
  | Postfix_suffix_missing { index; total } ->
    Printf.sprintf
      "postfix operator %d of %d needs a kind_suffix; a block with more than one gives \
       each its own"
      index
      total
  | Postfix_suffix_duplicate { suffix } ->
    Printf.sprintf "two postfix operators share the kind_suffix %S" suffix
  | Nullable_token -> "the token's regex matches the empty string"
  | Empty_pratt_atoms -> "an expression block with no atoms can never start"
  | Dup_pratt_op { token; category } ->
    Printf.sprintf
      "%S is declared twice as a %s operator"
      (Grammar.Name.Token.to_string token)
      category
  | Mixed_pratt_role { token } ->
    Printf.sprintf
      "%S is declared as both an infix and a postfix operator"
      (Grammar.Name.Token.to_string token)
  | Mixed_assoc_at_bp { bp; ops } ->
    Printf.sprintf
      "binding power %d carries both associativities: %s"
      bp
      (plain
         (List.map ops ~f:(fun (t, a) ->
            Printf.sprintf
              "%s (%s)"
              (Grammar.Name.Token.to_string t)
              (match (a : Grammar.assoc) with
               | Grammar.Left -> "left"
               | Grammar.Right -> "right"))))
  | Delimited_arity { children } ->
    Printf.sprintf
      "a delimited production wraps exactly one child; this one has %d"
      children
  | Separated_arity { children } ->
    Printf.sprintf
      "a separated production wraps exactly one element child; this one has %d"
      children
  | Unused_resync_anchors { body = Repeats_nothing } ->
    "a resync anchor ends a repeated body early, and this production repeats nothing"
  | Unused_resync_anchors { body = Ends_at_its_separator } ->
    "a resync anchor ends a repeated body early, and this list already ends wherever the \
     cursor leaves its separator"
  | Repeated_vs_single { children } ->
    Printf.sprintf
      "%s share a kind and mix repeated with single-valued: the repeated nodes pad the \
       list the single accessor indexes into"
      (plain (List.map ~f:Grammar.Name.Child.to_string children))
  | Repeated_vs_single_kinds { repeated; single; shared } ->
    Printf.sprintf
      "%S and %S admit a common kind (%s) from different buckets, so the repeated \
       accessor also returns the single one's node"
      (Grammar.Name.Child.to_string single)
      (Grammar.Name.Child.to_string repeated)
      (kinds shared)
  | Ambiguous_same_kind_child { children } ->
    Printf.sprintf
      "%s share a kind and an earlier one is optional: when it is absent the later \
       accessor returns the wrong node"
      (plain (List.map ~f:Grammar.Name.Child.to_string children))
  | Overlapping_single_kinds { a; a_kinds; b; b_kinds } ->
    Printf.sprintf
      "%S (%s) and %S (%s) admit a common kind, and each takes the first node its filter \
       accepts"
      (Grammar.Name.Child.to_string a)
      (kinds a_kinds)
      (Grammar.Name.Child.to_string b)
      (kinds b_kinds)
  | First_first_conflict { common } ->
    Printf.sprintf
      "two alternatives can both start with %s, so the later one is unreachable"
      (kinds common)
  | First_follow_conflict { common } ->
    Printf.sprintf
      "%s cannot settle whether a parser enters this child: it is both in the child's \
       FIRST and in what may follow it"
      (kinds common)
  | Left_recursion { members } ->
    (match members with
     | [ one ] ->
       Printf.sprintf
         "left recursion: %s reaches itself without taking a token"
         (Grammar.Name.Rule.to_string one)
     | _ ->
       Printf.sprintf
         "left recursion: %s reach each other without taking a token"
         (plain (List.map ~f:Grammar.Name.Rule.to_string members)))
  | Nullable_repeated { rule } ->
    Printf.sprintf
      "every alternative of this repeated child is nullable (%s derives empty), so the \
       loop cannot make progress"
      (Grammar.Name.Rule.to_string rule)
  | Nullable_pratt_atom { atom } ->
    Printf.sprintf "the atom %s is nullable; an atom must consume at least one token" atom
  | Nullable_separated_element { element } ->
    Printf.sprintf
      "the element %s is nullable; a separated element must consume at least one token"
      element
  | Empty_first_set { referenced_from } ->
    Printf.sprintf
      "nothing can start this production, but it is referenced from %s"
      (plain referenced_from)
  | Pratt_atom_conflict { common } ->
    Printf.sprintf
      "two atoms can both start with %s, and atom dispatch is ordered, so the later one \
       is unreachable"
      (kinds common)
  | Prefix_atom_conflict { how = Starts_the_atom { atom } } ->
    Printf.sprintf
      "the prefix operator can start the atom %s, which is therefore unreachable through \
       it"
      (Grammar.Name.Rule.to_string atom)
  | Prefix_atom_conflict { how = Is_the_atom } ->
    "the prefix operator is itself an atom of this block, and it is never read as one"
  | Resync_anchor_conflict { anchor } ->
    Printf.sprintf
      "an element of this body can start with %s, and the anchor ends the body there, so \
       it would never take one"
      (Grammar.Name.Token.to_string anchor)
  | Token_unreachable { reason } ->
    (match reason with
     | Empty_language -> "the lexer can never emit this token: it matches nothing"
     | Subsumed_by other ->
       Printf.sprintf
         "the lexer can never emit this token: %s wins wherever it could match"
         (Grammar.Name.Token.to_string other))
;;

let pp_where (fmt : Format.formatter) (w : where) : unit =
  match w with
  | At_grammar -> Format.pp_print_string fmt "grammar"
  | At_token t -> Format.fprintf fmt "token %a" Grammar.Name.Token.pp t
  | At_production p -> Format.fprintf fmt "production %a" Grammar.Name.Rule.pp p
  | At_child { production; child } ->
    Format.fprintf fmt "%a.%a" Grammar.Name.Rule.pp production Grammar.Name.Child.pp child
  | At_block b -> Format.fprintf fmt "expression block %a" Grammar.Name.Rule.pp b
  | At_operator { block; token } ->
    Format.fprintf
      fmt
      "%a operator %a"
      Grammar.Name.Rule.pp
      block
      Grammar.Name.Token.pp
      token
;;

let pp (fmt : Format.formatter) (e : t) : unit =
  Format.fprintf
    fmt
    "@[<v 2>[%s] %a: %s%a@]"
    (code e)
    pp_where
    e.where
    (message e)
    (fun fmt -> function
       | None -> ()
       | Some h -> Format.fprintf fmt "@,hint: %s" h)
    (hint e)
;;

let pp_list (fmt : Format.formatter) (es : t list) : unit =
  Format.fprintf
    fmt
    "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun fmt e ->
       Format.fprintf fmt "  - %a" pp e))
    es
;;

let to_string (e : t) : string = Format.asprintf "%a" pp e

let where_rank (w : where) : int =
  match w with
  | At_grammar -> 0
  | At_token _ -> 1
  | At_production _ -> 2
  | At_child _ -> 3
  | At_block _ -> 4
  | At_operator _ -> 5
;;

let compare_where (a : where) (b : where) : int =
  match a, b with
  | At_token x, At_token y -> Grammar.Name.Token.compare x y
  | At_production x, At_production y | At_block x, At_block y ->
    Grammar.Name.Rule.compare x y
  | At_child a, At_child b ->
    (match Grammar.Name.Rule.compare a.production b.production with
     | 0 -> Grammar.Name.Child.compare a.child b.child
     | n -> n)
  | At_operator a, At_operator b ->
    (match Grammar.Name.Rule.compare a.block b.block with
     | 0 -> Grammar.Name.Token.compare a.token b.token
     | n -> n)
  | _ -> Int.compare (where_rank a) (where_rank b)
;;

let compare (a : t) (b : t) : int =
  match String.compare (code a) (code b) with
  | 0 ->
    (match compare_where a.where b.where with
     | 0 -> String.compare (message a) (message b)
     | n -> n)
  | n -> n
;;
