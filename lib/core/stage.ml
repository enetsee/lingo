open StdLabels

type slot =
  | Prod of Grammar.production
  | Block of Grammar.expr_def
  | Role of
      { block_rule : Rule.id
      ; block : Grammar.expr_def
      ; role : Role.t
      }

type names =
  { grammar : Grammar.t
  ; manifest : Manifest.t
  ; kinds : Kind.Table.t
  ; tokens : Token.def array
  ; slots : slot array
  ; block_base : Rule.id
  ; rule_kind : Kind.t array
  ; rule_hole : Kind.t option array
  ; rule_by_name : (Grammar.Name.Rule.t, Rule.id) Hashtbl.t
  ; token_by_name : (Grammar.Name.Token.t, Token.id) Hashtbl.t
  ; kind_token : int array
  ; kind_rule : int array
  ; error_kind : Kind.t
  }

let slot_name (slot : slot) : Grammar.Name.Rule.t =
  match slot with
  | Prod p -> p.kind_name
  | Block b -> b.rule_name
  | Role { block; role; _ } ->
    Grammar.Name.Rule.of_string (Role.synthetic_name block role)
;;

let names (grammar : Grammar.t) : names =
  let manifest = Manifest.of_grammar grammar in
  let kinds = Kind.Table.of_names (Manifest.kind_names manifest) in
  let kind_of name =
    match Kind.Table.find kinds name with
    | Some k -> k
    (* Every name looked up here went into the table from the manifest that
       built it, so the lookup cannot fail. The fallback is here to say so
       without an assert, which keeps the stage total. *)
    | None ->
      (match Kind.Table.find kinds Kind.Name.error with
       | Some k -> k
       | None -> List.hd (Kind.Table.kinds kinds))
  in
  let error_kind = kind_of Kind.Name.error in
  (* Rule ids are assigned in this order: productions, then blocks, then each
     block's non-base roles. *)
  let prod_slots = List.map ~f:(fun p -> Prod p) grammar.productions in
  let block_base = List.length prod_slots in
  let block_slots = List.map ~f:(fun b -> Block b) grammar.expr in
  let role_slots =
    List.concat
      (List.mapi
         ~f:(fun bi (b : Grammar.expr_def) ->
           let block_rule = block_base + bi in
           let roles = Role.of_block b in
           (* The base role is the block rule itself. The kind a token atom produces
              and the kind other rules reference are one kind.

              An inactive role, such as [Bin] on a block with no infix operators,
              defines its kind constant but no rule. *)
           List.filter_map roles ~f:(fun role ->
             if role <> Role.Base && Role.is_active b role
             then Some (Role { block_rule; block = b; role })
             else None))
         grammar.expr)
  in
  let slots = Array.of_list (prod_slots @ block_slots @ role_slots) in
  let rule_kind =
    Array.map
      ~f:(fun s ->
        kind_of
          (match s with
           | Prod p -> Manifest.Kind_name.of_production p
           | Block b -> Manifest.Kind_name.of_role b Role.Base
           | Role { block; role; _ } -> Manifest.Kind_name.of_role block role))
      slots
  in
  let rule_hole =
    Array.map
      ~f:(fun s ->
        match s with
        | Prod p ->
          if p.has_hole
          then Some (kind_of (Manifest.Kind_name.hole_of_production p))
          else None
        | Block b -> Some (kind_of (Manifest.Kind_name.hole_of_block b))
        (* A missing expression takes the block's base kind, so a role defines 
           no hole of its own. *)
        | Role _ -> None)
      slots
  in
  let tokens =
    Array.of_list
      (List.mapi
         ~f:(fun i (td : Grammar.token_def) ->
           { Token.id = i
           ; kind = kind_of (Manifest.Kind_name.of_token td)
           ; name = td.token_name
           ; klass = td.token_class
           ; format = td.t_format
           ; trivia = td.trivia
           ; regex =
               (match Token.lower td.token_class with
                | Ok r -> r
                (* A check reports this as [invalid-token-literal]. The fallback 
                   here keeps the stage total while that happens. *)
                | Error _ -> Redfa.Regex.empty)
           })
         grammar.tokens)
  in
  (* First occurrence wins in both maps, so they stay total on a grammar that
     holds a repeat. A check reports the repeat as [dup-kind-name]. *)
  let rule_by_name = Hashtbl.create 32 in
  Array.iteri
    ~f:(fun i s ->
      match s with
      | Prod _ | Block _ ->
        let nm = slot_name s in
        if not (Hashtbl.mem rule_by_name nm) then Hashtbl.add rule_by_name nm i
      | Role _ -> ())
    slots;
  let token_by_name = Hashtbl.create 32 in
  Array.iter
    ~f:(fun (t : Token.def) ->
      if not (Hashtbl.mem token_by_name t.name) then Hashtbl.add token_by_name t.name t.id)
    tokens;
  let n_kinds = Kind.Table.count kinds in
  let kind_token = Array.make n_kinds (-1) in
  let kind_rule = Array.make n_kinds (-1) in
  Array.iter
    ~f:(fun (t : Token.def) ->
      let k = Kind.to_int t.kind in
      if kind_token.(k) < 0 then kind_token.(k) <- t.id)
    tokens;
  Array.iteri
    ~f:(fun i k ->
      let k = Kind.to_int k in
      if kind_rule.(k) < 0 then kind_rule.(k) <- i)
    rule_kind;
  { grammar
  ; block_base
  ; manifest
  ; kinds
  ; tokens
  ; slots
  ; rule_kind
  ; rule_hole
  ; rule_by_name
  ; token_by_name
  ; kind_token
  ; kind_rule
  ; error_kind
  }
;;

let find_rule (n : names) (name : Grammar.Name.Rule.t) : int option =
  Hashtbl.find_opt n.rule_by_name name
;;

let find_token (n : names) (name : Grammar.Name.Token.t) : int option =
  Hashtbl.find_opt n.token_by_name name
;;

let resolve (n : names) (symbol : Grammar.symbol) : Kind.t option =
  match symbol with
  | Token t ->
    Option.map
      (fun i -> n.tokens.(i).Token.kind)
      (find_token n (Grammar.Name.Token.of_string t))
  | Rule r ->
    Option.map (fun i -> n.rule_kind.(i)) (find_rule n (Grammar.Name.Rule.of_string r))
;;

(* -- resolved rules -------------------------------------------------------- *)

type shape =
  { names : names
  ; rules : Rule.def array
  ; blocks : Block.def array
  }

let shape (names : names) : shape =
  let res sym =
    match resolve names sym with
    | Some k -> k
    | None -> names.error_kind
  in
  let res_tok t =
    match find_token names t with
    | Some i -> names.tokens.(i).Token.kind
    | None -> names.error_kind
  in
  let mk_child ?(modifier = Grammar.Required) ?(greedy = false) ?recover_to name alts
    : Rule.child
    =
    { child_name = name
    ; alts
    ; kinds = Kind.Set.of_list (Array.to_list alts)
    ; modifier
    ; greedy
    ; recover_to
    }
  in
  let of_grammar_child (c : Grammar.child) : Rule.child =
    let alts =
      match c.sym with
      | Single s -> [| res s |]
      | Alternatives ss -> Array.of_list (List.map ~f:res ss)
    in
    mk_child
      ~modifier:c.modifier
      ~greedy:c.c_parse.greedy
      ?recover_to:
        (Option.map
           (fun toks -> Kind.Set.of_list (List.map ~f:res_tok toks))
           c.c_parse.recover_to)
      c.name
      alts
  in
  let of_sep = function
    | Grammar.No_sep -> None
    | With_sep { sep; trailing } -> Some { Rule.sep_tok = res_tok sep; trailing }
  in
  let of_framing = function
    | Grammar.Plain -> Rule.Plain
    | Grammar.Committed { boundary } -> Rule.Committed { boundary }
    | Grammar.Delimited { open_tok; close_tok; sep_policy; boundary } ->
      Rule.Delimited
        { open_ = res_tok open_tok
        ; close = res_tok close_tok
        ; sep = of_sep sep_policy
        ; boundary
        }
    | Grammar.Separated { sep; trailing; boundary } ->
      Rule.Separated { sep_tok = res_tok sep; trailing; boundary }
  in
  let default_format =
    Grammar.{ break_style = Fit; indent_width = 2; separator_lines = 1 }
  in
  let synthetic_recovery = Grammar.{ strategy = Insert_only } in
  let token_alts syms = Array.of_list (List.map ~f:res syms) in
  let build id slot : Rule.def =
    let kind = names.rule_kind.(id)
    and hole = names.rule_hole.(id) in
    match slot with
    | Prod p ->
      let children = Array.of_list (List.map ~f:of_grammar_child p.children) in
      { id
      ; kind
      ; name = p.kind_name
      ; children
      ; frame = of_framing p.framing
      ; body_from = 0
      ; hole
      ; origin = Rule.User
      ; recovery = p.recovery
      ; messages = Array.of_list p.error_messages
      ; resync = Kind.Set.of_list (List.map ~f:res_tok p.resync_anchors)
      ; format = p.format
      ; edge_space_before = p.edge_space_before
      ; edge_space_after = p.edge_space_after
      ; identity =
          Option.bind p.identity_child (fun nm ->
            let rec idx i = function
              | [] -> None
              | (c : Grammar.child) :: _ when Grammar.Name.Child.equal c.name nm -> Some i
              | _ :: tl -> idx (i + 1) tl
            in
            idx 0 p.children)
      }
    | Block b ->
      (* The base role is the node a token atom produces. A rule atom produces
         its own kind so only token atoms become children here. *)
      let token_atoms = List.filter ~f:Grammar.is_token b.atoms in
      let children =
        if token_atoms = []
        then [||]
        else
          [| mk_child
               ~modifier:Required
               (Grammar.Name.Child.of_string "atom")
               (token_alts token_atoms)
          |]
      in
      { id
      ; kind
      ; name = b.rule_name
      ; children
      ; frame = Rule.Plain
      ; body_from = 0
      ; hole
      ; origin = Rule.Pratt_block
      ; recovery = synthetic_recovery
      ; messages = [||]
      ; resync = Kind.Set.empty
      ; format = default_format
      ; edge_space_before = None
      ; edge_space_after = None
      ; identity = None
      }
    | Role { block_rule; block; role } ->
      let bk = names.rule_kind.(block_rule) in
      let operand nm =
        mk_child ~modifier:Required (Grammar.Name.Child.of_string nm) [| bk |]
      in
      let ops toks = Array.of_list (List.map ~f:res_tok toks) in
      let children, frame, body_from =
        match role with
        | Role.Base -> [||], Rule.Plain, 0
        | Role.Bin ->
          ( [| operand "lhs"
             ; mk_child
                 ~modifier:Required
                 (Grammar.Name.Child.of_string "op")
                 (ops
                    (List.map
                       ~f:(fun (op : Grammar.operator) -> op.op_token)
                       block.infix_ops))
             ; operand "rhs"
            |]
          , Rule.Plain
          , 0 )
        | Role.Prefix ->
          ( [| mk_child
                 ~modifier:Required
                 (Grammar.Name.Child.of_string "op")
                 (ops
                    (List.map
                       ~f:(fun (o : Grammar.operator) -> o.op_token)
                       block.prefix_ops))
             ; operand "operand"
            |]
          , Rule.Plain
          , 0 )
        | Role.Postfix i ->
          let p = List.nth block.postfix i in
          (match p.body with
           | Nothing ->
             ( [| operand "operand"
                ; mk_child
                    ~modifier:Required
                    (Grammar.Name.Child.of_string "op")
                    [| res_tok p.lead |]
               |]
             , Rule.Plain
             , 0 )
           | Then sym ->
             ( [| operand "operand"
                ; mk_child
                    ~modifier:Required
                    (Grammar.Name.Child.of_string "op")
                    [| res_tok p.lead |]
                ; mk_child
                    ~modifier:Required
                    (Grammar.Name.Child.of_string "rhs")
                    [| res sym |]
               |]
             , Rule.Plain
             , 0 )
           | Enclosed { close; content } ->
             (* This is the frame a production's [Delimited] carries, on a postfix. The
                operand comes before it, which is what [body_from] records. *)
             let sep, body_child =
               match content with
               | One sym ->
                 ( None
                 , mk_child
                     ~modifier:Required
                     (Grammar.Name.Child.of_string "body")
                     [| res sym |] )
               | Many { elem; sep } ->
                 ( of_sep sep
                 , mk_child
                     ~modifier:Repeated
                     (Grammar.Name.Child.of_string "args")
                     [| res elem |] )
             in
             ( [| operand "operand"; body_child |]
             , Rule.Delimited
                 { open_ = res_tok p.lead; close = res_tok close; sep; boundary = false }
             , 1 ))
      in
      { id
      ; kind
      ; name = Grammar.Name.Rule.of_string (Role.synthetic_name block role)
      ; children
      ; frame
      ; body_from
      ; hole
      ; origin = Rule.Pratt_role { block = block_rule; role }
      ; recovery = synthetic_recovery
      ; messages = [||]
      ; resync = Kind.Set.empty
      ; format = default_format
      ; edge_space_before = None
      ; edge_space_after = None
      ; identity = None
      }
  in
  let rules = Array.mapi ~f:build names.slots in
  (* Records where each block's roles landed in the id space. *)
  let role_rule_of = Hashtbl.create 8 in
  Array.iteri
    ~f:(fun i s ->
      match s with
      | Role { block_rule; role; _ } -> Hashtbl.replace role_rule_of (block_rule, role) i
      | _ -> ())
    names.slots;
  let blocks =
    Array.of_list
      (List.mapi
         ~f:(fun bi (b : Grammar.expr_def) ->
           let block_rule = names.block_base + bi in
           let op (o : Grammar.operator) : Block.op =
             { op_kind = res_tok o.op_token; bp = o.bp; assoc = o.op_assoc }
           in
           let body : Grammar.postfix_body -> Block.body = function
             | Nothing -> Block.Nothing
             | Then s -> Block.Then [| res s |]
             | Enclosed { close; content } ->
               Block.Enclosed
                 { close = res_tok close
                 ; content =
                     (match content with
                      | One s -> Block.One [| res s |]
                      | Many { elem; sep } ->
                        Block.Many { elem = [| res elem |]; sep = of_sep sep })
                 }
           in
           { Block.id = Block.id_of_int bi
           ; rule_id = block_rule
           ; kind = names.rule_kind.(block_rule)
           ; hole_kind = Option.get names.rule_hole.(block_rule)
           ; name = b.rule_name
           ; atoms = Array.of_list (List.map ~f:res b.atoms)
           ; prefix = Array.of_list (List.map ~f:op b.prefix_ops)
           ; infix = Array.of_list (List.map ~f:op b.infix_ops)
           ; postfix =
               Array.of_list
                 (List.mapi
                    ~f:(fun i (p : Grammar.postfix_op) : Block.postfix ->
                      { p_lead = res_tok p.lead
                      ; p_bp = p.bp
                      ; p_body = body p.body
                      ; p_rule =
                          (match
                             Hashtbl.find_opt role_rule_of (block_rule, Role.Postfix i)
                           with
                           | Some r -> r
                           | None -> block_rule)
                      })
                    b.postfix)
           ; infix_recovery = b.infix_recovery
           ; format = b.e_format
           })
         names.grammar.expr)
  in
  { names; rules; blocks }
;;

let lexer (n : names) : Redfa.Dfa.t =
  Redfa.Dfa.of_tokens
    (Array.to_list
       (Array.map ~f:(fun (t : Token.def) -> t.Token.id, t.Token.regex) n.tokens))
;;
