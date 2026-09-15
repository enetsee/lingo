open StdLabels

(* -- a grammar with productions in it -------------------------------------- *)

let empty_grammar (n : Stage.names) (acc : Error.t list) : Error.t list =
  if n.grammar.productions = []
  then Error.make ~detail:Error.Empty_grammar Error.At_grammar :: acc
  else acc
;;

(* -- names are identifiers ------------------------------------------------- *)

(* Every name here becomes a constructor, a binding or an accessor, and is
   emitted as written. So it has to be an identifier. A dot inside a name
   would scope as a qualified path. *)
let invalid_names (names : Stage.names) (acc : Error.t list) : Error.t list =
  let Grammar.{ tokens; productions; expr; _ } = names.grammar in
  let named =
    List.map
      ~f:(fun (token_def : Grammar.token_def) ->
        ( Error.At_token token_def.token_name
        , "token name"
        , Grammar.Name.Token.to_string token_def.token_name ))
      tokens
    @ List.map
        ~f:(fun (prod : Grammar.production) ->
          ( Error.At_production prod.kind_name
          , "production name"
          , Grammar.Name.Rule.to_string prod.kind_name ))
        productions
    @ List.map
        ~f:(fun (expr_def : Grammar.expr_def) ->
          ( Error.At_block expr_def.rule_name
          , "expression block name"
          , Grammar.Name.Rule.to_string expr_def.rule_name ))
        expr
    @ List.concat_map
        ~f:(fun (prod : Grammar.production) ->
          List.map
            ~f:(fun (c : Grammar.child) ->
              ( Error.At_child { production = prod.kind_name; child = c.name }
              , "child name"
              , Grammar.Name.Child.to_string c.name ))
            prod.children)
        productions
    (* A [kind_suffix] is spliced into three emitted identifiers.
       [Role.kind_suffix] uppercases it. [Role.snake_suffix] takes it as
       written. [Role.pascal_suffix] splits it on [_] and capitalises each part.

       The check runs before any of those three, and asks for an identifier. An
       identifier survives all three manglings.

       That is stricter than the emitted code needs. A suffix lands inside an
       identifier and never at its front, so a leading digit would still
       compile. Every other name in a grammar has to be an identifier, and an
       exception here would be one more rule to carry. *)
    @ List.concat_map
        ~f:(fun (expr_def : Grammar.expr_def) ->
          List.filter_map
            ~f:(fun (p : Grammar.postfix_op) ->
              (* An empty suffix means none was given. A block with one postfix
                 operator may do that. A block that needs one gets
                 [postfix-suffix-missing] from [pratt] instead. *)
              if p.kind_suffix = ""
              then None
              else
                Some
                  ( Error.At_operator { block = expr_def.rule_name; token = p.lead }
                  , "postfix kind_suffix"
                  , p.kind_suffix ))
            expr_def.postfix)
        expr
  in
  List.fold_left named ~init:acc ~f:(fun acc (where, what, name) ->
    match Mangle.ident_error name with
    | None -> acc
    | Some reason ->
      Error.make ~detail:(Error.Invalid_name { what; name; reason }) where :: acc)
;;

(* -- token literals lower to a regex --------------------------------------- *)

let invalid_token_literals (n : Stage.names) (acc : Error.t list) : Error.t list =
  List.fold_left n.grammar.tokens ~init:acc ~f:(fun acc (token_def : Grammar.token_def) ->
    match Token.lower token_def.token_class with
    | Ok _ -> acc
    | Error reason ->
      Error.make
        ~detail:(Error.Invalid_token_literal { reason })
        (Error.At_token token_def.token_name)
      :: acc)
;;

(* -- emitted-name collisions ----------------------------------------------- *)

(* The collision check, over every namespace. A collision is two manifest
   entries that share a scope and an emitted string.

   Two things pick the code: the scope, and whether either entry is a name a
   backend emits for every grammar. *)
let collisions ?(suppress_bases = []) (names : Stage.names) (acc : Error.t list)
  : Error.t list
  =
  let sources es = List.map ~f:(fun (e : Manifest.entry) -> e.base) es in
  let suppressed (c : Manifest.collision) =
    (* Two postfix operators with no suffix define one kind and one format
       function. A block that has already been told to give them distinct
       suffixes collides here as well. Report the suffix. *)
    suppress_bases <> []
    && List.for_all
         ~f:(fun (e : Manifest.entry) -> List.mem e.base ~set:suppress_bases)
         c.c_entries
  in
  List.fold_left
    (List.filter ~f:(fun c -> not (suppressed c)) (Manifest.collisions names.manifest))
    ~init:acc
    ~f:(fun acc (c : Manifest.collision) ->
      let is_builtin (e : Manifest.entry) = e.base = Manifest.builtin in
      let builtin_hit = List.exists ~f:is_builtin c.c_entries in
      let user_sources =
        let entries =
          List.filter ~f:(fun (e : Manifest.entry) -> not (is_builtin e)) c.c_entries
        in
        sources entries
      in
      let where =
        match user_sources with
        (* An entry's [base] is whichever declaration reached the emitted name, so
           it may be a production, a token or a child path. The site is a best
           effort at the first of them. *)
        | s :: _ -> Error.At_production (Grammar.Name.Rule.of_string s)
        | [] -> Error.At_grammar
      in
      if builtin_hit
      then
        Error.make
          ~detail:
            (Error.Reserved_name
               { sources = user_sources; emitted = c.c_emitted; scope = c.c_scope })
          where
        :: acc
      else if c.c_scope = Manifest.Scope.Kind_enum
      then
        Error.make
          ~detail:(Error.Dup_kind_name { sources = user_sources; emitted = c.c_emitted })
          where
        :: acc
      else (
        match c.c_scope with
        | Manifest.Scope.View_accessor m ->
          Error.make
            ~detail:
              (Error.Dup_child_name
                 { sources = user_sources; emitted = c.c_emitted; view_module = m })
            where
          :: acc
        | scope when Manifest.Scope.fails_to_compile scope ->
          Error.make
            ~detail:
              (Error.Name_collision
                 { sources = user_sources; emitted = c.c_emitted; scope })
            where
          :: acc
        (* A repeat in the remaining scope shadows, so it still compiles. It is
           listed for the cross-check against the emitted text, and left alone here. *)
        | _ -> acc))
;;

(* -- references resolve ---------------------------------------------------- *)

let sym_error
      (names : Stage.names)
      (where : Error.where)
      (sym : Grammar.symbol)
      (acc : Error.t list)
  : Error.t list
  =
  match sym with
  | Grammar.Token t ->
    let t = Grammar.Name.Token.of_string t in
    if Stage.find_token names t = None
    then Error.make ~detail:(Error.Unknown_token { name = t }) where :: acc
    else acc
  | Rule rule_name ->
    let rule = Grammar.Name.Rule.of_string rule_name in
    if Option.is_none (Stage.find_rule names rule)
    then Error.make ~detail:(Error.Unknown_rule { name = rule }) where :: acc
    else acc
;;

let token_ref
      (names : Stage.names)
      (detail : Grammar.Name.Token.t -> Error.detail)
      (where : Error.where)
      (token_name : Grammar.Name.Token.t)
      (acc : Error.t list)
  : Error.t list
  =
  if Stage.find_token names token_name = None
  then Error.make ~detail:(detail token_name) where :: acc
  else acc
;;

let production_refs (names : Stage.names) (acc : Error.t list) : Error.t list =
  List.fold_left
    names.grammar.productions
    ~init:acc
    ~f:(fun acc (prod : Grammar.production) ->
      let pn = prod.kind_name in
      let child_names = List.map ~f:(fun (c : Grammar.child) -> c.name) prod.children in
      let acc =
        List.fold_left prod.children ~init:acc ~f:(fun acc (c : Grammar.child) ->
          let where = Error.At_child { production = pn; child = c.name } in
          (* A recovery set replaces what the parse resumes on where a child
             cannot be read. An optional child's absence is silent and a
             repeated one just ends its loop, so neither has that moment. *)
          let acc =
            match c.modifier, c.c_parse.recover_to with
            | (Grammar.Optional | Grammar.Repeated), Some _ ->
              Error.make ~detail:(Error.Unused_recover_to { name = c.name }) where :: acc
            | (Grammar.Optional | Grammar.Repeated | Grammar.Required), _ -> acc
          in
          match c.sym with
          | Single s -> sym_error names where s acc
          | Alternatives [] -> Error.make ~detail:Error.Empty_alternatives where :: acc
          | Alternatives syms ->
            List.fold_left ~f:(fun acc s -> sym_error names where s acc) ~init:acc syms)
      in
      let acc =
        match prod.identity_child with
        | Some nm when not (List.mem nm ~set:child_names) ->
          Error.make
            ~detail:(Error.Unknown_identity_child { name = nm })
            (Error.At_production pn)
          :: acc
        | _ -> acc
      in
      let acc =
        List.fold_left prod.error_messages ~init:acc ~f:(fun acc (nm, _msg) ->
          match
            List.find_opt prod.children ~f:(fun (c : Grammar.child) ->
              Grammar.Name.Child.equal c.name nm)
          with
          | None ->
            Error.make
              ~detail:(Error.Unknown_message_child { name = nm })
              (Error.At_production pn)
            :: acc
          (* Only a required child reports, so only a required child has
             wording to replace. *)
          | Some { modifier = Grammar.Optional | Grammar.Repeated; _ } ->
            Error.make
              ~detail:(Error.Unused_message_child { name = nm })
              (Error.At_child { production = pn; child = nm })
            :: acc
          | Some { modifier = Grammar.Required; _ } -> acc)
      in
      let acc =
        match prod.framing with
        | Plain | Committed _ -> acc
        | Delimited { open_tok; close_tok; sep_policy; _ } ->
          let where = Error.At_production pn in
          let acc =
            token_ref
              names
              (fun name -> Error.Unknown_delimiter_token { name })
              where
              open_tok
              acc
          in
          let acc =
            token_ref
              names
              (fun name -> Error.Unknown_delimiter_token { name })
              where
              close_tok
              acc
          in
          (match sep_policy with
           | No_sep -> acc
           | With_sep { sep; _ } ->
             token_ref
               names
               (fun name -> Error.Unknown_delimiter_token { name })
               where
               sep
               acc)
        | Separated { sep; _ } ->
          token_ref
            names
            (fun name -> Error.Unknown_delimiter_token { name })
            (Error.At_production pn)
            sep
            acc
      in
      let acc =
        List.fold_left prod.children ~init:acc ~f:(fun acc (c : Grammar.child) ->
          match c.c_parse.recover_to with
          | None -> acc
          | Some toks ->
            List.fold_left toks ~init:acc ~f:(fun acc t ->
              token_ref
                names
                (fun name -> Error.Unknown_recover_to_token { name })
                (Error.At_child { production = pn; child = c.name })
                t
                acc))
      in
      List.fold_left prod.resync_anchors ~init:acc ~f:(fun acc t ->
        token_ref
          names
          (fun name -> Error.Unknown_resync_anchor { name })
          (Error.At_production pn)
          t
          acc))
;;

let block_refs (names : Stage.names) (acc : Error.t list) : Error.t list =
  List.fold_left names.grammar.expr ~init:acc ~f:(fun acc (expr_def : Grammar.expr_def) ->
    let bn = expr_def.rule_name in
    let at = Error.At_block bn in
    let op_tok t acc =
      if Stage.find_token names t = None
      then
        Error.make
          ~detail:(Error.Unknown_op_token { name = t })
          (Error.At_operator { block = bn; token = t })
        :: acc
      else acc
    in
    let sym s acc =
      match s with
      | Grammar.Token t -> op_tok (Grammar.Name.Token.of_string t) acc
      | Rule r ->
        let r = Grammar.Name.Rule.of_string r in
        if Stage.find_rule names r = None
        then Error.make ~detail:(Error.Unknown_rule { name = r }) at :: acc
        else acc
    in
    let acc = List.fold_left expr_def.atoms ~init:acc ~f:(fun acc s -> sym s acc) in
    let acc =
      List.fold_left
        (expr_def.prefix_ops @ expr_def.infix_ops)
        ~init:acc
        ~f:(fun acc (o : Grammar.operator) -> op_tok o.op_token acc)
    in
    List.fold_left
      expr_def.postfix
      ~init:acc
      ~f:(fun acc (postfix_op : Grammar.postfix_op) ->
        let acc = op_tok postfix_op.lead acc in
        match postfix_op.body with
        | Nothing -> acc
        | Then s -> sym s acc
        | Enclosed { close; content } ->
          let acc = op_tok close acc in
          (match content with
           | One s -> sym s acc
           | Many { elem; sep } ->
             let acc = sym elem acc in
             (match sep with
              | No_sep -> acc
              | With_sep { sep; _ } -> op_tok sep acc))))
;;

(* -- roots ----------------------------------------------------------------- *)

let roots (names : Stage.names) (acc : Error.t list) : Error.t list =
  match names.grammar.roots with
  | [] -> Error.make ~detail:Error.No_roots Error.At_grammar :: acc
  | rs ->
    let seen = Hashtbl.create 4 in
    List.fold_left rs ~init:acc ~f:(fun acc rule ->
      let acc =
        match Stage.find_rule names rule with
        | Some id ->
          (match names.slots.(id) with
           | Stage.Prod _ -> acc
           | Stage.Block _ | Stage.Role _ ->
             Error.make
               ~detail:(Error.Root_is_block { name = rule })
               (Error.At_production rule)
             :: acc)
        | None ->
          Error.make
            ~detail:(Error.Unknown_root { name = rule })
            (Error.At_production rule)
          :: acc
      in
      if Hashtbl.mem seen rule
      then
        Error.make ~detail:(Error.Dup_root { name = rule }) (Error.At_production rule)
        :: acc
      else (
        Hashtbl.add seen rule ();
        acc))
;;

(* -- tokens ---------------------------------------------------------------- *)

(* A regex matching the empty string gives the lexer a zero-byte match at
   every position. The lexer re-enters the initial state at the same offset.
   No transition fires. It emits an empty token, and loops.

   [kw ""] lowers to eps, so it is reported here too. *)
let nullable_tokens (names : Stage.names) (acc : Error.t list) : Error.t list =
  Array.fold_left names.tokens ~init:acc ~f:(fun acc (t : Token.def) ->
    if Redfa.Regex.is_nullable t.regex
    then Error.make ~detail:Error.Nullable_token (Error.At_token t.name) :: acc
    else acc)
;;

(* -- operator tables ------------------------------------------------------- *)

let postfix_tok (p : Grammar.postfix_op) : Grammar.Name.Token.t = p.lead

let pratt (names : Stage.names) (acc : Error.t list) : Error.t list =
  List.fold_left names.grammar.expr ~init:acc ~f:(fun acc (b : Grammar.expr_def) ->
    let at = Error.At_block b.rule_name in
    (* With no atoms, FIRST of the block is its prefix tokens. With no
       prefix operators either, FIRST is empty, and every reference to the
       block is dropped or unreachable. *)
    let acc =
      if b.atoms = [] then Error.make ~detail:Error.Empty_pratt_atoms at :: acc else acc
    in
    (* This looks for duplicates within one category. A token in two categories
       is a separate question, and prefix plus infix is how unary and binary
       minus work. See the mixed-role check below. *)
    let dups category toks acc =
      let seen = Hashtbl.create 8 in
      List.fold_left toks ~init:acc ~f:(fun acc t ->
        if Hashtbl.mem seen t
        then
          Error.make
            ~detail:(Error.Dup_pratt_op { token = t; category })
            (Error.At_operator { block = b.rule_name; token = t })
          :: acc
        else (
          Hashtbl.add seen t ();
          acc))
    in
    let prefix_toks = List.map b.prefix_ops ~f:(fun (o : Grammar.operator) -> o.op_token)
    and infix_toks = List.map b.infix_ops ~f:(fun (o : Grammar.operator) -> o.op_token)
    and postfix_toks = List.map b.postfix ~f:postfix_tok in
    let acc = dups "prefix" prefix_toks acc in
    let acc = dups "infix" infix_toks acc in
    let acc = dups "postfix" postfix_toks acc in
    (* Position decides between prefix and infix, and the two contexts are
       separate at parse time. The loop checks postfix triggers before infix
       dispatch, so a token declared as both parses as postfix every time. *)
    let acc =
      List.fold_left
        (List.sort_uniq ~cmp:Grammar.Name.Token.compare infix_toks)
        ~init:acc
        ~f:(fun acc t ->
          if List.mem t ~set:postfix_toks
          then
            Error.make
              ~detail:(Error.Mixed_pratt_role { token = t })
              (Error.At_operator { block = b.rule_name; token = t })
            :: acc
          else acc)
    in
    (* The threshold test is the same for every operator at a binding power. The
       parent of one operator cannot tell which associativity started the level,
       so right wins. Two infix operators that share a binding power want the
       same associativity. *)
    let acc =
      let by_bp = Hashtbl.create 8 in
      List.iter
        ~f:(fun (o : Grammar.operator) ->
          Hashtbl.replace
            by_bp
            o.bp
            ((o.op_token, o.op_assoc)
             ::
             (try Hashtbl.find by_bp o.bp with
              | Not_found -> [])))
        b.infix_ops;
      Hashtbl.fold
        (fun bp entries acc ->
           let assocs = List.map ~f:snd entries in
           if List.mem Grammar.Left ~set:assocs && List.mem Grammar.Right ~set:assocs
           then
             Error.make
               ~detail:(Error.Mixed_assoc_at_bp { bp; ops = List.rev entries })
               at
             :: acc
           else acc)
        by_bp
        acc
    in
    (* With two or more postfix operators, the suffix keeps their kinds and
       format functions apart. A block with one operator can leave it empty. *)
    match b.postfix with
    | [] | [ _ ] -> acc
    | entries ->
      let acc =
        let _idx, errs =
          List.fold_left
            ~f:(fun (i, acc) (p : Grammar.postfix_op) ->
              ( i + 1
              , if p.kind_suffix = ""
                then
                  Error.make
                    ~detail:
                      (Error.Postfix_suffix_missing
                         { index = i; total = List.length entries })
                    (Error.At_operator { block = b.rule_name; token = p.lead })
                  :: acc
                else acc ))
            ~init:(0, acc)
            entries
        in
        errs
      in
      let seen = Hashtbl.create 4 in
      List.fold_left entries ~init:acc ~f:(fun acc (p : Grammar.postfix_op) ->
        if p.kind_suffix = ""
        then acc
        else if Hashtbl.mem seen p.kind_suffix
        then
          Error.make
            ~detail:(Error.Postfix_suffix_duplicate { suffix = p.kind_suffix })
            (Error.At_operator { block = b.rule_name; token = p.lead })
          :: acc
        else (
          Hashtbl.add seen p.kind_suffix ();
          acc)))
;;

let run (names : Stage.names) : Error.t list =
  (* An empty grammar is answered on its own. Every other check here reads a
     relationship between declarations, and there are none to read. *)
  match empty_grammar names [] with
  | _ :: _ as es -> es
  | [] ->
    let pratt_errs = pratt names [] in
    (* These blocks have already been told that their postfix suffixes are
       wrong. Their kind and format-function collisions follow from that, so
       they are suppressed below. *)
    let suppress_bases =
      List.filter_map
        ~f:(fun (e : Error.t) ->
          match e.detail with
          | Error.Postfix_suffix_missing _ | Error.Postfix_suffix_duplicate _ ->
            (match e.where with
             | Error.At_block b -> Some (Grammar.Name.Rule.to_string b)
             | Error.At_operator { block; _ } -> Some (Grammar.Name.Rule.to_string block)
             | _ -> None)
          | _ -> None)
        pratt_errs
    in
    invalid_names names pratt_errs
    |> invalid_token_literals names
    |> collisions ~suppress_bases names
    |> production_refs names
    |> block_refs names
    |> roots names
    |> nullable_tokens names
;;
