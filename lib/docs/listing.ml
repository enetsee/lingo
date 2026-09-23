open StdLabels

let doc = Handsome.Utf8.text

let mark (mark : Mark.t) (inner : Render.document) : Render.document =
  Handsome.Utf8.annotate mark inner
;;

let notation (s : string) : Render.document = mark Mark.Notation (doc s)
let space : Render.document = Handsome.Utf8.line
let ( ^^ ) = Handsome.Utf8.( ^^ )

let find_token (grammar : Core.Grammar.t) (name : string) : Core.Grammar.token_def option =
  List.find_opt grammar.tokens ~f:(fun (token : Core.Grammar.token_def) ->
    String.equal (Core.Grammar.Name.Token.to_string token.token_name) name)
;;

(* A token is shown as the text it matches, where it has one. A pattern
   token has no fixed text, so that one is shown by its name. A reader types
   [struct], so the word itself is more use on the page than the name the
   grammar gave it. *)
let symbol (grammar : Core.Grammar.t) (symbol : Core.Grammar.symbol) : Render.document =
  match symbol with
  | Core.Grammar.Rule name -> mark (Mark.Reference name) (doc name)
  | Core.Grammar.Token name ->
    (match find_token grammar name with
     | Some { token_class = Core.Grammar.Keyword text; _ }
     | Some { token_class = Core.Grammar.Punctuation text; _ } ->
       mark (Mark.Literal name) (doc ("'" ^ text ^ "'"))
     | _ -> mark (Mark.Token name) (doc name))
;;

let alternatives (grammar : Core.Grammar.t) (symbols : Core.Grammar.symbol list)
  : Render.document
  =
  match symbols with
  | [] -> notation "( )"
  | [ one ] -> symbol grammar one
  | many ->
    let bar = notation "|" in
    Handsome.Utf8.group
      (notation "("
       ^^ Handsome.Utf8.nest
            2
            (Handsome.Utf8.softline
             ^^ Handsome.Utf8.concat
                  (List.concat
                     (List.mapi many ~f:(fun index one ->
                        if index = 0
                        then [ symbol grammar one ]
                        else [ space; bar; doc " "; symbol grammar one ]))))
       ^^ Handsome.Utf8.softline
       ^^ notation ")")
;;

let modifier (modifier : Core.Grammar.modifier) : Render.document =
  match modifier with
  | Core.Grammar.Exactly_one -> Handsome.Utf8.empty
  | Core.Grammar.Zero_or_one -> notation "?"
  | Core.Grammar.Zero_or_more -> notation "*"
  | Core.Grammar.One_or_more -> notation "+"
;;

let child (grammar : Core.Grammar.t) (child : Core.Grammar.child) : Render.document =
  let body = alternatives grammar (child.head :: child.rest) in
  mark Mark.Child (doc (Core.Grammar.Name.Child.to_string child.name))
  ^^ notation ":"
  ^^ body
  ^^ modifier child.modifier
;;

let token_text (grammar : Core.Grammar.t) (name : Core.Grammar.Name.Token.t)
  : Render.document
  =
  symbol grammar (Core.Grammar.Token (Core.Grammar.Name.Token.to_string name))
;;

(* A framed body shows its separator and its trailing policy. A reader has
   to know both to type the language. *)
let separator (grammar : Core.Grammar.t) (sep : Core.Grammar.sep_policy) : Render.document
  =
  match sep with
  | Core.Grammar.No_sep -> Handsome.Utf8.empty
  | Core.Grammar.With_sep { sep; trailing } ->
    doc " "
    ^^ notation "sep"
    ^^ doc " "
    ^^ token_text grammar sep
    ^^
      (match trailing with
      | Core.Grammar.Never -> Handsome.Utf8.empty
      | Core.Grammar.On_break -> doc " " ^^ notation "trailing-on-break"
      | Core.Grammar.Always -> doc " " ^^ notation "trailing")
;;

let production (grammar : Core.Grammar.t) (production : Core.Grammar.production)
  : Render.document
  =
  let children =
    Handsome.Utf8.concat
      (List.concat
         (List.mapi production.children ~f:(fun index one ->
            if index = 0 then [ child grammar one ] else [ space; child grammar one ])))
  in
  let body =
    match production.framing with
    | Core.Grammar.Plain -> children
    | Core.Grammar.Committed _ -> children
    | Core.Grammar.Delimited { open_tok; close_tok; sep_policy; _ } ->
      token_text grammar open_tok
      ^^ space
      ^^ children
      ^^ space
      ^^ token_text grammar close_tok
      ^^ separator grammar sep_policy
    | Core.Grammar.Separated { sep; trailing; _ } ->
      children ^^ separator grammar (Core.Grammar.With_sep { sep; trailing })
  in
  Handsome.Utf8.group
    (mark
       (Mark.Rule (Core.Grammar.Name.Rule.to_string production.kind_name))
       (doc (Core.Grammar.Name.Rule.to_string production.kind_name))
     ^^ doc " "
     ^^ notation "="
     ^^ Handsome.Utf8.nest 2 (space ^^ body))
;;

(* -- expression blocks ----------------------------------------------------- *)

let postfix_shape (grammar : Core.Grammar.t) (op : Core.Grammar.postfix_op)
  : Render.document
  =
  let lead = token_text grammar op.lead in
  match op.body with
  | Core.Grammar.Nothing -> notation "x" ^^ lead
  | Core.Grammar.Then rhs -> notation "x" ^^ lead ^^ symbol grammar rhs
  | Core.Grammar.Enclosed { close; content } ->
    let inside =
      match content with
      | Core.Grammar.One one -> symbol grammar one
      | Core.Grammar.Many { elem; sep } -> symbol grammar elem ^^ separator grammar sep
    in
    notation "x" ^^ lead ^^ inside ^^ token_text grammar close
;;

let block (grammar : Core.Grammar.t) (block : Core.Grammar.expr_def) : Render.document =
  let line (label : string) (body : Render.document) : Render.document =
    Handsome.Utf8.group (notation label ^^ Handsome.Utf8.nest 2 (space ^^ body))
    ^^ Handsome.Utf8.hardline
  in
  let operators (ops : Core.Grammar.operator list) : Render.document =
    Handsome.Utf8.concat
      (List.concat
         (List.mapi ops ~f:(fun index (op : Core.Grammar.operator) ->
            let one =
              token_text grammar op.op_token
              ^^ doc " "
              ^^ notation
                   (Printf.sprintf
                      "(%d %s)"
                      op.bp
                      (match op.op_assoc with
                       | Core.Grammar.Left -> "left"
                       | Core.Grammar.Right -> "right"))
            in
            if index = 0 then [ one ] else [ space; one ])))
  in
  mark
    (Mark.Rule (Core.Grammar.Name.Rule.to_string block.rule_name))
    (doc (Core.Grammar.Name.Rule.to_string block.rule_name))
  ^^ doc " "
  ^^ notation "= expression"
  ^^ Handsome.Utf8.hardline
  ^^ Handsome.Utf8.nest
       2
       (Handsome.Utf8.hardline
        ^^ line
             "atoms"
             (Handsome.Utf8.concat
                (List.concat
                   (List.mapi block.atoms ~f:(fun index one ->
                      if index = 0
                      then [ symbol grammar one ]
                      else [ space; symbol grammar one ]))))
        ^^ (match block.prefix_ops with
            | [] -> Handsome.Utf8.empty
            | ops -> line "prefix" (operators ops))
        ^^ (match block.infix_ops with
            | [] -> Handsome.Utf8.empty
            | ops -> line "infix" (operators ops))
        ^^
        match block.postfix with
        | [] -> Handsome.Utf8.empty
        | ops ->
          line
            "postfix"
            (Handsome.Utf8.concat
               (List.concat
                  (List.mapi ops ~f:(fun index (op : Core.Grammar.postfix_op) ->
                     let one =
                       postfix_shape grammar op
                       ^^ doc " "
                       ^^ notation (Printf.sprintf "(%d)" op.bp)
                     in
                     if index = 0 then [ one ] else [ space; one ])))))
;;
