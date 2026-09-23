type entry =
  { name : string
  ; grammar : Core.Grammar.t
  ; overrides : Scopes.override list
  }

let token (name : string) (scope : Scopes.Scope.t) : Scopes.override =
  Scopes.Token { token = Core.Grammar.Name.Token.of_string name; scope }
;;

let rule (name : string) (scope : Scopes.Scope.t) : Scopes.override =
  Scopes.Rule { rule = Core.Grammar.Name.Rule.of_string name; scope }
;;

let identity (name : string) (scope : Scopes.Scope.t) : Scopes.override =
  Scopes.Identity { rule = Core.Grammar.Name.Rule.of_string name; scope }
;;

let child (rule : string) (child : string) (scope : Scopes.Scope.t) : Scopes.override =
  Scopes.Child
    { rule = Core.Grammar.Name.Rule.of_string rule
    ; child = Core.Grammar.Name.Child.of_string child
    ; scope
    }
;;

(* JSON. No grammar states any of these: that [true] is a constant rather
   than a keyword, and that a pattern token holding quotes is a string. A
   pattern token is left uncoloured by default, because [ident] and [number]
   have one shape in a grammar. *)
let json : Scopes.override list =
  [ token "string" Scopes.Scope.String_quoted_double
  ; token "number" Scopes.Scope.Constant_numeric_other
  ; token "true" Scopes.Scope.Constant_language
  ; token "false" Scopes.Scope.Constant_language
  ; token "null" Scopes.Scope.Constant_language
  ; child "Member" "key" (Scopes.Scope.Custom "support.type.property-name")
  ]
;;

(* The query language. A reader picks out its keywords first, and every
   clause opens on one. *)
let wide : Scopes.override list =
  [ token "ident" Scopes.Scope.Variable_other
  ; token "number" Scopes.Scope.Constant_numeric_integer
  ; token "string" Scopes.Scope.String_quoted_double
  ; token "select" (Scopes.Scope.Keyword_control (Some "select"))
  ; token "from" (Scopes.Scope.Keyword_control (Some "from"))
  ; token "where" (Scopes.Scope.Keyword_control (Some "where"))
  ; token "join" (Scopes.Scope.Keyword_control (Some "join"))
  ; token "and" Scopes.Scope.Keyword_operator_logical
  ; token "or" Scopes.Scope.Keyword_operator_logical
  ; child "Alias" "name" Scopes.Scope.Variable_other_member
  ; child "Table" "name" (Scopes.Scope.Entity_name_type None)
  ]
;;

(* Rust, scoped the way a theme reads it.

   The identity overrides do the work here. [Struct] and [Fn] both name
   themselves by a child called [name], and both are an [ident], so only the
   position separates a type name from a function name. *)
let rust : Scopes.override list =
  [ token "ident" Scopes.Scope.Variable_other
  ; token "int" Scopes.Scope.Constant_numeric_integer
  ; token "struct" (Scopes.Scope.Storage_type (Some "struct"))
  ; token "enum" (Scopes.Scope.Storage_type (Some "enum"))
  ; token "trait" (Scopes.Scope.Storage_type (Some "trait"))
  ; token "fn" (Scopes.Scope.Storage_type (Some "fn"))
  ; token "let" (Scopes.Scope.Keyword_control (Some "let"))
  ; token "match" (Scopes.Scope.Keyword_control (Some "match"))
  ; identity "Struct" Scopes.Scope.Entity_name_type_struct
  ; identity "Enum" Scopes.Scope.Entity_name_type_enum
  ; identity "Trait" Scopes.Scope.Entity_name_type_trait
  ; identity "Fn" Scopes.Scope.Entity_name_function
  ; identity "MethodSig" Scopes.Scope.Entity_name_function
  ; child "Type" "name" (Scopes.Scope.Entity_name_type None)
  ; child "Field" "name" Scopes.Scope.Variable_other_member
  ; child "Param" "name" Scopes.Scope.Variable_parameter
  ; child "Variant" "name" (Scopes.Scope.Custom "variable.other.enummember")
  ; child "Let" "name" Scopes.Scope.Variable_other
    (* An expression block's postfix operators desugar into rules like any
       other. Scoping a role's name scopes the span a call occupies. *)
  ; rule "ExprPostfixCall" (Scopes.Scope.Meta "function-call")
  ; rule "ExprPostfixIndex" (Scopes.Scope.Meta "subscript")
  ; rule "ExprPostfixField" (Scopes.Scope.Meta "member-access")
  ]
;;

(* Effekt. The doc comment scope could not be derived. The token is
   meaningful rather than trivia, because the formatter may not touch its
   bytes. Nothing in the grammar says it reads as a comment. *)
let effekt : Scopes.override list =
  [ token "ident" Scopes.Scope.Variable_other
  ; token "int" Scopes.Scope.Constant_numeric_integer
  ; token "string" Scopes.Scope.String_quoted_double
  ; token "doc_comment" (Scopes.Scope.Comment_line_other "documentation")
  ; token "def" (Scopes.Scope.Storage_type (Some "def"))
  ; token "type" (Scopes.Scope.Storage_type (Some "type"))
  ; token "interface" (Scopes.Scope.Storage_type (Some "interface"))
  ; token "effect" (Scopes.Scope.Storage_type (Some "effect"))
  ; token "val" (Scopes.Scope.Keyword_control (Some "val"))
  ; token "var" (Scopes.Scope.Keyword_control (Some "var"))
  ; token "if" (Scopes.Scope.Keyword_control (Some "if"))
  ; token "else" (Scopes.Scope.Keyword_control (Some "else"))
  ; token "while" (Scopes.Scope.Keyword_control (Some "while"))
  ; token "return" (Scopes.Scope.Keyword_control (Some "return"))
  ; token "try" (Scopes.Scope.Keyword_control (Some "try"))
  ; token "with" (Scopes.Scope.Keyword_control (Some "with"))
  ; token "do" (Scopes.Scope.Keyword_control (Some "do"))
  ; token "module" (Scopes.Scope.Keyword_other (Some "module"))
  ; token "import" (Scopes.Scope.Keyword_other (Some "import"))
  ]
;;

let all : entry list =
  [ { name = "sexp"; grammar = Lingo_grammars.Sexp_grammar.grammar; overrides = [] }
  ; { name = "json"; grammar = Lingo_grammars.Json_grammar.grammar; overrides = json }
  ; { name = "calc"; grammar = Lingo_grammars.Calc_grammar.grammar; overrides = [] }
  ; { name = "rassoc"; grammar = Lingo_grammars.Rassoc_grammar.grammar; overrides = [] }
  ; { name = "postfix"; grammar = Lingo_grammars.Postfix_grammar.grammar; overrides = [] }
  ; { name = "shapes"; grammar = Lingo_grammars.Shapes_grammar.grammar; overrides = [] }
  ; { name = "unicode"; grammar = Lingo_grammars.Unicode_grammar.grammar; overrides = [] }
  ; { name = "recovery"
    ; grammar = Lingo_grammars.Recovery_grammar.grammar
    ; overrides = []
    }
  ; { name = "comments"
    ; grammar = Lingo_grammars.Comments_grammar.grammar
    ; overrides = []
    }
  ; { name = "rust"; grammar = Lingo_grammars.Rust_grammar.grammar; overrides = rust }
  ; { name = "effekt"
    ; grammar = Lingo_grammars.Effekt_grammar.grammar
    ; overrides = effekt
    }
  ; { name = "wide"; grammar = Lingo_grammars.Wide_grammar.grammar; overrides = wide }
  ]
;;

let find (name : string) : entry option =
  List.find_opt (fun (entry : entry) -> entry.name = name) all
;;

let scopes (entry : entry) : Scopes.t =
  match Core.Facts.of_grammar entry.grammar with
  | Error es -> failwith (Format.asprintf "%s: %a" entry.name Core.Error.pp_list es)
  | Ok facts ->
    (match Scopes.of_facts facts ~overrides:entry.overrides with
     | Ok scopes -> scopes
     | Error fs ->
       failwith
         (Format.asprintf "%s: %a" entry.name (Format.pp_print_list Scopes.pp_finding) fs))
;;

let examples (entry : entry) : (string * string) list =
  let inputs =
    match entry.name with
    | "sexp" -> Some Inputs.sexp
    | "json" -> Some Inputs.json
    | "calc" -> Some Inputs.calc
    | "rassoc" -> Some Inputs.rassoc
    | "postfix" -> Some Inputs.postfix
    | "shapes" -> Some Inputs.shapes
    | "unicode" -> Some Inputs.unicode
    | "recovery" -> Some Inputs.recovery
    | "comments" -> Some Inputs.comments
    | "rust" -> Some Inputs.rust
    | "effekt" -> Some Inputs.effekt
    | "wide" -> Some Inputs.wide
    | _ -> None
  in
  match inputs with
  | None -> []
  | Some inputs ->
    (* The corpus opens with the empty input. That one reaches a parse of
       nothing, and it shows nothing on a page. *)
    let worth_showing = List.filter (fun s -> String.trim s <> "") inputs.good in
    let short = List.filteri (fun index _ -> index < 2) worth_showing in
    let long =
      match List.find_opt (fun s -> String.contains s '\n') worth_showing with
      | Some source when not (List.mem source short) -> [ source ]
      | _ -> []
    in
    List.mapi
      (fun index source -> Printf.sprintf "Example %d" (index + 1), source)
      (short @ long)
;;
