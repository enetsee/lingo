(* -- a query language, for the width ------------------------------------------

   Every other grammar here has short productions. Their diagrams are a few
   boxes wide and fit the page, so the code that wraps a diagram across rows
   never ran.

   A select statement is a run of clauses in a fixed order, and most of the
   clauses are optional. Writing one down gives a production with eleven
   children:

   {v
     Select = 'select' distinct? ColumnList From Join* Where? GroupBy?
              Having? OrderBy? Limit? ';'
   v}

   One line of that comes to over a thousand units of diagram, against a page
   of seven hundred and sixty. Real languages have productions this wide.

   Each optional clause opens on a keyword of its own. The grammar stays
   LL(1), and no child needs [greedy].

   Source the grammar parses:

   {v
     select distinct name, price as p from items
       join stock on stock.id = items.id
       where price > 10 and name = "x"
       group by name having price > 1
       order by price desc limit 10 offset 5;
   v}
   -------------------------------------------------------------------------- *)

open Grammar

let grammar : t =
  let letter =
    Redfa.Regex.(alt (range_char ~lo:'a' ~hi:'z') (range_char ~lo:'A' ~hi:'Z'))
  in
  let digit = Redfa.Regex.range_char ~lo:'0' ~hi:'9' in
  let ident =
    Redfa.Regex.(
      seq
        (alt letter (singleton_char '_'))
        (star (alts [ letter; digit; singleton_char '_' ])))
  in
  let string_ =
    Redfa.Regex.(
      seqs
        [ singleton_char '"'
        ; star (not_chars (Ucharset.of_char_list [ '"'; '\\' ]))
        ; singleton_char '"'
        ])
  in
  let tokens =
    [ kw "select"
    ; kw "distinct"
    ; kw "from"
    ; kw "as"
    ; kw "join"
    ; kw "on"
    ; kw "where"
    ; kw "group"
    ; kw "by"
    ; kw "having"
    ; kw "order"
    ; kw "asc"
    ; kw "desc"
    ; kw "limit"
    ; kw "offset"
    ; kw "and"
    ; kw "or"
    ; punct ~space_before:false ~name:"comma" ","
    ; punct ~space_before:false ~name:"semi" ";"
    ; punct_tight ~name:"dot" "."
    ; punct_tight ~name:"lparen" "("
    ; punct_tight ~name:"rparen" ")"
    ; punct ~name:"eq" "="
    ; punct ~name:"lt" "<"
    ; punct ~name:"gt" ">"
    ; pat "ident" ident
    ; pat "number" (Redfa.Regex.plus digit)
    ; pat "string" string_
    ; pat
        ~trivia:Reformat
        "ws"
        Redfa.Regex.(plus (chars_of_char_list [ ' '; '\t'; '\n'; '\r' ]))
    ]
  in
  let file = prod ~break_style:Always "File" [ child_rep1 "stmt" (Rule "Select") ] in
  (* The grammar exists for this production. Seven of its eleven children may
     be absent, and each of those seven opens on a keyword of its own. *)
  let select =
    prod
      "Select"
      [ child_req "kw" (Token "select")
      ; child_opt "distinct" (Token "distinct")
      ; child_req "cols" (Rule "ColumnList")
      ; child_req "from" (Rule "From")
      ; child_rep "joins" (Rule "Join")
      ; child_opt "where" (Rule "Where")
      ; child_opt "group" (Rule "GroupBy")
      ; child_opt "having" (Rule "Having")
      ; child_opt "order" (Rule "OrderBy")
      ; child_opt "limit" (Rule "Limit")
      ; child_req "semi" (Token "semi")
      ]
    |> with_committed
  in
  let column_list =
    prod "ColumnList" [ child_rep1 "col" (Rule "Column") ] |> with_separator ~sep:"comma"
  in
  let column =
    prod "Column" [ child_req "value" (Rule "Expr"); child_opt "alias" (Rule "Alias") ]
  in
  let alias =
    prod "Alias" [ child_req "kw" (Token "as"); child_req "name" (Token "ident") ]
  in
  let from =
    prod "From" [ child_req "kw" (Token "from"); child_req "table" (Rule "Table") ]
  in
  let table =
    prod "Table" [ child_req "name" (Token "ident"); child_opt "alias" (Rule "Alias") ]
  in
  let join =
    prod
      "Join"
      [ child_req "kw" (Token "join")
      ; child_req "table" (Rule "Table")
      ; child_req "on" (Token "on")
      ; child_req "cond" (Rule "Expr")
      ]
    |> with_committed
  in
  let where =
    prod "Where" [ child_req "kw" (Token "where"); child_req "cond" (Rule "Expr") ]
  in
  let group_by =
    prod
      "GroupBy"
      [ child_req "kw" (Token "group")
      ; child_req "by" (Token "by")
      ; child_req "keys" (Rule "KeyList")
      ]
  in
  let key_list =
    prod "KeyList" [ child_rep1 "key" (Rule "Expr") ] |> with_separator ~sep:"comma"
  in
  let having =
    prod "Having" [ child_req "kw" (Token "having"); child_req "cond" (Rule "Expr") ]
  in
  let order_by =
    prod
      "OrderBy"
      [ child_req "kw" (Token "order")
      ; child_req "by" (Token "by")
      ; child_req "keys" (Rule "OrderList")
      ]
  in
  let order_list =
    prod "OrderList" [ child_rep1 "key" (Rule "OrderKey") ] |> with_separator ~sep:"comma"
  in
  let order_key =
    prod
      "OrderKey"
      [ child_req "value" (Rule "Expr"); child_opt "dir" (Rule "Direction") ]
  in
  let direction =
    prod
      "Direction"
      [ child_alt ~modifier:Exactly_one "how" [ Token "asc"; Token "desc" ] ]
  in
  let limit =
    prod
      "Limit"
      [ child_req "kw" (Token "limit")
      ; child_req "count" (Token "number")
      ; child_opt "offset" (Rule "Offset")
      ]
  in
  let offset =
    prod "Offset" [ child_req "kw" (Token "offset"); child_req "count" (Token "number") ]
  in
  let paren =
    prod "Paren" [ child_req "inner" (Rule "Expr") ]
    |> with_delimited ~open_tok:"lparen" ~close_tok:"rparen"
  in
  let expr =
    expr_block
      ~rule_name:"Expr"
      ~atoms:[ Token "ident"; Token "number"; Token "string"; Rule "Paren" ]
      ~infix_ops:
        [ infix ~token:"or" ~bp:5 ()
        ; infix ~token:"and" ~bp:10 ()
        ; infix ~token:"eq" ~bp:20 ()
        ; infix ~token:"lt" ~bp:20 ()
        ; infix ~token:"gt" ~bp:20 ()
        ]
      ~postfix:
        [ postfix_access ~kind_suffix:"field" ~token:"dot" ~rhs:(Token "ident") ~bp:90 ()
        ]
      ()
  in
  create
    ~expr:[ expr ]
    ~tokens
    ~roots:[ "File" ]
    [ file
    ; select
    ; column_list
    ; column
    ; alias
    ; from
    ; table
    ; join
    ; where
    ; group_by
    ; key_list
    ; having
    ; order_by
    ; order_list
    ; order_key
    ; direction
    ; limit
    ; offset
    ; paren
    ]
;;
