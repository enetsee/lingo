(lang dune 3.22)

(name lingo)

(generate_opam_files true)

(source
 (github enetsee/lingo))

(authors "Michael Thomas <mthomas180@gmail.com>")

(maintainers "Michael Thomas <mthomas180@gmail.com>")

(license MIT)

; ---------------------------------------------------------------------------
; The three packages are split because their dependency sets are disjoint. A
; dune sub-library gets no opam dependencies of its own, so a dependency only
; one part of the tree needs still reaches every consumer of the package.
;
; lingo_runtime must never depend on lingo. Generated code links the runtime,
; so ppxlib and yojson stay out of a consumer's build graph.
; ---------------------------------------------------------------------------

(package
 (name lingo_runtime)
 (synopsis "Runtime linked by lingo-generated parsers and formatters")
 (description
  "The cursor, builder, diagnostics, recovery, and the table-driven plan and layout engines that generated code calls into. Depends on siesta and handsome and nothing else of ours. A generated parser can link it and leave the code generator out of the build graph.")
 ; The diagnostic list is a Dynarray, which arrived in OCaml 5.2. handsome is
 ; declared and not yet linked. Only the layout engine needs a document
 ; algebra, and that engine is still unwritten.
 (depends
  (ocaml
   (>= 5.2))
  siesta
  handsome
  (odoc :with-doc)))

(package
 (name lingo)
 (synopsis "A grammar toolkit: lossless recovering parsers, formatters, editors")
 (description
  "Takes a grammar as a value: tokens, productions with typed children, an optional Pratt expression block. Emits a lossless recovering parser, a pretty-printer, a lexer, typed views, a reference manual, and editor integrations.")
 ; handsome arrives through lingo_runtime as well, and is named here because
 ; lingo.docs links it directly. A reference page lays a listing out and then
 ; marks it up, and a rendered stream lets one document do both.
 (depends
  (ocaml
   (>= 5.0))
  lingo_runtime
  ucharset
  redfa
  bolts
  handsome
  ppxlib
  (odoc :with-doc)))

(package
 (name lingo_editors)
 (synopsis "TextMate and tree-sitter backends for lingo")
 (description
  "Renders a lingo grammar to a .tmLanguage.json TextMate grammar and to a tree-sitter grammar.js. Separate from the lingo package because only these backends need yojson.")
 (depends
  (ocaml
   (>= 5.0))
  lingo
  yojson
  (odoc :with-doc))
 ; yojson belongs to this package alone, so the two backends sit in a package
 ; of their own. The tree-sitter backend writes its files directly: a
 ; grammar.js is JavaScript and a query file is s-expressions.
 (allow_empty))
