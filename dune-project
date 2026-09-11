(lang dune 3.22)

(name lingo)

(generate_opam_files true)

(source
 (github enetsee/lingo))

(authors "Michael Thomas <mthomas180@gmail.com>")

(maintainers "Michael Thomas <mthomas180@gmail.com>")

(license MIT)

; ---------------------------------------------------------------------------
; Three packages. The dependency sets are genuinely disjoint and dune
; sub-libraries do not get separate opam dependencies (01-ARCHITECTURE.md §2).
;
; The one rule a stanza must not break: lingo_runtime NEVER depends on lingo.
; Generated code links the runtime, and that is what keeps ppxlib and yojson
; out of a consumer's build graph.  test/laws/law_packaging.ml reads these
; stanzas back and fails if the edge appears.
; ---------------------------------------------------------------------------

(package
 (name lingo_runtime)
 (synopsis "Runtime linked by lingo-generated parsers and formatters")
 (description
  "The cursor, builder, diagnostics, recovery, and the table-driven plan and layout engines that generated code calls into. Depends on siesta and handsome and nothing else of ours, so linking a generated parser does not pull a code generator into the build graph.")
 (depends
  (ocaml
   (>= 5.0))
  siesta
  handsome
  (alcotest :with-test)
  (odoc :with-doc))
 ; Declared, not yet populated: the cursor, builder, diagnostics and recovery
 ; port is Phase 2 work, alongside the parser that calls them (03-BUILD.md §7).
 ; The stanza exists now so the dependency set is on record and the
 ; "never depends on lingo" rule is enforceable before there is code to break it.
 (allow_empty))

(package
 (name lingo)
 (synopsis "A grammar toolkit: lossless recovering parsers, formatters, editors")
 (description
  "Takes a grammar as a value -- tokens, productions with typed children, an optional Pratt expression block -- and emits a lossless recovering parser, a pretty-printer, a lexer, typed views, and editor integrations.")
 (depends
  (ocaml
   (>= 5.0))
  lingo_runtime
  ucharset
  redfa
  bolts
  ppxlib
  (alcotest :with-test)
  (odoc :with-doc)))

(package
 (name lingo_editors)
 (synopsis "TextMate and tree-sitter backends for lingo")
 (description
  "Renders a lingo grammar to a .tmLanguage.json TextMate grammar and to a tree-sitter grammar.js. Separate from the lingo package because yojson is its dependency and nothing else needs it.")
 (depends
  (ocaml
   (>= 5.0))
  lingo
  yojson
  (alcotest :with-test)
  (odoc :with-doc))
 ; Declared, not yet populated: Phase 6.
 (allow_empty))
