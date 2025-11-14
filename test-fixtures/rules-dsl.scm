;; Construction rules loaded via entity reference

(root
  (make entity
    system-id: "dsl-output.txt"
    (literal "=== DSL Template Test ===\n")
    (process-children)
    (literal "=== End ===\n")))

(element chapter
  (sosofo-append
    (literal "Chapter: ")
    (literal (gi (current-node)))
    (literal "\n")))

(element section
  (sosofo-append
    (literal "Section: ")
    (literal (gi (current-node)))
    (literal "\n")))
