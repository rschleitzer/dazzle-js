;; DSSSL style sheet with construction rules
;; Tests process-root, element/root rules, and process-children

;; Root rule - processes the document root
(root
  (make entity
    system-id: "output.txt"
    (literal "=== Document Processing ===\n")
    (process-children)
    (literal "=== End ===\n")))

;; Element rule - processes <element> tags
(element element
  (sosofo-append
    (literal "Found element: ")
    (literal (gi (current-node)))
    (literal "\n")))

;; Start processing from root
(process-root)
