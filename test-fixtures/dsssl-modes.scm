;; DSSSL style sheet with modes - multi-pass processing
;; Tests mode declaration and mode switching

;; Root rule - processes document in default mode
(root
  (make entity
    system-id: "output-modes.txt"
    (literal "=== First Pass (default mode) ===\n")
    (process-children)
    (literal "\n=== Second Pass (toc mode) ===\n")
    (process-children 'toc)
    (literal "\n=== End ===\n")))

;; Default mode rules
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

;; TOC mode - different processing for same elements
(mode toc
  (element chapter
    (sosofo-append
      (literal "TOC: Chapter\n")))

  (element section
    (sosofo-append
      (literal "TOC: Section\n"))))

;; Start processing
(process-root)
