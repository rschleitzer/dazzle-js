;; Helper functions for DSSSL processing

(define (element-name)
  (gi (current-node)))

(define (wrap-text prefix suffix)
  (sosofo-append
    (literal prefix)
    (literal (element-name))
    (literal suffix)))
