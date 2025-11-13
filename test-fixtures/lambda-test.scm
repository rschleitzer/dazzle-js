; Test lambda functions directly

; Define and call a simple lambda
((lambda (x) (* x x)) 5)

; Define and call a lambda with multiple args
((lambda (x y) (+ x y)) 3 7)

; Nested lambda
((lambda (x) ((lambda (y) (+ x y)) 10)) 5)
