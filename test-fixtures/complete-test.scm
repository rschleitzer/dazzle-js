; Complete test - all major features

; Basic primitives
(+ 10 20 30)

; String operations  
(string-append "Dazzle " "is " "working!")

; Lists
(length (list 1 2 3 4 5))

; Lambda functions
((lambda (x y) (+ (* x x) (* y y))) 3 4)

; Higher-order: map with lambda
(map (lambda (x) (* x 2)) (list 1 2 3 4 5))

; Higher-order: for-each (returns nil)
(for-each (lambda (x) (* x x)) (list 1 2 3))

; Higher-order: apply
(apply + (list 1 2 3 4 5))
