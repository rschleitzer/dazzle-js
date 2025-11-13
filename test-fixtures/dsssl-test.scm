; DSSSL Processing test - Basic flow object construction

; Test 1: Empty sosofo
(empty-sosofo)

; Test 2: Sosofo append
(sosofo-append (empty-sosofo) (empty-sosofo))

; Test 3: Make entity flow object
(make entity system-id: "output.txt")

; Test 4: Make formatting-instruction flow object
(make formatting-instruction data: "Hello, DSSSL!")
