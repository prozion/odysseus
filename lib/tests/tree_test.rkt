#lang racket

(module+ test

  (require rackunit)
  (require "../tree.rkt")
  (require "../debug.rkt")
  ; (require "../checks.rkt")

  (check-equal?
    (tree-filter
      (λ (el) (>= el 10))
      '(1 2 (3 (4 (5) 10 (11 3 12) (()) )) 80))
    '(((() 10 (11 12) (()))) 80))

  (check-equal?
    (tree-filter
      list?
      '(1 2 (3 (4 (5) 10 (11 3 12) (()) )) 80))
    '(((()()(())))))

  (check-equal?
    (hash-tree-flatten-with-paths (hash 1 (hash 'a (list (hash 'aa 10) (hash 'ab 20)) 'b (hash 'bb 30))))
    (list
      (hash 'aa 10 '_path (list 1 'a))
      (hash 'ab 20 '_path (list 1 'a))
      (hash 'bb 30 '_path (list 1 'b))))

  (check-equal?
    (hash-tree-flatten-with-paths (hash 1 (hash 'a (list (hash 'aa 10) (hash 'ab 20)) 'b (hash 'bb 30)) 2 (hash 3 (hash 4 (hash 'c 70)))))
    (list
      (hash 'aa 10 '_path (list 1 'a))
      (hash 'ab 20 '_path (list 1 'a))
      (hash 'bb 30 '_path (list 1 'b))
      (hash 'c 70 '_path (list 2 3 4))))

  (check-equal?
    (hash-tree-flatten-with-paths
      (hash 1 (list (hash 'a 10))
            2 (list (hash 'b 20) (hash 'c 30))))
    (list
      (hash 'a 10 '_path (list 1))
      (hash 'b 20 '_path (list 2))
      (hash 'c 30 '_path (list 2))))
)

; #hash(
;   (education . (list
;                   #hash((category . "learning") (interval . "09.1996-06.1997") (name . "Учёба на Физтехе"))
;                   #hash((category . "learning") (interval . "09.1997-04.2003") (name . "Учёба в МИЭТе"))))
;   (project . (list
;                   #hash((category . "knowledge") (interval . "08.2007-02.2008") (name . "PEZANIKI"))
;                   #hash((category . "activism") (interval . "06.2010-12.2010") (name . "Активный Никель")))))
