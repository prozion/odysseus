#lang racket

(module+ test

  (require rackunit)
  (require "../svg.rkt")
  (require "../../utils/all.rkt")

  (check-equal?
    (svg)
    (rtrim ; remove \r at the end of the string
    #<<svg
<svg></svg>
svg
  ))

  (check-equal?
    (svg (@ 'xmlns #t))
    (rtrim
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg"></svg>
svg
  ))

  (check-equal?
    (svg (@ 'xmlns #t 'xlink #t))
    (rtrim
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"></svg>
svg
  ))

  (check-equal? (g) "<g></g>")
  (check-equal? (g (g)) "<g><g></g></g>")
  (check-equal? (g (@ 'id "id0") (g)) "<g id=\"id0\"><g></g></g>")

  (check-equal? (g (rect x 10)) "<g><rect x=\"10\" /></g>")
  (check-equal? (string-length (g (rect x 10 y 10 width 100 height 100))) (string-length "<g><rect x=\"10\" y=\"10\" width=\"100\" height=\"100\" /></g>"))

  ; simple case
  (check-equal?
    (string-length
      (svg (@)
        (g
          (rect x 10 y 10 width 100 height 150 fill "red" data-comment "rect in the simple case"))))
    (string-length (rtrim
    #<<svg
<svg><g><rect x="10" y="10" width="100" height="150" fill="red" data-comment="rect in the simple case" /></g></svg>
svg
  )))

  ;; more complex case
  (check-equal?
    (string-length
      (svg (@ 'xmlns #t)
        (g)
        (g (@ 'id "group1")
          (rect x 10 y 10 width 100 height 150 fill "green")
          (g (g (@ 'id "subgroup1") (g (rect x 100 y 400 width 200 height 300)))))))
    (string-length (rtrim
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg"><g></g><g id="group1"><rect x="10" y="10" width="100" height="150" fill="green" /><g><g id="subgroup1"><g><rect x="100" y="400" width="200" height="300" /></g></g></g></g></svg>
svg
  )))
)
