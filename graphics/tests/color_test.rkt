#lang racket

(module+ test

  (require rackunit)
  (require "../color.rkt")

  ;(check-true? (string? (rand-color)))
  (check-equal? (string-length (rand-color)) 7)

  (check-equal? (rgb->hsv 0 0 0) '(0 0 0))
  (check-equal? (string-rgb->hsv "#000000") '(0 0 0))
  (check-equal? (rgb->hsl 0 0 0) '(0 0 0))
  (check-equal? (string-rgb->hsl "#000000") '(0 0 0))

  (check-equal? (rgb->hsv 255 255 255) '(0 0 100))
  (check-equal? (string-rgb->hsv "#ffffff") '(0 0 100))
  (check-equal? (rgb->hsl 255 255 255) '(0 0 100))
  (check-equal? (string-rgb->hsl "#ffffff") '(0 0 100))

  (check-equal? (rgb->hsv 165 109 190) '(281 43 75))
  (check-equal? (string-rgb->hsv "#a56dbe") '(281 43 75))
  (check-equal? (string-rgb->hsv "#a56DBE") '(281 43 75))
  (check-equal? (rgb->hsl 165 109 190) '(281 38 59))
  (check-equal? (string-rgb->hsl "#a56dbe") '(281 38 59))

  (check-equal? (hsv->rgb 0 0 0) '(0 0 0))
  (check-equal? (hsl->rgb 0 0 0) '(0 0 0))

  (check-equal? (hsv->rgb 0 0 100) '(255 255 255))
  (check-equal? (hsl->rgb 0 0 100) '(255 255 255))

  (check-equal? (hsv->rgb 0 100 0) '(0 0 0))
  (check-equal? (hsl->rgb 0 100 0) '(0 0 0))

  (check-equal? (hsv->rgb 0 100 100) '(255 0 0))
  (check-equal? (hsl->rgb 0 100 100) '(255 255 255))

  (check-equal? (hsv->rgb 360 0 100) '(255 255 255))
  (check-equal? (hsl->rgb 360 100 100) '(255 255 255))

  (check-equal? (hsv->rgb 253 45 84) '(139 118 214))
  (check-equal? (hsl->rgb 253 45 84) '(204 196 233))

  ;; various values
  (check-equal? (rgb->hsv 38 14 255) '(246 95 100))
  (check-equal? (rgb->hsv 150 0 71) '(332 100 59))
  (check-equal? (rgb->hsv 150 150 70) '(60 53 59))
  (check-equal? (rgb->hsv 38 113 205) '(213 81 80))
  (check-equal? (rgb->hsv 0 255 0) '(120 100 100))
  (check-equal? (rgb->hsv 0 255 5) '(121 100 100))
  (check-equal? (rgb->hsv 146 172 156) '(143 15 67))
  (check-equal? (rgb->hsv 56 33 48) '(321 41 22))
  (check-equal? (rgb->hsv 246 243 247) '(285 2 97))

  (check-equal? (rgb->hsl 150 0 71) '(332 100 29))
  (check-equal? (rgb->hsl 0 255 0) '(120 100 50))
  (check-equal? (rgb->hsl 146 172 156) '(143 14 62))
  (check-equal? (rgb->hsl 56 33 48) '(321 26 17))
  (check-equal? (rgb->hsl 246 243 247) '(285 20 96))

  (check-equal? (hsl->rgb 152 20 69) '(160 192 177))
  (check-equal? (hsl->rgb 11 96 97) '(255 243 240))

  (check-equal? (hsv->rgb 343 94 5) '(13 1 4))
  (check-equal? (hsv->rgb 322 4 95) '(242 233 239))
)
