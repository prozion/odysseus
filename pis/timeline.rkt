#lang racket

(require "../lib/all.rkt")

(provide (all-defined-out))

(define BDATE "28.08.1979")
(define REFDATE "01.06.1985")

;; (relative-hour-duration "01.06.1985" "11.05.2017" ) -> 4.89 [1 hour in 1985 is equal to 4.89 in 2017]
(define (relative-hour-duration t1 t2 (t0 BDATE))
  (/ (date-diff t2 t0) (date-diff t1 t0) 1.0))

; effective years passed since the reference date
(define (effective-years t (tr REFDATE))
  (let ((dr (date-diff tr BDATE))
        (n (date-diff t tr))) ; number of iterations
    (/
      (sum-seq
        (λ (k) (/ dr (+ k dr))) ; increment
        n
        dr) ; start sum value
      365.0)))

; TODO: extract travel memories:
(define (extract-by-tags timeline tags)
  (for (((k v) timeline))
    (println (last v))))

(define-syntax (@@ stx)
  (syntax-case stx ()
    ((_ item ...) #'(list (@@-parse-item item) ...))
    (else #'#f)))

(define-syntax (@@-parse-item stx)
  (syntax-case stx ()
    ((_ (date description)) #'(list date description (list)))
    ((_ (date description tag))
        #'(list date description (symbol->list tag)))
    (else #'(list))))
