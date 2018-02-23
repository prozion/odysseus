#lang racket

(require "../../lib/load/all.rkt")
(require "../timeline.rkt")
(require "../../../denis_personal/my_memories/timeline.rkt")

(provide (all-defined-out))

(define (timeline-filter tl tag)
  (filter
    (λ (item)
      (and
        (> (length item) 2))
        (indexof? (third item) tag))
    tl))

(define (list-travels (output-file #f))
  (let* ((res (timeline-filter ds_timeline 't))
        (res (for/fold
                ((s ""))
                ((event res))
                (format "~a\"~a\" \"~a\"~n~n" s (first event) (second event)))))
    (if output-file
      (write-file output-file res)
      res)))

;(list-travels "test.txt")
;(list-travels)

(define (info)
  (printf "All memories count: ~a~nTravels: ~a~n"
          (length ds_timeline)
          (length (timeline-filter ds_timeline 't))))
          
(info)
