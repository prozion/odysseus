#lang racket

(require racket/cmdline)

(require (file "c:/denis/denis_core/denis_personal/my_memories/timeline.rkt"))
(require "../lib/load/all.rkt")
(require "../graphics/console.rkt")
(require "../reports/csv.rkt")

(define ns (module->namespace (string->path "c:/denis/denis_core/denis_personal/my_memories/timeline.rkt")))

(define day-to-check #f)

(define (day-in-history events (day-to-check #f))
  (let* ((cur-dm (if day-to-check day-to-check (d.m (current-date))))
        (filtered-events (filter
                            (λ (p) (equal? cur-dm (d.m (first p))))
                            events)))
    (events-output filtered-events)))

(define (events-output filtered-events)
  (for ((e filtered-events))
    (displayln (format "~a: ~a" (first e) (second e)))))

;>ppl -q "(city=? \"Мурманск\")"
;>ppl -q "(city=? \"Мурманск\")" -f "phone,email"

(command-line
  #:program "dayhistory"
  #:multi
    [("-d" "--day") d
                    "day in the year for which find the events"
										(set! day-to-check d)]
  #:args
    ()
    (cond
      (day (day-in-history ds_timeline day-to-check))
      (else (day-in-history ds_timeline))))
