#lang racket

(require racket/cmdline)

; (require (file "c:/denis/denis_core/denis_personal/my_memories/timeline.rkt"))
(require "../lib/_all.rkt")
(require "../graphics/console.rkt")
(require "../report/csv.rkt")
(require "../tabtree/tab-tree.rkt")

(define timeline-cache "d:/denis/data/timeline/cache/timeline.txt" )

; (write-data-to-file
;     (parse-tab-tree "c:/denis/denis_core/denis_personal/my_memories/timeline.tree")
;     timeline-cache)

(define timeline ($3 timeline (read-serialized-data-from-file timeline-cache)))

(define day-to-check #f)
;
; (define ds_timeline
;                     ; (benchmark
;                       (filter
;                         (λ (x) (precise-date? ($ id x)))
;                           ; (benchmark
;                             ; (hash-keys (car (hash-values
;                             ($3 timeline
;                               ; (benchmark
;                                 (parse-tab-tree "c:/denis/denis_core/denis_knowledge/personal/timeline.tree"))))
;                                 ; "parse-tab-tree"))))
;                             ; "planarize"))))))
;                     ; "ds_timeline initialization"))
;
;
(define-catch (day-in-history events (day-to-check #f))
  (let* ((cur-dm (if day-to-check day-to-check (d.m (current-date))))
        (filtered-events (filter
                            ; (λ (p) (equal? cur-dm (d.m (first p))))
                            (λ (p)
                                  ; (--- ($ id p) cur-dm (d.m ($ id p)))
                                  (equal? cur-dm (d.m ($ id p))))
                            events)))
    (events-output filtered-events)))

(define-catch (events-output filtered-events)
  (for ((e filtered-events))
    (displayln (format "~a: ~a" ($ id e) ($ d e)))))
;
(command-line
  #:program "dayhistory"
  #:multi
    [("-d" "--day") d
                    "day in the year for which find the events"
										(set! day-to-check d)]
  #:args
    ()
    (cond
      (day (day-in-history timeline day-to-check))
      (else (day-in-history timeline))))
