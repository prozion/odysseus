#lang racket

(require racket/cmdline)

(require (file "c:/denis/denis_core/denis_personal/my_memories/timeline.rkt"))
(require "../lib/load/all.rkt")
(require "../graphics/console.rkt")
(require "../reports/csv.rkt")
(require "../kb/tab-tree.rkt")

; (---
;     (parse-tab-tree "c:/denis/denis_core/odysseus-knowledge/personal/timeline.tree"))

(define day-to-check #f)

(define ds_timeline (parse-tab-tree "c:/denis/denis_core/odysseus-knowledge/personal/timeline.tree"))

; (define ds_timeline
;                     ; (benchmark
;                       (filter
;                         (λ (x) (precise-date? ($ id x)))
;                           ; (benchmark
;                             ; (hash-keys (car (hash-values
;                             ($$$ timeline
;                               ; (benchmark
;                                 (parse-tab-tree "c:/denis/denis_core/odysseus-knowledge/personal/timeline.tree"))))
;                                 ; "parse-tab-tree"))))
;                             ; "planarize"))))))
;                     ; "ds_timeline initialization"))


(define-catch (day-in-history events (day-to-check #f))
  (--- events))
  ; (let* ((cur-dm (if day-to-check day-to-check (d.m (current-date))))
  ;       (filtered-events (filter
  ;                           ; (λ (p) (equal? cur-dm (d.m (first p))))
  ;                           (λ (p) (equal? cur-dm (d.m ($ id p))))
  ;                           events)))
  ;   (events-output filtered-events)))

(define-catch (events-output filtered-events)
  (for ((e filtered-events))
    (displayln (format "~a: ~a" ($ id e) ($ d e)))))

(command-line
  #:program "dayhistory"
  #:multi
    [("-d" "--day") d
                    "day in the year for which find the events"
										(set! day-to-check d)]
  #:args
    ()
    (--- ds_timeline))
    ; (cond
    ;   (day (day-in-history ds_timeline day-to-check))
    ;   (else (day-in-history ds_timeline))))
