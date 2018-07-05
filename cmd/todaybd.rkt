#lang racket

(require racket/cmdline)

(require (file "c:/denis/denis_core/denis_personal/my_people/all.rkt"))
(require "../lib/load/all.rkt")
(require "people.rkt")
(require "../graphics/console.rkt")
(require "../report/csv.rkt")

(define ns (module->namespace (string->path "c:/denis/denis_core/denis_personal/my_people/all.rkt")))

(define day #f)
(define total-count (make-parameter #f))

(define (todaybd people (day #f))
  (let* ((curdate (if day day (current-date)))
        (cur-day-month (d.m curdate))
        (filtered-people (filter
                  (λ (p) (let ((bd (hash-ref p 'bdate #f)))
                            (if bd
                                (equal? cur-day-month (d.m bd))
                                #f)))
                  people)))
    (ppl-output '("bdate" "phone" "sn") filtered-people)))

(define (count-bd people)
  (display
    (str
      (string-text-color 'green)
      (length (filter (λ (p) (hash-ref p 'bdate #f)) people))
      (string-text-color 'default))))

(define (ppl-output fields people-sublist (csvfile ""))
  (if (nil? csvfile)
    (display (people->string people-sublist fields))
    (write-csv-file
      (merge
        (list 'surname 'name)
        (map string->symbol fields))
      people-sublist
      csvfile)))

;>ppl -q "(city=? \"Мурманск\")"
;>ppl -q "(city=? \"Мурманск\")" -f "phone,email"

(command-line
  #:program "todaybd"
  #:multi
    [("-d" "--day") d
                    "day in the year for which find the birthdays"
										(set! day d)]
    [("-c" "--total-count")
                    "total amount of persons with birthdays in database"
										(total-count #t)]
  #:args
    ()
    (cond
      (day (todaybd people day))
      ((total-count) (count-bd people))
      (else (todaybd people))))
