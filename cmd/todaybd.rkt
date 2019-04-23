#lang racket

(require racket/cmdline)

(require "../lib/_all.rkt")
(require "../graphics/console.rkt")
(require "people.rkt")
(require "../tabtree-format/tab-tree.rkt")
(require "../report/csv.rkt")

(define day #f)
(define total-count (make-parameter #f))
(define people (get-level-under "people" (parse-tab-tree "c:/denis/denis_core/denis_personal/my_knowledge/people.tree") 2))

(define (todaybd people (day #f))
  (let* ((curdate (if day day (current-date)))
        (cur-day-month (d.m curdate))
        (filtered-people (filter
                  (λ (p) (and
                            ($ bdate p)
                            (equal? cur-day-month (d.m ($ bdate p)))))
                  people)))
    (display (people->string
                filtered-people
                (list
                  (λ (x) (or (get-name-surname-str x) ($ nickname x) "?"))
                  (λ (x) ($ phone x))
                  (λ (x) (or ($ vk x) ($ fb x) ($ ok x) ($ email x))))))))

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
                    "day in the year for which you want to find the birthdays"
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
