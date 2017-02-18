#lang racket

(require racket/cmdline)

(require (file "c:/denis/denis_core/denis_personal/my_people/all.rkt"))
(require "../lib/all.rkt")
(require "../pmf/people.rkt")
(require "../pmf/people_verify.rkt")
(require "../graphics/console.rkt")
(require "../reports/csv.rkt")

(define ns (module->namespace (string->path "c:/denis/denis_core/denis_personal/my_people/all.rkt")))

(define query "")
(define person "")
(define verifyf null)
(define fields "sn")
(define only-fields #f)
(define info null)
(define csvfile "")

(define (has-all-fields fields)
  (λ (p)
    (andmap
      (λ (f) (indexof? (hash-keys p) (string->symbol f)))
      fields)))

(define (filter-query people query fields only-fields?)
  (filter query
    (if only-fields?
      (filter (has-all-fields fields) people)
      people)))

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
  #:program "ppl"
  #:multi
    [("-q" "--query") q
                    "query to filter"
										(set! query q)]
    [("-p" "--person") p
                    "search by person's name and surname"
										(set! person p)]
    [("-v" "--verify") v
                    "verify uniqueness for values in specified field"
										(set! verifyf v)]
    [("-i" "--info") i
                    "various information on contacts database. Options: total|face2face - numbers of person records"
										(set! info i)]
    [("-f" "--fields") f
                    "fields to show"
										(set! fields f)]
    [("-F" "--only-with-fields") F
                    "show only those records that have specified fields"
										(begin
                      (set! fields F)
                      (set! only-fields #t))]
    [("-o" "--output") o
                    "redirect output to csv file in the current directory"
                    (begin
                      (set! csvfile o)
                      (console-output #f))]
  #:args
    ()
    (cond
      ((not (nil? verifyf)) (displayln (check-all-duplicates (string->symbol verifyf) people)))
      ((not (nil? info))
            (newline)
            (set-text-color 'green)
            (displayln
              (format
                " ~a"
                (cond
                  ((equal? info "total") (length people))
                  ((equal? info "face2face") (acqs people))
                  (else "?"))))
            (set-text-color 'default))
      ((not (nil? person))
        (ppl-output
          (split fields ",")
          (filter-query
            people
            (by-name-surname person)
            (split fields ",")
            only-fields)
          csvfile))
      (else
        (ppl-output
          (split fields ",")
          (filter-query
            people
            (eval (read (open-input-string query)) ns)
            (split fields ",")
            only-fields)
          csvfile)))
)
