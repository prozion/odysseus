#lang racket

(require racket/cmdline)

(require (file "c:/denis/denis_core/denis_personal/my_people/all.rkt"))
(require "../lib/load/all.rkt")
(require "../pis/people.rkt")
(require "../pis/people_verify.rkt")
(require "../graphics/console.rkt")
(require "../reports/csv.rkt")

(define ns (module->namespace (string->path "c:/denis/denis_core/denis_personal/my_people/all.rkt")))

(define query "")
(define verifyf null)
(define fields "sn")
(define person #f)
(define tags #f)
(define only-fields #f)
(define info null)
(define csvfile "")

(define (has-all-fields fields)
  (λ (p)
    (andmap
      (λ (f) (indexof? (hash-keys p) (string->symbol f)))
      fields)))

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
    [("-p" "--person") n
                    "regexp for person name/surname"
										(set! person n)]
    [("-v" "--verify") v
                    "verify uniqueness for values in specified field"
										(set! verifyf v)]
    [("-i" "--info") i
                    "various information on contacts database. Options: total|face2face - numbers of person records"
										(set! info i)]
    [("-f" "--fields") f
                    "fields to show"
										(set! fields f)]
    [("-t" "--tags") t
                    "fields to show"
										(set! tags t)]
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
      (else
        (let* ((people people)
              (fields (if fields (split fields ",") empty)))
          (when only-fields
            (set! people (filter (has-all-fields fields) people)))
          (when person
            (set! people (filter (λ (p) (re-matches? person (person-signature p))) people)))
          (when tags
            (set! people (filter (λ (p) (indexof? (explode tags) (hash-ref p 'tags #f))) people)))
          (ppl-output fields people csvfile))))
)
