#lang racket

(require racket/cmdline)

(require (file "c:/denis/denis_core/denis_personal/my_people/all.rkt"))
(require "../lib/all.rkt")
(require "../pmf/all.rkt")
(require "../pmf/people_verify.rkt")
(require "../graphics/console.rkt")

(define ns (module->namespace (string->path "c:/denis/denis_core/denis_personal/my_people/all.rkt")))

(define query "")
(define verifyf null)
(define fields "phone,email,sn")
(define info null)

(define (filter-query people query fields)
  (implode
    (map
      (λ (p) (person->string p query fields))
      (filter query people))
    "\n"))

;>ppl -q "(city=? \"Мурманск\")"
;>ppl -q "(city=? \"Мурманск\")" -p phone,email

(command-line
  #:program "ppl"
  #:multi
    [("-q" "--query") q
                    "query to filter"
										(set! query q)]
    [("-v" "--verify") v
                    "verify uniqueness for values in specified field"
										(set! verifyf v)]
    [("-i" "--info") i
                    "various information on contacts database. Options: total|offline - numbers of person records"
										(set! info i)]
    [("-f" "--fields") f
                    "fields to show"
										(set! fields f)]
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
                  ((equal? info "offline") (acqs people))
                  (else "?"))))
            (set-text-color 'default))
      (else
        (newline)
        (display (filter-query people (eval (read (open-input-string query)) ns) (split fields ",")))
        (void)))
)
