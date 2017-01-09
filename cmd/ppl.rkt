#lang racket

(require racket/cmdline)
(require racket/file)

(require (file "c:/denis/denis_core/denis_personal/my_people/all.rkt"))
(require "../lib/all.rkt")
(require "../pmf/all.rkt")

(define ns (module->namespace (string->path "c:/denis/denis_core/denis_personal/my_people/all.rkt")))

(define query "")

(define (filter-query people f)
  (implode
    (map
      (λ (p) (print-person p))
      (filter f people))
    "\n"))

;>ppl -q "(city=? \"Мурманск\")"
;>ppl -q "(city=? \"Мурманск\")" -p phone,email TODO

(command-line
  #:program "ppl"
  #:once-any
    [("-q" "--query") q
                    "query to filter"
										(set! query q)]
  #:args
    ()
    (begin
      (newline)
      (filter-query people (eval (read (open-input-string query)) ns))
      (void))
)
