#lang racket

(require compatibility/defmacro)
(require "../lib/all.rkt")

(provide (all-defined-out))

(define (div-iter body res)
  (cond
    ((empty? body) res)
    (else (div-iter
            (cdr body)
            (format
              "~a~n<div>~a</div>~n"
              res
              (let ((item (car body)))
                (cond
                  ((list? item) (implode item " – "))
                  (else (str item)))))))))

(define (li-iter body res)
  (cond
    ((empty? body) res)
    (else (li-iter (cdr body) (format "~a~n<li>~a</li>~n" res (str (car body)))))))

(define (write-html-file filename title body)
  (let* ( (html-format (read-file "c:/denis/denis_core/odysseus/templates/base.fhtml"))
          (body (cond
                  ((string? body) body)
                  ((list2? body) (div-iter body ""))
                  ((list? body) (format "<ul>~a</ul>" (li-iter body "")))
                  (else "uknown format")))
          (res (format html-format title title body)))
  (display "\nready!")
  (write-file filename res)))
