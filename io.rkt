#lang racket

(require compatibility/defmacro)
(require "hash.rkt")
(require "base.rkt")
(require "seqs.rkt")
(require "debug.rkt")
(require "regexp.rkt")
(require racket/serialize)

(provide (all-defined-out))

(define debug
  (lambda args (apply string-append
    (map
      (lambda (el)
        (cond
          ((number? el) (number->string el))
          ((list? el) (list->string el)) ; list of chars to string
          (else el)))
      args))))

; (define (write-data-to-file filename v)
;   (write-to-file v filename #:exists 'replace))
;
; (define var->file write-data-to-file)

(define (write-file filename v)
  (display-to-file v filename #:exists 'replace))

(define (append-file filename v)
  (display-lines-to-file v filename #:exists 'append))

(define (read-file filename)
  (file->string filename #:mode 'text))

(define (read-lines filename)
  (file->lines filename #:mode 'text #:line-mode 'linefeed))

(define (read-file-by-lines filename)
  (let ((ff (open-input-file filename #:mode 'text)))
    (local ((define (read-line-iter res line)
              ; (sleep 0.003)
              ; (--- line)
              (if (eof-object? line)
                res
                (read-line-iter (pushr res line) (read-line ff 'any)))))
        (read-line-iter (list) (read-line ff 'any)))))

(define (write-file-by-lines filename alist)
  (write-file filename (implode alist "\n")))

(define (write-file-to-dir #:file file #:dir dir v)
  ; first try thinking that dir is relative, then absolute path, if both are not directories then write to current-directory
  (let* (
          [dir-parts (split dir "/")]
          ; find and create non-existing directories
          (_ (let loop ((existed empty) (nonexisted dir-parts))
                                  (cond ((empty? nonexisted) 'end)
                                        (else
                                          (let ((next-directory (build-path (current-directory) (implode (pushr existed (car nonexisted)) "/"))))
                                            (if (directory-exists? next-directory)
                                                (loop (pushr existed (car nonexisted)) (cdr nonexisted))
                                                (begin
                                                  (make-directory next-directory)
                                                  (loop (pushr existed (car nonexisted)) (cdr nonexisted)))))))))
          [path (build-path (current-directory) dir)])
  (write-file
    (build-path path file)
    v)))

; read data from file into the s-expression and evaluate it
(define (load-data-from-file filepath namespace)
  (if (file-exists? filepath)
    (parameterize ([current-namespace namespace])
      (load filepath))
    #f))

; read data from file into the s-expression
(define (read-data-from-file filepath namespace)
  (if (file-exists? filepath)
    (read (open-input-string (read-file filepath)))
    #f))

; serializes data and save it into the file
(define (write-data-to-file data filepath)
  (write-to-file (serialize data) filepath #:exists 'replace))

; read serialized data and converts it to a normal value
(define (read-serialized-data-from-file filepath)
  (if (file-exists? filepath)
    (deserialize (file->value filepath))
    #f))

;;; shorthands
(define-syntax (--- stx)
  (syntax-case stx ()
    ((_ parameters ...)
      (with-syntax ((frmt #'(for/fold
                              ((s "~n"))
                              ((i (reverse (list parameters ...))))
                              (string-append "~a " s))))
        #'(if (debug-output)
            (append-file (debug-output) (format frmt parameters ...))
            (printf frmt parameters ...)
            )))))

(define (print-list lst)
	(for ((i lst))
    (if (debug-output)
      (append-file (debug-output) i)
	    (println i))))

(define (---- obj)
  (cond
    ((list? obj) (print-list obj))
    ((hash? obj) (print-list (for/list (((k v) obj)) (cons k v))))
    (else obj)))

(define-macro (terpri)
	`(--- ""))
