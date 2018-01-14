#lang racket

(require compatibility/defmacro)
(require "hash.rkt")
(require "base.rkt")
(require "seqs.rkt")
(require "debug.rkt")
(require "regexp.rkt")

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

(define (write-data-to-file filename v)
  (write-to-file v filename #:exists 'replace))

(define var->file write-data-to-file)

(define (write-file filename v)
  (display-to-file v filename #:exists 'replace))

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

(define (write-file-to-dir #:file file #:dir dir v)
  ; first try thinking that dir is relative, then absolute path, if both are not directories then write to current-directory
  (let* ( [path (if dir
                    (build-path (current-directory) dir)
                    (current-directory))]
          [path (if (directory-exists? path) path (build-path dir))]
          [path (if (directory-exists? path) path (current-directory))])
  (write-file
    (build-path path file)
    v)))

(define (read-data-from-file filepath namespace-anchor)
  (if (file-exists? filepath)
    (parameterize ([current-namespace (namespace-anchor->namespace namespace-anchor)])
      (load filepath))
    #f))
