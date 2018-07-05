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

(define (read-data-from-file filepath namespace-anchor)
  (if (file-exists? filepath)
    (parameterize ([current-namespace (namespace-anchor->namespace namespace-anchor)])
      (load filepath))
    #f))

; serializes data and save it into the file
(define (write-data-to-file data filepath)
  (write-to-file (serialize data) filepath #:exists 'replace))

; read serialized data and converts it to a normal value
(define (read-serialized-data-from-file filepath)
  (deserialize (file->value filepath)))
