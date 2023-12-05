#lang racket

(require racket/file)
(require racket/serialize)
(require compatibility/defmacro)
(require "base.rkt")
(require "debug.rkt")
(require "hash.rkt")
(require "regexp.rkt")
(require "list.rkt")
(require "string.rkt")
(require "type.rkt")

(provide (all-defined-out))

(define-catch (absolute-path? astr)
  (re-matches? "^(/|[A-Z]:)" astr))

(define (write-file filename v)
  (display-to-file v filename #:exists 'replace))

(define (append-file filename v)
  (display-to-file v filename #:exists 'append))

(define (read-file filename)
  (file->string filename #:mode 'text))

; FIXME: remove if it is the same function as read-file-by-lines
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
          [dir-parts (string-split dir "/")]
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
; FIXME: What is the use of it? Maybe there is the same racket core function?
(define (read-data-from-file filepath namespace)
  (if (file-exists? filepath)
    (read (open-input-string (read-file filepath)))
    #f))

; serializes data and save it into the file
(define (write-data-to-file data filepath)
  (write-to-file (serialize data) filepath #:exists 'replace))

; read serialized data and converts it to a normal value
(define-catch (read-serialized-data-from-file filepath (value-if-failed #f))
  (if (file-exists? filepath)
    (deserialize (file->value filepath))
    value-if-failed))

(define (get-path filepath)
  (let ((leading-slash? (equal? (string-first filepath) "/")))
    (str
      (if leading-slash? "/" "")
      (string-join
        (butlast (string-split filepath "/"))
        "/"))))

(define (get-filename filepath)
  (last (string-split filepath "/")))
