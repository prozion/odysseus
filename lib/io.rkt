#lang racket

(provide (all-defined-out))

(require compatibility/defmacro)

(define debug
  (lambda args (apply string-append
    (map
      (lambda (el)
        (cond
          ((number? el) (number->string el))
          ((list? el) (list->string el)) ; list of chars to string
          (else el)))
      args))))

(define (write-file filename v)
  (display-to-file v filename #:exists 'replace))

(define (read-file filename)
  (file->string filename #:mode 'text))

(define (write-file-to-dir #:file file #:dir dir v)
  ; first try thinking that dir is relative, then absolute path, if both are not directories then write to current-directory
  (let* ( [path (build-path (current-directory) dir)]
          [path (if (directory-exists? path) path (build-path dir))]
          [path (if (directory-exists? path) path (current-directory))])
  (write-file
    (build-path path file)
    v)))

(define (printc color text)
  ;; kernel32.dll SetConsoleTextAttribute
  #t
)
