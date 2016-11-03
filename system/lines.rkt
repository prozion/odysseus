#lang racket

;; count lines of code in the project

(let* (
  ;; read from command keys
  ;; --dir
  [startdir ...]

  ;; --ext
  [ext (pregexp ...)]

  ;; --nocomments
  [nocomments #f])
    (print-res (count-lines startdir)))

(define (count-lines dir)
  (cond
    ((file? dir) (count-lines-file dir))
    ((dir? dir) (apply + (map count-lines (files dir))))
    (else 0)))

(define (count-lines-file f)
  (countlines (read-file f)))
