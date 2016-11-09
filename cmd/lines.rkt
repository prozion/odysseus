#lang racket

(require racket/cmdline)
(require racket/file)

(require "../utils/misc.rkt")

;; count lines of code in the project

(define (count-lines pars f)
  (let ([ext (parameters-ext pars)]
        [iline (parameters-iline pars)])
    (cond
      ((file-exists? f)
        (if ext
          (if (regexp-match (pregexp (string-append ".*?\\." ext "$")) (path->string f)) ; STX regexp-match
            (count-lines-file pars f)
            0)
          (count-lines-file pars f)))
      ((directory-exists? f)
        (apply + (map (curry count-lines pars) (directory-list f #:build? #t))))
      (else 0))))

(define (count-lines-file pars f)
  (let ([iline-preg (parameters-iline pars)])
    (cond
      (iline-preg (count-lines-file-except f iline-preg))
      (else
        (length (file->lines f))))))

(define (count-lines-file-except f preg)
  (length
    (clean
      (curry regexp-match (pregexp preg))
      (file->lines f))))

(struct parameters (ext iline) #:mutable);; STX: add 'struct'

(define params (parameters #f #f))

(define (print-pars p)
  (printf "~a ~a ~n" (parameters-ext p) (parameters-iline p)))

(command-line
  #:program "lines"
  #:multi
    [("-e" "--ext") ext
                    "pregexp of file extension. Matched will be counted"
                    (set-parameters-ext! params ext)]
    [("-i" "--ignore-lines") iline
                    "pregexp for lines to ignore in counting. For example '^\\s*;+.*' will ignore commented lines"
                    ;; "(^\s*$|^\s*;+.*$)" - ignore blank and commented lines
                    (set-parameters-iline! params iline)]
  #:args
    (dir)
    (printf "Total lines: ~a" (count-lines params (string->path dir)))
)
