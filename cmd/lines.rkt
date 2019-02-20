#lang racket

(require racket/cmdline)
(require racket/file)

;(require (file "c:/denis/denis_core/odysseus/lib/base.rkt"))
(require "../lib/_all.rkt")
(require "../graphics/console.rkt")

(provide count-only-real-code-lines)

;; count lines of code in the project

(struct parameters (ext iline exclude-filenames) #:mutable)

(define params (parameters #f #f #f))
(define directory #f)

(define (count-only-real-code-lines #:exclude-tests (exclude-tests #t))
  (count-lines
    (parameters "rkt" "^\\s*$|^\\s*;+.*$" (and exclude-tests "_test"))
    (string->path (getenv "odysseus"))))

(define (count-lines pars f)
  (let ([ext (parameters-ext pars)]
        [iline (parameters-iline pars)]
        [exclude-filenames (parameters-exclude-filenames pars)])
    (cond
      ((file-exists? f)
        (if ext
          (if (and
                (regexp-match (pregexp (string-append ".*?\\." ext "$")) (path->string f))
                (if exclude-filenames
                  (not (regexp-match (pregexp exclude-filenames) (path->string f)))
                  #t))
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

(define (print-pars p)
  (printf "~a ~a ~a ~n" (parameters-ext p) (parameters-iline p) (parameters-exclude-filenames p)))

(command-line
  #:program "lines"
  #:multi
    [("-e" "--ext") ext
                    "pregexp of file extension. Matched will be counted"
                    (set-parameters-ext! params ext)]
    [("-F" "--exclude-filenames") exclude-filenames
                    "pregexp for files to exclude from count"
                    (set-parameters-exclude-filenames! params exclude-filenames)]
    [("-i" "--ignore-lines") iline
                    "pregexp for lines to ignore in counting. For example '^\\s*;+.*' will ignore commented lines"
                    ;; "(^\s*$|^\s*;+.*$)" - ignore blank and commented lines
                    (set-parameters-iline! params iline)]
    [("-d" "--directory") dir
                    "directory"
                    ;; "(^\s*$|^\s*;+.*$)" - ignore blank and commented lines
                    (set! directory dir)]
  #:args
    ()
    (if directory
      (begin
        (newline)
        (set-text-color 'yellow)
        (display "Total lines: ")
        (set-text-color 'green)
        (displayln (count-lines params (string->path directory)))
        (set-text-color 'default))
      (begin ; for the case we 'require' this module
        (void)))
)
