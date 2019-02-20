#lang racket

(require racket/cmdline)
(require racket/file (for-syntax racket/file)) ;
(require compatibility/defmacro)

(require "../lib/_all.rkt");  (for-syntax "../lib/syntax.rkt" "../lib/seqs.rkt"))
(require "../report/report.rkt")
(require "../graphics/svg.rkt")

(require "../lib/debug.rkt")

;(define ody #f)
;(define output #f)

(define-namespace-anchor a) ; bind status-output to the loaded namespace

(define (build #:in ody-file #:out (output-file null))
  (parameterize ([current-namespace (namespace-anchor->namespace a)])
    (namespace-require (string->path (string-append (getenv "odysseus") "/report/report.rkt")))
    (namespace-require (string->path (string-append (getenv "odysseus") "/lib/_all.rkt")))
    (let* ([v (load ody-file)]
          [output-filename (if (null? output-file)
                              (path-replace-extension (file-name-from-path ody-file) (@. v.output-file-ext))
                              output-file)])
      ((@. v.save-file) #:file output-filename #:dir (path-only ody-file)))))

(command-line
  #:program "ody-><some graphics file> transpiler"
  #:multi
  ;  [("-o" "--output") output-filename
  ;      "output file, by default <input-filename>.<ext>"
  ;      (set! output output-filename)]
  [("-s" "--status")
                  "display the process progress"
                  (begin (debug-output #t))]
  #:args
    (input output)
    ;(if output-file
    ;  (build #:in ody-file #:out output-file)
    ;  (build #:in ody-file))
    (build #:in input #:out output)
)
