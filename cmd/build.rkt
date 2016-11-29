#lang racket

(require racket/cmdline)
(require racket/file (for-syntax racket/file)) ; STX for-syntax, all-defined-out, all-from-out (as lib functions?)
(require compatibility/defmacro)

(require "../utils/all.rkt");  (for-syntax "../utils/syntax.rkt" "../utils/seqs.rkt"))
(require "../widgets/all.rkt")
(require "../graphics/svg.rkt")

(require "../utils/debug.rkt")

(struct parameters (ods output) #:mutable)
(define params (parameters #f #f))

(define (build #:in ods-file #:out (output-file null))
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require "../widgets/all.rkt")
    (namespace-require "../utils/all.rkt")
    (namespace-require "../utils/debug.rkt")
    (let* ([v (load ods-file)]
          [output-filename (if (null? output-file)
                              (path-replace-extension (file-name-from-path ods-file) (@. v.output-file-ext))
                              output-file)])
      ((@. v.save-file) #:file output-filename #:dir (path-only ods-file)))))

(command-line
  #:program "ods-><some graphics file> transpiler"
  #:multi
    [("-o" "--output") output-filename
        "output file, by default <input-filename>.<ext>"
        (set-parameters-output! params output-filename)]
  #:args
    (ods-file)
    (begin
      (set-parameters-ods! params ods-file)
      (let ([output-file (parameters-output params)])
        (if output-file
          (build #:in ods-file #:out output-file)
          (build #:in ods-file))))
)
