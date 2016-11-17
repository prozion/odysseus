#lang racket

(require racket/cmdline)
(require racket/file (for-syntax racket/file)) ; STX for-syntax, all-defined-out, all-from-out (as lib functions?)
(require compatibility/defmacro)

(require "../utils/all.rkt");  (for-syntax "../utils/syntax.rkt" "../utils/seqs.rkt"))
(require "../widgets/all.rkt")
(require "../graphics/svg.rkt")

(struct parameters (ods svg) #:mutable)
(define params (parameters #f #f))

(define (build #:in ods-file #:out (svg-file #f))
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require "../widgets/all.rkt")
    (let ([v (load ods-file)]
          [svg-filename (if svg-file
                            svg-file
                            (path-replace-extension (file-name-from-path ods-file) #".svg"))])
      (write-file-to-dir
        #:file svg-filename
        #:dir (path-only ods-file)
        (svg xmlns xlink styles v)))))

(command-line
  #:program "ods->svg transpiler"
  #:multi
    [("-o" "--output") output-filename
        "output file, by default <input-filename>.svg"
        (set-parameters-svg! params output-filename)]
  #:args
    (ods-file)
    (begin
      (set-parameters-ods! params ods-file)
      (let ([svg-file (parameters-svg params)])
        (if svg-file
          (build #:in ods-file #:out svg-file)
          (build #:in ods-file))))
)
