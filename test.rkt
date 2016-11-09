#lang racket

(require "scrap/file.rkt")
(require "utils/seqs.rkt")

;(write-file "scrap/test.txt" "a;b\n100;200")
;(print (nth (read-csv-file "scrap/test.txt") 1))

;(print (slice (read-file "examples/dna/e.coli_0157.gbk") 2 12))

;(print (substring (read-file "examples/dna/e.coli_0157.gbk") 2 12))

;(define gene-block (pregexp "(?sm:CDS.*?gene)"));(?=^\\s*gene))"))
;
;(print (regexp-match gene-block (read-file "examples/dna/e.coli_0157.gbk")))

;(require "graphics/svg.rkt")
;
;(print
;  (svg
;    (svg-g (svg-g) (svg-foobar (svg-ract)))))

(require (for-syntax racket/syntax racket/string))

(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
      (let*
        ( [names/sym (map syntax-e (syntax->list #'(names ...)))]
          [names/str (map symbol->string names/sym)]
          [name/str (string-join names/str "-")]
          [name/sym (string->symbol name/str)])
            (with-syntax
              ([name (datum->syntax stx name/sym)])
              #'(define (name args ...) body0 body ...)))]))
