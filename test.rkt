#lang racket

(require "scrap/file.rkt")
(require "utils/seqs.rkt")

;(write-file "scrap/test.txt" "a;b\n100;200")
;(print (nth (read-csv-file "scrap/test.txt") 1))

;(print (slice (read-file "examples/dna/e.coli_0157.gbk") 2 12))

;(print (substring (read-file "examples/dna/e.coli_0157.gbk") 2 12))

(define gene-block (pregexp "(?sm:CDS.*?gene)"));(?=^\\s*gene))"))

(print (regexp-match gene-block (read-file "examples/dna/e.coli_0157.gbk")))
