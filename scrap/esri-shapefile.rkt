#lang racket

(require "../../lib/_all.rkt")

(provide shapefile->points)

(define HEADER-SIZE 100)

(define shp
          (@
            'file-header
              (@
                'file-code (@ 'pos 0 'type 'integer 'mode 'big)
                'file-length (@ 'pos 24 'type 'integer 'mode 'big)
                'version (@ 'pos 28 'type 'integer 'mode 'little)
                'mode (@ 'pos 32 'type 'integer 'mode 'little)
                'xmin (@ 'pos 36 'type 'double 'mode 'little)
                'ymin (@ 'pos 44 'type 'double 'mode 'little)
                'xmax (@ 'pos 52 'type 'double 'mode 'little)
                'ymax (@ 'pos 60 'type 'double 'mode 'little)
           )

            'record-header
              (@
                'record-number (@ 'pos 0 'type 'integer 'mode 'big)
                'content-length (@ 'pos 4 'type 'integer 'mode 'big))

            'null-shape-record
              (@
                'shape-type (@ 'pos 0 'type 'integer 'mode 'little)) ; 0
            'point-record
              (@
                'shape-type (@ 'pos 0 'type 'integer 'mode 'little) ; 1
                'x (@ 'pos 4 'type 'double 'mode 'little)
                'y (@ 'pos 12 'type 'double 'mode 'little))
            'multipoint-record
              (@
                'shape-type (@ 'pos 0 'type 'integer 'mode 'little) ; 8
                'box (@ 'pos 4 'type 'double 'mode 'little 'rep 4)
                'numpoints (@ 'pos 36 'type 'integer 'mode 'little)
                'points (@ 'pos 40 'type 'point 'mode 'little 'rep 'numpoints))
            'polyline-record
              (@
                'shape-type (@ 'pos 0 'type 'integer 'mode 'little) ; 3
                'box (@ 'pos 4 'type 'double 'mode 'little 'rep 4)
                'numparts (@ 'pos 36 'type 'integer 'mode 'little)
                'numpoints (@ 'pos 40 'type 'integer 'mode 'little)
                'parts (@ 'pos 44 'type 'integer 'mode 'little 'rep 'numparts)
                'points (@ 'pos '44+4*numparts 'type 'point 'mode 'little 'char-mode 'little 'rep 'numpoints))
            'polygon-record
              (@
                'shape-type (@ 'pos 0 'type 'integer 'mode 'little) ; 5
                'box (@ 'pos 4 'type 'double 'mode 'little 'rep 4)
                'numparts (@ 'pos 36 'type 'integer 'mode 'little)
                'numpoints (@ 'pos 40 'type 'integer 'mode 'little)
                'parts (@ 'pos 44 'type 'integer 'mode 'little 'rep 'numparts)
                'points (@ 'pos '44+4*numparts 'type 'point 'mode 'little 'char-mode 'little 'rep 'numpoints))
))

; b1-16 -> pair
(define (bytes->point bstr #:char-mode (char-mode 'little) #:word-mode (word-mode 'little))
  (let* (
          (b1 (bytes-slice bstr 1 8))
          (b2 (bytes-slice bstr 9 16))
          (bs (map
                (λ (x) (bytes->double x #:char-mode char-mode #:word-mode word-mode))
                (list b1 b2))))
          (cons (first bs) (second bs))))

(define transform-fs (@ 'integer bytes->integer 'double bytes->double 'point bytes->point))
(define type-sizes (@ 'char 1 'integer 4 'double 8 'point 16))

;;;;;;;

(define (shapefile-parameter header section field #:type (default-type 'integer))
  (let* (
          (section (hash-ref shp section))
          (field (hash-ref section field))
          (pos (hash-ref field 'pos))
          (word-mode (hash-ref field 'mode 'little))
          (char-mode (hash-ref field 'char-mode 'little))
          (type (hash-ref field 'type default-type))
     )
    (case type
      ((integer) (bytes->integer (bytes-slice header (inc pos) (+ pos 4)) #:char-mode 'little #:word-mode word-mode))
      ((double) (bytes->double (bytes-slice header (inc pos) (+ pos 8)) #:char-mode char-mode #:word-mode word-mode))
      (else header))))

(define (read-next-field istream record-type field-name)
  (let* (
          (parameters (hash-ref (hash-ref shp record-type) field-name))
          (parameter-type (@. parameters.type))
          (f (hash-ref transform-fs parameter-type identity))
          (size (hash-ref type-sizes parameter-type 1))
          (word-mode (@. parameters.mode))
          (res
            (f (read-bytes size istream) #:word-mode word-mode #:char-mode 'little)))
    res))

(define (read-next-point-record istream)
  (let* ((record-header (read-bytes 8 istream))
        (content-length (shapefile-parameter record-header 'record-header 'content-length))
        (full-content-length (+ 8 (* 2 content-length)))
        (_ (read-next-field istream 'point-record 'shape-type))
        (x (read-next-field istream 'point-record 'x))
        (y (read-next-field istream 'point-record 'y)))
          (cons x y)))

(define (read-next-poly-record istream #:record-type (record-type 'polyline-record))
  (let* ((record-header (read-bytes 8 istream))
        (content-length (shapefile-parameter record-header 'record-header 'content-length))
        (full-content-length (+ 8 (* 2 content-length)))
        (_ (read-next-field istream record-type 'shape-type))
        (_ (gen (read-next-field istream record-type 'box) 4))
        (numparts (read-next-field istream record-type 'numparts))
        (numpoints (read-next-field istream record-type 'numpoints))
        (_ (println numpoints))
        (parts (for/fold ((s (list))) ((i (range numparts))) (pushr s (read-next-field istream record-type 'parts))))
        (points
          (for/fold
            ((s (list)))
            ((i (range numpoints)))
            (pushr
              s
              (read-next-field istream record-type 'points))))
        (points (break-seq points parts))
     )
    (hash 'points points 'size full-content-length)))

; file-> (listof ... (listof double double))
(define (shapefile->points shapefile)
  (let* ((shp-stream (open-input-file shapefile #:mode 'binary))
        (header (read-bytes HEADER-SIZE shp-stream))
        (file-length (* 2 (shapefile-parameter header 'file-header 'file-length))))
    (define (accumulate-points istream acc read-bytes)
      (cond
        ((>= read-bytes file-length) (hash 'points acc 'read-bytes read-bytes))
        (else (let ((record (read-next-poly-record istream)))
                (accumulate-points
                  istream
                  (pushr acc (@. record.points))
                  (+ read-bytes (@. record.size)))))))
      (hash-ref (accumulate-points shp-stream (list) HEADER-SIZE) 'points)))
