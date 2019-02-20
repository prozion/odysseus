#lang racket

(require racket/cmdline)

(require (file "c:/denis/denis_core/settings/web.rkt"))
(require "../lib/_all.rkt")
(require "../graphics/console.rkt")

(define query "")
(define count "")

(define (print-records h)
	;(displayln "== PASSWORDS ==")
	(for (((k v) h))
		(set-text-color 'yellow)
		(displayln (format " ~a" k))
		(set-text-color 'grey)
		(displayln
			(record->string v))
		(newline))
	(set-text-color 'default)
)

(define (record->string record)
	(cond
		((list? record) (implode
										(map
											(λ (h) (single-record->string h))
											record)
										(format
											"\n~a   ---~a\n"
											(string-text-color 'grey)
											(string-text-color 'blue))
											))
		((string? record) (format " as ~a" record))
		(else
			(single-record->string record))))

(define (single-record->string h)
		(format  "~a   ~a~n~a   ~a"
								(string-text-color 'blue)
								(zor (@. h.login) "??")
								(string-text-color 'red)
								(zor (@. h.password) "??")))

(command-line
  #:program "pwd"
	#:once-any
		[("-c" "--count")
											"total number of records"
											(set! count #t)]
   ; [("-q" "--query") q
   ;                 "query to filter"
		; 								(set! query q)]
  #:args
    (query)
    ; (cond
			; ((not (nil? count))
			; 	(newline)
      ;   (set-text-color 'green)
      ;   (displayln
      ;     (format "Total records: ~a" (hash-count websites)))
      ;   (set-text-color 'default))
			(cond
				((not (nil? count))
					(newline)
	        (set-text-color 'green)
	        (displayln
	          (format "Total records: ~a" (hash-count websites)))
	        (set-text-color 'default))
				(else
	      	(newline)
					(print-records
			      (hash-regex-filter
							(regexp query)
							websites))
		      (void))
))
