#lang racket

(require racket/cmdline)
(require racket/file)

(require (file "c:/denis/denis_core/denis_personal/goals.rkt"))
(require "../lib/load/all.rkt")
(require "../graphics/console.rkt")

(define level "0")

(define (get-goal l)
	(case l
		[("0") hcosmicus]
		[("1") goal1]
		[("2") goal2]
		[("3") goal3]
		[("4") goal4]
		[else "Такого уровня цели пока нет!"]))


(command-line
  #:program "goal"
  ;#:once-any
  ;  [("-l" "--level") l
  ;                  "level of my goals"
	;									(set! level l)]
  #:args
    (level)
    (begin
      (newline)
			(set-text-color 'green)
			(displayln (str " ЦЕЛЬ " level ":"))
			(set-text-color 'yellow)
			(displayln (format " ~a" (get-goal level)))
			(set-text-color 'default)
      (void))
)
