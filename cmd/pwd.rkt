#lang racket

(require racket/cmdline)
(require racket/file)

(require "../../settings/pconfig.rkt")
(require "../lib/all.rkt")

(define (get-pwd url)
	(let* (
					(res (hash-ref PWD url (hash))))
		(if (nil? res)
			(display "No results found.")
			(display (format "~nwebsite: ~a~nlogin: ~a~npwd: ~a~n" url (@. res.login) (@. res.pwd))))))

(command-line
  #:program "pwd"
  #:args
    (resource)
    (get-pwd resource)
)
