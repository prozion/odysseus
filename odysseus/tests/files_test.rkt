#lang racket

(require "../files.rkt")

(require rackunit)

(check-true (absolute-path? "/var/tmp/something/"))
(check-true (absolute-path? "C:/User/Users/AppData"))
(check-true (absolute-path? "C:\\User\\Users\\AppData"))
(check-false (absolute-path? "sandbox/foobar"))
(check-false (absolute-path? "foobarbaz"))
