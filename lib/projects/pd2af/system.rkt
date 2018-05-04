#lang racket

(require racket/file)
(require "../../load/all.rkt")

(provide (all-defined-out))

(define (where-am-i)
  (let ((curdir (path->string (current-directory))))
    (cond
      ((regexp-match #rx"^c:.*" curdir)
        (let* ((user (getenv "userprofile"))
              (ms (get-matches "C:\\\\Users\\\\(.+)" user))
              (user (match ms
                      (`((,_ ,username)) username)
                      (else #f))))
          (cond
            ((equal? user "User") 'ab-24)
            (else 'unknown-win))))
      ((regexp-match #rx"^/*" curdir)
        (let ((hostname (string-trim (read-file "/etc/hostname"))))
          (cond
            ((equal? hostname "xpolaris") 'xpolaris)
            (else 'other-linux))))
      (else #f))))
