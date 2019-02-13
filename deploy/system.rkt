#lang racket

(require racket/file)
(require "../lib/load/all.rkt")

(provide (all-defined-out))

(define (where-am-i)
  (let ((curdir (path->string (current-directory))))
    (cond
      ((regexp-match #rx"^[cC]:.*" curdir)
        (let* ((user (getenv "userprofile"))
              (ms (get-matches "C:\\\\Users\\\\(.+)" user))
              (ms (and (not-empty? ms) (first ms)))
              (user (if (> (length ms) 1) (second ms) #f)))
          (cond
            ((equal? user "User") 'inwin-win7)
            (else 'unknown-win))))
      ((regexp-match #rx"^/*" curdir)
        (let ((hostname (string-trim (read-file "/etc/hostname"))))
          (cond
            ((equal? hostname "xpolaris") 'digitalocean)
            (else 'other-linux))))
      (else #f))))    
