#lang racket

(require racket/file)
(require compatibility/defmacro)

(provide (all-defined-out))

; (define (where-am-i)
;   (let ((curdir (path->string (current-directory))))
;     (cond
;       ((regexp-match #rx"^[cC]:.*" curdir)
;         (let* ((user (getenv "userprofile"))
;               (ms (get-matches "C:\\\\Users\\\\(.+)" user))
;               (ms (and (not-empty? ms) (first ms)))
;               (user (if (> (length ms) 1) (second ms) #f)))
;           (cond
;             ((equal? user "User") 'inwin-win7)
;             (else 'unknown-win))))
;       ((regexp-match #rx"^/*" curdir)
;         (let ((hostname (string-trim (read-file "/etc/hostname"))))
;           (cond
;             ((equal? hostname "xpolaris") 'digitalocean)
;             (else 'other-linux))))
;       (else #f))))

(define (server?)
  (getenv "ODYSSEUS_SERVER"))
  ; (case (where-am-i)
  ;   ((inwin-win7 unknown-win) #f)
  ;   ((digitalocean) #t)
  ;   (else #f)))

(define-macro (-s . exprs)
  `(when (server?) ,@exprs))
