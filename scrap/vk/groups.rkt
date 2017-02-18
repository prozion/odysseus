#lang racket

(require net/url)
(require json)
(require compatibility/defmacro)
(require "../../lib/all.rkt")
(require "vk.rkt")
;(require (file "c:/denis/denis_core/settings/bots.rkt"))

(provide (all-defined-out))

(define (get-group-name groupid)
  (let ((res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/groups.getById?group_id=~a&v=5.52" groupid)))))
    (if (@. res.error)
      null
      (hash-ref (car (hash-ref res 'response)) 'name))))

(define (raw-community? type)
  (λ (groupid)
    (regexp-match (pregexp (str (symbol->string type) "\\d+")) groupid)))

(define raw-group? (raw-community? 'club))
(define raw-public? (raw-community? 'public))
(define raw-event? (raw-community? 'event))

(define (extract-pure-id groupid)
  (cond
    ((raw-group? groupid) (ltrim groupid (len "club")))
    ((raw-public? groupid) (ltrim groupid (len "public")))
    ((raw-event? groupid) (ltrim groupid (len "event")))
    (else groupid)))

(define (get-group-users groupid)
  (when (status-output) (display (format "~a~n" groupid)) (flush-output))
  (let* ( (groupid (extract-pure-id groupid))
          (res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/groups.getMembers?group_id=~a&v=5.52" groupid)))))
    (if (@. res.error)
      null
      (map str (@. res.response.items)))))

(define (vk/intersect-groups . groups)
  (when (status-output) (display "\nscanning groups: \n"))
  (apply set-intersect (map get-group-users groups)))

(define (vk/minus-groups . groups)
  (when (status-output) (display "\nscanning groups: \n"))
  (apply minus (map get-group-users groups)))

(define (vk/difference-groups . groups)
  (when (status-output) (display "\nscanning groups: \n"))
  (apply difference (map get-group-users groups)))

; for expressions in the command line:
(define-macro (g-op op)
  `(λ groups
    (let
      ((uids-by-groups
          (map
            (λ (group)
              (cond
                ((list? group) group)
                ((string? group) (get-group-users group))
                (else group)))
            groups)))
      (apply ,op uids-by-groups))))

(define g^ (g-op set-intersect))
(define g- (g-op minus))
(define g-- (g-op difference))
