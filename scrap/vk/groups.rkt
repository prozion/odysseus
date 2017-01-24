#lang racket

(require net/url)
(require json)
(require "../../lib/all.rkt")
(require "vk.rkt")
;(require (file "c:/denis/denis_core/settings/bots.rkt"))

(provide vk/intersect-groups)

(define (get-group-users groupid)
  (when (status-output) (display (format "~a~n" groupid)) (flush-output))
  (let ((res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/groups.getMembers?group_id=~a&v=5.52" groupid)))))
    (if (@. res.error)
      null
      (map str (@. res.response.items)))))

(define (vk/intersect-groups . groups)
  (when (status-output) (display "\nscanning groups: \n"))
  (apply set-intersect (map get-group-users groups)))
