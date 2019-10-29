#lang racket

(require net/url)
(require json)
(require "../lib/_all.rkt")
(require "vk.rkt")
;(require (file "c:/denis/denis_core/settings/APIs.rkt"))

(provide vk/find-paths vk/friends vk/username vk/user-properties)

(define status-output (make-parameter #t))

(define (find-friends user)
  (when (status-output) (display ".") (flush-output))
  (let ((res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/friends.get?user_id=~a&v=5.52" user)))))
    (if (@. res.error)
      null
      (let ((items (@. res.response.items)))
        (if (and (friends-limit) (> (length items) (friends-limit)))
          (take items (friends-limit))
          items)))))

(define (lasts paths)
  (map (λ (x) (last x)) paths))

(define (next-layer paths)
  (let ((used-ids (alist-flatten paths))
        (last-layer (lasts paths)))
    (alist-flatten
      (map
        (λ (x)
          (map
            (λ (y) (pushr x y))
            (clean
              (λ (z) (indexof? used-ids z))
              (find-friends (last x)))))
        paths))))

(define (filter-solutions paths solutions)
  (filter
    (λ (x) (indexof? solutions (last x)))
    paths))

(define (find-by-last alst el)
  (let ((flst (filter
            (λ (x) (equal? (last x) el))
            alst)))
    (if (empty? flst) null (car flst))))

(define (stick-paths paths1 paths2 solutions)
  (let ((p1 (filter-solutions paths1 solutions))
        (p2 (filter-solutions paths2 solutions)))
    (map
      (λ (x)
        (merge x
          (cdr (reverse (find-by-last p2 (last x))))))
      p1)))

; ALGORITHM 2: Start search from the either sides:
(define (find-paths-iter paths1 paths2)
  (let ((solutions (intersect (lasts paths1) (lasts paths2))))
    ;(printf "~a ~a, ~a ~a~n" (length (car paths1)) (length paths1) (length (car paths2)) (length paths2))
    (if (empty? solutions)
      (find-paths-iter (next-layer paths1) (next-layer paths2))
      (stick-paths paths1 paths2 solutions))))

(define (vk/find-paths user1 user2)
  (when (status-output) (display "finding paths"))
  (find-paths-iter (list (list user1)) (list (list user2))))

(define vk/friends find-friends)

;; output
(define memo-get-url (memoize get-url))

(define (vk/user-properties id)
  (let* ((fields "first_name,last_name,domain,sex,bdate,city,home_town,country,contacts")
        (res (json->hash (memo-get-url (format "https://api.vk.com/method/users.get?user_ids=~a&fields=~a&v=5.52" id fields)))))
    (when (status-output) (display "+") (flush-output))
    (if (@. res.error)
      (hash 'id id 'error #t 'status "user info scrap error")
      (car (@. res.response)))))
