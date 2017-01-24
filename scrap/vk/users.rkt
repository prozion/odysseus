#lang racket

(require net/url)
(require json)
(require "../../lib/all.rkt")
(require "vk.rkt")
;(require (file "c:/denis/denis_core/settings/bots.rkt"))

(provide vk/find-paths vk/id->href vk/ids->hrefs vk/link vk/friends)

(define (find-friends user)
  (when (status-output) (display ".") (flush-output))
  (let ((res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/friends.get?user_id=~a&v=5.52" user)))))
    (if (@. res.error)
      null
      (let ((items (map str (@. res.response.items))))
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

;; user properties
(define (request-username id)
  (when (status-output) (display ".") (flush-output))
  (let ((res (string->jsexpr
                (get-url (format "https://api.vk.com/method/users.get?user_ids=~a&v=5.52" id)))))
    (if (@. res.error)
      "n/a"
      (let ((u (car (@. res.response))))
        (format "~a ~a" (@. u.first_name) (@. u.last_name))))))

(define (memoized-request-username)
  (let ((users-hash (hash)))
    (λ (user)
      (let* ((hashed (hash-ref users-hash user #f))
            (res (or hashed (request-username user))))
        (set! users-hash (hash-insert users-hash (cons user res)))
        res))))

;; output
(define (vk/id->href id)
  (format "<a href=\"~a\" target=\"_blank\">~a</a>" (vk/link id) (request-username id)))

(define (vk/ids->hrefs paths)
  (when (status-output) (display "\nrequesting names"))
  (let ((m-request-username (memoized-request-username))) ; function with static hash through closure
    (map
      (λ (x) (map
                (λ (id) (format "<a href=\"~a\" target=\"_blank\">~a</a>" (vk/link id) (m-request-username id)))
                x))
      paths)))

(define (vk/link id)
  (format "https://vk.com/id~a" id))
