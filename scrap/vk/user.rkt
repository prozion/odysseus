#lang racket

(require net/url)
(require json)
(require "../../lib/load/all.rkt")
(require "../../../settings/APIs.rkt")
(require "vk.rkt")
;(require (file "c:/denis/denis_core/settings/APIs.rkt"))

(provide (all-defined-out))

(define special-values (list "<user-deactivated>"))

(define user-basic-fields '(first_name last_name deactivated hidden))
(define user-optional-fields-all '(
                                about activities
                                bdate blacklisted blacklisted_by_me books
                                can_post can_see_all_post can_write_private_message career city common_count connections contacts counters country crop_photo
                                domain
                                education exports
                                first_name_nom first_name_gen first_name_dat first_name_acc first_name_ins first_name_abl
                                followers_count friend_status
                                games
                                has_mobile has_photo home_town
                                interests is_favorite is_friend is_hidden_from_feed
                                last_name_nom last_name_gen last_name_dat last_name_acc last_name_ins last_name_abl
                                last_seen lists
                                maiden_name military movies music
                                nickname
                                occupation online
                                personal
                                photo_id photo_max photo_max_orig photo_50 photo_100 photo_200_orig photo_400_orig
                                quotes
                                relatives relation
                                schools screen_name sex site status
                                timezone trending tv
                                universities
                                verified
                                wall_default
                              ))
(define personal-object-fields '(political langs religion inspired_by people_main life_main smoking alcohol))
(define schools-array-fields '(id country city name year_from year_to year_graduated class speciality type type_str))
(define universities-array-fields '(id country city name faculty faculty_name chair chair_name graduation education_form education_status))

(define user-basic-fields-used '(first_name last_name))
(define user-optional-fields-used
                              '(
                                bdate
                                ; blacklisted blacklisted_by_me
                                books
                                ;can_post can_see_all_post can_write_private_message
                                career city common_count connections contacts counters country
                                ; crop_photo
                                domain
                                education exports
                                ; first_name_nom first_name_gen first_name_dat first_name_acc first_name_ins first_name_abl
                                followers_count friend_status
                                games
                                has_mobile has_photo home_town
                                interests is_favorite is_friend is_hidden_from_feed
                                ; last_name_nom last_name_gen last_name_dat last_name_acc last_name_ins last_name_abl
                                last_seen lists
                                maiden_name military movies music
                                nickname
                                occupation online
                                personal
                                ; photo_id
                                photo_max
                                ; photo_max_orig photo_50 photo_100 photo_200_orig photo_400_orig
                                quotes
                                relatives relation
                                schools screen_name sex site status
                                timezone trending tv
                                universities
                                verified
                                wall_default
                              ))

(define user-fields (append user-basic-fields-used user-optional-fields-used))

(define-catch (get-user-info user-id #:fields (fields user-fields) #:status (status #f) #:access-token (access-token #f))
  (when status (display status) (flush-output))
  (let* (
        (fields (implode fields ","))
        (request (format "https://api.vk.com/method/users.get?user_ids=~a&fields=~a&v=5.52" user-id fields))
        (access_token (@. vk/odysseus.access_token)) ; comment this line if you need get-user-info function to be more universal
        (request (if access_token (format "~a&access_token=~a" request access_token) request))
        (res (json->hash (get-url request))))
        ; (res (hash)))
    (if (@. res.error)
        (hash 'error (@. res.error))
        (car (@. res.response)))))

(define-catch (get-friends-of-user user-id #:status (status #f) #:friends-limit (friends-limit #f))
  (when status (display status) (flush-output))
  (let ((res (string->jsexpr
                    (get-url (format "https://api.vk.com/method/friends.get?user_id=~a&v=5.52" user-id)))))
    (cond
      ((@. res.error)
        (list "<user-deactivated>"))
      (else
        (let ((items (@. res.response.items)))
          (if (and friends-limit (> (length items) friends-limit))
            (take items friends-limit)
            items))))))

(define-catch (dump-ids-to-file ids)
  (let ((comma-separated-ids (implode ids ",")))
    (write-file "ids.txt" comma-separated-ids)))

(define-catch (read-ids-from-file filename)
  (let ((ids-string (read-file filename)))
    (split ids-string ",")))

(define-catch (output-user-file filename)
  (let ((users (read-serialized-data-from-file filename)))
    (for ((user (hash-values users)))
      (when (and ($ first_name user) ($ last_name user) (not (equal? ($ first_name user) "DELETED")))
        (display (format "~a ~a,~a,~a  ~a~n"
                      (or ($ first_name user) "")
                      (or ($ last_name user) "")
                      (or ($ bdate user) "")
                      (or
                        ($ city.title user) ($ home_town user)
                        "")
                      (let ((status ($ status user)))
                        (if (no-words? status) "" (str "[" (string-replace status "\n" "") "]")))
                      ))))))

(define-catch (number-of-users filename)
  (let ((users (read-serialized-data-from-file filename)))
    (--- (length (hash-keys users)))))

; (define (find-friends user)
;   (when (status-output) (display ".") (flush-output))
;   (let ((res (string->jsexpr
;                     (get-url (format "https://api.vk.com/method/friends.get?user_id=~a&v=5.52" user)))))
;     (if (@. res.error)
;       null
;       (let ((items (@. res.response.items)))
;         (if (and (friends-limit) (> (length items) (friends-limit)))
;           (take items (friends-limit))
;           items)))))
;
; (define (lasts paths)
;   (map (λ (x) (last x)) paths))
;
; (define (next-layer paths)
;   (let ((used-ids (alist-flatten paths))
;         (last-layer (lasts paths)))
;     (alist-flatten
;       (map
;         (λ (x)
;           (map
;             (λ (y) (pushr x y))
;             (clean
;               (λ (z) (indexof? used-ids z))
;               (find-friends (last x)))))
;         paths))))
;
; (define (filter-solutions paths solutions)
;   (filter
;     (λ (x) (indexof? solutions (last x)))
;     paths))
;
; (define (find-by-last alst el)
;   (let ((flst (filter
;             (λ (x) (equal? (last x) el))
;             alst)))
;     (if (empty? flst) null (car flst))))
;
; (define (stick-paths paths1 paths2 solutions)
;   (let ((p1 (filter-solutions paths1 solutions))
;         (p2 (filter-solutions paths2 solutions)))
;     (map
;       (λ (x)
;         (merge x
;           (cdr (reverse (find-by-last p2 (last x))))))
;       p1)))
;
; ; ALGORITHM 2: Start search from the either sides:
; (define (find-paths-iter paths1 paths2)
;   (let ((solutions (intersect (lasts paths1) (lasts paths2))))
;     ;(printf "~a ~a, ~a ~a~n" (length (car paths1)) (length paths1) (length (car paths2)) (length paths2))
;     (if (empty? solutions)
;       (find-paths-iter (next-layer paths1) (next-layer paths2))
;       (stick-paths paths1 paths2 solutions))))
;
; (define (vk/find-paths user1 user2)
;   (when (status-output) (display "finding paths"))
;   (find-paths-iter (list (list user1)) (list (list user2))))
;
; (define vk/friends find-friends)
;
; ;; user properties
; (define (request-username id (glue " "))
;   (when (status-output) (display "+") (flush-output))
;   (let ((res (string->jsexpr
;                 (get-url (format "https://api.vk.com/method/users.get?user_ids=~a&v=5.52" id)))))
;     (if (@. res.error)
;       "n/a"
;       (let ((u (car (@. res.response))))
;         (format "~a~a~a"
;                   (replace-all (@. u.first_name) " " glue)
;                   glue
;                   (replace-all (@. u.last_name) " " glue))))))
;
; (define vk/username request-username)
;
; ;; output
; (define (vk/id->href id)
;   (format "<a href=\"~a\" target=\"_blank\">~a</a>" (vk/link id) (request-username id)))
;
; (define (vk/ids->hrefs paths)
;   (when (status-output) (display "\nrequesting names"))
;   (let ((m-request-username (memoize request-username))) ; function with static hash through closure
;     (map
;       (λ (x) (map
;                 (λ (id) (format "<a href=\"~a\" target=\"_blank\">~a</a>" (vk/link id) (m-request-username id)))
;                 x))
;       paths)))
;
; (define (vk/link id)
;   (format "https://vk.com/id~a" id))
;
; (define memo-get-url (memoize get-url))
;
; (define (vk/user-properties id)
;   (let* ((fields "first_name,last_name,domain,sex,bdate,city,home_town,country,contacts")
;         (res (json->hash (memo-get-url (format "https://api.vk.com/method/users.get?user_ids=~a&fields=~a&v=5.52" id fields)))))
;     (when (status-output) (display "+") (flush-output))
;     (if (@. res.error)
;       (hash 'id id 'error #t 'status "user info scrap error")
;       (car (@. res.response)))))
