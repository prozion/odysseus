#lang racket

(require "../lib/_all.rkt")
(require "vk.rkt")

(provide (all-defined-out))

(define-catch (user-ids->tabtree user-ids)
  (let* ((users-tree (for/fold
                ((res empty))
                ((id user-ids))
                (let* ((user (get-user-info id))
                      (name_id (str (or ($ first_name user) "") "_" (or ($ last_name user) "")))
                      (place ($ city.title user))
                      (vk (format "vk.com/id~a" id))
                      (f ($ counters.friends user))
                      )
                  (pushr res (hash 'id name_id 'place place 'vk vk 'f f)))))
        (users-tree (sort users-tree (λ (a b) (string<? ($ id a) ($ id b)))))
        (tree-file-contents (for/fold
                              ((res ""))
                              ((u users-tree))
                              (format "~a~a place:~a vk:~a f:~a~n" res ($ id u) ($ place u) ($ vk u) ($ f u)))))
    tree-file-contents))
