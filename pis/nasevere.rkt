#lang racket

(require "../lib/all.rkt")

(provide (all-defined-out))

(define (tags->something corresponds)
  (λ (tagstr)
    (let ((taglst (split tagstr))
          (res ""))
        (implode
          (clean
            nil?
            (map
              (λ (x) (hash-ref corresponds x ""))
              taglst))
          ","))))

(define
  tags->publish-status
  (tags->something
    (hash
      "w" "стена"
      "t" "тема"
      "f" "закреп"
      "s" "фотослот"
      "p" "фотоальбом"
      "c" "комментарии/частный пост"
      "o" "другие группы"
      "d" "неактивное"
      "i" "нарушает закон?"
      "_" "не рекламировался")))

(define
  tags->owner
  (tags->something
    (hash
      "D" "Дима"
      "I" "Ирина"
      "V" "Валера"
      "E" "Денис"
      "O" "Оксана"
      "S" "Сергей")))
