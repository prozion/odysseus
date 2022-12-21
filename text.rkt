#lang racket

(require "base.rkt")
(require "type.rkt")
(require "seqs.rkt")
(require "strings.rkt")
(require "regexp.rkt")
(require "controls.rkt")
(require "alist.rkt")
(require "debug.rkt")
(require "io.rkt")
(require "hash.rkt")

(provide (all-defined-out))

(define-catch (match-words? text words)
  (and
    words
    (ormap
      (λ (w)
        (let ((w (string-downcase w)))
          (cond
            ((regular-expression-string? w)
              (and
                (re-matches? w text)
                (caar (get-matches (->pre w) text))))
            (else
              (and
                (string-contains? (string-downcase text) w)
                w)))))
      words)))

(define clean-text
  (change-text
    (list
      ; remove unrecognized smiles
      (cons "🕺" "")
      (cons "🌲" "")
      (cons "🥇" "1 ")
      (cons "🥈" "2 ")
      (cons "🥉" "3 ")
      (cons "5⃣" "5")
      (cons "🏃🏻‍" "")
      (cons #rx"[👉✅]" "&mdash; ")
      (cons #rx"[🚩🏛⏰⌚🏡📅🗣🔊]" "&ndash; ")
      (cons #rx"[🌞🤦]" "")
      (cons #rx"[👫🌏🌀🌏🌿💫🚋🚗♥🎭🐾📝🎡💰🎸☀🌠📰🍒🌻🤩‼❗🏆💯🔥🗿🏃‍♂📖✍🏻👍🏻😊‍♀🔔🐟👍😎💥🎶🎄⛄🌈🍊❄️💚🎅🥂🎁🇷🇺🇧🇾🇺🇦🇰🇿✨🔮🍀📷💦😜😉😁🤞🥳🎉☃💪😃🎂🎊🍾🎈😩⚠✌🔶🆘🏁🚌📍🎬⚡⛳✏🍃🌳👋👌💣📌🎳🚧🇮🇳🇨🇳]" "")
      ; remove vk links
      (cons #rx"\\[.+?\\|" "")
      (cons #rx"\\]" "")
      ; improve punctuation
      (cons #rx"\\ +?\\)" ")")
      (cons #rx"\\(\\ +?" "(")
      (cons #rx"(?<=[A-Za-z0-9А-Яа-я\\-_])\\(" " (")
      (cons #rx"\\)(?=[A-Za-z0-9А-Яа-я\\-_])" ") ")
      (cons #rx"\\ +,\\ +" ", ")
      (cons #rx"\\ *:\\ *(?=[A-Za-zА-Яа-я])" ": "))))

(define htmlify-text
  (change-text
    (list
      ; add line breaks
      (cons "\r\n" "<br>")
      (cons "\n" "<br>")
      (cons "\"" "&quot;")
    )))

(define clean-htmlify (--> clean-text htmlify-text))

(define clean-value
          (change-text
            (list
              (cons "\"" " ")
              (cons "&nbsp;" " ")
              ; (cons "," "")
              (cons "\n" " ")
              (cons "\t" "")
              (cons "  " " ")
              (cons " ." ".")
              (cons "<span>" "")
              (cons "</span>" ""))))

(define remove-hashtags
  (change-text
    (list
      (cons #rx"#[A-Za-zА-Яа-яЁё0-9_]+\\ *?\\." "")
      (cons #rx"#[A-Za-zА-Яа-яЁё0-9_]+" ""))))

(define (text-size-difference text1 text2)
  (let* ((l1 (string-length text1))
        (l2 (string-length text2)))
    (/ (min l1 l2) (max l1 l2) 1.0)))

(define-catch (normalize-text text)
  (let* ((text (remove-hashtags text))
        (text (string-downcase text))
        (text (string-replace text "ё" "e"))
        (text (regexp-replace* #rx"[^a-zа-я0-9]" text "")))
    text))

(define nt normalize-text)

(define-catch (letters-distribution text)
  (let* ((letters (explode text)))
    (for/fold
      ((res (hash)))
      ((letter letters))
      (hash-union-c
        (hash letter (+ 1 (hash-ref res letter 0)))
        res))))

(define-catch (letters-distribution-size distribution)
  (and (hash? distribution) (apply + (hash-values distribution))))

(define-catch (get-text-distance text1 text2)
  (let* (
        (distribution1 (letters-distribution text1))
        (distribution2 (letters-distribution text2))
        (intersection-distance
          (for/fold
            ((res 0))
            ((letter (intersect (hash-keys distribution1) (hash-keys distribution2))))
            (+ res (abs (- (hash-ref distribution1 letter) (hash-ref distribution2 letter))))))
        (1-distance
          (for/fold
            ((res 0))
            ((letter (minus (hash-keys distribution1) (hash-keys distribution2))))
            (+ res (hash-ref distribution1 letter))))
        (2-distance
          (for/fold
            ((res 0))
            ((letter (minus (hash-keys distribution2) (hash-keys distribution1))))
            (+ res (hash-ref distribution2 letter)))))
      (+ intersection-distance 1-distance 2-distance)))

(define-catch (similar-text? text1 text2 #:tolerance (tolerance 0.3))
  (cond
    ; when sizes are obviously different, consider texts different
    ((< (text-size-difference text1 text2) 0.8) #f)
    (else
      (let* ((text1 (nt text1))
            (text2 (nt text2))
            (size1 (string-length text1))
            (size2 (string-length text2)))
        (cond
          ((equal? text1 text2) #t)
          (else
            (let* ((distance (get-text-distance (nt text1) (nt text2))))
              (< distance (* tolerance (max size1 size2))))))))))

(define-catch (any-in-text? txt expressions)
  (and
    txt
    expressions
    (ormap
      (λ (expression)
        (string-contains? txt expression))
      expressions)))
