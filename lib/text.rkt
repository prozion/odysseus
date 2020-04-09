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

(provide (all-defined-out))

(define-catch (match-words? text words)
  (ormap
    (λ (w)
      (cond
        ((regular-expression-string? w) (re-matches? w text))
        (else
          (string-contains? (string-downcase text) (string-downcase w)))))
    words))

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
      (cons #rx"[‼❗🏆💯🔥🏃‍♂📖✍🏻👍🏻😊‍♀🔔🐟👍😎💥🎶🎄⛄🌈🍊❄️💚🎅🥂🎁🇷🇺🇧🇾🇺🇦🇰🇿✨🔮🍀📷💦😜😉😁🤞🥳🎉☃💪😃🎂🎊🍾🎈😩⚠✌🔶🆘🏁🚌📍🎬⚡⛳✏🍃🌳👋👌💣📌🎳🚧🇮🇳🇨🇳]" "")
      (cons #rx"[👉🏛]" "&ndash;")
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
      (cons "\n" "<br>"))))

(define clean-htmlify (-> clean-text htmlify-text))

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

(define-catch (nt/text->distribution text)
  (frequency-clist (explode (nt text))))

(define-catch (nt/get-size distribution (res 0))
  (cond
    ((empty? distribution) res)
    (else (+ (cdar distribution) (nt/get-size (cdr distribution))))))

(define-catch (nt/get-distance distribution1 distribution2 (distance 0))
  (cond
    ((empty? distribution1) (+ distance (nt/get-size distribution2)))
    ((empty? distribution2) (+ distance (nt/get-size distribution1)))
    (else
      (let* ((char1 (caar distribution1))
            (value1 (cdar distribution1))
            (pair2 (assoc char1 distribution2))
            (value2 (if pair2 (cdr pair2) 0))
            (char_diff (abs (- value1 value2)))
            (new_distribution1 (cdr distribution1))
            (new_distribution2 (clist-remove distribution2 char1)))
        (nt/get-distance new_distribution1 new_distribution2 (+ distance char_diff))))))

(define (get-text-difference text1 text2)
  (nt/get-distance (nt/text->distribution text1) (nt/text->distribution text2)))

(define-catch (similar-text? text1 text2)
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
          ((< (text-size-difference text1 text2) 0.8) #f)
          (else
            (let* ((distribution1 (frequency-clist (explode text1)))
                  (distribution2 (frequency-clist (explode text2)))
                  (distance (nt/get-distance distribution1 distribution2)))
              (< distance (* 0.1 (max size1 size2))))))))))
