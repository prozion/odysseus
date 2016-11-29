#lang racket

(require net/url)
(require "../utils/all.rkt")
;(require "../utils/seqs.rkt" "../utils/http.rkt" "../utils/hash.rkt")
(require json)

(provide (all-defined-out))

; необходимо открыть гугл таблицу через File > Publish to the web..., иначе будет выдавать 403 Forbidden - ошибку доступа
; узнать worksheet id можно из этого xml: https://spreadsheets.google.com/feeds/worksheets/spreadsheet_id/private/full
; в поиск вбить "full/" и посмотреть все id после выделенных фрагментов, выбрать тот id, который ближе к <title>название_листа</title>
(define (google-spreadsheet/get-json spreadsheet_id worksheet_id)
  (let* (
        [spreadsheet_url
          (format
            "https://spreadsheets.google.com/feeds/list/~a/~a/public/values?alt=json-in-script"
            spreadsheet_id
            worksheet_id)]
        [function-prefix "gdata.io.handleScriptLoaded)"]
        [res-str (str (get-url spreadsheet_url))]
        [json-str (substring res-str (string-length function-prefix) (- (string-length res-str) 2))]
        [json-hash (string->jsexpr json-str)]
        [full-list (hash-path json-hash 'feed 'entry)])
    full-list))

;; QUESTION: does row number really correspond to row's order in the sheet or hashing breaks the order?
(define (google-spreadsheet/get-content json-list row column-name)
  (hash-path (nth json-list row)
    (string->symbol (str "gsx$" column-name))
    (string->symbol "$t")))

;(define res (google-spreadsheet/get-json "12GjbHIlT739wOMcRTSBwfbjpUsaHvLgGAd1EEobKEaY" "od6"))
;;(define res (google-spreadsheet/get-json "1CMuhp9ZTSGlCp0Ly0Czkb-4JhVBHJOQ5sxUILv7o5Do" "ot9n3e7"))
;
;(write-file "res" (google-spreadsheet/get-content
;  ; (google-spreadsheet/get-json "1CMuhp9ZTSGlCp0Ly0Czkb-4JhVBHJOQ5sxUILv7o5Do" "ot9n3e7")
;  res
;  32 "фильм"))
