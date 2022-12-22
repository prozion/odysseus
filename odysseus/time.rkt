#lang racket

(require "base.rkt")
(require "type.rkt")
(require "hash.rkt")
(require "list.rkt")
(require "math.rkt")
(require "string.rkt")
(require "regexp.rkt")
(require "debug.rkt")
(require "files.rkt")

(provide (all-defined-out))

;;;; basic data for functions

(define days '(31 28 31 30 31 30 31 31 30 31 30 31))
(define months (string-split "jan feb mar apr may jun jul aug sep oct nov dec" " "))
(define months-full (string-split "january february march april may june july august september october november december" " "))
(define months-ru (string-split "янв фев мар апр май июн июл aвг сен окт ноя дек" " "))
(define months-ru-full (string-split "январь февраль март апрель май июнь июль август сентябрь октябрь ноябрь декабрь" " "))
(define months-ru-full-genetive (string-split "января февраля марта апреля мая июня июля aвгуста сентября октября ноября декабря" " "))
(define leap-days '(31 29 31 30 31 30 31 31 30 31 30 31))

(define (acc-days d)
  (for/fold
    ((s '(0)))
    ((i d))
    (pushr s (+ (last s) i))))

(define days-acc (acc-days days))
(define leap-days-acc (acc-days leap-days))

;;;; functions to work with time

(define (date day month year)
  (hash 'day day 'month month 'year year))

(define (time->seconds astr)
  (let* ( (time-units (string-split astr ":"))
          (seconds (string->number (last time-units)))
          (minutes (string->number (if (null? (nth time-units -2)) "0" (nth time-units -2))))
          (hours (string->number (if (null? (nth time-units -3)) "0" (nth time-units -3)))))
    (+ (* 60 60 hours) (* 60 minutes) seconds)))

(define (seconds->days sec)
  (quotient sec (* 60 60 24)))

(define (seconds->hours sec)
  (quotient sec 3600))

(define (seconds->minutes sec)
  (quotient sec 60))

(define (seconds->time sec)
  (let* ((hours (seconds->hours sec))
        (minutes (- (seconds->minutes sec) (* 60 hours)))
        (seconds (- sec (* 3600 hours) (* 60 minutes)))
        (hours-print (~r #:min-width 2 #:pad-string "0" hours))
        (minutes-print (~r #:min-width 2 #:pad-string "0" minutes))
        (seconds-print (~r #:min-width 2 #:pad-string "0" seconds)))
    (cond
      ((> hours 0) (format "~a:~a:~a" hours-print minutes-print seconds-print))
      ((> minutes 0) (format "~a:~a" minutes-print seconds-print))
      (else (format "~a" seconds-print)))))

; gives a string with 'timestamp' for logging and other purposes
(define-catch (timestamp (seconds (current-seconds)))
  (let* ((adate (seconds->date seconds)))
        ; (adate (struct->list adate)) ; (require racket/struct)
        ; (_ (when (< (length adate) 9) (error (format "Wrong argument value in (timestamp seconds): ~a" seconds)))))
    ; (match-let* (((list _ seconds minutes hours day month year ...) reslst))
      (format "~a:~a:~a ~a.~a.~a"
        (date-hour adate)
        (format-number "dd" (date-minute adate) #:filler "0")
        (format-number "dd" (date-second adate) #:filler "0")
        (format-number "dd" (date-day adate) #:filler "0")
        (format-number "dd" (date-month adate) #:filler "0")
        (date-year adate)
        )))

(define (time-diff t1 t2)
  (seconds->time
    (abs
      (- (time->seconds t2) (time->seconds t1)))))

(define (leap-year? year)
  (or
    (and
      (= 0 (remainder year 4))
      (!= 0 (remainder year 100)))
    (= 0 (remainder year 400))))

(define (last-day my)
  (let* (
        (parts (string-split my (regexp "[\\.\\.]")))
        (month (->number (first parts)))
        (year (->number (second parts))))
    (case month
      ((1 3 5 7 8 10 12) 31)
      ((2) (if (leap-year? year) 29 28))
      (else 30))))

(define (date? adate)
  (let ((adate (->string adate)))
    (and
      adate
      (or
        (re-matches? "^[0-9x]{2}\\.[0-9x]{2}\\.[0-9x]{4}$" adate)
        (re-matches? "^[0-9x]{2}\\.[0-9x]{2}$" adate)
        (re-matches? "^(0[1-9x]|1[0-2x])\\.[0-9x]{4}$" adate)))))

(define (precise-date? adate)
  (let ((adate (->string adate)))
    (and
      adate
      (re-matches? "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}\\??$" adate))))

(define-catch (valid-date? d)
  (let* ((d (if (string? d) (parse-date d) d))
        (day (->number ($ day d)))
        (month (->number ($ month d)))
        (year (->number ($ year d))))
    (cond
      ; general limits on days and months
      ((or (> day 31) (> month 12)) #f)
      ; check 31 and 30 days in month
      ((and (= day 31) (index-of? '(2 4 6 9 11) month)) #f)
      ; check for 29 February in a non-leap year
      ((and (not (leap-year? year)) (= 29 day) (= 2 month)) #f)
      ; if no checks triggered, consider d a valid date
      (else #t))))

(define-catch (date->days datestr)
  (define (parse-datestr datestr)
    (let ((date-lst (string-split datestr ".")))
      (cond
        ((= 3 (length date-lst))
          (hash 'day (->number (first date-lst)) 'month (->number (second date-lst)) 'year (->number (third date-lst))))
        ((= 2 (length date-lst))
          (hash 'day #f 'month (->number (first date-lst)) 'year (->number (second date-lst))))
        (else (error (format "wrong format for date->days: ~a" datestr))))))
  (let*
      (
      (parsed (parse-datestr datestr))
      (day ($ day parsed))
      (month ($ month parsed))
      (year ($ year parsed))
      (day (if (not day) 1 day))
      (years (range 1 year))
      (leap-years (filter leap-year? years))
      (non-leap-years (clean leap-year? years)))
    (cond
      ((not (valid-date? (date day month year))) #f) ; (error (format "~a is not a valid date" datestr)))
      (else
        (+
          (* 366 (length leap-years))
          (* 365 (length non-leap-years))
          ;; count days in months before:
          (nth
            (if (leap-year? year)
              leap-days-acc
              days-acc)
            month)
          ;; plus days in the current month:
          day)))))

(define-catch (days->date days #:leap (leap #f) #:year (year #f))
  (define (full-year val (curyear 1))
    (cond
      ((and (leap-year? curyear) (<= val 366)) (list curyear val))
      ((<= val 365) (list curyear val))
      (else (full-year (- val (if (leap-year? curyear) 366 365)) (inc curyear)))))
  (define (full-month acc-arr val (count 0))
    (cond
      ((empty? acc-arr) count)
      ((>= (car acc-arr) val) (inc count))
      (else (full-month (cdr acc-arr) val (inc count)))))
  (let* ((m-len '(31 28 31 30 31 30 31 31 30 31 30 31))
        (m-len-leap '(31 29 31 30 31 30 31 31 30 31 30 31))
        (d-by-m (accumulate m-len))
        (d-by-m-leap (accumulate m-len-leap))
        (d-by-m (if leap d-by-m-leap d-by-m))
        (_ (full-year days))
        (y (first _))
        (days (second _))
        (m (full-month d-by-m days))
        (d (- days (if (> m 1)
                        (nth d-by-m (dec m))
                        0)))
        (d (if (= d 0) 1 d)))
      (vk->date
        (if year
          (implode (list d m y) ".")
          (implode (list d m) ".")))))

(define-catch (date-diff d1 d2)
  (let ((days1 (date->days d1))
        (days2 (date->days d2)))
      (and days1 days2 (- days1 days2))))

(define-catch (hours-diff hdate1 hdate2)
  (let* (
        (hour1 (->number ($ hour hdate1)))
        (hour2 (->number ($ hour hdate2)))
        (days-diff (date-diff (hdate->datestr hdate1) (hdate->datestr hdate2)))
        (hours-diff (- hour1 hour2)))
    (cond
      ((and (= 0 days-diff)) (- hour1 hour2))
      (else (+ hour1 (- 24 hour2) (* 24 (dec days-diff)))))))

(define (date-diff-abs d1 d2)
  (abs (date-diff d1 d2)))

(define (month-diff m1 m2)
  (let* ((m1 (string-split m1 "."))
        (m2 (string-split m2 "."))
        (m1-month (->number (first m1)))
        (m1-year (->number (second m1)))
        (m2-month (->number (first m2)))
        (m2-year (->number (second m2)))
        )
    (+ (- m2-month m1-month) (* 12 (- m2-year m1-year)))))

(define (comparison-f comp-op conversion)
  (λ (a b)
    (let* ((a (if (list? a) (car a) a))
          (b (if (list? b) (car b) b)))
      (comp-op (conversion a) (conversion b)))))

; this function must work faster than comparison-f for functions like d> and d<
(define (compare-date comp-op)
  (λ (date1 date2)
    (define (compare-list-neq comp-op lst1 lst2)
      (cond
        ((and (empty? lst1) (empty? lst2)) #f)
        ((empty? lst1) (comp-op 1 (car lst2)))
        ((empty? lst2) (comp-op (car lst1) 1))
        ((comp-op (car lst1) (car lst2)) #t)
        ((= (car lst1) (car lst2)) (compare-list-neq comp-op (cdr lst1) (cdr lst2)))
        (else #f)))
    (define (compare-list-eq comp-op lst1 lst2)
      (cond
        ((and (empty? lst1) (empty? lst2)) #t)
        ((empty? lst1) (comp-op 1 (car lst2)))
        ((empty? lst2) (comp-op (car lst1) 1))
        ((= (car lst1) (car lst2)) (compare-list-eq comp-op (cdr lst1) (cdr lst2)))
        ((comp-op (car lst1) (car lst2)) #t)
        (else #f)))
    (let* (
          (parsed1 (reverse (map ->number* (string-split date1 "."))))
          (parsed2 (reverse (map ->number* (string-split date2 ".")))))
      (cond
        ((ormap (λ (x) (equal? x comp-op)) (list > <)) (compare-list-neq comp-op parsed1 parsed2))
        ((ormap (λ (x) (equal? x comp-op)) (list = >= <=)) (compare-list-eq comp-op parsed1 parsed2))
        (else (compare-list-neq comp-op parsed1 parsed2))))))

; (define d> (comparison-f > date->days))
; (define d>= (comparison-f >= date->days))
; (define d< (comparison-f < date->days))
; (define d<= (comparison-f <= date->days))
; (define d= (comparison-f = date->days))

(define d> (compare-date >))
(define d>= (compare-date >=))
(define d< (compare-date <))
(define d<= (compare-date <=))
(define d= (compare-date =))

(define (d+ adate days)
  (days->date (+ (date->days adate) days) #:year #t))

(define-catch (date-between? date-interval-cons adate)
  (and
    (d>= adate (car date-interval-cons))
    (d<= adate (cdr date-interval-cons))))

  ;(~r #:min-width 2 #:pad-string "0" 1)

;; add hours and minutes
(define (+h . hs)
  (define (rst a) (- a (floor a)))
    (let* ( [intsum (apply + (map floor hs))]
            [fractsum (apply + (map rst hs))]
            [h_fractsum (floor (/ fractsum 0.6))]
            [m (- fractsum (* 0.6 h_fractsum))])
  ;(printf "h1=~a, prod=~a, h2=~a, m=~a\n" h1 prod h2 m)
      (/
        (round
          (*
            100
            (+ intsum h_fractsum m)))
        100)))

; minutes and seconds to decimals
(define (ms->decimals m s)
  (+ (/ m 60.0) (/ s 3600)))

(define (decimals->ms x)
  (let* ((i (int x))
        (f (fract x))
        (m (*f 60 f))
        (s (*f 3600 (- f (/ m 60)))))
    (list i m s)))

; 2017-01-19T18:00:00 -> (hash 'year "2017" 'month "01" 'day "19" 'hour "18" 'min "00" 'sec "00")
(define (parse-time timestr)
  (let* (
        (ts (first (string-split timestr "+"))) ; exclude time zone
        (ts (string-split ts "T"))
        (t1s (string-split (nth ts 1) "-"))
        (t2s (string-split (nth ts 2) ":"))
        (year (nth t1s 1))
        (month (nth t1s 2))
        (day (nth t1s 3))
        (hour (nth t2s 1))
        (minute (nth t2s 2))
        (sec (nth t2s 3)))
    (hash 'year year 'month month 'day day 'hour hour 'min minute 'sec sec)))

(define timestr->hdate parse-time)

(define-catch (seconds->hdate seconds)
  (let* ((date-struct (seconds->date seconds))
        (sec (date-second date-struct))
        (minute (date-minute date-struct))
        (hour (date-hour date-struct))
        (day (date-day date-struct))
        (month (date-month date-struct))
        (year (date-year date-struct)))
    (hash 'year year 'month month 'day day 'hour hour 'min minute 'sec sec)))

(define (hdate->string hdate)
  (format "~a.~a.~a ~a:~a"
    (format-number "dd" ($ day hdate) #:filler "0")
    (format-number "dd" ($ month hdate) #:filler "0")
    ($ year hdate)
    (format-number "dd" ($ hour hdate) #:filler "0")
    (format-number "dd" ($ min hdate) #:filler "0")))

(define (hdate->datestr hdate)
  (format "~a.~a.~a"
    (format-number "dd" ($ day hdate) #:filler "0")
    (format-number "dd" ($ month hdate) #:filler "0")
    ($ year hdate)))

(define (seconds->datestr seconds)
  (let* ((date-struct (seconds->date seconds))
        (day (format-number "dd" (date-day date-struct) #:filler "0"))
        (month (format-number "dd" (date-month date-struct) #:filler "0"))
        (year (date-year date-struct)))
    (format "~a.~a.~a" day month year)))

(define (current-date)
  (seconds->datestr (current-seconds)))

(define (current-day)
  (list-ref (string-split (current-date) ".") 0))

(define (current-month)
  (list-ref (string-split (current-date) ".") 1))

(define (current-year)
  (list-ref (string-split (current-date) ".") 2))

(define (dd.mm.yyyy->yyyy-mm-dd date-string)
  (let* ((t (string-split date-string "."))
        (day (first t))
        (month (second t))
        (year (third t)))
    (format "~a-~a-~a" year month day)))

(define (d.m datestr)
  (let* ((parts (get-matches
                  #px"([0-9x]{2}).([0-9x]{2})(.([0-9x]{4}[~?]?))?"
                  datestr))
        (parts (if (not-nil? parts) (car parts) #f)))
    (if parts
      (str (second parts) "." (third parts))
      "")))

(define (m.y hdate)
  (format "~a.~a" (month hdate) (year hdate)))

(define (parse-date datestr)
  (match datestr
    ((pregexp #px"^([0-9x]{2})\\.([0-9x]{2})\\.([0-9x]{4})$" (list _ day month year))
        (hash 'day day 'month month 'year year))
    ((pregexp #px"^([0-9x]{2})\\.([0-9x]{2})$" (list _ day month))
        (hash 'day day 'month month 'year #f))
    ((pregexp #px"^([0-9x]{2})\\.([0-9x]{1,4})$" (list _ month year))
        (hash 'day #f 'month month 'year year))
    ((pregexp #px"^([0-9x]{1,4})$" (list _ year))
        (hash 'day #f 'month #f 'year year))
    (else
        (hash))))

(define datestr->hdate parse-date)

(define (date->seconds datestr)
  (let* ((days (date->days datestr)))
    (* 60 60 24 days)))

(define (datetimestr->seconds datetimestr)
  (let* ((parsed_datetime (string-split datetimestr " "))
        (adate (first parsed_datetime))
        (atime (and (several-elements? parsed_datetime) (second parsed_datetime)))
        (atime (or
                (and atime (time->seconds atime))
                0)))
    (+ atime (date->seconds adate))))

(define (dtstr->dstr datetime)
  (string-first-word datetime #:delimeter " "))

(define-catch (vk->date vk-datestr)
  (cond
    ((or (not vk-datestr) (equal? vk-datestr "")) #f)
    (else
      (let* ((parts (string-split vk-datestr "."))
            (dd (car parts))
            (dd (format-number "dd" dd #:filler "0"))
            (parts (cdr parts))
            (mm (if (not-empty? parts) (car parts) #f))
            (mm (if mm (format-number "dd" mm #:filler "0") #f))
            (parts (if (not-empty? parts) (cdr parts) parts))
            (yyyy (if (not-empty? parts) (car parts) #f))
            (yyyy (if yyyy (format-number "dddd" yyyy) #f)))
        (format "~a~a~a" (if dd dd "") (if mm (str "." mm) "") (if yyyy (str "." yyyy) ""))))))

(define (day datestr)
  (hash-ref (parse-date datestr) 'day #f))

(define (month datestr)
  (hash-ref (parse-date datestr) 'month #f))

(define (year datestr)
  (hash-ref (parse-date datestr) 'year #f))

(define (next-month m)
  (hash-ref
    (hash "01" "02" "02" "03" "03" "04" "04" "05" "05" "06" "06" "07" "07" "08" "08" "09" "09" "10" "10" "11" "11" "12" "12" "01")
    m))

; for finding year ticks on the timeline:
(define (first-month? month shift)
  (= 1 (remainder (+ month shift) 12)))

(define (date->month datestr)
  (+ (->number (month datestr)) (* 12 (dec (->number (year datestr))))))

(define (month->year amonth)
  (inc (quotient (dec amonth) 12)))

(define (get-zodiac-sign datestr)
  (when (not (date? datestr)) (error (format "wrong date format: ~a" datestr)))
  (let* (
        (Capricorn1 (cons "01.01.1979" "19.01.1979")) ; Козерог
        (Aquarius (cons "20.01.1979" "18.02.1979")) ; Водолей
        (Pisces (cons "19.02.1979" "20.03.1979")) ; Рыбы
        (Aries (cons "21.03.1979" "19.04.1979")) ; Овен
        (Taurus (cons "20.04.1979" "20.05.1979")) ; Телец
        (Gemini (cons "21.05.1979" "20.06.1979")) ; Близнецы
        (Cancer (cons "21.06.1979" "22.07.1979")) ; Рак
        (Leo (cons "23.07.1979" "22.08.1979")) ; Лев
        (Virgo (cons "23.08.1979" "22.09.1979")) ; Дева
        (Libra (cons "23.09.1979" "22.10.1979")) ; Весы
        (Scorpio (cons "23.10.1979" "21.11.1979")) ; Скорпион
        (Sagittarius (cons "22.11.1979" "21.12.1979")) ; Стрелец
        (Capricorn2 (cons "22.12.1979" "31.12.1979")) ; Козерог
        (hdate (parse-date datestr))
        (datestr (str (d.m datestr) ".1979"))
        )
    (cond
      ((or (date-between? Capricorn1 datestr) (date-between? Capricorn2 datestr)) 'Capricorn)
      ((date-between? Aquarius datestr) 'Aquarius)
      ((date-between? Pisces datestr) 'Pisces)
      ((date-between? Aries datestr) 'Aries)
      ((date-between? Taurus datestr) 'Taurus)
      ((date-between? Gemini datestr) 'Gemini)
      ((date-between? Cancer datestr) 'Cancer)
      ((date-between? Leo datestr) 'Leo)
      ((date-between? Virgo datestr) 'Virgo)
      ((date-between? Libra datestr) 'Libra)
      ((date-between? Scorpio datestr) 'Scorpio)
      ((date-between? Sagittarius datestr) 'Sagittarius))))

(define (with-month-fullname-ru datestr)
  (let* ((parsed-date (parse-date datestr))
        (day ($ day parsed-date))
        (month-n (->number ($ month parsed-date)))
        (month-name (nth months-ru-full month-n))
        (month-name-gen (nth months-ru-full-genetive month-n))
        (year ($ year parsed-date))
        )
    (cond
      ((not datestr) #f)
      ((not day)
        (format "~a ~a" month-name year))
      (else
        (format "~a ~a ~a" day month-name-gen year)))))

; get number of days from the string of following format: '\d+[dw]?'
; 14.12.2018 added m option for by-month gantt diagrams
(define-catch (days-count astr)
  (let* ((regexped (get-matches #px"(\\d+)([dwm]?)" (->string astr)))
        (regexped (car regexped))
        (num (->number (second regexped)))
        (postfix (third regexped)))
    (case postfix
      (("d" "") num)
      (("w") (* 7 num))
      (("m") (exact-floor (* (if (> num 6) 30.45 30.5) num))))))

(define-catch (months-count astr)
  (when (not (re-matches? #px"\\d+[myd]?" (->string astr))) (error (format "wrong month string format: ~a" astr)))
  (let* ((regexped (get-matches #px"(\\d+)([myd]?)" (->string astr)))
        (regexped (car regexped))
        (num (->number (second regexped)))
        (postfix (third regexped)))
    (case postfix
      (("m") num)
      (("y") (* 12 num))
      (("d" "") (quotient num 30))
      (else (error (format "wrong month string format: ~a" astr))))))

(define-catch (weekday datestr)
  (case (remainder (date->days datestr) 7)
    ((1) 'mon)
    ((2) 'tue)
    ((3) 'wed)
    ((4) 'thu)
    ((5) 'fri)
    ((6) 'sat)
    ((0) 'sun)))

(define-catch (holiday? datestr)
  (index-of? '(sat sun) (weekday datestr)))

(define-catch (long-month? mon)
  (index-of? (list "01" "03" "05" "07" "08" "10" "12") mon))

(define-catch (short-month? mon)
  (index-of? (list "04" "06" "09" "11") mon))

(define (february? mon)
  (equal? "02" mon))

(define-catch (month-name month-number #:lang (lang 'en))
  (let* ((month-number (->number month-number))
        (month-number (if (> month-number 12) (remainder month-number 12) month-number))
        (month-number (if (= month-number 0) 12 month-number)))
    (if (< 0 month-number 13)
      (nth
        (case lang
          ((ru) months-ru-full)
          (else months-full))
        month-number)
      "?")))

(define-catch (iso8601-time-string seconds)
  (let ((d (seconds->date seconds)))
    (format "~a-~a-~aT~a:~a:~aZ"
            (date-year d)
            (format-number "dd" (date-month d) #:filler "0")
            (format-number "dd" (date-day d) #:filler "0")
            (format-number "dd" (date-hour d) #:filler "0")
            (format-number "dd" (date-minute d) #:filler "0")
            (format-number "dd" (date-second d) #:filler "0")
            )))