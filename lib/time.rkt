#lang racket

(require "base.rkt")
(require "type.rkt")
(require "hash.rkt")
(require "seqs.rkt")
(require "math.rkt")
(require "strings.rkt")
(require "regexp.rkt")
(require "debug.rkt")

(provide (all-defined-out))

;;;; basic data for functions

(define days '(31 28 31 30 31 30 31 31 30 31 30 31))
(define months (split "jan feb mar apr may jun jul aug sep oct nov dec" " "))
(define months-ru (split "янв фев мар апр май июн июл aвг сен окт ноя дек" " "))
(define months-ru-full (split "январь февраль март апрель май июнь июль aвгуст сентябрь октябрь ноябрь декабрь" " "))
(define months-ru-full-genetive (split "января февраля марта апреля мая июня июля aвгуста сентября октября ноября декабря" " "))
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
  (let* ( (time-units (split astr ":"))
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
      ((and (= day 31) (indexof? '(2 4 6 9 11) month)) #f)
      ; check for 29 February in a non-leap year
      ((and (not (leap-year? year)) (= 29 day) (= 2 month)) #f)
      ; if no checks triggered, consider d a valid date
      (else #t))))

(define-catch (date->days d)
  (let*
      ((date-lst (split d "."))
      ;(day (->number (first (split (first date-lst) "-")))) ; take first day in the days interval
      (day (->number (first date-lst)))
      (month (->number (second date-lst)))
      (year (->number (third date-lst)))
      (years (range 1 year))
      (leap-years (filter leap-year? years))
      (non-leap-years (clean leap-year? years)))
    (cond
      ((not (valid-date? (date day month year))) #f)
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

(define (date-diff-abs d1 d2)
  (abs (date-diff d1 d2)))

(define (month-diff m1 m2)
  (let* ((m1 (split m1 "."))
        (m2 (split m2 "."))
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

(define d> (comparison-f > date->days))
(define d>= (comparison-f >= date->days))
(define d< (comparison-f < date->days))
(define d<= (comparison-f <= date->days))
(define d= (comparison-f = date->days))

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
  (let* ((ts (split timestr "T"))
        (t1s (split (nth ts 1) "-"))
        (t2s (split (nth ts 2) ":"))
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

(define (current-date)
  (let* ((curdate (seconds->date (current-seconds)))
        (day (format-number "dd" (date-day curdate) #:filler "0"))
        (month (format-number "dd" (date-month curdate) #:filler "0"))
        (year (date-year curdate)))
    (format "~a.~a.~a" day month year)))

(define (date->string)
  (let* ((curdate (seconds->date (current-seconds)))
        (day (format-number "dd" (date-day curdate) #:filler "0"))
        (month (format-number "dd" (date-month curdate) #:filler "0"))
        (year (date-year curdate)))
    (format "~a.~a.~a" day month year)))

(define (d.m adate)
  (let* ((parts (get-matches
                  #px"([0-9x]{2}).([0-9x]{2})(.([0-9x]{4}[~?]?))?"
                  adate))
        (parts (if (notnil? parts) (car parts) #f)))
    (if parts
      (str (second parts) "." (third parts))
      "")))

(define (m.y adate)
  (format "~a.~a" (month adate) (year adate)))

(define (parse-date adate)
  (match adate
    ((pregexp #px"^([0-9x]{2})\\.([0-9x]{2})\\.([0-9x]{4})$" (list _ day month year))
        (hash 'day day 'month month 'year year))
    ((pregexp #px"^([0-9x]{2})\\.([0-9x]{2})$" (list _ day month))
        (hash 'day day 'month month 'year #f))
    ((pregexp #px"^([0-9x]{2})\\.([0-9x]{4})$" (list _ month year))
        (hash 'day #f 'month month 'year year))
    (else
        (hash))))

(define-catch (vk->date adate)
  (cond
    ((or (not adate) (equal? adate "")) #f)
    (else
      (let* ((parts (split adate "."))
            (dd (car parts))
            (dd (format-number "dd" dd #:filler "0"))
            (parts (cdr parts))
            (mm (if (not-empty? parts) (car parts) #f))
            (mm (if mm (format-number "dd" mm #:filler "0") #f))
            (parts (if (not-empty? parts) (cdr parts) parts))
            (yyyy (if (not-empty? parts) (car parts) #f))
            (yyyy (if yyyy (format-number "dddd" yyyy) #f)))
        (format "~a~a~a" (if dd dd "") (if mm (str "." mm) "") (if yyyy (str "." yyyy) ""))))))

(define (day adate)
  (hash-ref (parse-date adate) 'day #f))

(define (month adate)
  (hash-ref (parse-date adate) 'month #f))

(define (year adate)
  (hash-ref (parse-date adate) 'year #f))

; for finding year ticks on the timeline:
(define (first-month? month shift)
  (= 1 (remainder (+ month shift) 12)))

(define (get-zodiac-sign adate)
  (when (not (date? adate)) (error (format "wrong date format: ~a" adate)))
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
        (adate-str adate)
        (adate (parse-date adate))
        (adate (str (d.m adate-str) ".1979"))
        )
    (cond
      ((or (date-between? Capricorn1 adate) (date-between? Capricorn2 adate)) 'Capricorn)
      ((date-between? Aquarius adate) 'Aquarius)
      ((date-between? Pisces adate) 'Pisces)
      ((date-between? Aries adate) 'Aries)
      ((date-between? Taurus adate) 'Taurus)
      ((date-between? Gemini adate) 'Gemini)
      ((date-between? Cancer adate) 'Cancer)
      ((date-between? Leo adate) 'Leo)
      ((date-between? Virgo adate) 'Virgo)
      ((date-between? Libra adate) 'Libra)
      ((date-between? Scorpio adate) 'Scorpio)
      ((date-between? Sagittarius adate) 'Sagittarius))))

(define (with-month-fullname-ru adate)
  (let* ((parsed-date (parse-date adate))
        (day ($ day parsed-date))
        (month-n (->number ($ month parsed-date)))
        (month-name (nth months-ru-full month-n))
        (month-name-gen (nth months-ru-full-genetive month-n))
        (year ($ year parsed-date))
        )
    (cond
      ((not adate) #f)
      ((not day)
        (format "~a ~a" month-name year))
      (else
        (format "~a ~a ~a" day month-name-gen year)))))

; get nymber of days from the string of following format: '\d+[dw]?'
(define-catch (days-count astr)
  (let* ((regexped (get-matches #px"(\\d+)([dw]?)" (->string astr)))
        (regexped (car regexped))
        (num (->number (second regexped)))
        (postfix (third regexped)))
    (case postfix
      (("d" "") num)
      (("w") (* 7 num)))))

(define-catch (weekday adate)
  (case (remainder (date->days adate) 7)
    ((1) 'mon)
    ((2) 'tue)
    ((3) 'wed)
    ((4) 'thu)
    ((5) 'fri)
    ((6) 'sat)
    ((0) 'sun)))

(define-catch (holiday? adate)
  (indexof? '(sat sun) (weekday adate)))

(define-catch (long-month? mon)
  (indexof? (list "01" "03" "05" "07" "08" "10" "12") mon))

(define-catch (short-month? mon)
  (indexof? (list "04" "06" "09" "11") mon))

(define (february? mon)
  (equal? "02" mon))
