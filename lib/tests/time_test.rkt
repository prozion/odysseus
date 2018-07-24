#lang racket

(module+ test

  (require rackunit)
  (require "../time.rkt")
  (require "../checks.rkt")

  (check-equal? (time->seconds "05") 5)
  (check-equal? (time->seconds "45") 45)
  (check-equal? (time->seconds "2:45") 165)
  (check-equal? (time->seconds "02:45") 165)
  (check-equal? (time->seconds "3:02:45") 10965)
  (check-equal? (time->seconds "03:02:45") 10965)

  (check-equal? (seconds->minutes 165) 2)
  (check-equal? (seconds->minutes 60) 1)
  (check-equal? (seconds->minutes 10) 0)

  (check-equal? (seconds->hours 10) 0)
  (check-equal? (seconds->hours 285) 0)
  (check-equal? (seconds->hours 3600) 1)
  (check-equal? (seconds->hours 10985) 3)

  (check-equal? (seconds->days 285) 0)
  (check-equal? (seconds->days 86400) 1)
  (check-equal? (seconds->days 34560010) 400)

  (check-equal? (seconds->time 10965) "03:02:45")

  (check-equal? (time-diff "1:56:48" "4:12:50") "02:16:02")
  (check-equal? (time-diff "01:00:48" "00:12:50") "47:58")
  (check-equal? (time-diff "04:12:50" "01:56:48") "02:16:02")

  (check-true (valid-date? "31.01.1970"))
  (check-true (valid-date? "29.02.2000"))
  (check-true (valid-date? "29.02.1996"))
  (check-false (valid-date? "29.02.1997"))
  (check-false (valid-date? "35.03.1997"))
  (check-false (valid-date? "01.65.1997"))
  (check-false (valid-date? "10.13.1997"))
  (check-false (valid-date? "31.13.1997"))
  (check-false (valid-date? "31.04.2018"))

  (check-equal? (days->date 1) "01.01")
  (check-equal? (days->date 34) "03.02")
  (check-equal? (days->date 60) "01.03")
  (check-equal? (days->date 90) "31.03")
  (check-equal? (days->date 91) "01.04")
  (check-equal? (days->date 318) "14.11")
  (check-equal? (days->date 318 #:year #t) "14.11.1")
  (check-equal? (days->date 319 #:leap #t) "14.11")
  (check-equal? (days->date 60 #:leap #t) "29.02")
  (check-equal? (days->date 59 #:leap #t) "28.02")

  (check-equal? (date-diff "15.03.2017" "07.04.2017") -23)
  (check-equal? (date-diff "07.04.2017" "15.03.2017") 23)
  (check-equal? (date-diff "28.08.1979" "07.04.2017") -13737)
  (check-equal? (date-diff "29.02.2000" "28.02.2000") 1)
  (check-equal? (date-diff "29.02.2000" "01.01.2000") 59)
  (check-equal? (date-diff "31.03.2000" "01.01.2000") 90)
  (check-equal? (date-diff "31.12.2000" "01.01.2000") 365)
  (check-false (date-diff "29.02.1999" "28.02.1999"))
  (check-false (date-diff "35.03.1999" "01.02.1999"))
  (check-false (date-diff "12.03.1999" "48.02.1999"))

  (check-equal? (hours-diff
                  (hash 'year 1979 'month 3 'day 10 'hour 22 'min 0 'sec 0)
                  (hash 'year 1979 'month 3 'day 9 'hour 2 'min 0 'sec 0))
                44)

  (check-equal? (hours-diff
                  (hash 'year 1979 'month 3 'day 10 'hour 22 'min 0 'sec 0)
                  (hash 'year 1979 'month 3 'day 8 'hour 2 'min 0 'sec 0))
                68)

  (check-equal? (hours-diff
                  (hash 'year 2018 'month 7 'day 1 'hour 2 'min 0 'sec 0)
                  (hash 'year 2018 'month 6 'day 30 'hour 22 'min 0 'sec 0))
                4)

  (check-equal? (hours-diff
                  (hash 'year 1979 'month 3 'day 10 'hour 22 'min 0 'sec 0)
                  (hash 'year 1979 'month 3 'day 10 'hour 20 'min 10 'sec 30))
                2)

  (check-equal? (date-diff-abs "15.03.2017" "07.04.2017") 23)
  (check-equal? (date-diff-abs "07.04.2017" "15.03.2017") 23)
  (check-equal? (date-diff-abs "28.08.1979" "07.04.2017") 13737)

  (check-equal? (month-diff "03.2017" "04.2017") 1)
  (check-equal? (month-diff "03.2016" "04.2017") 13)
  (check-equal? (month-diff "08.2016" "04.2017") 8)
  (check-equal? (month-diff "08.1979" "04.2017") 452)

  (check-false (d> "28.08.1979" "07.04.2017"))
  (check-true (d< "28.08.1979" "07.04.2017"))

  (check-true (d> "07.04.2017" "28.08.1979"))
  (check-false (d< "07.04.2017" "28.08.1979"))

  (check-false (d= "07.04.2017" "28.08.1979"))
  (check-true (d= "7.04.2017" "07.04.2017"))

  (check-true (d>= "01.10.1985" "07.12.1978"))
  (check-false (d<= "01.10.1985" "07.12.1978"))
  (check-true (d>= "01.10.1985" "01.10.1985"))

  (check-false (d>= "01.10.1985" "07.12.1998"))
  (check-true (d<= "01.10.1985" "07.12.1998"))
  (check-true (d<= "01.10.1985" "01.10.1985"))

  (check-equal? (d+ "01.10.1985" 1) "02.10.1985")
  (check-equal? (d+ "01.10.1985" 3) "04.10.1985")
  (check-equal? (d+ "01.10.1985" 50) "20.11.1985")
  (check-equal? (d+ "30.11.1985" 37) "06.01.1986")

  (check-equal? (d.m "07.04.2017") "07.04")
  (check-equal? (d.m "06.06.198x") "06.06")
  (check-equal? (d.m "06.10") "06.10")

  ;(check-equal? (datetime-diff "15.03.2017 1:56:48" "07.04.2017 4:12:50") ...)

  (check-equal? (day "03.11.1965") "03")
  (check-equal? (month "03.11.1965") "11")
  (check-equal? (year "03.11.1965") "1965")

  (check-true (first-month? 12 1))
  (check-true (first-month? 1 0))
  (check-true (first-month? 5 8))
  (check-true (first-month? 17 8))
  (check-false (first-month? 13 1))
  (check-false (first-month? 5 3))

  (check-true (date? "28.08.1979"))
  (check-true (date? "28.08.xxxx"))
  (check-true (date? "28.08"))
  ; (check-true (date? "8.8"))
  ; (check-true (date? "29.5"))
  ; (check-true (date? "1.5.1991"))
  (check-true (date? "08.1979"))
  (check-true (date? "08.19xx"))
  (check-false (date? "1979"))
  (check-false (date? ""))
  (check-false (date? "28.1979"))
  (check-false (date? 3))
  (check-false (date? #f))

  (check-true (precise-date? "28.08.1979"))
  (check-true (precise-date? "28.08.1979?"))
  (check-true (precise-date? '28.08.1979))
  (check-false (precise-date? "2x.08.1979"))
  (check-false (precise-date? "10.xx.1979"))
  (check-false (precise-date? "xx.10.1979"))


  (check-hash-equal? (parse-date "28.08.1979") (hash 'day "28" 'month "08" 'year "1979"))
  (check-hash-equal? (parse-date "28.08.xxxx") (hash 'day "28" 'month "08" 'year "xxxx"))

  (check-equal? (vk->date "16.6.1964") "16.06.1964")
  (check-equal? (vk->date "15.11.1965") "15.11.1965")
  (check-equal? (vk->date "7.7.1985") "07.07.1985")
  (check-equal? (vk->date "29.6") "29.06")
  (check-equal? (vk->date "2.9") "02.09")
  (check-equal? (vk->date "1.11") "01.11")
  (check-equal? (vk->date "10.1") "10.01")

  (check-true (date-between? (cons "01.10.1989" "10.11.1991") "10.06.1990"))
  (check-true (date-between? (cons "01.10.1989" "10.11.1991") "25.12.1990"))
  (check-false (date-between? (cons "01.10.1989" "10.11.1991") "17.07.1987"))

  (check-equal? (get-zodiac-sign "28.08.1979") 'Virgo)
  (check-equal? (get-zodiac-sign "21.01.1979") 'Aquarius)
  (check-equal? (get-zodiac-sign "11.10.2001") 'Libra)
  (check-equal? (get-zodiac-sign "01.04") 'Aries)

  ; (check-= (get-time-length "4") 4)
  (check-equal? (days-count "4d") 4)
  (check-equal? (days-count "4w") 28)
  (check-equal? (days-count "1w") 7)

  (check-equal? (weekday "15.06.2018") 'fri)
  (check-equal? (weekday "24.06.1990") 'sun)
  (check-equal? (weekday "29.02.2000") 'tue)

  (check-true (holiday? "24.06.1990"))
  (check-false (holiday? "29.02.2000"))
)
