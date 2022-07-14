(ns org.clojars.prozion.odysseus.time
  (:require
            [clojure.string :as s]
            [org.clojars.prozion.odysseus.utils :refer :all]
            [org.clojars.prozion.odysseus.debug :refer :all]
            ))

(defn ->number* [x]
  "Returns exact day or month for a fuzzy date"
  (and
    x
    (-> x
      ; (s/replace #"[<>?+a-z]" "")
      str
      (s/replace #"[<>]" "")
      (s/replace "xx." "01.")
      (s/replace "0x." "01.")
      (s/replace "1x" "15")
      (s/replace "2x" "25")
      (s/replace "3x" "35")
      (s/replace "x" "5")
      (s/replace "h1" "25")
      (s/replace "h2" "75")
      (s/replace "q1" "12")
      (s/replace "q2" "37")
      (s/replace "q3" "62")
      (s/replace "q4" "87")
      ->number)))

(defn normalize-day [day]
  (cond
    (= "x" day) "xx"
    (= (count day) 1) (format "0%s" day)
    :else day))

(def normalize-month normalize-day)

(defn normalize-year [year]
  (cond
    (= "x" year) "xxxx"
    (= (count year) 2) (format "20%s" year)
    (= (count year) 3) year
    :else year))

(defn parse-date [datestr]
  (let [[match-day-month-year day1 month1 year1] (re-matches #"^([0-9x]{1,2})\.([0-9x]{1,2})\.([0-9x]{2,4})$" datestr)
        [match-day-month day2 month2] (re-matches #"^([0-9x]{1,2})\.([0-9x]{1,2})$" datestr)
        [match-month-year month3 year3] (re-matches #"^([0-9x]{1,2})\.([0-9x]{4})$" datestr)
        [match-year year4] (re-matches #"^([0-9x]{1,4})$" datestr)
        day (normalize-day (or day1 day2))
        month (normalize-month (or month1 month2 month3))
        year (normalize-year (or year1 year3 year4))]
      {:day day :month month :year year}))

(defn leap-year? [year]
  (or
    (and
      (= 0 (rem year 4))
      (not= 0 (rem year 100)))
    (= 0 (rem year 400))))

(defn valid-date? [d]
  (let [d (str d)
        d (parse-date d)
        day (->integer (:day d))
        month (->integer (:month d))
        year (->integer (:year d))]
    (cond
      (not (or day month year)) false
      ; general limits on days and months
      (or (and day (> day 31)) (and month (> month 12))) false
      ; check 31 and 30 days in month
      (and day month (= day 31) (index-of? [2 4 6 9 11] month)) false
      ; check for 29 February in a non-leap year
      (and year (not (leap-year? year)) (= 29 day) (= 2 month)) false
      ; if no checks triggered, consider d a valid date
      :else true)))

(defn compare-date [comp-op]
  (fn [date1 date2]
    (defn compare-list-neq [comp-op lst1 lst2]
      (cond
        (and (empty? lst1) (empty? lst2)) false
        (empty? lst1) (comp-op 1 (first lst2))
        (empty? lst2) (comp-op (first lst1) 1)
        (comp-op (first lst1) (first lst2)) true
        (= (first lst1) (first lst2)) (compare-list-neq comp-op (rest lst1) (rest lst2))
        :else false))
    (defn compare-list-eq [comp-op lst1 lst2]
      (cond
        (and (empty? lst1) (empty? lst2)) true
        (empty? lst1) (comp-op 1 (first lst2))
        (empty? lst2) (comp-op (first lst1) 1)
        (= (first lst1) (first lst2)) (compare-list-eq comp-op (rest lst1) (rest lst2))
        (comp-op (first lst1) (first lst2)) true
        :else false))
    (let [
          parsed1 (reverse (map ->number* (s/split date1 #"\.")))
          parsed2 (reverse (map ->number* (s/split date2 #"\.")))]
      (cond
        (ormap (fn [x] (= x comp-op)) (list > <)) (compare-list-neq comp-op parsed1 parsed2)
        (ormap (fn [x] (= x comp-op)) (list = >= <=)) (compare-list-eq comp-op parsed1 parsed2)
        :else (compare-list-neq comp-op parsed1 parsed2)))))

(def d> (compare-date >))
(def d>= (compare-date >=))
(def d< (compare-date <))
(def d<= (compare-date <=))
(def d= (compare-date =))

(defn date-between? [[start end] adate]
  (and
    (d>= adate start)
    (d<= adate end)))

(defn get-zodiac-sign [datestr]
  (when (not (valid-date? datestr)) (error (format "wrong date: %s" datestr)))
  (let [
        Capricorn1 ["01.01" "19.01"] ; Козерог
        Aquarius ["20.01" "18.02"] ; Водолей
        Pisces ["19.02" "20.03"] ; Рыбы
        Aries ["21.03" "19.04"] ; Овен
        Taurus ["20.04" "20.05"] ; Телец
        Gemini ["21.05" "20.06"] ; Близнецы
        Cancer ["21.06" "22.07"] ; Рак
        Leo ["23.07" "22.08"] ; Лев
        Virgo ["23.08" "22.09"] ; Дева
        Libra ["23.09" "22.10"] ; Весы
        Scorpio ["23.10" "21.11"] ; Скорпион
        Sagittarius ["22.11" "21.12"] ; Стрелец
        Capricorn2 ["22.12" "31.12"] ; Козерог
        {day :day month :month} (parse-date datestr)
        datestr (format "%s.%s" day month)
        ]
    (cond
      (or (date-between? Capricorn1 datestr) (date-between? Capricorn2 datestr)) :Capricorn
      (date-between? Aquarius datestr) :Aquarius
      (date-between? Pisces datestr) :Pisces
      (date-between? Aries datestr) :Aries
      (date-between? Taurus datestr) :Taurus
      (date-between? Gemini datestr) :Gemini
      (date-between? Cancer datestr) :Cancer
      (date-between? Leo datestr) :Leo
      (date-between? Virgo datestr) :Virgo
      (date-between? Libra datestr) :Libra
      (date-between? Scorpio datestr) :Scorpio
      (date-between? Sagittarius datestr) :Sagittarius
      :else nil)))
