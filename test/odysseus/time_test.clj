(ns odysseus.time-test
  (:require [clojure.test :refer :all]
            [org.clojars.prozion.odysseus.utils :refer [truthy? falsy?]]
            [org.clojars.prozion.odysseus.time :refer :all]
            ))

(deftest time-unit-tests
  (testing "time comparison"
    (is (falsy? (d> "28.08.1979" "07.04.2017")))
    (is (truthy? (d< "28.08.1979" "07.04.2017")))

    (is (truthy? (d> "07.04.2017" "28.08.1979")))
    (is (falsy? (d< "07.04.2017" "28.08.1979")))

    (is (falsy? (d= "07.04.2017" "28.08.1979")))
    (is (truthy? (d= "7.04.2017" "07.04.2017")))

    (is (truthy? (d>= "01.10.1985" "07.12.1978")))
    (is (falsy? (d<= "01.10.1985" "07.12.1978")))
    (is (truthy? (d>= "01.10.1985" "01.10.1985")))

    ; latin and cyrillic dot?
    (is (truthy? (d<= "01.03.2020" "01.12.2020")))
    (is (truthy? (d<= "01.03.2020" "01.12.2020")))
    (is (truthy? (d<= "01.03.2020" "01.12.2020")))
    (is (truthy? (d<= "01.03.2020" "01.12.2020")))

    (is (falsy? (d>= "01.10.1985" "07.12.1998")))
    (is (truthy? (d<= "01.10.1985" "07.12.1998")))
    (is (truthy? (d<= "xx.10.1985" "07.12.1998")))
    (is (truthy? (d<= "01.10.1985" "01.10.1985")))

    ; years comparison
    (is (truthy? (d<= "1785" "1829")))
    (is (truthy? (d> "1785" "1630")))
    (is (truthy? (d> "1785" "05.11.1630")))
    (is (truthy? (d>= "1785" "05.11.1630")))
    (is (truthy? (d= "1836" "1836")))

    ; check shortcuts
    (is (truthy? (d> "183x" "1831")))
    (is (truthy? (d< "183x" "1836")))
    (is (truthy? (d> "19xx" "185x"))))

  (testing "date parsing"
    (is (= (parse-date "28.08.1979") {:day "28" :month "08" :year "1979"}))
    (is (= (parse-date "08.1979") {:day nil :month "08" :year "1979"}))
    (is (= (parse-date "1979") {:day nil :month nil :year "1979"}))
    (is (= (parse-date "10.20") {:day "10" :month "20" :year nil}))
    (is (= (parse-date "10.12") {:day "10" :month "12" :year nil}))
    (is (= (parse-date "28.08.xxxx") {:day "28" :month "08" :year "xxxx"})))

  (testing "date validation"
    (is (truthy? (date? "19.4")))
    (is (truthy? (valid-date? "31.01.1970")))
    (is (truthy? (valid-date? "29.02.2000")))
    (is (truthy? (valid-date? "29.02.1996")))
    (is (falsy? (valid-date? "29.02.1997")))
    (is (falsy? (valid-date? "35.03.1997")))
    (is (falsy? (valid-date? "01.65.1997")))
    (is (falsy? (valid-date? "10.13.1997")))
    (is (falsy? (valid-date? "31.13.1997")))
    (is (falsy? (valid-date? "31.04.2018"))))

  (testing "check date between"
    (is (truthy? (date-between? ["01.10.1989" "10.11.1991"] "10.06.1990")))
    (is (truthy? (date-between? ["01.10.1989" "10.11.1991"] "25.12.1990")))
    (is (falsy? (date-between? ["01.10.1989" "10.11.1991"] "17.07.1987")))
    (is (falsy? (date-between? ["01.01.1979" "19.01.1979"] "01.04.1979"))))

  (testing "detect Zodiac sign"
    (is (= (get-zodiac-sign "28.08.1979") :Virgo))
    (is (= (get-zodiac-sign "21.01.1979") :Aquarius))
    (is (= (get-zodiac-sign "11.10.2001") :Libra))
    (is (= (get-zodiac-sign "25.4.1995") :Taurus))
    (is (= (get-zodiac-sign "01.04") :Aries)))
)
