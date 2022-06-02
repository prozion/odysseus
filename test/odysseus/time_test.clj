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
    (is (truthy? (d< "183x" "1831")))
    (is (truthy? (d< "183x" "1836")))
    (is (truthy? (d> "19xx" "185x")))
    ))
