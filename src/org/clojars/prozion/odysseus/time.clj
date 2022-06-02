(ns org.clojars.prozion.odysseus.time
  (:require
            [clojure.string :as s]
            [org.clojars.prozion.odysseus.utils :refer :all]
            ))

(defn ->number* [x]
    (-> x
      ; (s/replace #"[<>?+a-z]" "")
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
      ->number))

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
