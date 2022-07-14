(ns org.clojars.prozion.odysseus.stat
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [org.clojars.prozion.odysseus.debug :refer :all]
            ))

(defn average [v]
  (/ (apply + v) (count v)))

(defn median [v]
  (let [sorted-v (sort v)
        n (count v)
        i1 (quot n 2)
        i2 (inc i1)]
    (if (even? n)
      (average [(nth sorted-v i1) (nth sorted-v i2)])
      (nth sorted-v i2))))

(defn confidence-interval [v]
  (let [n (count v)
        avg (average v)
        sqr #(* % %)
        s1 (apply + (map #(sqr (- % avg)) v))
        dispersion (/ s1 (- n 1))
        sko (Math/sqrt dispersion)
        med (median v)
        left (- med sko)
        right (+ med sko)]
    [left, right]))
