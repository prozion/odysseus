(ns org.clojars.prozion.odysseus.utils
  (:require [clojure.set :as set]
            [clojure.string :as s]
            ))

(defn truthy? [val]
  (if val true false))

(defn falsy? [val]
  (not (truthy? val)))

(defn xor [a b]
  (or
    (and (truthy? a) (falsy? b))
    (and (falsy? a) (truthy? b))
    false))

(defn index-of? [coll el]
  (and
    coll
    (not (empty? coll))
    (.contains coll el)))

(defn ->int [x]
  (Integer. (str x)))

(defn ->integer [astr]
  (try
    (let [only-digits (s/join "" (filter #(re-seq #"[0-9]" (str %)) astr))]
      (Integer. only-digits))
    (catch Throwable e nil)))

(defn map-map [f m]
  (into (empty m) (map f m)))

(defn filter-map [f m]
  (into (empty m) (map f (filter f m))))

(defn minus [coll1 coll2]
  (into (empty coll1) (set/difference (set coll1) (set coll2))))

(defn symbol-split [sym]
  (map symbol (clojure.string/split (str sym) #"\.")))

(defn unique-concat [seq1 seq2]
  ; (into (empty seq1) (set/union (set seq1) (set seq2))))
  (concat
    seq1
    (remove
      #(index-of? seq1 %)
      seq2)))

(defn sort-by-order [seq given-order-v]
  (->> given-order-v (filter #(index-of? seq %)) (#(concat % (minus seq given-order-v)))))
