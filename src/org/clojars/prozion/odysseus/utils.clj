(ns org.clojars.prozion.odysseus.utils
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [org.clojars.prozion.odysseus.debug :refer :all]
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

(defn ->number [x]
  (let [sx (str x)
        float? (re-seq #"\." sx)]
    (cond
      float? (Double. sx)
      :else (Integer. sx))))

(defn ->int [x]
  (Integer. (str x)))

(defn ->integer [astr]
  (try
    (let [only-digits (s/join "" (filter #(re-seq #"[0-9]" (str %)) astr))]
      (Integer. only-digits))
    (catch Throwable e nil)))

(defn minus [coll & colls]
  (reduce
    (fn [acc next-coll]
      (into (empty coll) (set/difference (set acc) (set next-coll))))
    coll
    colls))

(defn symbol-split [sym]
  (map symbol (clojure.string/split (str sym) #"\.")))

(defn map-map [f m]
  (into (empty m) (map f m)))

(defn filter-map [f m]
  (into (empty m) (filter f m)))

(def map-hash map-map)
(def filter-hash filter-map)

; (defn ormap [f & args]
;   (cond
;     (empty? (first args)) false
;     (not (coll? (first args)))
;       (if (f (first args))
;           true
;           (ormap f (rest args)))
;     (not (apply f (map first args))) (apply ormap f (map rest args))
;     :else true))

; suggested by Sergey Trofimov:
; (defn ormap
;   [f & colls]
;   (->> (apply map f colls)
;        (reduce (fn [_ x] (when x (reduced x))) nil)))

; suggested by Sergey Trofimov, v2:
(defn ormap
  [f & colls]
  (some truthy? (apply map f colls)))

; or even so:
; (defn ormap
;   ([f c]
;    (some identity (map f c)))
;   ([f c1 c2]
;    (some identity (map f c1 c2)))
;   ([f c1 c2 c3]
;    (some identity (map f c1 c2 c3)))
;   ([f c1 c2 c3 & colls]
;    (some identity (apply map f c1 c2 c3 colls))))

(defn unique-concat [seq1 seq2]
  ; (into (empty seq1) (set/union (set seq1) (set seq2))))
  (concat
    seq1
    (remove
      #(index-of? seq1 %)
      seq2)))

(defn sort-by-order [seq given-order-v]
  (->> given-order-v (filter #(index-of? seq %)) (#(concat % (minus seq given-order-v)))))
