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

(defn ** [a x]
  (cond
    (= x 0) 1
    :else (* a (** a (- x 1)))))

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
  (let [astr (str astr)]
    (try
      (let [only-digits (s/join "" (filter #(re-seq #"[0-9]" (str %)) astr))]
        (Integer. only-digits))
      (catch Throwable e nil))))

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

(defn remove-hash [f m]
  (filter-hash #(-> % f not) m))

(defn revert-hash [m]
  (zipmap (vals m) (keys m)))

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

(defn only-or-first [v]
  (if (coll? v)
    (if (empty? v)
      nil
      (first v))
    v))

(defn rotate [coll & [n]]
  (let [n (or n 1)]
    ; (concat (drop n coll) (take n coll))))
    (cond
      (< n 1) coll
      (= n 1) `(~@(rest coll) ~(first coll))
      :else (rotate (rotate coll 1) (- n 1)))))

(defn orf [& args]
  "OR to use in the places where function required (such as in `map`, `filter` etc.)"
  (cond
    (empty? args) false
    (first args) (first args)
    :else (apply orf (rest args))))

(defn andf [& args]
  "AND to use in the places where function required (such as in `map`, `filter` etc.)"
  (cond
    (empty? args) true
    (empty? (rest args)) (first args)
    (not (first args)) false
    :else (apply andf (rest args))))

(defn not-empty? [v]
  (not (empty? v)))

(defn one-element? [coll]
  (= (count coll) 1))

(defn choose-not-empty [& args]
  (cond
    (empty? args) nil
    (not-empty? (first args)) (first args)
    :else (apply choose-not-empty (rest args))))

(defn include-to-coll [pred & pairs]
  (cond
    (empty? pairs) []
    :else
      (let [[pred-value result-value] (first pairs)]
        (if (pred pred-value)
          (concat [result-value] (apply include-to-coll pred (rest pairs)))
          (concat [] (apply include-to-coll pred (rest pairs)))))))

(defn insert [coll index el]
  (into (empty coll) (concat (take index coll) [el] (drop index coll))))

(defn disj-coll [coll el]
  (into (empty coll) (disj (set coll) el)))
