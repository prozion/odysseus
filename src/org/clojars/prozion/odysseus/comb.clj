(ns org.clojars.prozion.odysseus.comb
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [org.clojars.prozion.odysseus.utils :refer :all]
            [org.clojars.prozion.odysseus.debug :refer :all]
            ))

(defn permutations [es]
  (cond
    (empty? es) es
    (one-element? es) [es]
    :else
          (reduce
            (fn [acc e]
              (concat
                acc
                (map
                  #(cons e %)
                  (permutations (disj-coll es e)))))
            []
            es)))

; перестановки
(defn ! [n]
  (cond
    (= n 0)
      1
    (and (> n 16) (not= (type n) java.math.BigInteger))
      (! (biginteger n))
    :else
      (* n (! (- n 1)))))

; упорядоченная с возвращением
; (** n k)

; упорядоченная без возвращения
(defn Akn [k n]
  (let [result (/ (! n) (! (- n k)))]
    (cond
      (and
          (= (type result) clojure.lang.BigInt)
          (< result Long/MAX_VALUE))
        (long result)
      :else
        result)))

; неупорядоченная без возвращения
(defn Ckn [k n]
  (/ (Akn k n) (! k)))

; неупорядоченная с возвращением
(defn Ck-1n+k-1 [k n]
  (Ckn (+ k -1) (+ n k -1)))
