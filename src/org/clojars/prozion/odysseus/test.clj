(ns org.clojars.prozion.odysseus.test
  (:require
            [clojure.string :as s]
            ))

(defn =* [a b delta]
  (< (Math/abs (- a b)) delta))
