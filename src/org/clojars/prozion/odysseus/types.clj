(ns org.clojars.prozion.odysseus.types
  (:require
            [org.clojars.prozion.odysseus.debug :refer :all]
            ))

(defn scalar? [v]
  (not (coll? v)))
