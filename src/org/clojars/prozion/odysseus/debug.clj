(ns org.clojars.prozion.odysseus.debug
    (:require [clojure.string :as s]))

(def --- println)

(defn ---- [coll]
  (print (s/join "\n" coll)))

(defn stop [& [msg]]
  (println (or msg ">"))
  (read-line))

(defn hard-exit
  ([] (hard-exit 0))
  ([exit-code] (System/exit exit-code)))

(defn soft-exit
  ([] (soft-exit "soft-exit"))
  ([message]
    (throw (Exception. message))))

(def error soft-exit)

(def exit soft-exit)
