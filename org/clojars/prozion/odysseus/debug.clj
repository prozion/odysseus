(ns odysseus.debug)

(def --- println)

(defn stop [& [msg]]
  (println (or msg ">"))
  (read-line))
