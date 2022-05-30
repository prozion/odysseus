(ns odysseus.io
  (:require
            [clojure.java.io :as io]
            )
  (:use [clojure.java.io]))

; (defn pf [& body]
;   (do
;     (apply print body)
;     (flush)))

(defmacro p [& body]
  `(do
      (print ~@body)
      (flush)))

(defn read-file-by-lines [filepath]
  (with-open [f (io/reader filepath)]
    (doall (line-seq f))))

(defn write-to-file [filepath content]
  (with-open [wrtr (writer filepath)]
    (.write wrtr content)))

(defn clean-directory [directory-path]
  (map
    #(.delete %)
    (.listFiles (java.io.File. directory-path))))
