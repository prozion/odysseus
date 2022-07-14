(ns org.clojars.prozion.odysseus.io
  (:require
            [clojure.java.io :as io]
            [org.clojars.prozion.odysseus.debug :refer :all]
            [clojure.data.csv :as csv]
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

(defn read-file [filepath]
  (slurp filepath))

(defn read-file-by-lines [filepath]
  (with-open [f (io/reader filepath)]
    (doall (line-seq f))))

(defn write-to-file [filepath content]
  (with-open [w (writer filepath)]
    (.write w content)))

(defn create-directory [dirpath]
  (.mkdir (java.io.File. dirpath)))

(defn clean-directory [directory-path]
  (map
    #(.delete %)
    (.listFiles (java.io.File. directory-path))))

(defn file-exists? [filepath]
  (.exists (java.io.File. filepath)))

(defn copy-from-url [url file]
  (with-open [in (io/input-stream url)
              out (io/output-stream file)]
    (io/copy in out)))

(defn list-files [dirpath]
  (map
    #(.getName %)
    (.listFiles (java.io.File. dirpath))))

(defn get-cwd []
  (-> (java.io.File. ".") .getAbsolutePath))

(defn read-tsv [filepath]
  (with-open [reader (io/reader filepath)]
    (rest
      (doall
        (csv/read-csv reader :separator \tab)))))
