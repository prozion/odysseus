(ns org.clojars.prozion.odysseus.files
  (:require
            [clojure.string :as s]
            [org.clojars.prozion.odysseus.utils :refer :all]
            ))

(defn get-file-extension [path]
  (some-> path (s/split #"\.") last))

(defn absolute-path? [astr]
  (->> astr (re-seq #"^(/|[A-Z]:)") truthy?))

(defn ext-file? [re-extension]
  (fn [path]
    (truthy?
      (re-seq
        (re-pattern
          (str "\\." (str re-extension) "$"))
        path))))

(def tabtree-file? (ext-file? #"m?tree"))
(def ttl-file? (ext-file? #"ttl"))
