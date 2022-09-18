(ns org.clojars.prozion.odysseus.text
  (:require
            [clojure.string :as s]
            ))

(defn remove-urls [text]
  (->
    text
    (s/replace #"https?://\S+" "")
    (s/replace #"\s+" " ")))

(defn make-markdown-link [text url]
  (let [url (if (s/starts-with? url "http") url (format "https://%s" url))]
    (format "[%s](%s)" text url)))

(defn make-html-link [text url]
  (let [url (if (s/starts-with? url "http") url (format "https://%s" url))]
    (format "<a href=\"%s\">%s</a>" url text)))

(defn boldify [txt]
  (and txt (format "<b>%s</b>" txt)))

(defn titlefy [txt]
  (str (s/upper-case (subs txt 0 1)) (subs txt 1)))

(defn colons [txt]
  (and txt (str txt ": ")))

(defn ->str [txt]
  (if txt (str txt) ""))

(defn dupstr [s n]
  (clojure.string/join
    ""
    (map (fn [_] s) (range n))))

(defn url? [s]
  (re-seq #"^https?://" (str s)))