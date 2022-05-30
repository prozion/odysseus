(defproject org.clojars.prozion/odysseus "0.0.1.110.2"
  :description "Clojure descendant of Racket odysseus library - common lisp functions to help in the work"
  :url "https://github.com/prozion/odysseus"
  :license {:name "MIT License"
            :url  "https://github.com/aws/mit-0"}
  :dependencies [
                [org.apache.jena/jena-arq "3.2.0"]
                [org.apache.jena/jena-iri "3.2.0"]
                [org.apache.jena/jena-tdb "3.2.0"]
                [org.slf4j/slf4j-api "1.7.5"]
                [org.slf4j/slf4j-simple "1.6.4"]
                ]
  :plugins [
            ; [lein-ancient "0.6.15"]
            ]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0"]]}}
  :deploy-repositories [["releases"
                            {
                              :url  "https://repo.clojars.org"
                              :creds :gpg
                             }]]
  :release-tasks [
                  ["deploy"]]
  :repl-options {
    ; :init-ns org.clojars.prozion.tabtree.rdf
    :init-ns org.clojars.prozion.odysseus.utils
  }
)
