(defproject clooj "0.4.4"
  :description "clooj, a small IDE for clojure"
  :url "https://github.com/arthuredelstein/clooj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main clooj.core
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-inspector "0.0.12"]
                 [slamhound "1.2.0"]
                 [com.cemerick/pomegranate "0.0.11"]
                 [com.fifesoft/rsyntaxtextarea "2.0.2"]
                 [org.clojure/tools.nrepl "0.2.3"]])
