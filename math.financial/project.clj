(defproject org.soulspace.clj/math.financial "0.7.0"
  :description "The math.financial library contains financial math functions in Clojure."
  :url "https://github.com/lsolbach/CljMath"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  
  ; use deps.edn dependencies
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}
  
;  :dependencies [[org.clojure/clojure "1.11.1"]
;                 [org.soulspace.clj/math.core "0.8.0"]]

  :test-paths ["test"]
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]])
