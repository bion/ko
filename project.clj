(defproject ko "0.1.0-SNAPSHOT"
  :description "make music"
  :url "http://example.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [overtone "0.9.1"]
                 [midje "1.8-alpha1" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-midje "3.2-RC4"]]
  :main ^:skip-aot ko.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
