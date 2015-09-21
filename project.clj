(defproject ko "0.1.0-SNAPSHOT"
  :description "make music"
  :url "http://example.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [overtone "0.9.1"]]

  :main ^:skip-aot ko.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
