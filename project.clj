(defproject ko "0.1.0-SNAPSHOT"
  :description "Declarative scores for Overtone"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [overtone "0.9.1"]
                 [byte-streams "0.2.1"]
                 [midje "1.8-alpha1" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.trace "0.7.9"]]
  :plugins [[lein-midje "3.2-RC4"]
            [lein-checkouts "1.1.0"]]
  :profiles {:uberjar {:aot :all}})
