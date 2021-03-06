(defproject ko "0.1.0-SNAPSHOT"
  :description "Declarative scores for Overtone"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [overtone "0.10.1"]
                 [com.rpl/specter "0.13.0"]
                 [prismatic/schema "1.1.1"]]
  :plugins [[lein-checkouts "1.1.0"]]
  :profiles {:uberjar {:aot :all}})
