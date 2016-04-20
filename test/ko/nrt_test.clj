(ns ko.score-test
  (:use clojure.test
        ko.scheduling)
  (:require [overtone.core :as ot]))

;; [ 0.2, [\s_new, \NRTsine, 1001, 0, 0]].asRawOSC;
;; -> Int8Array[ 35, 98, 117, 110, 100, 108, 101, 0, 0, 0, 0, 0, 51, 51, 51, 51, 0, 0, 0, 36, 47, 115, 95, 110, 101, 119, 0, 0, 44, 115, 105, 105, 105, 0, 0, 0, 78, 82, 84, 115, 105, 110, 101, 0, 0, 0, 3, -23, 0, 0, 0, 0, 0, 0, 0, 0 ];

(def input [ 0.2, ["s_new", :NRTsine, 1001, 0, 0]])
(def result [35 98 117 110 100 108 101 0 0 0 0 0 51 51 51 51 0 0 0 36 47 115 95 110 101 119 0 0 44 115 105 105 105 0 0 0 78 82 84 115 105 110 101 0 0 0 3 -23 0 0 0 0 0 0 0 0])
