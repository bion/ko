(ns ko.score-test
  (:use clojure.test
        ko.scheduling)
  (:require [overtone.core :as ot]))

(deftest resolve-next-index-test
  (let [score [{0 "blah"}
               {1 "haha"}
               {2 "haha"}
               {3 "haha"}
               {4 "weee"}]
        score (with-meta score {:labels {:one 0}
                                :jumps {3 {:label :one
                                           :should-jump? #(do false)}
                                        4 {:label :one
                                           :should-jump? #(do true)}}})]
    (are [x y] (= x y)

      (resolve-next-index score 0) 1
      (resolve-next-index score 1) 2
      (resolve-next-index score 2) 3
      (resolve-next-index score 3) 0)))

(deftest quant-to-timestamp-test
  (are [x y] (= x y)
    (quant-to-timestamp 1.0 0 0.5) 0.0
    (quant-to-timestamp 1.5 1000 0.5) 1250.0
    (quant-to-timestamp 4.5 5000 0.5) 6750.0))
