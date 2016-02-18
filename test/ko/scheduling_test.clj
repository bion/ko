(ns ko.score-test
  (:use midje.sweet
        ko.scheduling)
  (:require [overtone.core :as ot]))

(facts "about `resolve-next-index`"
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

         (fact (resolve-next-index score 0) => 1)
         (fact (resolve-next-index score 1) => 2)
         (fact (resolve-next-index score 2) => 3)
         (fact (resolve-next-index score 3) => 0)))
