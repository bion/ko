(ns ko.core-test
  (:use midje.sweet)
  (:require [ko.core :refer :all]))

(facts "about `quant-to-timestamp`"
       (fact
        (quant-to-timestamp 1.0 0 0.5) => 0.0)
       (fact
        (quant-to-timestamp 1.5 1000 0.5) => 1250.0)
       (fact
        (quant-to-timestamp 4.5 5000 0.5) => 6750.0))
