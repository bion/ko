(ns ko.synth-args-test
  [:require [overtone.core :as ot]]
  (:use clojure.test
        ko.synth-args))

(deftest resolve-synth-args-test
  "resolves note keywords to HZ and negative numbers to amps"
  (is (= (resolve-synth-args [:freq :Eb4
                              :amp -12])
         [:freq 311.1269837220809
          :amp 0.251188643150958])))
