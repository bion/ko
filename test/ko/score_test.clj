(ns ko.score-test
  (:use clojure.test
        ko.score
        ko.gesture
        ko.scheduling)
  (:require [overtone.core :as ot]))

(if-not (ot/server-connected?)
  (ot/boot-server))

(ko-defsynth test-synth
             [arg 1 arg-2 2]
             (ot/out 0 (ot/sin-osc:ar 220)))

(def test-parsed-score
  [{1 [(begin :ssg :my-gesture-name {:instr test-synth, :freq 200})
       (begin :ssg :other-gesture-name {:instr test-synth, :freq 400})]}])

(def test-curves
  {:my-gesture-name
   [{:measure 1, :quant 1, :timestamp 0N,
     :spec [:spec {:instr test-synth, :freq 200}]}
    {:spec {:freq [220 :exp], :amp [0.1 :exp]}, :timestamp 10/9}]})

(comment
  (deftest zip-curves-test
    (is (= (zip-curves test-parsed-score test-curves)
           [{1 ['(begin :ssg :my-gesture-name {:instr test-synth, :freq 200}
                        [{:measure 1, :quant 1, :timestamp 0N,
                          :spec [:spec {:instr test-synth, :freq 200}]}
                         {:spec {:freq [220 :exp], :amp [0.1 :exp]},
                          :timestamp 10/9}])
                '(begin :ssg :other-gesture-name {:instr test-synth, :freq 400} [])]}]))))

(deftest filter-empty-curves-test
  (let [curves {:one [1 2 3] :two [1] :three [1 2]}]
    (is (= (filter-empty-curves curves)
           {:one [1 2 3] :three [1 2]}))))

(comment
  (deftest parse-score-test
    (binding [*beats-per-minute* (atom 60)
              *beats-per-bar* (atom 4)
              *jump-data* (atom {:labels {} :jumps {}})]

      (let [[actions curves jumps]
            (parse-score
             '(beats-per-bar
               4
               beats-per-minute 108

               label :one
               1 [(begin :ssg :my-gesture-name {:instr test-synth :freq 200})
                  (begin :ssg :other-gesture-name {:instr test-synth :freq 400})]

               jump-to :one
               3 [(curve :my-gesture-name {:freq [220 :exp] :amp [0.1 :exp]})]))
            expected-actions [{1
                               [(begin :ssg :other-gesture-name
                                       {:freq 400, :instr test-synth})
                                (begin :ssg :my-gesture-name
                                       {:freq 200, :instr test-synth})]}]
            expected-curves {:my-gesture-name
                             [{:measure 1,
                               :quant 1,
                               :spec {:freq 200, :instr test-synth},
                               :timestamp 0N}
                              {:measure 2,
                               :quant 3,
                               :spec {:amp [0.1 :exp], :freq [220 :exp]},
                               :timestamp 10/3}],
                             :other-gesture-name
                             [{:measure 1,
                               :quant 1,
                               :spec {:freq 400, :instr test-synth},
                               :timestamp 0N}]}
            expected-jumps {:jumps
                            {1 {:label :one :should-jump? fn?}}
                            :labels
                            {:one 0}}]
        (are [x y] (= x y)
          actions expected-actions
          curves expected-curves
          jumps expected-jumps)))))

(deftest true-for-n-test
  (let [instance (true-for-n 2)]
    (are [x y] (= x y)
      (instance) true
      (instance) true
      (instance) false)))
