(ns ko.score-test
  (:use midje.sweet
        ko.scheduling
        ko.gesture
        ko.score
        ko.util
        ko.nrt)
  (:require [overtone.core :as ot]))

;; fixture generated with sclang code:
;; x = [
;; [0.0, [ \s_new, \sine, 1000, 0, 0, \freq, 1413 ]]
;; [0.1, [ \s_new, \sine, 1001, 0, 0, \freq, 712 ]]
;; [0.2, [ \s_new, \sine, 1002, 0, 0, \freq, 417 ]]
;; [0.3, [ \s_new, \sine, 1003, 0, 0, \freq, 1238 ]]
;; [0.4, [ \s_new, \sine, 1004, 0, 0, \freq, 996 ]]
;; [3.0, [\c_set, 0, 0]] ];
;; Score.write(x, "Users/bion/dev/ko/test/ko/fixtures/test_score.osc");

(init-nrt)
(teardown-nrt)

(ko-defsynth sine [freq 110]
             (ot/out 0 (ot/sin-osc freq)))

(defscore test-score
  beats-per-bar 4
  beats-per-minute 120
  1 [(begin :ssg :foo {:instr sine :freq 1413})]
  2 [(finish :foo)])

(nrt-write-score-to-osc test-score "nrt_test.osc")

(def fixture (read-binary-file "test/ko/fixtures/test_score.osc"))
(def nrt-result (read-binary-file "/tmp/ko_nrt_test.osc"))

(facts "about NRT"
       (fact (.equals nrt-result fixture) => true))
