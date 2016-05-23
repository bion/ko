(ns ko.acceptance-test
  (:require [overtone.core :as ot] :reload)
  (:use [ko.gesture]
        [ko.scheduling]
        [ko.score] :reload))

(if (ot/server-connected?)
  (do (ot/stop)
      (reset-groups!))
  (ot/boot-server))

(ko-defsynth test-synth
             [freq 1 amp 1 bus 0]
             (ot/out bus (* (ot/saw freq) amp)))

(ko-defsynth test-filter
             [in-bus 8 out-bus 0 cutoff 440]
             (ot/out out-bus (ot/lpf (ot/in in-bus) cutoff)))

(def source-spec {:instr test-synth
                  :freq 220
                  :amp -12
                  :bus "test-bus"})

(def filt-spec {:instr test-filter
                :in-bus "test-bus"
                :out-bus 0
                :cutoff 200})

(register-group "source")
(register-group "filter" "source" :after)

(defn gest-begin [name freq]
  (begin :ssg name (assoc source-spec :freq freq) "source"))

(defn gest-curve [name freq]
  (let [freq (ot/midi->hz (ot/note freq))]
    (curve name {:freq [freq :exp]})))

(defscore test-score
  (beats-per-bar 4)
  (beats-per-minute 108)

  1 [
     (gest-begin :one :F4)
     (gest-begin :two :Gb4)
     (gest-begin :thr :Bb4)
     (gest-begin :fou :F5)
     (gest-begin :fiv :Bb5)
     (gest-begin :six :F6)
     (begin :ssg :filt filt-spec "filter")]

  1 [(gest-curve :one :F3)
     (gest-curve :two :Gb3)
     (gest-curve :thr :Bb3)
     (gest-curve :fou :F4)
     (gest-curve :fiv :Bb4)
     (gest-curve :six :F5)
     (curve :filt {:cutoff [10000 :exp]})]
  3 [(finish :one :two :thr :fou :fiv :six :filt)])

(play-score test-score)
(stop-score test-score)

(ot/pp-node-tree)

(clojure.pprint/pprint test-score)
(clojure.pprint/pprint (ot/midi-connected-devices))

(ot/event-debug-on)
(ot/event-debug-off)

(defonce kbd-notes* (atom {}))

(ot/on-event [:midi :note-on]
             (fn [e] (let [note (:note e)
                           vel  (:velocity e)
                           synth (test-synth :freq (ot/midi->hz note)
                                             :amp (* 0.2 (/ vel 128)))]
                       (swap! kbd-notes* #(assoc % note synth))))
             ::kbd-note-on-handler)

(ot/on-event [:midi :note-off]
             (fn [e]
               (let [note (:note e)
                     synth (@kbd-notes* note)]
                 (ot/kill synth)
                 (swap! kbd-notes* #(assoc note nil))))
             ::kbd-note-off-handler)

(ot/remove-event-handler ::kbd-note-on-handler)
(ot/remove-event-handler ::kbd-note-off-handler)
