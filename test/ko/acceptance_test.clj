(ns ko.acceptance-test
  (:require [overtone.core :as ot] :reload)
  (:use [ko.gesture]
        [ko.scheduling]
        [ko.score :only (defscore)] :reload))

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

(ko-defsynth bark-delay-test
             [bus 0 freq 220 amp 0.5 delay-scale 1.0 delay-width 1.0]
             (let [src (* (ot/sin-osc freq) amp)
                   src (bark-delay src 1.0 1.0 delay-width delay-scale)]
               (ot/out bus src)))

(def source-spec {:instr test-synth
                  :freq 220
                  :amp -6
                  :bus "test-bus"})

(def filt-spec {:instr test-filter
                :in-bus "test-bus"
                :out-bus 0
                :cutoff 100})

(register-group "source")
(register-group "filter" "source" :after)

(defscore test-score
  beats-per-bar 4
  beats-per-minute 108

  1 [(begin :ssg :g-one (merge source-spec {:freq 440}) "source")
     (begin :ssg :filt filt-spec "filter")]

  silent

  1 [(! :filt {:cutoff [10000 :exp]})]
  3 [(finish :g-one :filt)])

(clojure.pprint/pprint test-score)
(play-score test-score)
(ot/pp-node-tree)
(ot/stop)

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

(overtone.libs.event/remove-event-handler ::kbd-note-on-handler)
