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
             (let [sig (ot/in in-bus)]
               (ot/out out-bus (ot/lpf sig cutoff))))

(def source-spec {:instr test-synth
                  :freq 220
                  :amp -12
                  :bus "test-bus"})

(def filt-spec {:instr test-filter
                :in-bus "test-bus"
                :out-bus 0
                :cutoff 1000})

(register-group "source")
(register-group "filter" "source" :after)

(defn note->hz [note-name]
  (ot/midi->hz (ot/note note-name)))

(defn notes [& notes]
  (map note->hz notes))

(defscore test-score
  (beats-per-bar 4)
  (beats-per-minute 108)

  1 [(begin :ssg :filt filt-spec "filter")
     (begin :msg :one (assoc source-spec
                             :freq (notes :F3 :Gb3 :Bb3 :F4 :Bb4 :F5))
            "source")]

  1 [(curve :one {:freq [(notes :F4 :Gb4 :Bb4 :F5 :Bb5 :F6) :exp]})]
  2 [(curve :one {:freq [(notes :F3 :Gb3 :Bb3 :F4 :Bb4 :F5) :exp]})]
  3 [(curve :filt {:cutoff [10000 :exp]})
     (finish :one :filt)])

test-score
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
