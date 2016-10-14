(ns ko.acceptance-test
  (:use [ko.gesture]
        [ko.scheduling]
        [overtone.core]
        [ko.score] :reload))

(if (server-connected?)
  (do (stop)
      (reset-groups!))
  (boot-server))

(ko-defsynth out-synth
             [in-bus 10]
             (out:ar 0 (in:ar in-bus)))

(ko-defsynth test-synth
             [freq 1 amp 1 bus 0]
             (out bus (* (saw freq) amp)))

(ko-defsynth test-filter
             [in-bus 8 out-bus 0 cutoff 440]
             (let [sig (in in-bus)]
               (out out-bus (lpf sig cutoff))))

(def source-spec
  {:instr test-synth
   :freq 220
   :amp (db->amp -40)
   :bus "test-bus"})

(def filt-spec
  {:instr test-filter
   :in-bus "test-bus"
   :out-bus "out-bus"
   :cutoff 1000})

(register-group "source")
(register-group "filter" "source" :after)

(defn note->hz [note-name]
  (midi->hz (note note-name)))

(defn notes [& notes]
  (map note->hz notes))

(defscore test-score
  (beats-per-bar 4)
  (beats-per-minute 108)

  (label :beginning)
  1 [(begin :ssg :out {:instr out-synth :in-bus "out-bus"})
     (begin :ssg :filt filt-spec "filter")
     (begin :msg :one (assoc source-spec
                             :freq (notes :F3 :Gb3 :Bb3 :F4 :Bb4 :F5))
            "source")]

  1 [(curve :one {:freq [(notes :F4 :Gb4 :Bb4 :F5 :Bb5 :F6) :exp]})]
  2 [(curve :one {:freq [(notes :F3 :Gb3 :Bb3 :F4 :Bb4 :F5) :exp]})]

  4.99 [(curve :filt {:cutoff [10000 :exp]})
        (finish :one :filt)]
  (jump :beginning)

  1 [(begin :msg :two (assoc source-spec
                             :freq (notes :C3 :F3 :G3)
                             :bus 0))]

  (silent-measure)

  1 [(finish :two :out)])

(play-score test-score)
(stop-score test-score)
(pp-node-tree)

(clojure.pprint/pprint test-score)
(clojure.pprint/pprint (midi-connected-devices))

(event-debug-on)
(event-debug-off)

(defonce kbd-notes* (atom {}))

(on-event [:midi :note-on]
          (fn [e] (let [note (:note e)
                        vel  (:velocity e)
                        synth (test-synth :freq (midi->hz note)
                                          :amp (* 0.2 (/ vel 128)))]
                    (swap! kbd-notes* #(assoc % note synth))))
          ::kbd-note-on-handler)

(on-event [:midi :note-off]
          (fn [e]
            (let [note (:note e)
                  synth (@kbd-notes* note)]
              (kill synth)
              (swap! kbd-notes* #(assoc note nil))))
          ::kbd-note-off-handler)

(remove-event-handler ::kbd-note-on-handler)
(remove-event-handler ::kbd-note-off-handler)
