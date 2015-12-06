(ns ko.gesture
  [:require [overtone.core :as ot]]
  (:gen-class))

(def ko-synth-templates (atom {}))

(defmacro ko-defsynth
  "Define an overtone synth as per usual, but also store the
  params andbody of the synth in `ko-synth-templates`"
  [s-name args body]
  (let [kword-s-name (keyword s-name)]
    `(do
       (ot/defsynth ~s-name ~args ~body)
       (swap! ko-synth-templates
              #(assoc % ~kword-s-name '(~args ~body))))))

(defmacro with-mutations
  "Returns a function that plays the given synth with mutations applied"
  [synth-template-name mutations]
  (let [s-template (synth-template-name @ko-synth-templates)]
    (if-not s-template
      (throw (Exception. (str "no synth template found for: " synth-template-name))))

    (let [s-name (symbol (str (name synth-template-name) "-" (gensym)))
          [s-name params ugen-form] (ot/synth-form s-name s-template)]

      `(ot/synth ~s-name ~params ~ugen-form))))

(defn enveloped-param [param initial snapshots]
  (let [bus (ot/audio-bus 1 (str (name param) (gensym)))
        start-time (:timestamp initial)
        levels (transient [(->> initial :spec param)])
        durs (transient [])
        curves (transient [])]
    (loop [remaining-snapshots snapshots]
      (if (empty? remaining-snapshots)
        {:param-name param
         :bus bus
         :envelope (map persistent! [levels durs curves])}
        (let [snapshot (first remaining-snapshots)
              [level curve] (->> snapshot :spec param)
              dur (- (:timestamp snapshot) start-time)]
          (conj! levels level)
          (conj! durs dur)
          (conj! curves curve)
          (recur (rest remaining-snapshots)))))))

(defn creates-envelopes-from-mutations
  "converts a vector of gesture states into a hash of the form
  {:param-name envelope}"
  [gesture-events]
  (let [param-keys (->> gesture-events first :spec keys)
        initial (first gesture-events)
        mutation-events (rest gesture-events)]
    (for [param param-keys
          :let [param-snapshots (filter #(param (% :spec)) mutation-events)]
          :when (not (empty? param-snapshots))]
      (enveloped-param param initial param-snapshots))))

(ko-defsynth test-synth
             [freq 1]
             (ot/out 0 (ot/sin-osc freq)))

(with-mutations :test-synth [])
