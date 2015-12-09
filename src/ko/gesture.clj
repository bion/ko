(ns ko.gesture
  [:require [overtone.core :as ot]]
  (:gen-class))

(def ko-synth-templates (atom {}))
(def keyword->symbol #(symbol (str (name %))))

(defmacro ko-defsynth
  "Define an overtone synth as per usual, but also store the
  params andbody of the synth in `ko-synth-templates`"
  [s-name args body]
  (let [kword-s-name (keyword s-name)]
    `(do
       (ot/defsynth ~s-name ~args ~body)
       (swap! ko-synth-templates
              #(assoc % ~kword-s-name '(~args ~body))))))

(defn remove-param [param-list removed-param-names]
  (let [param-map (hash-map param-list)
        remaining-params (clojure.set/difference (set (keys param-map))
                                                 (set removed-param-names))]
    (loop [params (transient [])
           add-params remaining-params]
      (cond (empty? add-params)
            (persistent! params)

            :else
            (let [param-key (first add-params)
                  param-val (param-key param-map)]
              (conj! params param-key)
              (conj! params param-val)
              (recur params (rest add-params)))))))

(defn enveloped-param
  "creates an envelope for a given mutating param"
  [param initial snapshots]
  (let [start-time (:timestamp initial)
        levels (transient [(->> initial :spec param)])
        durs (transient [])
        curves (transient [])]
    (loop [remaining-snapshots snapshots]
      (if (empty? remaining-snapshots)
        {:param-name param
         :envelope (map persistent! [levels durs curves])}
        (let [snapshot (first remaining-snapshots)
              [level curve] (->> snapshot :spec param)
              dur (- (:timestamp snapshot) start-time)]
          (conj! levels level)
          (conj! durs dur)
          (conj! curves curve)
          (recur (rest remaining-snapshots)))))))

(defn mutations->envelopes
  "converts a vector of gesture states into a vector of hashes
  of the form {:param-name \"some param\" :envelope envelope}}"
  [gesture-events]
  (let [param-keys (->> gesture-events first :spec keys)
        initial (first gesture-events)
        mutation-events (rest gesture-events)]
    (for [param param-keys
          :let [param-snapshots (filter #(param (% :spec)) mutation-events)]
          :when (not (empty? param-snapshots))]
      (enveloped-param param initial param-snapshots))))

(defn envelope-binding-form
  "given a collection of envelopes, returns a vector "
  [envelopes]
  (-> (map #([(keyword->symbol (:param-name %)) (conj () (:envelope %) 'env-gen)])
           envelopes)
      flatten
      vec))

(defn apply-mutations
  "for each mutating param, removes the param from the arglist and
  injects an envelope in its place"
  [s-template mutations]
  (let [envelopes (mutations->envelopes mutations)
        envelope-bindings (envelope-binding-form envelopes)
        param-list (remove-param (first s-template) (map :param-name envelopes))
        new-ugen-forms (conj () (last s-template) envelope-bindings 'let)
        new-template (conj () new-ugen-forms param-list)]
    new-template))

(defmacro with-mutations
  "Returns a function that plays the given synth with mutations applied"
  [synth-template-name mutations]
  (let [s-template (synth-template-name @ko-synth-templates)]
    (if-not s-template
      (throw (Exception. (str "no synth template found for: " synth-template-name))))

    (let [s-name (symbol (str (name synth-template-name) "-" (gensym)))
          s-template (apply-mutations s-template mutations)
          [s-name params ugen-form] (ot/synth-form s-name s-template)]

      `(ot/synth ~s-name ~params ~ugen-form))))
