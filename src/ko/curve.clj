(ns ko.curve
  [:use
   [ko.synth-args]
   [overtone.core]])

(def keyword->symbol #(symbol (str (name %))))

(defn var->keyword [item]
  (keyword (name item)))

(defn define-synth [s-name params ugen-form]
  (eval `(synth ~s-name ~params ~ugen-form)))

(defn remove-param [param-list removed-param-names]
  (let [param-map (apply hash-map param-list)
        removed-params-symbols (map keyword->symbol removed-param-names)
        remaining-params (clojure.set/difference (set (keys param-map))
                                                 (set removed-params-symbols))]
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
  "creates an envelope record (not an overtone.core/envelope)
  for a given mutating param"
  [param initial snapshots]
  (let [start-time (:timestamp initial)
        levels (transient [(->> initial :spec param)])
        durs (transient [])
        curves (transient [])]

    (loop [remaining-snapshots snapshots
           last-timestamp start-time]

      (if (empty? remaining-snapshots)
        ;; return the envelope
        (let [[levels durs curves] (map persistent! [levels durs curves])
              levels (map resolve-synth-arg levels)]
          {:param-name param
           :envelope (envelope levels durs curves)})

        ;; keep recurring
        (let [snapshot (first remaining-snapshots)
              _ (if-not (coll? (->> snapshot :spec param))
                  (throw (Exception.
                          (str "synth params for `curve` actions must "
                               "be a tuple containing the new value and "
                               "a curve. instead param " param " is: "
                               (->> snapshot :spec param)))))
              [level curve] (->> snapshot :spec param)
              timestamp (:timestamp snapshot)
              dur (- timestamp last-timestamp)]
          (conj! levels level)
          (conj! durs dur)
          (conj! curves curve)
          (recur (rest remaining-snapshots) timestamp))))))

(defn- get-param-keys [g-events]
  (remove #{:instr} (->> g-events first :spec (into {}) keys)))

(defn curves->envelopes
  "converts a vector of gesture states into a vector of hashes
  of the form {:param-name \"some param\" :envelope envelope}}"
  [gesture-events]
  (let [param-keys (get-param-keys gesture-events)
        initial (first gesture-events)
        curve-events (rest gesture-events)]
    (for [param param-keys
          :let [param-snapshots (filter #(param (% :spec)) curve-events)]
          :when (not (empty? param-snapshots))]
      (enveloped-param param initial param-snapshots))))

(defn envelope-binding-form
  "given a collection of envelopes, returns a vector "
  [envelopes]
  (apply concat
         (vec
          (map #(vec
                 [(keyword->symbol (:param-name %))
                  (conj () (:envelope %) 'env-gen)])
               envelopes))))

(defn apply-curves
  "for each mutating param, removes the param from the arglist and
  injects an envelope in its place"
  [s-template curves]
  (let [envelopes (curves->envelopes curves)
        envelope-bindings (envelope-binding-form envelopes)
        param-list (remove-param (first s-template) (map :param-name envelopes))
        new-ugen-forms (conj () (last s-template) (vec envelope-bindings) 'let)
        new-template (conj () new-ugen-forms param-list)]
    new-template))

(def with-curves-implementation
  (memoize
   (fn [instr-name curves templates*]
     (let [s-template (instr-name @templates*)]
       (if-not s-template
         (throw (Exception. (str "no synth template found for: " instr-name))))

       (let [s-name (symbol (str (name instr-name) "-" (gensym)))
             s-template (apply-curves s-template curves)
             [s-name params ugen-form] (synth-form s-name s-template)]
         (define-synth s-name params ugen-form))))))

(defn with-curves
  "Returns a function that plays the given synth with curves applied"
  [instr-name curves templates*]
  (with-curves-implementation instr-name curves templates*))
