(ns ko.gesture
  [:require [overtone.core :as ot]]
  (:gen-class))

(def ko-synth-templates (atom {}))
(def keyword->symbol #(symbol (str (name %))))
(defn var->keyword [item]
  (keyword (name item)))

(defn resolve-synth-arg [arg]
  (cond (= clojure.lang.Keyword (type arg))
        (ot/midi->hz (ot/note arg))

        (>= 0 arg)
        (ot/db->amp arg)

        :default
        arg))

(defn resolve-synth-args [args]
  (flatten (map (fn [[param-name param-val]]
                  [param-name (resolve-synth-arg param-val)])
                (partition 2 args))))

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
           :envelope (apply ot/envelope levels durs curves)})

        ;; keep recurring
        (let [snapshot (first remaining-snapshots)
              [level curve] (->> snapshot :spec param)
              timestamp (:timestamp snapshot)
              dur (- timestamp last-timestamp)]
          (conj! levels level)
          (conj! durs dur)
          (conj! curves curve)
          (recur (rest remaining-snapshots) timestamp))))))

(defn- get-param-keys [g-events]
  (remove #{:instr} (->> g-events first :spec keys)))

(defn mutations->envelopes
  "converts a vector of gesture states into a vector of hashes
  of the form {:param-name \"some param\" :envelope envelope}}"
  [gesture-events]
  (let [param-keys (get-param-keys gesture-events)
        initial (first gesture-events)
        mutation-events (rest gesture-events)]
    (for [param param-keys
          :let [param-snapshots (filter #(param (% :spec)) mutation-events)]
          :when (not (empty? param-snapshots))]
      (enveloped-param param initial param-snapshots))))

(defn envelope-binding-form
  "given a collection of envelopes, returns a vector "
  [envelopes]
  (apply concat
         (vec
          (map #(vec
                 [(keyword->symbol (:param-name %))
                  (conj () (:envelope %) 'ot/env-gen)])
               envelopes))))

(defn apply-mutations
  "for each mutating param, removes the param from the arglist and
  injects an envelope in its place"
  [s-template mutations]
  (let [envelopes (mutations->envelopes mutations)
        envelope-bindings (envelope-binding-form envelopes)
        param-list (remove-param (first s-template) (map :param-name envelopes))
        new-ugen-forms (conj () (last s-template) (vec envelope-bindings) 'let)
        new-template (conj () new-ugen-forms param-list)]
    new-template))

(defn define-synth [s-name params ugen-form]
  (eval `(ot/synth ~s-name ~params ~ugen-form)))

(defn with-mutations
  "Returns a function that plays the given synth with mutations applied"
  [instr-name mutations]
  (let [s-template (instr-name @ko-synth-templates)]
    (if-not s-template
      (throw (Exception. (str "no synth template found for: " instr-name))))

    (let [s-name (symbol (str (name instr-name) "-" (gensym)))
          s-template (apply-mutations s-template mutations)
          [s-name params ugen-form] (ot/synth-form s-name s-template)]
      (define-synth s-name params ugen-form))))

;; returns a function that when called begins playing the gesture
;; and returns a representation of the playing gesture
(defn ssg-gest
  [spec mutations]
  (let [instr (:instr spec)]
    (if-not instr
      (throw (Exception. (str "no instr specified in `ssg` gesture"
                              " with `spec`: " spec))))

    (let [instr-name (keyword (:name instr))
          synth-args (flatten (into [] (dissoc spec :instr)))
          synth-args (resolve-synth-args synth-args)
          synth-fn (if mutations
                     (with-mutations instr-name mutations)
                     instr)]
      #(apply synth-fn synth-args))))
