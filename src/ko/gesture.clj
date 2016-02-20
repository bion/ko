(ns ko.gesture
  [:require [overtone.core :as ot]]
  (:gen-class))

(defonce living-gestures* (atom {}))
(defonce groups* (atom {}))
(defonce synth-templates* (atom {}))
(defonce busses* (atom {}))

(def keyword->symbol #(symbol (str (name %))))

(defn remove-from-atom-map [atom-map k]
  (swap! atom-map
         (fn [atom-map-val]
           (apply dissoc (into [atom-map-val] k)))))

(defn var->keyword [item]
  (keyword (name item)))

(defn add-bus [bus-name]
  (let [new-bus (ot/audio-bus)]
    (swap! busses*
           #(assoc % bus-name new-bus))
    new-bus))

(defn resolve-synth-arg [arg]
  (cond (= clojure.lang.Keyword (type arg))
        (ot/midi->hz (ot/note arg))

        (and (= java.lang.String (type arg))
             (re-matches #".*-bus$" arg))
        (or (@busses* arg) (add-bus arg))

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
  params andbody of the synth in `synth-templates*`"
  [s-name args body]
  (let [kword-s-name (keyword s-name)]
    `(do
       (ot/defsynth ~s-name ~args ~body)
       (swap! synth-templates*
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
           :envelope (ot/envelope levels durs curves)})

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
  (let [s-template (instr-name @synth-templates*)]
    (if-not s-template
      (throw (Exception. (str "no synth template found for: " instr-name))))

    (let [s-name (symbol (str (name instr-name) "-" (gensym)))
          s-template (apply-mutations s-template mutations)
          [s-name params ugen-form] (ot/synth-form s-name s-template)]
      (define-synth s-name params ugen-form))))

(defn- resolve-optional-ssg-begin-args [args]
  (let [arg-count (count args)]
    (cond (= 2 arg-count)
          [(second args) (first args)]

          (= 1 arg-count)
          [(first args) nil])))

(defn- default-group []
  (ot/foundation-default-group))

(defn- default-group-position []
  [:after (default-group)])

(defn- default-synth-position []
  [:head (default-group)])

(defn group? [obj]
  (= overtone.sc.node.SynthGroup (type obj)))

(defn- resolve-group-target [target]
  (if (group? target) target
      (let [target-group (@groups* target)]
        (if-not (group? target-group)
          (throw (Exception. (str "unrecognized group target: " target))))
        (if-not (ot/node-live? target-group)
          (throw (Exception. (str "group target not alive: " target))))

        target-group)))

(defn reset-groups! []
  (doseq [group (vals @groups*)] (ot/node-free* group))
  (reset! groups* {}))

;; TODO
;; make `register-groups` func that takes a tree with group
;; names as leafs and makes it real.
;; e.g. (register-groups [:first-group [:first-child :second-child]
;;                       :adjacent-to-first-group])
(defn register-group
  "Registers a new group, allowing it to be referenced by
  name as a position for `begin` events. If a target is
  provided, can be either a `group` or a name of an already
  referenced group."
  ([group-name]
   (register-group group-name (default-group) :head))

  ([group-name target]
   (register-group group-name target :after))

  ([group-name target add-action]
   (let [existing-group (@groups* group-name)]
     (if (and (ot/node? existing-group) (ot/node-live? existing-group))
       (throw (Exception. (str "group already registered with name: "
                               group-name)))))

   (let [resolved-target (resolve-group-target target)
         new-group (ot/group group-name add-action resolved-target)]
     (swap! groups* #(assoc % group-name new-group))
     (println (str "registered group: " group-name))
     new-group)))

(defn- resolve-position [position]
  (let [[add-action group-name] (if (vector? position)
                                  position
                                  [:head position])
        existing-group (@groups* group-name)]
    (if (nil? existing-group)
      (throw (Exception. (str "no group found with name "
                              group-name))))
    [add-action existing-group]))

;; returns a function that when called begins playing the gesture
;; and returns a representation of the playing gesture
(defn ssg-gest
  [spec position mutations]
  (let [instr (:instr spec)]
    (if-not instr
      (throw (Exception. (str "no instr specified in `ssg` gesture"
                              " with `spec`: " spec))))

    (let [instr-name (keyword (:name instr))
          synth-args (flatten (into [] (dissoc spec :instr)))
          synth-args (resolve-synth-args synth-args)
          synth-args (conj synth-args position)
          synth-fn (if (empty? mutations)
                     instr
                     (with-mutations instr-name mutations))]
      #(apply synth-fn synth-args))))

;; clear previous definition of multimethod
(def begin nil)
(defmulti begin
  "Begin playing a gesture. `begin` events can take one
  of several types.

  Single-synth Gestures (ssg) take the form

  (begin :ssg :gesture-name spec)

  where `spec` must be a map containing a :instr key specifying
  a ko-synthdef along with all other params to the synth. Spec
  can itself be a map, a var referring to a map, or form that when
  evaluated returns a map."
  (fn [type & args]
    type))

(defmethod begin :ssg
  [g-type g-name spec & remaining]
  (let [[mutations position] (resolve-optional-ssg-begin-args remaining)
        position (if position (resolve-position position)
                     (default-group))
        g-instance (ssg-gest spec position mutations)]
    #(do
       (println (str "playing " g-name))
       (if (g-name @living-gestures*)
         (println "didn't begin" g-name ", key already found in living-gestures")
         (let [g-nodes (g-instance)]
           (swap! living-gestures*
                  (fn [lgm] (assoc lgm g-name g-nodes))))))))

(defn adjust
  "Send control messages to a running gesture.
  Messages are specified as alternating argument key value pairs"
  [g-name & rest]
  #(do
     (println (str "adjust " g-name))
     ;; assumes nodes are stored here
     (let [g-nodes (@living-gestures* g-name)]
       (apply ot/ctl (apply conj g-nodes rest)))))

(defn finish
  "Send end message to gestures and remove from `living-gestures*`"
  [& g-names]
  #(do
     (println "finish " g-names)
     (doseq [node (map @living-gestures* g-names)]
       (ot/kill node))
     (remove-from-atom-map living-gestures* g-names)))
