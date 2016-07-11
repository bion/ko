(ns ko.gesture
  [:require [overtone.core :as ot]]
  [:use [ko.curve]
   [com.rpl.specter]
   [ko.synth-args]
   [overtone.sc.machinery.server.comms :refer [with-server-sync]]])

(defrecord Action [name action-type gesture-type func args mutator]
  clojure.lang.IFn
  (invoke [this living-gestures*] (func living-gestures* args))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args)))

(defn add-curves [action curves]
  ((:add-curves-fn action) action curves))

(defmethod print-method Action [action writer]
  (.write writer (format "#<Action[%s]: %s %s %s>"
                         (name (:action-type action))
                         (name (:name action))
                         (name (:gesture-type action))
                         (str (:args action)))))

(defonce synth-templates* (atom {}))
(defonce groups* (atom {}))

(defn remove-from-atom-map [atom-map k]
  (swap! atom-map
         (fn [atom-map-val]
           (apply dissoc (into [atom-map-val] k)))))

(defmacro ko-defsynth
  "Define an overtone synth as per usual, but also store the
  params andbody of the synth in `synth-templates*`"
  [s-name args body]
  (let [kword-s-name (keyword s-name)]
    `(do
       (ot/defsynth ~s-name ~args ~body)
       (swap! synth-templates*
              #(assoc % ~kword-s-name '(~args ~body))))))

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
        (if-not (or (ot/node-live? target-group) (ot/node-loading? target-group))
          (throw (Exception. (str "group target not alive: " target))))

        target-group)))

(defn reset-groups! []
  (doseq [group (vals @groups*)] (ot/node-free* group))
  (reset! groups* {}))

(defn register-group-async
  ([group-name]
   (register-group-async group-name (default-group) :head))

  ([group-name target]
   (register-group-async group-name target :after))

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
   (with-server-sync
     #(register-group-async group-name
                            target
                            add-action)
     (str "registering ko group " group-name))))

(defn resolve-position [position]
  (if position
    (let [[add-action group-name] (if (vector? position)
                                    position
                                    [:head position])
          existing-group (@groups* group-name)]
      (if (nil? existing-group)
        (throw (Exception. (str "no group found with name "
                                group-name))))
      [add-action existing-group])

    (default-group)))

(def begin nil) ;; clear previous definition of begin multimethod
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

(defn adjust
  "Send control messages to a running gesture.
  Messages are specified as alternating argument key value pairs"
  [g-name & g-params]
  (let [action-func (fn [living-gestures* [action-g-name action-g-params]]
                      (println (str "adjust " g-name))
                      ;; assumes nodes are stored here
                      (let [g-nodes (@living-gestures* action-g-name)]
                        (apply ot/ctl (apply conj g-nodes action-g-params))))]
    (map->Action
     {:name g-name
      :action-type :adjust
      :gesture-type :ssg
      :func action-func
      :args [g-name g-params]})))

(defn curve
  "Specify a breakpoint along a control curve.
  Curve actions are not real actions, but are used by ko to specify
  control envelopes for gestures. ko.score removes curve actions from
  the score and applies them to their begin actions."
  [g-name g-spec]
  (map->Action
   {:name g-name
    :action-type :curve
    :gesture-type :ssg-msg
    :func #(throw (Exception. "Curve actions cannot be invoked"))
    :args g-spec}))

(defn finish
  "Send end message to gestures and remove from `living-gestures*`"
  [& g-names]
  (let [action-func (fn [living-gestures* action-g-names]
                      (apply println (conj g-names "finish"))
                      (doseq [node (map @living-gestures* action-g-names)]
                        (ot/kill node))
                      (remove-from-atom-map living-gestures* action-g-names))]

    (map->Action
     {:name (apply str g-names)
      :action-type :finish
      :gesture-type :ssg
      :func action-func
      :args g-names})))

;; ________________________________________________________________
;; :ssg gesture type

(defn ssg-gest
  ([spec position] (ssg-gest spec position []))
  ([spec position curves]
   (let [instr (:instr spec)]
     (if-not instr
       (throw (Exception. (str "no instr specified in single-synth gesture"
                               " with `spec`: " spec))))

     (let [instr-name (keyword (:name instr))
           synth-args (flatten (into [] (dissoc spec :instr)))
           synth-args (resolve-synth-args synth-args)
           synth-args (conj synth-args position)
           synth-fn (if (empty? curves)
                      instr
                      (with-curves instr-name curves synth-templates*))]

       [synth-fn synth-args]))))

(defn ssg-player [g-name synth-func]
  (fn [living-gestures* action-args]
    (if (g-name @living-gestures*)
      (println (str "didn't begin "
                    g-name
                    ", key already found in living-gestures"))
      (let [g-nodes (apply synth-func action-args)]
        (println (str "playing ssg " g-name))
        (swap! living-gestures*
               (fn [lgm] (assoc lgm g-name g-nodes)))))))

(defn ssg-apply-curves [spec]
  (fn [action curves]
    (let [[synth-func synth-args] (ssg-gest spec (:position action) curves)
          g-name                  (:name action)
          func                    (ssg-player g-name synth-func)]
      (assoc action :func func :args synth-args))))

(defmethod begin :ssg
  [g-type g-name spec & remaining]
  (let [position                (resolve-position (first remaining))
        [synth-func synth-args] (ssg-gest spec position)
        action-func             (ssg-player g-name synth-func)]
    (map->Action
     {:name g-name
      :action-type :begin
      :gesture-type :ssg
      :func action-func
      :args synth-args
      :position position
      :add-curves-fn (ssg-apply-curves spec)})))

;; ________________________________________________________________
;; :msg gesture type

(defn length-of-value-collection
  "Takes a map and finds a value that is a collection and returns its length.
  Returns zero if no values are collections."
  [coll]
  (->> coll
       vals
       (filter coll?)
       first
       count))

(def map-coll-values-selector (comp-paths ALL (filterer coll?) LAST))

(defn resolve-child-args [args i]
  (try
    (compiled-transform map-coll-values-selector
                        #(nth % i)
                        args)
    (catch Exception e
      (throw
       (Exception. (str "Synth args poorly formed: " args))))))

(defn play-msg-child [{:keys [func args]}]
  (apply func args))

(defn msg-player [g-name children]
  (fn [living-gestures* _]
    (if (g-name @living-gestures*)
      (println (str "didn't begin " g-name
                    ", key already found in living-gestures"))

      (let [g-nodes (doall
                     (for [child children]
                       (play-msg-child child)))]
        (println (str "playing msg " g-name))
        (swap! living-gestures*
               (fn [lgm] (assoc lgm g-name g-nodes)))))))

(def curve-spec-attr-coll-selector
  (comp-paths ALL :spec (filterer #(coll? (second %))) ALL LAST FIRST))

(def curve-init-spec-attr-coll-selector
  (comp-paths :spec (filterer #(-> % second coll?)) ALL LAST))

(defn curves-for-child-index [curves child-index]
  (let [init-curve (transform curve-init-spec-attr-coll-selector
                              (fn [children-attr-vals]
                                (nth children-attr-vals child-index))
                              (first curves))
        rest-curves (into []
                          (transform curve-spec-attr-coll-selector
                                     (fn [children-attr-vals]
                                       (nth children-attr-vals child-index))
                                     (rest curves)))]
    (concat [init-curve] rest-curves)))

(defn msg-children
  ([g-name spec position] (msg-children g-name spec position []))
  ([g-name spec position curves]
   (if-not (:instr spec)
     (throw (Exception. (str "no instr specified in multi-synth-gesture"
                             " with `spec`: " spec))))

   (let [instr (:instr spec)
         synth-args (dissoc spec :instr)
         instr-name (keyword (:name instr))
         children-count (length-of-value-collection synth-args)
         children-count (if (= children-count 0) 1 children-count)]

     (doall
      (for [child-index (range children-count)]
        (let [child-args (resolve-child-args synth-args child-index)
              child-args (->> child-args (into []) flatten)
              child-args (resolve-synth-args child-args)
              child-args (conj child-args position)
              child-fn (if (empty? curves)
                         instr
                         (with-curves instr-name
                           (curves-for-child-index curves child-index)
                           synth-templates*))]

          {:name (keyword (str (name g-name) "-child-" child-index))
           :action-type :begin
           :gesture-type :msg-child
           :func child-fn
           :args child-args
           :position position
           :add-curves-fn (ssg-apply-curves spec)}))))))

(defn msg-apply-curves [spec]
  (fn [action curves]
    (let [g-name   (:name action)
          children (msg-children g-name spec (:position action) curves)
          func     (msg-player g-name children)]
      (assoc action :func func))))

(defmethod begin :msg
  [g-type g-name spec & remaining]
  (let [position    (resolve-position (first remaining))
        children    (msg-children g-name spec position)
        action-func (msg-player g-name children)]

    (map->Action
     {:name g-name
      :action-type :begin
      :gesture-type :msg
      :func action-func
      :args (conj (apply concat (into [] (dissoc spec :instr))) position) ;; lol
      :position position
      :add-curves-fn (msg-apply-curves spec)})))

;; ________________________________________________________________
;; :anon gesture type

(defmethod begin :anon
  [g-type spec & remaining]
  (let [position                (resolve-position (first remaining))
        [synth-func synth-args] (ssg-gest spec position)
        action-func             (fn [living-gestures* action-args]
                                  (apply synth-func action-args))]

    (map->Action
     {:name (keyword (str "anon-" (gensym)))
      :action-type :begin
      :gesture-type :ssg
      :func action-func
      :args synth-args
      :add-curves-fn #(throw (Exception. "can't mutate an :anon gesture"))})))
