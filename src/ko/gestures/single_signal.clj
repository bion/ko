(ns ko.gestures.single_signal
  [:use
   [ko.synth_defs.single_signal]
   [ko.gesture]
   [ko.scheduling]]
  [:require
   [overtone.core :as ot]]
  (:gen-class))

(deftype SingleSynthGesture [g-name spec node-vec-atom]
  IGesture
  (g-nodes [this] @node-vec-atom)

  (g-start [this gesture-map-atom]
    (let [synth-args (flatten (into [] (dissoc spec :instr)))
          synth-id (apply (:instr spec) synth-args)]
      (swap! node-vec-atom conj synth-id)
      (swap! gesture-map-atom assoc g-name this)))

  (g-ctl [this spec]
    (let [target (@node-vec-atom g-name)]
      (apply ot/ctl (conj spec target))))

  (g-end [this gesture-map-atom]
    (let [target (g-nodes this)]
      (ot/kill target)
      (remove-from-atom-map gesture-map-atom g-name))))

(defn ssg [g-name spec gesture-map-atom]
  (let [g-instance (BasicGesture. g-name spec (atom []))]
    (swap! gesture-map-atom #(assoc % g-name g-instance))))

(apply zipmap (reduce (fn [acc next]
          [(conj (first acc) (first next))
           (conj (second acc) (second next))])
        [[] []]
        (partition 2 [:one 2 :three 4])))
