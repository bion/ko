(ns ko.synth-args
  [:require [overtone.core :as ot]])

(defonce busses* (atom {}))

(defn add-bus [bus-name]
  (let [new-bus (ot/audio-bus 2)]
    (swap! busses*
           #(assoc % bus-name new-bus))
    new-bus))

(defn resolve-synth-arg [arg]
  (cond (= clojure.lang.Keyword (type arg))
        (ot/midi->hz (ot/note arg))

        (and (= java.lang.String (type arg))
             (re-matches #".*-bus$" arg))
        (or (@busses* arg) (add-bus arg))

        :default
        arg))

(defn resolve-synth-args [args]
  (flatten (map (fn [[param-name param-val]]
                  [param-name (resolve-synth-arg param-val)])
                (partition 2 args))))
