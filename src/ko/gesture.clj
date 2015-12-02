(ns ko.gesture
  [:require [overtone.core :as ot]]
  (:gen-class))

(def ko-synth-templates (atom {}))

(defmacro ko-defsynth [s-name args body]
  (let [kword-s-name (keyword s-name)]
  `(do
     (ot/defsynth ~s-name ~args ~body)
     (swap! ko-synth-templates
            #(assoc % ~kword-s-name '(~args ~body))))))
