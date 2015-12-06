(ns ko.gesture
  [:require [overtone.core :as ot]]
  (:gen-class))

(def ko-synth-templates (atom {}))

(defmacro var->keyword [var]
  (keyword var))

(defmacro var->string [var]
  (str var))

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

    (let [s-name (symbol (str synth-template-name "-" (gensym)))
          [s-name params ugen-form] (ot/synth-form s-name s-template)]

      `(ot/synth ~s-name ~params ~ugen-form))))

(ko-defsynth test-synth
             [one 1 two 2]
             (ot/out 0 0))

(@ko-synth-templates :test-synth)

(with-mutations :test-synth nil)
