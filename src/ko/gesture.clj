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
              #(assoc % ~kword-s-name ['~args '~body])))))

(defn with-mutations [synth mutations]
  (let [[s-args s-body] ((var->keyword synth) ko-synth-templates)
        s-name (str (var->string synth) "-" (gensym))
        s-form (conj s-body s-args)
        [s-name params ugen-form] (ot/synth-form s-name s-form)]
    (prn s-name params ugen-form)))
