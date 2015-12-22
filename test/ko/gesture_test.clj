(ns ko.gesture-test
  [:require [overtone.core :as ot]]
  (:use midje.sweet
        ko.gesture))

(fact "keyword->symbol converts the var name to a keyword"
      (keyword->symbol :foo) => 'foo
      (keyword->symbol :bar) => 'bar
      (keyword->symbol :baz) => 'baz)

(fact "`remove-param` removes a parameter from a defsynth-style param list"
      (remove-param ['foo 1 'bar 2 'baz 3] (lazy-seq [:foo :baz])) => ['bar 2])

(fact "`envelope-binding-form` returns a vector for binding envelopes to vars"
      (envelope-binding-form
       [{:param-name :foo :envelope 1}
        {:param-name :bar :envelope 2}]) => ['foo '(env-gen 1) 'bar '(env-gen 2)])

(fact "`enveloped-param` turns mutating param and turns it into a envelope"
      (enveloped-param
       :freq
       {:measure 1 :quant 1 :timestamp 1.5 :spec {:freq 100 :amp 1}}
       [{:measure 2 :quant 2.5 :timestamp 2 :spec {:freq [300 :exp]}}
        {:measure 3 :quant 1 :timestamp 4.5 :spec {:freq [200 :exp]}}])
      => {:envelope (apply ot/envelope [[100 300 200] [0.5 2.5] [:exp :exp]])
          :param-name :freq})

(fact "`mutations->envelopes` creates envelopes from mutations"
      (let [mutations [{:measure 1 :quant 1 :timestamp 0N
                        :spec {:instr :foo :freq 200 :amp 0.2}}
                       {:timestamp 10/9 :measure 2 :quant 3
                        :spec {:freq [2200 :exp]}}]]
        (mutations->envelopes mutations) =>
        '({:envelope [200 1 -99 -99 2200 10/9 2 0]
           :param-name :freq})))

(fact "`define-synth` creates envelopes from mutaitons"
      (let [name 'foo
            params ['one 440]
            ugen-form (ot/out 0 (ot/sin-osc 'one))]
        (define-synth name params ugen-form) =>
        ()))

(def s-name 'foo)
(def params ['one 440])
(def ugen-form '(ot/out 0 (ot/sin-osc one)))

(macroexpand-1 '(define-synth s-name params ugen-form))

(define-synth s-name params ugen-form)
(def foo 1)
(def bar 2)
(defmacro add [one two]
  `(+ ~one ~two))
(eval `(add ~foo ~bar))
