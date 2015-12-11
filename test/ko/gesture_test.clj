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
