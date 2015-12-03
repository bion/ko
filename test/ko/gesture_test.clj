(ns ko.gesture-test
  [:require [overtone.core :as ot]]
  (:use midje.sweet
        ko.gesture))

(fact "var->keyword converts the var name to a keyword"
      (var->keyword foo) => :foo
      (var->keyword bar) => :bar
      (var->keyword baz) => :baz)

(fact "var->string converts the var name to a string"
      (var->string foo) => "foo"
      (var->string bar) => "bar"
      (var->string baz) => "baz")

(facts "about `ko-defsynth`"
       (against-background
        [(before :facts (reset! ko-synth-templates {}))
         (after :facts (ns-unmap *ns* 'test-synth))]

        (fact "it adds the body of the synth to the ko-synth-templates atom"
              (do
                (ko-defsynth test-synth
                             [one 1 two 2]
                             (ot/out 0 0))
                @ko-synth-templates) => {:test-synth ['[one 1 two 2]
                                                      '(ot/out 0 0)]})))
