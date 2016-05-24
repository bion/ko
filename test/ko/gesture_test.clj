(ns ko.gesture-test
  (:use clojure.test
        ko.gesture))

(deftest length-of-value-collection-test
  (is (= (length-of-value-collection {:one 1 :two [1 2]})
         2))
  (is (= (length-of-value-collection {:one 1 :two '(3 4 5)})
         3))
  (is (= (length-of-value-collection {:one 1 :two "two"})
         0)))

(deftest resolve-child-args-test
  (is (= (resolve-child-args {:one 1 :two [nil :answer]} 1)
         {:one 1 :two :answer})))

(deftest curves-for-child-index-test
  (let [curves [{:measure 1 :quant 1 :timestamp 1.12 :spec {:freq '(1 2) :amp 1}}
                {:measure 2 :quant 2.5 :timestamp 23.123 :spec {:freq ['(1 2) :exp]}}
                {:measure 3 :quant 2 :timestamp 2.12 :spec {:freq ['(3 4) :exp]}}]]
    (is (= (curves-for-child-index curves 0)
           [{:measure 1 :quant 1 :timestamp 1.12 :spec {:freq 1 :amp 1}}
            {:measure 2 :quant 2.5 :timestamp 23.123 :spec {:freq [1 :exp]}}
            {:measure 3 :quant 2 :timestamp 2.12 :spec {:freq [3 :exp]}}]))))
