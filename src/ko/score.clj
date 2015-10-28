(ns ko.score
  (:use ko.scheduling)
  (:require [clojure.core.match :refer [match]])
  (:gen-class))

;; (defn a-gesture
;;   ([freq amp] (a-gesture amp (gensym)))
;;   ([freq amp g-name]
;;    '[begin ssg g-name {:instr sin-synth :freq freq :amp amp :action 0}]))

;; (defn start-stop
;;   ([freq amp] (start-stop freq amp (gensym)))
;;   ([freq amp g-name]
;;    {:name g-name
;;     :begin [begin ssg g-name {:instr sin-synth :freq freq :amp amp :action 0}]
;;     :end [finish g-name]}))

(comment
  {:gesture-name
   {:begin-state {:freq 200 :amp 1}
    :mutations [{:measure 1 :quant 2.5 :spec {:freq 300 :curve :exp}}
                {:measure 3 :quant 1 :spec {:freq 200 :curve :exp}}]}})

(defn gesture-record [g-spec]
  {:begin-state g-spec :mutations []})

(deftrace record-begin-events [measure-num quant begin-events mutations]
  (if (empty? begin-events)
    mutations
    (reduce (fn [memo event]
              (let [g-name (second event)
                    g-spec (second (nth event 2))]
                (merge memo {g-name (gesture-record g-spec)})))
            mutations
            begin-events)))

(deftrace record-mutations [measure-num quant mutations-events mutations]
  (if (empty? mutations-events)
    mutations
    (reduce (fn [memo event]
              (let [g-name (:name event)
                    gesture (mutations g-name)]
                (if-not gesture
                  (throw (Exception.
                          (str "No gesture found for `!` (mutation): " g-name))))
                (update-in mutations [g-name :mutations] conj event)))
            mutations
            (map second mutations-events))))

(defn event-type [form]
  (cond (seq? form)
        (cond (= '! (first form)) :mutation-events
              (= 'begin (first form)) :begin-events
              :else :basic-scheduled-events)
        :else (throw (Exception.
                      (str "Unrecognized event " form)))))

(defn extract-measure [score measure-num mutations]
  (loop [measure []
         remaining-score score
         mutations-acc mutations]

    (let [quant (first remaining-score)
          {:keys [basic-scheduled-events
                  mutation-events
                  begin-events]} (group-by event-type (second remaining-score))
          scheduled-events (apply conj basic-scheduled-events begin-events)
          next-remaining-score (-> remaining-score rest rest)
          next-item-in-score (first next-remaining-score)
          next-measure (if (empty? scheduled-events)
                         measure (conj measure quant (into [] scheduled-events)))
          next-mutations (record-mutations measure-num
                                           quant
                                           mutation-events
                                           (record-begin-events measure-num
                                                                quant
                                                                begin-events
                                                                mutations-acc))]
      (cond
        (and (number? next-item-in-score) (< quant next-item-in-score))
        (recur next-measure next-remaining-score next-mutations)

        :else [next-measure next-remaining-score next-mutations]))))

(defn extract-silence [score]
  [ [0 []] (rest score) {} ])

(defn extract-next-measure [score measure-num mutations]
  (cond (number? (first score)) (extract-measure score measure-num mutations)
        (= 'silent (first score)) (extract-silence score)
        :else
        (throw
         (Exception.
          (str "Unrecognized input around measure " measure-num ": " score)))))

(defn parse-score [input-score]
  (loop [expanded-score []
         mutations {}
         score input-score
         measure-num 1]

    (let [[measure
           remaining-input-score
           this-measure-mutations] (extract-next-measure score
                                                         measure-num
                                                         mutations)
          next-expanded-score (conj expanded-score measure)
          next-mutations (merge mutations this-measure-mutations)]

      (if (empty? remaining-input-score)
        [next-expanded-score next-mutations]
        (recur next-expanded-score
               next-mutations
               remaining-input-score
               (inc measure-num))))))

(defmacro read-score [& input-score]
  (if (empty? input-score)
    []
    (parse-score input-score)))
