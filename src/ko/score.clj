(ns ko.score
  (:use ko.scheduling)
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

(def ^:dynamic *mutations*)

{:gesture-name
 {:begin-state {:freq 200 :amp 1}
  :mutations [{:measure 1 :quant 2.5 :spec {:freq 300 :curve :exp}}
              {:measure 3 :quant 1 :spec {:freq 200 :curve :exp}}]}}

(defn gesture-record [g-spec]
  {:begin-state g-spec :mutations []})

(defn record-begin-events [measure-num quant begin-events]
  (doseq [event begin-events]
    (let [g-name (second event)
          g-spec (second (nth event 2))]

      (swap! *mutations* assoc g-name (gesture-record g-spec)))))

(defn record-mutations [measure-num quant mutations-events]
  (doseq [mutation (map second mutations-events)]
    (let [g-name (:name mutation)
          gesture (@*mutations* g-name)]

      (if-not gesture
        (throw (Exception.
                (str "No gesture found for `!` (mutation): " g-name))))

      (swap! *mutations* update-in [g-name :mutations] conj mutation))))

(defn event-type [form]
  (cond (seq? form)
        (cond (= '! (first form)) :mutations
              (= 'begin (first form)) :begin-events
              :else :basic-scheduled-events)

        :else (throw (Exception.
                      (str "Unrecognized event " form)))))

(defn extract-measure [score measure-num]
  (loop [measure []
         remaining-score score]

    (let [quant (first remaining-score)
          events (second remaining-score)
          {:keys [basic-scheduled-events
                  mutations
                  begin-events]} (group-by event-type events)
          scheduled-events (apply conj basic-scheduled-events begin-events)
          next-remaining-score (-> remaining-score rest rest)
          next-item-in-score (first next-remaining-score)
          next-measure (if (empty? basic-scheduled-events)
                         measure (conj measure quant basic-scheduled-events))]

      (if-not (empty? begin-events) (record-begin-events measure-num quant begin-events))
      (if-not (empty? mutations) (record-mutations measure-num quant mutations))

      (cond
        (and (number? next-item-in-score) (< quant next-item-in-score))
        (recur next-measure next-remaining-score)

        :else [next-measure next-remaining-score]))))

(defn extract-silence [score]
  [ [0 []] (rest score) ])

(defn extract-next-measure [score measure-num]
  (cond (number? (first score)) (extract-measure score measure-num)
        (= 'silent (first score)) (extract-silence score)
        :else
        (throw
         (Exception.
          (str "Unrecognized input around measure " measure-num ": " score)))))

(defn parse-score [input-score]
  (binding [*mutations* (atom {})]
    (loop [expanded-score []
           score input-score
           measure-num 1]

      (let [[measure remaining-input-score] (extract-next-measure score measure-num)
            next-expanded-score (conj expanded-score measure)]

        (if (empty? remaining-input-score)
          next-expanded-score
          (recur next-expanded-score remaining-input-score (inc measure-num)))))))

(defmacro read-score [& input-score]
  (if (empty? input-score)
    []
    (parse-score input-score)))
