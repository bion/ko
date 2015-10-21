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

(defn event-type [form]
  (cond (and (seq? form) (= '! (first form))) :mutations
        :else :client-scheduled-events))

(defn- score-reducer [score [quant events]]
  (let [grouped-events (group-by event-type events)
        {:keys [mutations client-scheduled-events]} grouped-events]

    (cond client-scheduled-events (conj score quant client-scheduled-events)
          :else score)))

(defn extract-measure [score]
  (loop [measure []
         remaining-score score]

    (let [quant (first remaining-score)
          events (second remaining-score)
          next-measure (conj measure quant events)
          next-remaining-score (-> remaining-score rest rest)
          next-item-in-score (first next-remaining-score)]
      (if (and (number? next-item-in-score) (< quant next-item-in-score))
        (recur next-measure next-remaining-score)
        [next-measure next-remaining-score]))))

(defn extract-silence [score]
  [ [-1 []] (rest score) ])

(defn extract-mutation [score]
  [nil (rest (rest score))])

(defn extract-next-measure [score]
  (cond (number? (first score)) (extract-measure score)
        (= 'silent (first score)) (extract-silence score)))

(defn if-conj [coll item]
  (if item (conj coll item) coll))

(defmacro read-score [& input-score]
  (loop [expanded-score []
         score input-score]

    (let [[measure remaining-input-score] (extract-next-measure score)
          next-expanded-score (if-conj expanded-score measure)]

      (if (empty? remaining-input-score)
        next-expanded-score
        (recur next-expanded-score remaining-input-score)))))
