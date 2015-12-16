(ns ko.score
  (:require [clojure.core.match :refer [match]])
  (:use ko.scheduling)
  (:gen-class))

(def ^:dynamic *beats-per-bar* (atom nil))
(def ^:dynamic *beats-per-minute* (atom nil))

(defn gesture-record
  "begin event must specify inital state for all mutations
  {:gesture-name
     ;; first one is beginning of gesture
     [{:measure 1 :quant 1 :timestamp 1.12 :spec {:instr foo :freq 200 :amp 1}}
      {:measure 2 :quant 2.5 :timestamp 23.123 :spec {:freq [300 :exp]}}
      {:measure 3 :quant 1 :timestamp 43.12 :spec {:freq [200 :exp]}}]}"
  [g-spec measure-num quant timestamp]
  [{:measure measure-num :quant quant :timestamp timestamp :spec g-spec}])

(defn record-begin-events [measure-num quant begin-events mutations timestamp]
  (if (empty? begin-events)
    mutations
    (reduce (fn [memo event]
              (let [g-name (second event)
                    g-spec (second (nth event 2))]
                (merge memo {g-name (gesture-record g-spec
                                                    measure-num
                                                    quant
                                                    timestamp)})))
            mutations
            begin-events)))

(defn record-mutations [measure-num quant mutations-events mutations timestamp]
  (if (empty? mutations-events)
    mutations
    (reduce (fn [memo event]
              (let [g-name (:name event)
                    event-record (assoc event :timestamp timestamp)
                    gesture (mutations g-name)]
                (if-not gesture
                  (throw (Exception.
                          (str "No gesture found for `!` (mutation): " g-name))))
                (update-in mutations [g-name] conj event-record)))
            mutations
            (map second mutations-events))))

(defn event-type [form]
  (cond (seq? form)
        (cond (= '! (first form)) :mutation-events
              (= 'begin (first form)) :begin-events
              :else :basic-scheduled-events)
        :else (throw (Exception.
                      (str "Unrecognized event " form)))))

(defn- beat-dur []
  (/ 60 @*beats-per-minute*))

(defn- quant->duration [quant]
  (* (beat-dur) (- quant 1)))

(defn- inc-measure-timestamp [timestamp]
  (+ timestamp (* (beat-dur) @*beats-per-bar*)))

(defn extract-measure [score measure-num mutations measure-timestamp]
  (loop [measure {}
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
                         measure (assoc measure quant (into [] scheduled-events)))

          quant-timestamp (+ measure-timestamp (quant->duration quant))
          next-mutations (record-begin-events measure-num
                                              quant
                                              begin-events
                                              mutations-acc
                                              quant-timestamp)
          next-mutations (record-mutations measure-num
                                           quant
                                           mutation-events
                                           next-mutations
                                           quant-timestamp)]

      (cond
        (and (number? next-item-in-score) (< quant next-item-in-score))
        (recur next-measure next-remaining-score next-mutations)

        :else [next-measure
               next-remaining-score
               next-mutations
               (inc-measure-timestamp measure-timestamp)]))))

(defn- set-global [global]
  (fn [remaining-score expanded-score mutations measure-num measure-timestamp]
    (let [new-val (second remaining-score)
          next-remaining-score (-> remaining-score rest rest)]
      (reset! global new-val)
      [next-remaining-score expanded-score mutations measure-num measure-timestamp])))

(def token-handlers
  ;; can-handle? => handle pairs
  ;; handler params [score expanded-score mutations measure-num timestamp]
  ;; returns [next-remaining-score next-expanded-score next-mutations next-measure-num next-timestamp]

  ;; normal measure handler
  {#(number? %)
   (fn [remaining-score expanded-score mutations measure-num time]
     (let [[next-measure
            next-remaining-score
            next-mutations] (extract-measure remaining-score measure-num mutations time)

           next-expanded-score (conj expanded-score next-measure)]
       [next-remaining-score next-expanded-score next-mutations (inc measure-num) time]))

   ;; insert one measure of silence
   #(= 'silent %)
   (fn [remaining-score expanded-score mutations measure-num timestamp]
     (let [next-measure [0 []]
           next-remaining-score (rest remaining-score)
           next-expanded-score (conj expanded-score
                                     next-measure)
           next-timestamp (inc-measure-timestamp timestamp)]
       [next-remaining-score
        next-expanded-score
        mutations
        (inc measure-num)
        next-timestamp]))

   ;; time signature
   #(= 'set-beats-per-bar %)
   (set-global *beats-per-bar*)

   ;; tempo
   #(= 'set-beats-per-minute %)
   (set-global *beats-per-minute*)})

(defn resolve-handler [score measure-num]
  (let [next-token (first score)
        handler (last (first (filter
                              (fn [[can-handle? handle]]
                                (can-handle? next-token))
                              token-handlers)))]
    (if (nil? handler) (throw
                        (Exception.
                         (str "Unrecognized input around measure " measure-num
                              ": " score)))
        handler)))

(defn parse-score [input-score]
  (loop [expanded-score []
         mutations {}
         score input-score
         measure-num 1
         timestamp 0]
    (let [handler (resolve-handler score measure-num)
          foo (prn handler)
          [next-remaining-score
           next-expanded-score
           next-mutations
           next-measure-num
           next-timestamp] (handler score
                                    expanded-score
                                    mutations
                                    measure-num
                                    timestamp)]

      (if (empty? next-remaining-score)
        [next-expanded-score next-mutations]
        (recur next-expanded-score
               next-mutations
               next-remaining-score
               next-measure-num
               next-timestamp)))))

(defn zip-mutations
  "appends matching mutations to begin events in `score`"
  [{:keys [score mutations]}]
  (reduce (fn [updated-score [g-name g-mutation-list]]
            (let [{:keys [measure quant]} (first g-mutation-list)]
              (update-in score
                         [measure quant]
                         (fn [events]
                           (map (fn [event]
                                  (if (and (= 'begin (first event))
                                           (= g-name (second event)))
                                    (concat event '(g-mutation-list))
                                    event))
                                events)))))
          score
          mutations))

(defmacro defscore [score-name & input-score]
  (if (empty? input-score)
    []
    (let [[score mutations] (parse-score input-score)
          score (zip-mutations score mutations)]
      (prn score)
      `(def ~score-name ~score))))
