(ns ko.score
  (:use ko.scheduling)
  (:gen-class))

(def beats-per-bar (atom nil))
(def beats-per-minute (atom nil))

(defn gesture-record
  "begin event must specify inital state for all mutations
  {:gesture-name
     ;; first one is beginning of gesture
     [{:measure 1 :quant 1 :timestamp 1.12 :spec {:instr foo :freq 200 :amp 1}}
      {:measure 2 :quant 2.5 :timestamp 23.123 :spec {:freq [300 :exp]}}
      {:measure 3 :quant 1 :timestamp 43.12 :spec {:freq [200 :exp]}}]}"
  [g-spec measure-num quant timestamp]
  [{:measure measure-num :quant quant :timestamp timestamp :spec g-spec}])

(defn resolve-spec
  "Resolves symbols and evaluates lists until a map is produced or throws"
  ([g-spec]
   (resolve-spec g-spec 0))
  ([g-spec depth]
   (if (>= depth 10)
     (throw (Exception.
             (str "Recursed too many times in `resolve-spec` with spec:"
                  g-spec)))
     (cond
       (map? g-spec) g-spec
       (var? g-spec) (resolve-spec (var-get g-spec) (inc depth))
       (list? g-spec) (resolve-spec (eval g-spec) (inc depth))
       (symbol? g-spec) (resolve-spec (resolve g-spec) (inc depth))

       :else
       (throw (Exception. (str "Unrecognized spec in `resolve-spec` "
                               g-spec)))))))

(defn record-begin-events
  [measure-num quant begin-events mutations timestamp]
  (if (empty? begin-events)
    mutations
    (reduce (fn [memo event]
              (let [g-name (nth event 2)
                    ;; this doesn't make sense when the gesture has no name
                    g-spec (resolve-spec (nth event 3))]
                (merge memo {g-name
                             (gesture-record g-spec
                                             measure-num
                                             quant
                                             timestamp)})))
            mutations
            begin-events)))

(defn record-mutations [measure-num quant mutations-events mutations timestamp]
  (if (empty? mutations-events)
    mutations
    (reduce (fn [memo event]
              (let [g-name (second event)
                    g-spec (resolve-spec (nth event 2))
                    event-record {:timestamp timestamp
                                  :measure measure-num
                                  :quant quant
                                  :spec g-spec}
                    gesture (mutations g-name)]
                (if-not gesture
                  (throw (Exception.
                          (str "No gesture found for `!` (mutation): "
                               g-name))))
                (update-in mutations [g-name] conj event-record)))
            mutations
            mutations-events)))

(defn event-type [form]
  (cond (seq? form)
        (cond (= '! (first form)) :mutation-events
              (= 'begin (first form)) :begin-events
              :else :basic-scheduled-events)
        :else (throw (Exception.
                      (str "Unrecognized event " form)))))

(defn- beat-dur []
  (/ 60 @beats-per-minute))

(defn- quant->duration [quant]
  (* (beat-dur) (- quant 1)))

(defn- inc-measure-timestamp [timestamp]
  (+ timestamp (* (beat-dur) @beats-per-bar)))

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

(defn extract-normal-measure
  [remaining-score expanded-score mutations measure-num timestamp]
  (let [[next-measure
         next-remaining-score
         next-mutations
         next-timestamp] (extract-measure remaining-score measure-num mutations timestamp)

        next-expanded-score (if (empty? next-measure)
                              expanded-score
                              (conj expanded-score next-measure))]

    [next-remaining-score next-expanded-score next-mutations (inc measure-num) next-timestamp]))

(defn extract-silent-measure
  [remaining-score expanded-score mutations measure-num timestamp]
  (let [next-measure {0 []}
        next-remaining-score (rest remaining-score)
        next-expanded-score (conj expanded-score
                                  next-measure)
        next-timestamp (inc-measure-timestamp timestamp)]
    [next-remaining-score
     next-expanded-score
     mutations
     (inc measure-num)
     next-timestamp]))

(def token-handlers
  ;; can-handle? => handle pairs
  ;; handler params [score expanded-score mutations measure-num timestamp]
  ;; returns [next-remaining-score next-expanded-score next-mutations next-measure-num next-timestamp]

  ;; normal measure handler
  {#(number? %)
   extract-normal-measure

   ;; insert one measure of silence
   #(= 'silent %)
   extract-silent-measure

   #(= 'beats-per-bar %)
   (set-global beats-per-bar)

   #(= 'beats-per-minute %)
   (set-global beats-per-minute)})

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
  [score mutations]
  (reduce (fn [updated-score [g-name g-mutation-list]]
            (let [{:keys [measure quant]} (first g-mutation-list)]
              (update-in score
                         [(dec measure) quant]
                         (fn [events]
                           (vec
                            (map (fn [event]
                                   (if (and (= 'begin (first event))
                                            (= g-name (nth event 2)))
                                     (concat event `(~g-mutation-list))
                                     event))
                                 events))))))
          score
          mutations))

(defn filter-mutations
  "returns mutations that have more than just a beginning event"
  [mutations]
  (into {}
        (remove (fn [[g-name mutation-list]]
                  (empty? (rest mutation-list)))
                mutations)))

(defmacro defscore
  "Define a ko score and prepare it for playing. A basic score consists
  of number vector pairs. Numbers indicate beats in a measure e.g. 1.5
  is the first offbeat of the measure. The vector contains `begin`, `adjust`,
  `finish` and `!` events to be executed at the time that corresponds with the
  adjacent number.

  E.g. The following plays two gestures, one starting on beat
  one and the other starting the offbeat of beat two. Both end on beat one of
  the following measure.

  (defscore
    1 [(begin :ssg :my-gesture my-gesture-spec)]
    2.5 [(begin :ssg :next-gesture next-gesture-spec)])

    1 [(finish :my-gesture :next-gesture)]

  `adjust` and `!` events control gestures as they are playing, but do so
  differently.

  `adjust` is used to alter parameters of a running synth at
  a specific time while it is playing. The following will change
  the `amp` param of :my-gesture to -12 decibels on beat 3 of the
  corresponding measure:

  3 (adjust :my-gesture {:amp -12})

  `!` is used to specify control envelope breakpoints for smooth
  changes over the course of a gesture by calculating the time
  difference between a gesture's `begin` and successive `!` events.
  Unlike `alter`, `!` generates a new synthdef under the hood and does
  not send additional OSC messages to scsynth while the score is playing.

  The following begins a gesture on beat two that crescendos along an
  exponential curve (specified by :exp) to -6 decibels on beat one of
  the following measure before ending on beat three:

  2 (begin :ssg :my-gesture {:instr test-synth :amp -24 :freq :c4})

  1 (! :my-gesture {:amp [-6 :exp]})
  3 (finish :my-gesture)

  Aside from specifying gestures, the defscore macro provides for setting
  the time signature:

  beats-per-bar 4
  beats-per-minute 80

  and specifying a no events take place in a measure:

  silent"
  [score-name & input-score]
  (if (empty? input-score)
    []
    (let [[score mutations] (parse-score input-score)
          score (zip-mutations score (filter-mutations mutations))]
      `(def ~score-name ~score))))
