(ns ko.score
  (:use ko.gesture))

(def ^:dynamic *beats-per-bar* nil)
(def ^:dynamic *beats-per-minute* nil)
(def ^:dynamic *jump-data* nil)

(defn gesture-record
  "begin action must specify inital state for all curves
  {:gesture-name
     ;; first one is beginning of gesture
     [{:measure 1 :quant 1 :timestamp 1.12 :spec {:instr foo :freq 200 :amp 1}}
      {:measure 2 :quant 2.5 :timestamp 23.123 :spec {:freq [300 :exp]}}
      {:measure 3 :quant 1 :timestamp 43.12 :spec {:freq [200 :exp]}}]}"
  [g-spec measure-num quant timestamp]
  (let [begin-action {:measure measure-num
                      :quant quant
                      :timestamp timestamp
                      :spec g-spec}]
    (with-meta [begin-action] {:arg-type :curves})))

(defn record-begin-actions
  [measure-num quant begin-actions curves timestamp]
  (if (empty? begin-actions)
    curves
    (reduce (fn [memo action]
              (let [g-name (:name action)
                    g-spec (apply hash-map (rest (:args action)))]
                (merge memo {g-name
                             (gesture-record g-spec
                                             measure-num
                                             quant
                                             timestamp)})))
            curves
            begin-actions)))

(defn record-curves [measure-num quant curves-actions curves timestamp]
  (if (empty? curves-actions)
    curves
    (reduce (fn [memo action]
              (let [g-name (:name action)
                    g-spec (:args action)
                    action-record {:timestamp timestamp
                                   :measure measure-num
                                   :quant quant
                                   :spec g-spec}
                    gesture (curves g-name)]
                (if-not gesture
                  (throw (Exception.
                          (str "No gesture found for curve: "
                               g-name))))
                (update-in curves [g-name] conj action-record)))
            curves
            curves-actions)))

(defn group-actions-by-type [actions]
  (group-by (fn [action]
              (let [action-type (:action-type action)]
                (if-not (.contains [:begin :curve :alter :finish] action-type)
                  (throw (Exception.
                          (str "Unrecognized action " (with-out-str (prn action))))))
                (cond (= :begin action-type) :begin-actions
                      (= :curve action-type) :curve-actions
                      :else :basic-scheduled-actions)))
            actions))

(defn- beat-dur []
  (/ 60 @*beats-per-minute*))

(defn- quant->duration [quant]
  (* (beat-dur) (- quant 1)))

(defn- inc-measure-timestamp [timestamp]
  (+ timestamp (* (beat-dur) @*beats-per-bar*)))

(defn- add-measure-to-score [score measure]
  (let [metadata {:beat-dur (beat-dur) :beats-per-bar @*beats-per-bar*}]
    (conj score (with-meta measure metadata))))

(defn extract-measure [score measure-num curves measure-timestamp]
  (loop [measure {}
         remaining-score score
         curves-acc curves]
    (let [quant                   (first remaining-score)
                                  ;; eval the actions
          actions                 (map eval (second remaining-score))
          {:keys [basic-scheduled-actions
                  curve-actions
                  begin-actions]} (group-actions-by-type actions)
          scheduled-actions       (apply conj basic-scheduled-actions begin-actions)
          next-remaining-score    (-> remaining-score rest rest)
          next-item-in-score      (first next-remaining-score)
          next-measure            (if (empty? scheduled-actions)
          measure                 (assoc measure quant (into [] scheduled-actions)))

          quant-timestamp         (+ measure-timestamp (quant->duration quant))
          next-curves             (record-begin-actions measure-num
                                            quant
                                            begin-actions
                                            curves-acc
                                            quant-timestamp)
          next-curves             (record-curves measure-num
                                     quant
                                     curve-actions
                                     next-curves
                                     quant-timestamp)]

      (cond
        (and (number? next-item-in-score) (< quant next-item-in-score))
        (recur next-measure next-remaining-score next-curves)

        :else [next-measure
               next-remaining-score
               next-curves
               (inc-measure-timestamp measure-timestamp)]))))

(defn- set-global [global]
  (fn [parse-state]
    (let [score (:score parse-state)
          new-val (second score)
          next-score (-> score rest rest)]
      (reset! (var-get global) new-val)
      (assoc parse-state :score next-score))))

(defn extract-normal-measure
  [parse-state]
  (let [{:keys [expanded-score score measure-num curves timestamp]} parse-state
        [next-measure
         next-score
         next-curves
         next-timestamp] (extract-measure score
                                          measure-num
                                          curves
                                          timestamp)

        next-expanded-score (if (empty? next-measure)
                              expanded-score
                              (add-measure-to-score expanded-score next-measure))]

    {:expanded-score next-expanded-score
     :score next-score
     :measure-num (inc measure-num)
     :curves next-curves
     :timestamp next-timestamp}))

(defn extract-silent-measure
  [parse-state]
  (let [next-measure {0 []}
        {:keys [score expanded-score measure-num timestamp]} parse-state
        next-score (rest score)
        next-expanded-score (add-measure-to-score expanded-score
                                                  next-measure)
        next-timestamp (inc-measure-timestamp timestamp)]
    (assoc parse-state
           :score next-score
           :measure-num (inc measure-num)
           :expanded-score next-expanded-score
           :timestamp next-timestamp)))

(defn set-label
  [parse-state]
  (let [{:keys [score measure-num]} parse-state
        label (second score)
        next-score (-> score rest rest)]
    (swap! *jump-data*
           (fn [md] (update-in md [:labels]
                               ;; dec measure-num to get index
                               #(assoc % label (dec measure-num)))))
    (assoc parse-state :score next-score)))

(defn jump-to-label
  [should-jump? parse-state]
  (let [{:keys [score measure-num]} parse-state
        next-score (-> score rest rest)
        label (second score)
        jump {:label label
              :should-jump? should-jump?}]

    (swap! *jump-data*
           (fn [md] (update-in md [:jumps]
                               ;; dec measure-num to get index
                               #(assoc % (dec measure-num) jump))))

    (assoc parse-state :score next-score)))

(defn true-for-n [times]
  (let [call-count* (atom 0)]
    #(do
       (reset! call-count* (inc @call-count*))
       (<= @call-count* times))))

(def token-handlers
  ;; can-handle? => handle pairs

  {#(number? %) extract-normal-measure ;; normal measure handler
   #(= 'label %) set-label
   #(= 'jump-to %) (partial jump-to-label (true-for-n 1))
   #(= 'silent %) extract-silent-measure ;; insert one measure of silence
   #(= 'beats-per-bar %) (set-global #'*beats-per-bar*)
   #(= 'beats-per-minute %) (set-global #'*beats-per-minute*)})

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
  (let [initial {:expanded-score []
                 :curves {}
                 :score input-score
                 :measure-num 1
                 :timestamp 0}]
    (loop [parse-state initial]
      (let [{:keys [score measure-num]} parse-state
            handler (resolve-handler score measure-num)
            next-parse-state (handler parse-state)]

        (if (empty? (:score next-parse-state))
          (let [{:keys [expanded-score curves]} next-parse-state
                jump-data @*jump-data*
                return [expanded-score curves jump-data]]
            return)

          (recur next-parse-state))))))

(defn zip-curves
  "Appends matching curves to begin actions in `score`
  all actions will end with either a list of curves or
  an empty vector."
  [score curves]
  (let [score-with-curves
        (reduce (fn [updated-score [g-name g-curve-list]]
                  (let [{:keys [measure quant]} (first g-curve-list)]
                    (update-in
                     score
                     [(dec measure) quant]
                     (fn [actions]
                       (vec
                        (map (fn [action]
                               (if (and (= :begin (:action-type action))
                                        (= g-name (:name action)))
                                 (add-curves action g-curve-list)
                                 action))
                             actions))))))
                score
                curves)]
    score-with-curves))

(defn filter-empty-curves
  "returns curves that have more than just a beginning action"
  [curves]
  (into {}
        (remove (fn [[g-name curve-list]]
                  (empty? (rest curve-list)))
                curves)))

(defmacro defscore
  "Define a ko score and prepare it for playing. A basic score consists
  of number vector pairs. Numbers indicate beats in a measure e.g. 1.5
  is the first offbeat of the measure. The vector contains `begin`, `adjust`,
  `finish` and `curve` actions to be executed at the time that corresponds with the
  adjacent number.

  E.g. The following plays two gestures, one starting on beat
  one and the other starting the offbeat of beat two. Both end on beat one of
  the following measure.

  (defscore
    1 [(begin :ssg :my-gesture my-gesture-spec)]
    2.5 [(begin :ssg :next-gesture next-gesture-spec)])

    1 [(finish :my-gesture :next-gesture)]

  `adjust` and `curve` actions control gestures as they are playing, but do so
  differently.

  `adjust` is used to alter parameters of a running synth at
  a specific time while it is playing. The following will change
  the `amp` param of :my-gesture to -12 decibels on beat 3 of the
  corresponding measure:

  3 (adjust :my-gesture {:amp -12})

  `curve` is used to specify control envelope breakpoints for smooth
  changes over the course of a gesture by calculating the time
  difference between a gesture's `begin` and successive `curve` actions.
  Unlike `alter`, `curve` generates a new synthdef under the hood and does
  not send additional OSC messages to scsynth while the score is playing.

  The following begins a gesture on beat two that crescendos along an
  exponential curve (specified by :exp) to -6 decibels on beat one of
  the following measure before ending on beat three:

  2 (begin :ssg :my-gesture {:instr test-synth :amp -24 :freq :c4})

  1 (curve :my-gesture {:amp [-6 :exp]})
  3 (finish :my-gesture)

  Aside from specifying gestures, the defscore macro provides for setting
  the time signature:

  beats-per-bar 4
  beats-per-minute 80

  and specifying a no actions take place in a measure:

  silent"
  [score-name & input-score]
  (if (empty? input-score)
    []
    (binding [*beats-per-minute* (atom 60)
              *beats-per-bar* (atom 4)
              *jump-data* (atom {:labels {} :jumps {}})]

      (let [[score curves jump-data] (parse-score input-score)
            score                    (zip-curves score (filter-empty-curves curves))
            metadata                 (merge jump-data {:living-gestures (atom {})})
            score                    (with-meta score metadata)]

        `(def ~score-name ~score)))))
