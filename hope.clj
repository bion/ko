(comment
  (def s-one-drums
    {:type :percussion
     :notes [[snare [2 4] :wrap-note-args some-func
              kick [1 3]
              hat (into [] (take 16 (iterate #(+ 0.25 %1) 1)))]
             [snare [2 4]
              kick [1 3]
              hat (into [] (take 8 (iterate #(+ 0.5 %1) 1)))]]
     :wrap-note-args (fn [note-args beat-in-form]
                       (assoc note-args :amp (* (note :amp)
                                                (env-val drums-env beat-in-form))))})

  (def s-one-chords
    {:type :process
     :gamp -12
     :instr dan-dan
     :amp [gesture-begin -60
           mark-one -40 :exp ;; keyword is for curvature from last to this point
           mark-two -40
           (ahead mark-two (current-beat)) -30
           mark-three -60]
     ;; something like this
     :note [gesture-begin [:C3 :E3 :G3 :B3]
            mark-one same
            mark-two [:Gs4 :B4 :Ds5 :Fs5]
            mark-three same]
     ;; or
     :note [:C3 :maj7 mark-one
            :Gs4 :min7 mark-two]})

  (defscore example-score
    :tempo 120
    [[s-one-drums s-one-chords]
     [s-two-drums s-two-chords s-two-lead]])

  (play-score example-score)
  ;; or
  (play-score (compile-score example-score))
  )

[[{1.0 [{:name "chords"
         :amp -12
         :instr dan-dan
         :env perc-2
         :note [:C3 :maj7
                :Gs4 :min7]}]}

  {1.25 [{:name "chords"
          :env {:attr :note :val :gliss}
          :note [:G3 :maj7
                 :F4 :min7]}]}]]

(read-score
 label :beginning
 set-beats-per-bar 4
 set-beats-per-minute 108

 1 [(begin (ssg :my-gesture-name {:freq 200}))
    ((begin (ssg :other-gesture-name {:freq 400})))]
 3 [(! {:name :my-gesture-name :spec {:freq 220}})]

 1 [(begin (ossg {:freq 300 :dur 1}))]
 2 [(begin (ossg {:freq 400 :dur 1}))]
 3 [(begin (ossg {:freq 500 :dur 1}))]
 4 [(begin (ossg {:freq 600 :dur 1}))]

 set-beats-per-minute 150

 silent ;; one measure of silence
 silent-4 ;; four measures of silence

 1 [(finish :my-gesture-name :other-gesture-name)
    (begin (ssg :third-gesture-name {:freq 500}))]

 1 [(begin (ossg {:freq 600 :dur 1}))]
 2 [(begin (ossg {:freq 500 :dur 1}))]
 3 [(begin (ossg {:freq 400 :dur 1}))]
 4 [(begin (ossg {:freq 300 :dur 1}))]

 jump-to :beginning (fn [jump-count] (< 1 jump-count)) [(finish :third-gesture-name)]
 or
 jump-to :beginning continue-flag [(finish :third-gesture-name)]
 )
