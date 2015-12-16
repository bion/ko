(ns ko.acceptance-test
  [:require [overtone.core :as ot]]
  [:use [ko.gesture :only (ko-defsynth)]
        [ko.scheduling]
        [ko.score :only (defscore)]])

(ko-defsynth test-synth
             [freq 1]
             (ot/out 0 (ot/sin-osc freq)))

(defscore test-score
  set-beats-per-bar 4
  set-beats-per-minute 108

  1 [(begin :ssg :my-gesture-name {:instr test-synth :freq 200})
     (begin :ssg :other-gesture-name {:instr test-synth :freq 400})]

  ;; specify envelope nodes
  3 [(! :my-gesture-name {:freq [220 :exp] :amp [0.1 :exp]})])

(ot/defcgen bark-delay
  [in {:default 0 :doc "the input signal"}
   maxdelay {:default 1.0 :doc "the maximum delay time"}
   freqmul {:default 1.0 :doc "scale center frequencies"}
   width {:default 1.0 :doc "scale filter widths"}]
  (:ar
   (let [deltimes (vec (for [x (range 25)] (+ 0.2 (rand 0.3))))
         feedback (vec (for [x (range 25)] (+ 0.7 (rand 0.2))))
         rq [1.16 0.38666666666667 0.232 0.16571428571429
             0.14177777777778 0.12210526315789 0.116 0.10357142857143
             0.0928 0.094188034188033 0.088905109489053 0.087
             0.087783783783783 0.086325581395351 0.08816
             0.089999999999998 0.093823529411763 0.1015 0.10875 0.11
             0.10771428571429 0.12282352941176 0.13809523809524
             0.15037037037037 1.178125]
         cf [50 150 '250 350 450 570 700 840 1000 1170 1370 1600 1850
             2150 2500 2900 3400 4000 4800 5800 7000 8500 10500 13500
             16000]
         filterbank (ot/b-band-pass (+ in (ot/local-in:ar 1))
                                    (* cf freqmul)
                                    (* rq width))
         delays (ot/delay-n filterbank maxdelay (- deltimes (ot/control-dur:ir)))]
     (ot/local-out:ar (ot/sum (* delays feedback)))
     (ot/sum delays)))
  (:default :ar))
