(ns ko.acceptance-test
  (:require [overtone.core :as ot] :reload)
  (:use [ko.gesture]
        [ko.scheduling]
        [ko.score :only (defscore)] :reload))

(if (ot/server-connected?)
  (do (ot/stop)
      (reset-groups!))
  (ot/boot-server))

(ko-defsynth test-synth
             [freq 1 amp 1 bus 0]
             (ot/out bus (* (ot/saw freq) amp)))

(ko-defsynth test-filter
             [in-bus 8 out-bus 0 cutoff 440]
             (ot/out out-bus (ot/lpf (ot/in in-bus) cutoff)))

(ko-defsynth bark-delay-test
             [bus 0 freq 220 amp 0.5 delay-scale 1.0 delay-width 1.0]
             (let [src (* (ot/sin-osc freq) amp)
                   src (bark-delay src 1.0 1.0 delay-width delay-scale)]
               (ot/out bus src)))

(def source-spec {:instr test-synth
                 :freq 220
                 :amp -6
                 :bus "test-bus"})

(def filt-spec {:instr test-filter
                :in-bus "test-bus"
                :out-bus 0
                :cutoff 100})

(register-group "source")
(register-group "filter" "source" :after)
(ot/pp-node-tree)

(defscore test-score
  beats-per-bar 4
  beats-per-minute 108

  1 [(begin :ssg :g-one (merge source-spec {:freq 440}) "source")
     (begin :ssg :filt filt-spec "filter")]

  silent

  1 [(! :filt {:cutoff [10000 :exp]})]
  2 [(finish :g-one :filt)])

(clojure.pprint/pprint test-score)
(play-score test-score)
(ot/pp-node-tree)
(ot/stop)

(ot/defcgen bark-delay
  [in {:default 0 :doc "the input signal"}
   maxdelay {:default 1.0 :doc "the maximum delay time"}
   freqmul {:default 1.0 :doc "scale center frequencies"}
   width {:default 1.0 :doc "scale filter widths"}
   delay-scale {:default 1.0 :doc "scale delay times"}]
  (:ar
   (let [deltimes (vec (for [x (range 25)] (+ 0.2 (rand 0.3))))
         deltimes (* delay-scale (- deltimes (ot/control-dur:ir)))
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
         delays (ot/delay-c filterbank maxdelay deltimes)]
     (ot/local-out:ar (ot/sum (* delays feedback)))
     (ot/sum delays)))
  (:default :ar))
