# ko
A composition library to aid in writing declaritive scores for [Overtone](http://overtone.github.io/).
## Usage Example

```clojure
;; bring in the essentials, currently a messy affair
(use [ko.gesture :only (ko-defsynth)]
     [ko.scheduling :only (begin alter finish)]
     [ko.score :only (defscore)])
(require [overtone.core :as ot])

;; define a synthdef
(ko-defsynth test-synth
             [freq 1 amp 1]
             (ot/out 0 (* (ot/sin-osc freq) amp)))

;; define a score
(defscore test-score
  set-beats-per-bar 4
  set-beats-per-minute 108

  1 [(begin :ssg :my-gesture {:instr test-synth
                              :freq :C4
                              :amp 12})]
  2 [(begin :ssg :other-gesture {:instr test-synth
                                 :freq :Bf3
                                 :amp -12})]

  silent

  1 [(! :my-gesture {:freq [:G4 :exp]
                     :amp [-6 :exp]})
     (alter :other-gesture {:freq :C5})]
  2 [(finish :my-gesture :other-gesture)])

;; play it
(play-score test-score)
```

## Essentials

Use `ko-defsynth` to define synthdefs instead of Overtone's
`defsynth` (`ko-defsynth` was chosen so users can `use` both Overtone and Ko in the same ns).
Then use `defscore` to define a score. A basic score consists
of pairs of numbers and vectors. Numbers indicate beats in a measure e.g. 1.5
is the first offbeat of the measure. Measures are inferred by numbers lower than their predecessors.
Vectors contain `begin`, `adjust`, `finish` and `!` events to be executed at the time that corresponds with their
corresponding number.

The following plays two gestures, one starting on beat
one and the other starting on the offbeat of beat two. Both end on beat one of
the following measure.

```clojure
(defscore
  1 [(begin :ssg :my-gesture my-gesture-spec)]
  2.5 [(begin :ssg :next-gesture next-gesture-spec)]

  1 [(finish :my-gesture :next-gesture)])
```

`adjust` and `!` events control gestures as they are playing, but do so
differently.

`adjust` is used to alter parameters of a running synth at
a specific time while it is playing. The following will change
the `amp` param of `:my-gesture` to -12 decibels on beat three of the
corresponding measure:

```clojure
3 (adjust :my-gesture {:amp -12})
```

`!` is used to specify control envelope breakpoints for smooth
changes over the course of a gesture by calculating the time
difference between a gesture's `begin` and successive `!` events.
Unlike `alter`, `!` generates a new synthdef under the hood and does
not send additional OSC messages to scsynth while the score is playing.

The following begins a gesture on beat two that crescendos along an
exponential curve (specified by `:exp`) to -6 decibels on beat one of
the following measure before decrescendoing to -32 decibels along a curve
value of 4 (see SuperCollider's [env](http://doc.sccode.org/Classes/Env.html) docs
for more info on envelope curvature) and ending on beat three:

```clojure
2 [(begin :ssg :my-gesture {:instr test-synth :amp -24 :freq :c4})]

1 [(! :my-gesture {:amp [-6 :exp]})]
3 [(! :my-gesture {:amp [-32 4]})
   (finish :my-gesture)]
```

The typical Overtone equivalent of this would be to define a synthdef with `freq` bound
to
```clojure
(envelope (map db->amp [-24 -6 -32])
          [time-between-meas-1-beat-2-and-meas-2-beat-1 time-between-meas-2-beat-1-and-meas-2-beat-3]
          [:exp 4])
```

Aside from specifying gestures, the defscore macro provides for setting
the time signature:

```clojure
beats-per-bar 4
beats-per-minute 80
```

and for notating measures in which no events occur:

```clojure
silent
```
