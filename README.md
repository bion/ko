# ko

## Usage Example

```clojure
;; make sure you've got overtone around
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

Use `ko-defsynth` to define synthdefs instead of overtone's
`defscore`. Then use `defscore` to define a score.A basic score consists
of number vector pairs. Numbers indicate beats in a measure e.g. 1.5
is the first offbeat of the measure. The vector contains `begin`, `adjust`,
`finish` and `!` events to be executed at the time that corresponds with the
adjacent number.

E.g. The following plays two gestures, one starting on beat
one and the other starting the offbeat of beat two. Both end on beat one of
the following measure.

```clojure
(defscore
  1 [(begin :ssg :my-gesture my-gesture-spec)]
  2.5 [(begin :ssg :next-gesture next-gesture-spec)])

  1 [(finish :my-gesture :next-gesture)]
  ```

`adjust` and `!` events control gestures as they are playing, but do so
differently.

`adjust` is used to alter parameters of a running synth at
a specific time while it is playing. The following will change
the `amp` param of :my-gesture to -12 decibels on beat 3 of the
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
exponential curve (specified by :exp) to -6 decibels on beat one of
the following measure before ending on beat three:

```clojure
  2 (begin :ssg :my-gesture {:instr test-synth :amp -24 :freq :c4})

  1 (! :my-gesture {:amp [-6 :exp]})
  3 (finish :my-gesture)
```

Aside from specifying gestures, the defscore macro provides for setting
the time signature:

```clojure
  set-beats-per-bar 4
  set-beats-per-minute 80
  ```

and specifying a no events take place in a measure:

```clojure
silent
```
