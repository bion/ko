# ko

Declarative scores for [Overtone](http://overtone.github.io/).

## Usage Example

```clojure
;; bring in the essentials, currently a messy affair
(use [ko.gesture]
     [ko.scheduling :only (play-score)]
     [ko.score :only (defscore)])
(require [overtone.core :as ot])

;; define a synthdef
(ko-defsynth test-synth
             [freq 1 amp 1]
             (ot/out 0 (* (ot/sin-osc freq) amp)))

;; define a score
(defscore test-score
  beats-per-bar 4
  beats-per-minute 108

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

## Basic Usage

Use `ko-defsynth` to define synthdefs instead of Overtone's `defsynth`
(`ko-defsynth` was chosen so users can `use` both Overtone and Ko in
the same ns). Then use `defscore` to define a score. A basic score
consists of pairs of numbers and vectors. Numbers indicate beats in a
measure e.g. 1.5 is the first offbeat of the measure. Measures are
inferred by numbers lower than their predecessors. Vectors contain
`begin`, `adjust`, `finish` and `!` actions to be executed at the time
that corresponds with their corresponding number.

The following plays two gestures, one starting on beat one and the
other starting on the offbeat of beat two. Both end on beat one of the
following measure.

```clojure
(defscore
  1 [(begin :ssg :my-gesture my-gesture-spec)]
  2.5 [(begin :ssg :next-gesture next-gesture-spec)]

  1 [(finish :my-gesture :next-gesture)])
```

## Actions

### `begin`

`begin` currently only support one type: `:ssg` or single-synth gestures.
Single-synth gestures take the form

```clojure
(begin :ssg :gesture-name spec)
```

where `spec` must be a map containing an `:instr` key specifying
a `ko-synthdef` along with all other params to the synth. Spec
can itself be a map, a var referring to a map, or form that when
evaluated returns a map.

### `adjust` and `!`

`adjust` and `!` actions control gestures as they are playing, but do so
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
difference between a gesture's `begin` and successive `!` actions.
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

### `finish`

Ends a running gesture. In the case of `:ssg` gestures, simply sends a
`\n_free` or 'node free' message to the server for the corresponding
synth. Ending two gestures:

```clojure
1 [(finish :some-gesture-name :another-gesture-name)]
```

## Routing

To route sound from one synth to any number of synths, pass a string
ending in `-bus` for params mapped to busses. The following
demonstrates routing the output of a synth called `:gen-saw-wave` to
a synth called `:shared-low-pass`, which in turn routes to hardware
output 0:

```clojure
1 [(begin :ssg :gen-saw-wave {:instr saw-wave
                              :freq :C4
                              :amp -12
                              :out-bus "shared-low-pass-in-bus"})
   (begin :ssg :shared-low-pass {:instr low-pass
                                 :in-bus "shared-low-pass-in-bus"
                                 :out-bus 0})]
```

Under the hood, Ko looks for string arguments ending in `-bus` and
substitutes them for audio busses. For this reason, do not end a
string argument to a synth in `-bus` unless it's for a bus.

## Synth node ordering

If no target is specified for a `begin` action, it will be added to
the head of the "Overtone Default" group. This isn't ideal for even
simple routing schemas that rely on sources and filters to be in a
specific order. To explicitly order synth nodes, first use
`register-group` to specifiy a graph of groups, then reference the
desired target group by name in the `begin` action.

Create a group named `"source"` at the head of the "Overtone
Default", a group called `"filter"` directly after it, then a
group `middling` in between the two:

```clojure
;; single-arity adds to head of "Overtone Default"
(register-group "source")

;; two-arity adds after the second arg
(register-group "filter" "source")

;; three-arity specifies add-action
(register-group "middling" "filter" :before)

(overtone/pprint-node-tree)
;; =>
;;  --snip--
;;    {:type :group,
;;     :name "Overtone Default"
;;     :id 76,
;;     :children
;;     ({:type :group, :id 110, :name "source", :children nil}
;;      {:type :group, :id 112, :name "middling", :children nil}
;;      {:type :group, :id 111, :name "filter", :children nil})}
;;  --snip--
```

Specify `begin` actions for a source and filter, each targeting
the head of their appropriate group:

```clojure
1 [(begin :ssg :g-one source-spec "source")
   (begin :ssg :filt filt-spec "filter")]
```

Additionally specify add-actions:

```clojure
1 [(begin :ssg :g-one source-spec [:tail "source"])
(begin :ssg :filt filt-spec [:head "filter"])]
```

Remove all registered groups:

```clojure
(reset-groups!)
```

## Other score elements

Aside from specifying gestures, the defscore macro provides for setting
the time signature:

```clojure
beats-per-bar 4
beats-per-minute 80
```

and for notating measures in which no actions occur:

```clojure
silent
```

## Similar projects

* [leipzig](https://github.com/ctford/leipzig/)
