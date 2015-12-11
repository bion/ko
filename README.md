# ko

## Usage

Define a synthdef:

```clojure
;; make sure you've got overtone around
(require [overtone.core :as ot])

(ko-defsynth test-synth
             [freq 1]
             (ot/out 0 (ot/sin-osc freq)))
```
