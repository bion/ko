(ns ko.metronome
  [:use
   [ko.gesture]
   [ko.synths]])

(defn play-metronome [& args]
  (begin :anon (merge args {:instr metronome-beep-> :outbus 0 :freq 1800})))
