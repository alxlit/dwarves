(ns dwarves.cello
  (:require [overtone.core :refer :all]
            [overtone.orchestra.cello :as cello]))

(definst cello-with-duration
 [note 60 amp 1.0 duration 250 length 1 rate 1.0 reverb 0.0]
 (let [duration (/ duration 1e3)
       l-buf (index:kr (:id cello/length-buffer) length)
       buf (index:kr l-buf note)
       dry (scaled-play-buf 1 buf :loop 1 :rate rate)
       dry (* amp dry)
       wet (free-verb dry reverb)
       envelope (env-gen (adsr 0 1 1 0.1 1 -4)
                        :gate 1
                        :action FREE)
       envelope (* envelope (line 1 0 duration FREE))
       inputs (map (partial * envelope) [dry wet])]
   (out 0 inputs)))

(defn sampled-cello
  [& args]
  (let [args (into [] args)
        n (.indexOf args :duration)
        duration (args (inc n))
        note (args 0)
        ;; longer samples by shifting up an octave and playing at 1/2 speed
        note (+ 12 note)
        rate 0.5
        n (.indexOf args :amp)

        ;; playing at half the speed reduces energy (by half, right??), boost it
        amp (if (pos? n) (args (inc n)) 1)
        amp (* 2 amp)

        length (condp <= duration
                 1500 3
                 1000 2
                  500 1
                  250 0
                    0 0)
        args (assoc args 0 note)
        args (conj args :length length :rate rate)]
    (apply cello-with-duration args)))
