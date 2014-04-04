(ns dwarves.flute-vibrato
  "Flute vibrato, adapted from Joseph Wilk's blog post:
   http://blog.josephwilk.net/clojure/creating-instruments-with-overtone.html"
  (:require [overtone.core :refer :all]))

(defn- registered-samples
  "Fetch flute samples from the asset store if they have been manually
  registered"
  []
  (registered-assets ::TransverseFluteTenutoVibrato))

(def FREESOUND-VIBRATO-FLUTE-SAMPLES
  "Freesound ids for all samples in the Vibrato Transverse Flute pack"
  {154274 :C7  154273 :B6 154272 :A#6 154271 :A6 154270 :G#6 154269 :G6  154268 :F#6
   154267 :F6  154266 :E6 154265 :D#6 154264 :D6 154263 :C#6 154262 :C6  154261 :B5
   154260 :A#5 154259 :A5 154258 :G#5 154257 :G5 154256 :F#5 154255 :F5  154254 :E5
   154253 :D#5 154252 :D5 154251 :C#5 154250 :C5 154249 :B4  154248 :A#4 154247 :A4
   154246 :G#4 154245 :G4 154244 :F#4 154243 :E4 154242 :F4  154241 :D#4 154240 :D4
   154239 :C#4 154238 :C4})

(def FLUTE-SAMPLE-IDS (keys FREESOUND-VIBRATO-FLUTE-SAMPLES))

(def flute-samples
  (doall (map freesound-sample FLUTE-SAMPLE-IDS)))

(defn- buffer->midi-note [buf]
  (-> buf :freesound-id FREESOUND-VIBRATO-FLUTE-SAMPLES name note))

(defn- note-index
  "Returns a map of midi-note values [0-127] to buffer ids."
  [buffers]
  (reduce (fn [index buf]
            (let [id (-> buf :id)
                  note (buffer->midi-note buf)]
              (assoc index note id)))
          {}
          buffers))

;; Silent buffer used to fill in the gaps.
(defonce ^:private silent-buffer (buffer 0))

(defonce index-buffer
  (let [tab (note-index flute-samples)
        buf (buffer 128)]
    (buffer-fill! buf (:id silent-buffer))
    (doseq [[idx val] tab]
      (buffer-set! buf idx val))
    buf))

(definst sampled-flute-vibrato
  [note 60 amp 1.0 duration 250 loop? 1 rate 1.0 reverb 0.5]
  (let [buf (index:kr (:id index-buffer) note)
        duration (/ duration 1e3)
        start (* 0.25 44.1e3)
        dry (scaled-play-buf 1 buf :start-pos start :loop loop? :rate rate)
        dry (* amp dry)
        wet (free-verb dry reverb)
        envelope (env-gen (adsr 0 1 1 0.1 1 -4)
                     :gate 1
                     :action FREE)
        envelope (* envelope (line 1 0 duration FREE))
        inputs (map (partial * envelope) [dry wet])]
    (out 0 inputs)))
