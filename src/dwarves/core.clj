(ns dwarves.core
  (:require [clojure.java.io :as io]
            [dwarves.df-theme :as df-theme]
            [overtone.core :refer :all]
            [environ.core :refer [env]]))
          
(when-not (server-connected?)
  (connect-external-server (Integer. (env :sc-port))))

;; load samples  
(require '[dwarves.flute-vibrato :refer [sampled-flute-vibrato]])
(require '[dwarves.cello :refer [sampled-cello]])

(def signature {:bpm 60
                :measure 10
                :unit 1})

(def accumulate (partial reductions +))

(defn notes [& notes] (into #{} notes))
(defn note? [x] (or (keyword? x) (integer? x)))
(defn notes? [x] (and (set? x) (every? note? x)))

(defn beat->ms
  "Converts note duration (e.g. 1/8) to milliseconds based on the signature."
  [beats]
  (let [{:keys [bpm unit]} signature]
    (/ (* beats 60e3) bpm unit)))

(defn sample->beats
  "Calculate the number of beats a sample lasts for."
  [s & {:keys [rate] :or {rate 1}}]
  (let [{:keys [bpm unit]} signature]
    (* (/ (:duration s) 60 rate) bpm unit)))
   
(def notes->midi (partial map note))

(defn auto-inst [s & _] (s))

(definst beep
  "Simple tone, the default instrument for playing notes."
  [midi 60 duration 250]
  (* (line 1 0 (/ duration 1e3) FREE)
     (sin-osc (midicps midi))))

(defsynth rompler
  "Plays a sample.

   sample    index of the buffer containing the sample
   amp       linear gain (default 1.0)
   delay     delay (within the duration), in ms
   duration  envelope (a line) length, in ms (default 10s)
   feedback  ratio to feedback (TODO)
   loop?     loop the sample throughout duration (specify 1 or 0, default 1)
   rate      playback speed (negative for backwards)
   reverb    reverb amount (ratio of dry/wet, default 0.0)

   Keep in mind that while these are similar effects to what the L2Ork instrument
   provides, they aren't exactly the same."
  [sample 0 amp 1.0 delay 0 duration 10e3 fade-in? 0 feedback 0.0 loop? 1 rate 1.0 reverb 0.0]
  (let [delay (/ delay 1e3)
        duration (/ duration 1e3)
        dry (play-buf 1 sample :loop loop? :rate rate)
        dry (* amp dry)
        dry (if (> delay 0) (delay-n dry delay delay) dry)
        wet (free-verb dry reverb)
        envelope (line 1 0.2 duration FREE)
;       envelope (if (= 1 fade-in?)
;                  (* envelope (line 0 1 (/ duration 2) NO-ACTION))
;                  envelope)
        inputs (map (partial * envelope) [dry wet])]
    (out 0 inputs)))

(defn throw-unknown-sound [s]
  (throw (Exception. (str "Unknown kind of sound " s))))

(defn sound
  "Takes a 'sound' (notes or sample) and options, and returns a function that
   produces a synth-node that plays the sound."
  [& [s & {:as options}]]
  (when s
    (let [inst (or (:inst options)
                   (cond (notes?  s) beep
                         (sample? s) rompler
                         (ifn?    s) auto-inst
                         :else (throw-unknown-sound s)))
          args (-> options (dissoc :inst) seq flatten)]
      (if (notes? s)
        ;; treat a chord (of notes) as a single 'sound'
        (fn [& more-args]
          (let [args (concat args more-args)
                midi (notes->midi s)]
            (mix (map #(apply inst % args) midi))))
        ;; other sounds
        (apply partial inst s args)))))

(defn prepare!
  "Convert durations to milliseconds, notes to Hz, sounds to synth-nodes, and
   computes relative timings."
  [song]
  (let [{:keys [bpm unit]} signature
        durations (map (comp beat->ms first) song)
        times (cons 0 (accumulate durations))
        sounds (map (comp (partial apply sound) rest) song)]
    (partition 3 (interleave times durations sounds))))

(defn play!
  "Plays a prepared song."
  [song]
  (let [schedule (partial + (now))]
    (doseq [[time duration sound] song]
      (when sound
        ;; using 'apply-at' rather than 'at' because it's more flexible
        (apply-at (schedule time) #(sound :duration duration))))))

(defn metronome!
  [bpm sound]
  (let [metro (if (number? bpm) (metronome bpm) bpm)
        next-beat (metro)]
    (at (metro next-beat) (sound))
    (apply-at (metro next-beat) metronome! metro sound [])))

(defn throw-resource-not-found [f & [e]]
  (throw (Exception. (str "Unable to locate " f) e)))

(defn resource->sample [f]
  (try
    (-> f io/resource io/file .getPath load-sample)
    (catch Exception e
      (throw-resource-not-found f e))))

(defmacro def-samples [name n]
  `(def ~name
     (vec (for [m# (range ~n)]
            (with-meta
              (resource->sample (str '~name "-" m# ".aiff"))
              {:name '~name})))))

(def-samples ahem 1)
(def-samples bees 1)
(def-samples bees-and-scream 1)
(def-samples blow-flame 1)
(def-samples breaking-glass 1)
(def-samples burp 1)
(def-samples chair 5)
(def-samples cough 2)
(def-samples cork-pop-and-pour 1)
(def-samples creaky-tree 1)
(def-samples dip-pen-in-ink 1)
(def-samples distant-drums 1)
(def-samples door-squeak 2)
(def-samples fart 4)
(def-samples fire 1)
(def-samples goblin 1)
(def-samples hmm 2)
(def-samples horse-drawn-wagon 2)
(def-samples horse-snort 1)
(def-samples joviality 2)
(def-samples mining 8)
(def-samples ogre 4)
(def-samples parchment-rustle 3)
(def-samples pickaxe 6)
(def-samples pile-of-wood 1)
(def-samples pouring-drink 1)
(def-samples scream 1)
(def-samples scribble 5)
(def-samples sniffle 6)
(def-samples strike-match 1)
(def-samples theme 1)
(def-samples tree-falling 1)
(def-samples walking 5)
(def-samples walk-in-woods 1)
(def-samples wood-chop 1)

(defn not-vec-or-seq? [x] (not (or (vector? x) (seq? x))))

(defmacro def-composed
  "The secret sauce. Well, it's a little messy and weak at the moment but the
   essence of it is there..."
  [name & sounds]
  `(let [[options# song#] (split-with not-vec-or-seq? [~@sounds])
         output-dir# (str (:user-dir env) "/resources/composed")
         output# (str output-dir# "/" '~name)
         last-output# (when (.exists (io/as-file output#)) (slurp output#))
         needs-recording?# (or (nil? last-output#)
                               (not= last-output# (str '~sounds)))

         ;; allow singly-nested vectors (for simple repeats and such)
         song# (reduce #(if (every? vector? %2) (concat %1 %2) (conj %1 %2)) [] song#)
         prepared# (prepare! song#)

         ;; amount of rest at end to cut when recording
         trim# (->> (reverse prepared#)
                    (take-while (comp nil? #(nth % 2)))
                    (map second)
                    (reduce +))

         beats# (reduce + (map first song#))
         duration# (reduce + (map second prepared#))
         play-song# #(play! prepared#)]

     ;; take a moment to record the composed sound
     (when needs-recording?#
       (println (str "recording " '~name ".wav..."))
       (recording-start (str output# ".wav"))
       (play-song#)
       (Thread/sleep (+ duration# (- trim#) 1.5e3))
       (println "done!")
       (recording-stop)
       (spit output# (str '~sounds)))

     ;; define it; note that the additional options will not affect recording or
     ;; playback, but may affect notation...
     (def ~name
       (vec (concat [beats#
                     (with-meta play-song# {:name '~name :composed? true})]
                    options#)))
             
     ;; allow 'inline' defs
     ~name))

(defmacro def-player [name & song]
  `(do
     (defonce players [])
     (def ~name
       (let [song# [~@song]
             beats# (reduce + (map first song#))]
         (with-meta (conj song# [(- 300 beats#)]) {:name '~name})))
     (alter-var-root #'players conj ~name)))

(def-player player-1
  [3/1]
  (def-composed walking-on-wood-1 :loop? 1 :fade-out? 1 :rate "1-1.5x" :slice? 1
    [7/1 (walking 0) :rate 1.5]
    [3/1]
    [5/1 (walking 3) :rate 1.5 :loop? 0]
    [2/1]
    [8/1 (walking 4) :amp 0.5 :rate 1.5 :loop? 0] ; 25
    [27/1])
  [15/1]
  (def-composed sad-melody-1 :loop? 0 :fade-out? 1 :reverb "??" :slice? 1
    (map (fn [[beat notes]]
           [(* 16 beat) notes :inst sampled-cello :reverb 0.8])
         (take 24 df-theme/melody)))
  [20/1]
  (def-composed mining-1 :loop? 1 :fade-in? 1 :fade-out? 1 :rate "??" :reverb "??" :slice? 1
    [1/1 (pickaxe 1) :loop? 0]
    [1/1 (pickaxe 2) :loop? 0]
    [1/1 (pickaxe 3) :loop? 0]
    [1/1 (pickaxe 4) :loop? 0]
    [1/1 (pickaxe 5) :loop? 0]
    [47/1])
  [35/1]
  [49/1 walking-on-wood-1 :fade-out? 1 :rate "run!!!" :reverb "??"  :slice? 1]
)

(def-player player-2
  (def-composed doors-and-walking-2 :loop? 1 :fade-out? 1 :slice? 1
    [6/1 (door-squeak 0) :amp 1 :loop? 0 :rate 0.8]
    [1/1]
    [7/1 (walking 1) :loop? 0]
    [2/1]
    [5/1 (walking 1) :loop? 0 :rate 1.5]
    [2/1]
    [5/1 (door-squeak 1) :amp 0.5 :loop? 0 :rate 1.2]
    [27/1])
  [15/1]
  (def-composed sad-bass-2 :loop? 0 :fade-out? 1 :reverb "??"
    (map (fn [[beat notes]]
           [(* 16 beat) notes :inst sampled-cello :reverb 0.8])
         (take 4 df-theme/bass)))
  [58/1]
  (def-composed conviviality-2 :fade-in? 1 :fade-out? 1 :loop? 1 :rate "1-1.5x"
    [14/1 (joviality 0) :loop? 0]
    [5/1 (joviality 1) :loop? 0]
    [25/1])
  [5/1]
  [39/1 doors-and-walking-2 :fade-out? 1 :rate "run!!!" :reverb "??" :slice? 1]
)

(def-player player-3
  [6/1]
  (def-composed doors-and-walking-3 :loop? 1 :fade-out? 1 :rate "1-1.5x" :slice? 1
    [5/1 (door-squeak 0) :rate 1.3 :loop? 0]
    [7/1 (walking 3) :rate 1.3 :amp 0.8]
    [4/1]
    [3/1 (chair 2) :loop? 0]
    [2/1]
    [3/1 (chair 0) :loop? 0]
    [25/1]) ; 50
  [20/1]
  (def-composed horse-wagon-3 :fade-in? 1 :fade-out? 1 :loop? 1 :rate "1-1.5x"
    [10/1 (horse-drawn-wagon 0) :loop? 1]
    [5/1 (horse-snort 0)]
    [13/1 (horse-drawn-wagon 1) :loop? 1]
    [30/1])
  [60/1]
  (def-composed pouring-drinks-break-glass-3 :fade-out? 1 :loop? 1 :rate "1-1.5x" :reverb "??"
    [14/1 (cork-pop-and-pour 0) :loop? 0]
    [1/2]
    [1/1 (burp 0) :loop? 0 :amp 0.5]
    [1/2]
    [12/1 (pouring-drink 0) :loop? 0]
    [1/2]
    [5/1 (breaking-glass 0) :loop? 0]
    [1/2]
    [3/1 (scream 0) :loop? 0]
    [15/1])
  [5/1]
  [30/1 doors-and-walking-3 :fade-out? 1 :rate "run!!!" :reverb "??" :slice? 1]
)

(def-player player-4
  [9/1]
  (def-composed doors-and-walking-4 :loop? 1 :fade-out? 1 :rate "1-1.5x" :slice? 1
    [1/2 (sniffle 1) :amp 1.5 :loop? 0]
    [1/2]
    [1/2 (sniffle 2) :amp 1 :loop? 0]
    [1/2 (sniffle 3) :amp 1 :loop? 0]
    [1/1]
    [8/1 (walking 1) :rate 0.9]
    [4/1]
    [5/2 (chair 3) :loop? 0]
    [3/2 (chair 3) :loop? 0 :rate 1.5] ; 36
    [27/1])
  [25/1]
  (def-composed crunchy-leaves-creaky-trees-4 :amp 1 :loop? 1 :fade-out? 1 :rate "1-1.25x" :reverb "??"
    [10/1 (walk-in-woods 0) :loop? 0 :rate 1.1 :amp 0.5]
    [50/1 (creaky-tree 0) :loop? 0 :fade-out? 1])
  [10/1]
  (def-composed mining-4 :amp 1 :loop? 1 :fade-in? 1 :fade-out? 1 :rate "??" :reverb "??" :slice? 1
    [1/2 (mining 1) :loop? 0]
    [1/2 (mining 1) :loop? 0]
    [1/1 (mining 5) :loop? 0]
    [1/2 (mining 3) :loop? 0]
    [1/2 (mining 3) :loop? 0]
    [1/1 (mining 2) :loop? 0]
    [82/2])
  [30/1]
  [55/1 doors-and-walking-4 :fade-out? 1 :rate "run!!!" :reverb "??" :slice? 1]
)

(def-player player-5
  [35/1]
  (def-composed lighting-hearth-5 :loop? 1 :fade-out? 1 :rate "1-1.25x"
    [5/1 (strike-match 0) :loop? 0]
    [1/2]
    [2/1 (blow-flame 0) :amp 2 :fade-in? 1 :rate 2 :loop? 0]
    [1/2]
    [47/1 (fire 0) :amp 0.8 :fade-in? 1])
  [45/1]
  (def-composed mining-5 :fade-in? 1 :fade-out? 1 :rate "??" :reverb "??" :slice? 1
    [3/1 (mining 0)]
    [3/1 (mining 6)]
    [3/1 (mining 0)]
    [3/1 (mining 3) :loop? 0]
    [48/1])
  [35/1]
  (def-composed goblins-and-ogres-5 :fade-out? 1 :rate "so scary" :reverb "0.5+" :slice? 1
    [3/1 (goblin 0) :loop? 0 :amp 0.5]
    [2/1 (ogre 0) :loop? 0]
    [1/1 (ogre 1) :loop? 0]
    [1/1 (ogre 2) :loop? 0]
    [1/1 (ogre 3) :loop? 0]
    [45/1])
)

(def-player player-6
  [40/1]
  (def-composed scribbling-plans-6 :loop? 1 :fade-out? 1 :slice? 1
    [1/1 (chair 0) :loop? 0 :rate 2]
    [1/1]
    [3/1 (parchment-rustle 0)]
    [4/1 (parchment-rustle 1)]
    [2/1 (dip-pen-in-ink 0) :loop? 0]
    [7/1 (scribble 1)]
    [3/2 (scribble 0) :amp 0.1 :loop? 0]
    [3/1 (scribble 4)]
    [4/1 (parchment-rustle 2) :loop? 0 :rate 1.2]
    [20/1])
  [97/2]
  (def-composed mining-6 :fade-in? 1 :fade-out? 1 :loop? 1 :reverb "??" :rate "??" :slice? 1
    [2/3 (pickaxe 0) :loop? 0] [1/3]
    [2/3 (pickaxe 5) :loop? 0] [1/3]
    [2/3 (pickaxe 0) :loop? 0] [1/3]
    [2/3 (pickaxe 2) :loop? 0] [1/3]
    [2/3 (pickaxe 0) :loop? 0] [1/3]
    [2/3 (pickaxe 1) :loop? 0] [1/3]
    [2/3 (pickaxe 0) :loop? 0] [1/3]
    [2/3 (pickaxe 4) :loop? 0] [1/3]
    [52/1])
  [30/1]
  (def-composed distant-drums-6 :fade-in? 1 :fade-out? 1 :loop? 1 :rate "1-1.5x"
    [60/1 (distant-drums 0)])
)

(def-player player-7
  [15/1]
  (def-composed guitar-theme-7 :fade-in? 1 :fade-out? 1 :amp 0.5 :rate "1-1.5x"
    [60/1 (theme 0) :loop? 1])
  [18/1]
  (def-composed chopping-wood-7 :loop? 1 :fade-out? 1 :rate "1-1.5x" :slice? 1
    [10/1 (wood-chop 0) :loop? 1 :amp 0.5]
    [8/1 (tree-falling 0) :loop? 0]
    [8/1 (bees 0) :loop? 0 :fade-in? 1 :reverb 0.1]
    [2/1 (pile-of-wood 0) :loop? 0 :reverb 0.1]
    [15/1])
  [25/1]
  (def-composed mining-discovery-7 :fade-in? 1 :loop? 1 :reverb? "??" :slice? 1
    [6/1 (mining 4) :loop? 0]
    [3/1 (mining 5) :amp 1.5]
    [1/1]
    [3/1 (mining 7) :amp 2.5 :loop? 0]
    [30/1])
  [50/1]
  [40/1 guitar-theme-7 :amp 0.5 :fade-in? 1 :fade-out? 1]
)

(comment
  (->> players (map prepare!) (apply concat) play!)
  (stop)
)
