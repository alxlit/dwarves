(ns dwarves.notation
  (:require [clojure.java.io :as io]
            [dwarves.core :refer :all]
            [quil.core :refer :all]))

(comment
  (notate! player-3)

  ;; render scores for all players
  (doall (map (comp sketch-close notate!) players))
)

(def floor #(java.lang.Math/floor %))
(def sum (partial reduce +))

(def black #(color 0))
(def dark-gray #(color 100))
(def light-gray #(color 227))
(def white #(color 255))

(def margin 10)
(def actual-size [1024 72])

(defn mm:ss
  [s]
  (let [mm (floor (/ s 60))
        ss (- s (* 60 mm))]
    (format "%d:%d" (int mm) (int ss))))

(defn hex->rgb
  [x]
  (for [n [16 8 0]]
    (-> (bit-shift-left 0xff n) (bit-and x) (bit-shift-right n))))

(def color-hex (comp (partial apply color) hex->rgb))

(def colors (atom {}))
(def colors-available
  (atom #{0xf0f0f0 0xececec 0xeaeaea 0xe6e6e6 0xe1e1e1 0xdedede
          0xd8d8d8 0xd4d4d4 0xd2d2d2}))

;  (atom #{0x93a1a1 0xeee8d5 0xfdf6e3 0xdc322f 0x2aa198 0x268bd2
;          0x6c71c4 0xd33682}))

(defn pick-color
  [name]
  (color-hex
   (or (get @colors name)
       (let [c (rand-nth (into [] @colors-available))]
         (swap! colors-available disj c)
         (swap! colors assoc name c)
         c))))

(defn draw-measures
  [{:keys [dx dt num-measures num-beats]} song]
  (let [{:keys [measure]} signature
        n (dec num-measures)
        spaces #(->> % float (repeat n) accumulate)
        xn (spaces (* dx measure))
        tn (spaces (* dt measure))
        y1 margin
        y2 (- (height) margin)]
    (doseq [[x t] (partition 2 (interleave xn tn))]
      (stroke (black))
      (stroke-weight 2)
      (fill (black))
      (line x y1 x y2)
      (text (mm:ss t) (+ x 6) margin))))

(defn draw-name
  [info song]
  (let [name (:name (meta song))]
    (fill (black))
    (text (str name) 0 margin)))

(defn icon
  [f]
  #(-> (str "icons/" f) io/resource io/file .getPath load-image))

(def loop-icon (icon "refresh-16.png"))
(def reverb-icon (icon "rss-16.png"))
(def slice-icon (icon "cut-16.png"))

(defn draw-fade-out
  [x y w h [s & {:keys [amp] :or {amp 1}}]]
  (let [x1 (+ 1 x)
        y1 (+ 1 y (* (- 1 amp) h))
        x2 (+ x w -2)
        y2 (+ y h -1)
        x3 x1
        y3 y2]
    (triangle x1 y1 x2 y2 x3 y3)))

(defn draw-fade-in
  [x y w h [s & {:keys [amp] :or {amp 1}}]]
  (let [x1 x
        y1 (+ y h -1)
        x2 (+ x w -2)
        y2 (+ 1 y (* (- 1 amp) h))
        x3 x2
        y3 (+ y h -1)]
    (triangle x1 y1 x2 y2 x3 y3)))

(defn draw-fade-in-and-out
  [x y w h s]
  (draw-fade-in x y (/ w 2) h s)
  (draw-fade-out (+ x (/ w 2) -3) y (/ w 2) h s))

(defn draw-sound
  [x y w h [s & {:keys [amp delay fade-in? fade-out? loop? rate reverb slice?]
                 :or {amp 1 delay 0 fade-in? 0 fade-out? 0 feedback 0 loop? 1
                      rate 1 reverb 0 slice? 0}}
            :as sound]]
  (when s
    (stroke (black))
    (stroke-weight 1)
    (fill (white))
    (rect x y (- w 1) h)

    ;; fill the rectangle to indicate volume
    (stroke-weight 0)
    (fill (pick-color name))

    ;; whatever
    (cond
     (and (= 1 fade-out?)
          (= 1 fade-in?)) (draw-fade-in-and-out x y w h sound)
     (= 1 fade-in?)       (draw-fade-in x y w h sound)
     (= 1 fade-out?)      (draw-fade-out x y w h sound)
     :eles
     (let [x (+ 1 x)
           y (+ 1 y (* (- 1 amp) h))
           w (- w 3)
           h (- (* h amp) 1)]
       (rect x y w h)))

    ;; indicate playback rate with an arrow (forwards/backwards)
    (let [x1 (+ x 2)
          y1 (+ y (/ h 2))
          x2 (+ x (- w 2) -2)
          y2 y1]
      (fill (black))
      (text (if (string? rate) rate (str rate "x")) (+ x1 (/ w 2) -10) (+ y1 15))
      (stroke (black))
      (stroke-weight 1)
      (line x1 y1 x2 y2)
      (let [[x1 x2] (if (or (string? rate) (pos? rate))
                      [(- x2 8) x2]
                      [(+ x1 8) x1])]
        (line x1 (- y1 6) x2 y1)
        (line x1 (+ y1 6) x2 y1)))

    ;; indicate looping with an icon
    (when (= loop? 1)
      (image (loop-icon) (+ x 2) (+ y h -18)))

    ;; indicate reverb
    (when (or (string? reverb) (pos? reverb))
      (image (reverb-icon) (+ x (if (= loop? 1) 22 2)) (+ y h -18))
      (stroke (black))
      (text (str reverb) (+ x (if (= loop? 1) 40 22)) (+ y h -6)))

    ;; indicate slicing
    (when (= slice? 1)
      (image (slice-icon) (+ x w -20) (+ y h -18)))

    ;; indicate delay
    (when (pos? delay)
      (stroke (black))
      (text (str "delay " delay " ms") (+ x 100) (+ y 12)))

    ;; write the sound name
    (when-let [name (or (:name (meta s))
                        (:name (meta (s 1))))]
      (fill (black))
      (text (str name) (+ x 5) (+ y 12)))))

(defn draw-sounds
  [{:keys [dx] :as info} song]
  (let [{:keys [unit]} signature
        h 40
        y (/ (- (height) h) 2)
        beats (map first song)
        beat->dx (comp float (partial * (/ dx unit)))
        sounds (map rest song)
        wn (map beat->dx beats)
        xn (cons 0 (drop-last (accumulate wn)))]
    (doall (map (partial apply draw-sound)
                (partition 5 (interleave xn (repeat y) wn (repeat h) sounds))))))

(defn draw
  [song]
  (let [{:keys [bpm measure unit]} signature
        num-measures (->> song
                          (map first)
                          (reduce +)
                          (* (/ 1 measure unit))
                          ceil)
        num-beats (* num-measures measure)
        dx (/ (actual-size 0) num-beats) ; spacing per beat
        dt (/ 60 bpm) ; seconds per beat
        info {:dx dx
              :dt dt
              :num-measures num-measures
              :num-beats num-beats}]
    (draw-name info song)
    (draw-measures info song)
    (draw-sounds info song)
    (save (str (:name (meta song)) ".png"))))

(defn setup
  [song]
  (frame-rate 0)
  (smooth)
  (background (white))
  (text-font (create-font "Roboto" 10 true))
  (draw song))

(defn notate!
  [song]
  (sketch :draw nil
          :setup (partial setup song)
          :size actual-size))

