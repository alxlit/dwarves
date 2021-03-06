(ns dwarves.df-theme)

;; original time and key signature information
(def signature {:bpm 120
                :measure 4
                :unit 1/4
                :key :D4
                :scale :major})

(def melody
  [[1/8]
   [1/8 #{:F#5 :B4}]
   [1/8 #{:E#5 :D#5 :B4}]
   [1/8 #{:A4 :F#4}]
   [1/8 #{:G#4 :F#4}]
   [1/8 #{:A4 :F#4}]
   [1/8 #{:E#5 :C#5}]
   [1/4 #{:F#5 :B4}] ; --
   [1/8 #{:F#5 :B4}]
   [1/8 #{:E#5 :B4}]
   [1/8 #{:A4 :F#4}]
   [1/8 #{:G#4 :F#4}]
   [1/8 #{:A4 :F#4}]
   [1/8 #{:E#5 :C#5}]
   [1/4 #{:F#5 :B4}] ; --
   [1/8 #{:F#5 :B4}]
   [1/8 #{:Eb5 :B4}]
   [1/8 #{:B4 :A4}]
   [1/8 #{:F#4 :E4}]
   [1/8 #{:B4 :F#4 :E4}]
   [1/8 #{:Eb5 :A4}]
   [1/4 #{:F#5 :B4}] ; --
   [1/8 #{:F#5 :B4}]
   [1/8 #{:E5 :B4}]
   [1/8 #{:B4 :A4}]
   [1/8 #{:F#4 :E4}]
   [1/8 #{:B4 :F#4 :E4}]
   [1/8 #{:D#5 :A4}]
   [1/4 #{:F#5 :B4}]])

(def bass
  [[1/1 #{:C#3 :C#2}]
   [1/1 #{:C#3 :C#2}]
   [1/1 #{:B3 :B2}]
   [1/1 #{:B3 :B2}]
   [1/1 #{:C#3 :C#2}]])
