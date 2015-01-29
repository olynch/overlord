(ns overlord.endlessly-rising
  (:use [overtone.live]
        [overtone.synth.sampled-piano]))

(defn my-note [notename]
  (case notename
    :_ -1
    (note notename)))

(defn create-melody [pitches durations]
  (map vector (map my-note pitches) durations))

(def top
  (create-melody
    [:d6 :e6 :f6 :f#6 :g6 :g#6
     :a6 :a#6 :g6 :d#6 :d6
     :c#6 :_ :a6 :g#6 :g6 :f#6 :f6 :e6 :d#6 :c6 :b5
     :e6 :_ :a6 :g6 :f#6]
    [3/8 1/8 1/8 1/8 1/8 1/8
     1/2 5/16 1/16 1/16 1/16
     1/4 1/2 1/2 1/2 1/2 1/2 1/2 3/8 1/8 1/8 1/8
     1/4 1/2 1/2 1/4 1/2]))

(def middle
  (create-melody
    [:_ :a5 :c6 :b5 :a5 :g#5 :a5 :b5 :c6 :d6 :f6 :e6 :d6
     :c6 :a5 :c6 :e6 :a6 :g6 :f#6
     :g6 :c#6 :b5 :c#6 :d6 :a5 :d6 :e6 :f6
     :f6 :e6 :d6 :c6 :d6 :c6 :b6
     :a5 :a#5 :b5 :_ :c6 :b5 :a5
     :g#5 :a5 :g#5 :a5 :b5 :a5 :g5 :f#5 :e5 :f#5 :g5 :a5 :f#5 :g5 :a5
     :b5 :a6 :g6 :f#6 :g6 :e6 :c#6 :f6 :e6
     :d#6 :e6 :f#6 :_ :a#5 :b5 :_]
    [1/8 3/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16
     1/16 1/16 1/16 1/16 1/2 1/8 1/8
     5/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 3/8
     1/8 1/8 1/8 5/16 1/16 1/16 1/16
     1/8 1/8 1/4 1/8 1/8 1/8 1/8 1/8
     1/8 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16
     1/8 1/4 1/16 1/16 1/8 1/8 1/8 1/16 1/16
     1/8 1/16 1/16 1/8 1/8 1/4 1/4]))

(def base
  (create-melody
    [:f4 :d4 :f4 :a4 :d5 :c5 :b4
     :c5 :f#4 :e4 :f#4 :g4 :d4 :g4 :a4 :a#4
     :a#4 :a4 :g4 :f4 :g4 :f4 :e4
     :d4 :d#4 :e4 :e4 :f4 :e4 :d4
     :c#4 :d4 :c#4 :d4 :e4 :d4 :c4 :b3 :a3 :b3 :c4 :d4 :b3 :c4 :d4
     :e4 :d5 :c5 :b4 :c5 :a4 :f#4 :b4 :a4
     :g#4 :a4 :b4 :c5 :d#5 :e4 :_
     :_ :e4 :g4 :f#4 :e4 :d#4 :e4 :f#4 :g4 :a4 :c5 :b4 :a4]
    [1/16 1/16 1/16 1/16 1/2 1/8 1/8
     5/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 3/8
     1/8 1/8 1/8 5/16 1/16 1/16 1/16
     1/8 1/8 1/4 1/8 1/8 1/8 1/8
     1/8 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16
     1/8 1/4 1/16 1/16 1/8 1/8 1/8 1/16 1/16
     1/8 1/16 1/16 1/8 1/8 1/4 1/4
     1/8 3/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16]))

(def metro (metronome 70))

(sampled-piano :decay (* 2 1/2) :sustain 0.2)

(defn play [beat notes]
  (let [[note duration] (first notes)
        next-beat (+ beat (* duration 4))]
    (case note
      -1 ()
      (at (metro beat) (sampled-piano :note note :decay (* duration 8))))
    (apply-by (metro next-beat) #'play [next-beat (rest notes)])))

(defn transpose [steps notes]
  (map (fn [[note offset duration]] [(+ steps note) offset duration]) notes))

(defn infinite-melody
  ([melody] (infinite-melody melody 0))
  ([melody n] (concat (transpose n melody) (lazy-seq (infinite-melody melody (+ 2 n))))))

(play (metro) (infinite-melody top))
(defn infinite-play [melody]
  (play (metro) (infinite-melody melody)))
(map infinite-play (map (partial transpose -2) [top middle base]))
(stop)

(play (metro) (concat melody (transpose melody 3)))
(play (metro) melody)
(play (metro) (transpose melody 1))
(transpose melody 0)
