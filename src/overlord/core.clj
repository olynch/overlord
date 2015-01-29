(ns overlord.core
  (:use [overtone.live]
        [overtone.synth.sampled-piano]))

(definst foo [freq 440] (sin-osc freq))
(sampled-piano 50)

(def metro (metronome 100))
(defn player [beat]
  (at (metro beat) (sampled-piano 62))
  (apply-by (metro (inc beat)) #'player (inc beat) []))

(defn play-notes [beat notes]
  (at (metro beat) (sampled-piano (nth (first notes) 0)))
  (let [next-beat (+ beat (nth (first notes) 1))]
    (apply-by (metro next-beat) #'play-notes [next-beat (rest notes)])))

(defn play-notes-trem [beat notes]
  (at (metro beat) (trem (midi->hz (nth (first notes) 0)) 10 6 0.5))
  (let [next-beat (+ beat (nth (first notes) 1))]
    (apply-by (metro next-beat) #'play-notes-trem [next-beat (rest notes)])))

(definst trem [freq 440 depth 10 rate 6 length 3]
  (* 0.6
     (env-gen (env-adsr 0 0.75 0 0) 1 1 0 1 FREE)
     (saw (+ freq (* depth (sin-osc:kr rate))))))

(definst our-saw [freq 440 a 0.5 d 0.5 s 1 r 0 cutoff 400 reverb 5]
  (*
   10
   (env-gen (env-adsr a d s r) 1 1 0 1 FREE)
   (free-verb (lpf (saw freq) cutoff)) 0.4 0.80 0.50))

(reduce * 1 [1 2 3 4])
(map our-saw (map midi->hz (chord :f4 :major)))
(stop)
(our-saw)
(trem 220)
(kill trem)


(play-notes (metro) (cycle [[60 4] [64 1] [67 3] [71 4]]))
(play-notes-trem (metro) (cycle [[60 1] [64 3] [67 2] [71 2]]))
(metro-bpm metro 300)
(player (metro))
(foo 440)
(stop)
