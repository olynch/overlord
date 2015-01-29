(ns overlord.rand-chords
  (:use [overtone.live]
        [overtone.synth.sampled-piano]))

(choose (keys CHORD))
(choose (keys NOTES))
(defn rand-note [octave]
  (keyword (str (name (choose (vals REVERSE-NOTES))) octave)))
(rand-note 4)

(defn my-rand-chord 
  ([octave]
   [(rand-note octave) (choose (keys CHORD))])
  ([octave chord-types]
   [(rand-note octave) (choose chord-types)])
  ([octave possible-tonics chord-types]
   [(choose possible-tonics) (choose chord-types)]))

(defn get-scale [tonic scale-type]
  (map #(+ (note tonic) %) (reductions + 0 (scale-type SCALE))))

(doall (my-rand-chord 4 (get-scale :C4 :purvi) [:M]))
(get-scale :C4 :arabic)
(rand-chord :C4 :m7 3 12)
(map sampled-piano (rand-chord :C4 :M7 4 36))
(map sampled-piano (apply chord (my-rand-chord 4 (get-scale :C4 :neapolitan-major) [:M :M7])))
(map sampled-piano (get-scale :C4 :bartok))
(def minor-chord-progs [{:scale-type :minor :prog [:i :VI :III :VII]}
                        {:scale-type :minor :prog [:i :VI :iv  :V  ]}
                        {:scale-type :minor :prog [:i :IV :VI  :V  ]}])
(def major-chord-progs [{:scale-type :major :prog [:I :vi :iii :V  ]}
                        {:scale-type :major :prog [:I :III :IV :iv ]}
                        {:scale-type :major :prog [:I :II :VII :iii]}])

(defn degree->chord [tonic the-scale the-degree]
  (let [[degree chord-type]
        (if (Character/isUpperCase (first (name the-degree)))
          [(keyword (clojure.string/lower-case (name the-degree))) :M]
          [(keyword (clojure.string/lower-case (name the-degree))) :m])
        chord-start (+ (note tonic) (degree->interval degree the-scale))]
    [chord-start chord-type]))

(defn prog->midi-chords [tonic the-scale prog]
  (map (partial degree->chord tonic the-scale) prog))

(defsynth square-pluck [note 60 amp 0.7 a 0.2 d 0.25 s 0 r 0 rvb-mix 0.8 rvb-room 0.9 rvb-damp 0.6]
  (let [freq (midicps note)
        snd (square freq)
        env (env-gen (adsr a d s r) :action FREE)
        snd (free-verb (* snd env) rvb-mix rvb-room rvb-damp)]
    (out [0 1] snd)))

(square-pluck)
(show-graphviz-synth square-pluck)

(defn play-chord-prog
  ([progression metro reps] (play-chord-prog progression metro (metro) reps))
  ([progression metro beat reps]
   (if (empty? progression)
     () 
     (let [the-chord (first progression)
           next-beat (+ beat reps)]
       (doall
         (map-indexed (fn [i [tonic chord-type]]
                        (at (metro (+ beat i)) (doall (map square-pluck (chord tonic chord-type (rand-int 3))))))
                      (repeat reps the-chord)))
       (apply-at (metro next-beat) #'play-chord-prog [(rest progression) metro next-beat reps])))))

(play-chord-prog (inf-prog :A3 minor-chord-progs) (metronome 80) 1 4)
(stop)

(defn inf-prog [tonic chord-progs]
  (concat (let [the-chord-prog (choose chord-progs)]
            (prog->midi-chords tonic (:scale-type the-chord-prog) (:prog the-chord-prog))) (lazy-seq (inf-prog tonic chord-progs))))

(play-chord-prog  :A 4 :minor)
(stop)

(degree->interval :iv :major)

