(ns overlord.screen-cast
  (:use [overtone.live]
        [overlord.samples]
        [clojure.core.match :refer [match]]))

(defn create-mel [pitches timings]
  (map vector pitches (cycle timings)))

(def melody1
  (create-mel
    [[:A#2 :9sus4] [:A#2 :9sus4] [:B2 :M7] [:C#3 :7] [:D#3 :m7] [:D#3 :m9] [:G#2 :7] [:G#2 :7]]
    [12 4]))

(def melody2
  (create-mel
    [[:G#2 :m7] [:G#2 :m7] [:A#2 :m7] [:A#2 :7] [:D#3 :m7] [:D#3 :m9] [:D#3 :7] [:D#3 :9]]
    [12 4]))

(def live-mel (atom melody1))
(reset! live-mel melody2)

(defn live-mel-sequencer
  ([metro live-patterns the-synth] 
   (let [cur-beat (metro)]
     (live-mel-sequencer metro live-patterns the-synth cur-beat 0)))
  ([metro live-melody the-synth beat offset]
   (let [[notes dur] (nth @live-melody (mod offset (count @live-melody)))
         next-beat (+ beat dur)
         play-fn (fn [n]
                   (let [s (#'electric-organ n)]
                     (at (- (metro next-beat) 250) (ctl s :gate 0))
                     ))]
     (match [notes]
            [:_]
            :_
            [(n :guard keyword?)]
            (at (metro beat) (play-fn (note n)))
            ; if it's a symbol, play that note
            [[start chord-type]]
            ; if it's two symbols, play the chord defined by those symbols
            (doall (map play-fn (chord start chord-type)))
            [([& the-chord] :seq)]
            (doall (map (comp play-fn note) the-chord)))
     ;(apply-by (metro next-beat) #'live-mel-sequencer [metro live-melody the-synth next-beat (inc offset)])
     )))

(live-mel-sequencer (metronome 240) live-mel electric-organ)

(defsynth electric-organ [note 60 amp 0.5 a 0.0 d 0.0 s 1 r 0.0 detune 1 gate 1 cutoff 800]
  (let [freq (midicps note)
        saws (concat (repeat 8 freq) (repeat 8 (* freq 2)))
        saws (map (fn [freq] (+ freq (* detune (rand)))) saws)
        snd  (saw saws)
        snd  (lpf snd cutoff)
        env  (env-gen (adsr a d s r) :gate gate :action FREE)]
    (out [0 1] (* amp env snd))))

(def mpp (midi-poly-player electric-organ))
