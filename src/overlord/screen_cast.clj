(ns overlord.screen-cast
  (:use [overtone.live]
        [overlord.samples]
        [clojure.core.match :refer [match]]))

(defsynth scratch [amp 1 cutoff-lo 0 cutoff-hi 2000 gate 1 a 0.01 d 0.3 s 0 r 0]
  (let [snd (brown-noise)
        snd (lpf snd (lin-lin:kr (mouse-y) 0 1 cutoff-lo cutoff-hi))
        snd (+ snd (delay-n snd))
        delay-amp (delay-n amp)
        env (env-gen (adsr a d s r) :gate gate)]
    (out 0 (* amp env snd))))

(defn live-perc-sequencer
  ([metro live-patterns] 
   (let [cur-beat (metro)]
     (live-perc-sequencer metro live-patterns cur-beat cur-beat)))
  ([metro live-patterns beat start-beat]
   (doseq [[sound pattern] @live-patterns
           :let [cur-note (nth pattern (mod (- beat start-beat) (count pattern)))]
           :when (not= cur-note 0)]
     (if (sample? sound)
       (at (metro beat) (sound))
       (do
         (at (metro beat) (ctl sound :gate 0 :note (note cur-note)))
         (at (+ (metro beat) 50) (ctl sound :gate 1 :note (note cur-note))))))
   (let [next-beat (inc beat)]
     (apply-by (metro next-beat) #'live-perc-sequencer [metro live-patterns next-beat start-beat]))))

(def scr (scratch))
(def live-pats (atom {scr [1]}))
(def metro (metronome 240))
(def _ 0)
(live-perc-sequencer metro live-pats)
(swap! live-pats assoc scr [1])
(swap! live-pats assoc distkick [1 _ _ _])
(swap! live-pats assoc openhat [_ _ 1 _])
(swap! live-pats assoc clickhat [_ 1 _ _ _ _])
(swap! live-pats assoc clapsnare [_ _ _ _ 1 _ _ 1 _ _ _ 1 _ 1 _ _])
(swap! live-pats assoc glitchy [_ 1 _ _ _ _ _ _ _ _ _ _ _ _ _ _])
(swap! live-pats dissoc scr)
(swap! live-pats dissoc distkick)
(swap! live-pats dissoc clapsnare)
(swap! live-pats dissoc clickhat)
(swap! live-pats dissoc openhat)
(stop)

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
     (apply-by (metro next-beat) #'live-mel-sequencer [metro live-melody the-synth next-beat (inc offset)]))))

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

(defsynth electric-organ [note 60 amp 0.3 a 0.0 d 0.0 s 1 r 0.0 detune 1 gate 1 cutoff 800]
  (let [freq (midicps note)
        saws (concat (repeat 8 freq) (repeat 8 (* freq 2)))
        saws (map (fn [freq] (+ freq (* detune (rand)))) saws)
        snd  (saw saws)
        snd  (lpf snd cutoff)
        env  (env-gen (adsr a d s r) :gate gate :action FREE)]
    (out [0 1] (* amp env snd))))

(def live-mel (atom melody1))
(reset! live-mel melody2)
(stop)
(live-mel-sequencer metro live-mel electric-organ)

(defsynth long-wobble [note 32 amp 0.2 lfo-freq 10 lfo-depth 10 cutoff 600 detune 0.5 a 0.1 d 0 s 1 r 0.2 gate 1 fm-sin-freq 10 fm-sin-depth 5]
  (let [freq (midicps note)
        saws (repeat 8 (+ freq (* fm-sin-depth (sin-osc fm-sin-freq))))
        saws (map (fn [freq] (+ freq (* detune (rand)))) saws)
        saws (saw saws)
        snd  (+ saws (delay-n saws 0.1) (delay-n saws 0.2))
        snd  (lpf snd (+ cutoff (* lfo-depth (sin-osc lfo-freq))))
        env  (env-gen (adsr a d s r) :gate gate :action FREE)]
    (out 0 (* amp env snd))))

(def l (long-wobble))
(ctl l :amp 0.0)
(stop)
(ctl l :lfo-freq 30)
()
