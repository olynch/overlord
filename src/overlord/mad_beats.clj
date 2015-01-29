(ns overlord.mad-beats
  (:use [overtone.live]
        [overlord.samples]
        [clojure.core.match :refer [match]]))

(def _ 0)
(def pats {subkick   [1 _ _ _]
           clapsnare [_ _ _ _ 1 _ _ 1 _ _ _ 1 _ 1 _ _]
           glitchy   [_ 1 _ _ _ _ _ _ _ _ _ _ _ _ _ _]
           clickhat  [_ 1 _ _ _ _]
           openhat   [_ _ 1 _]})

(def live-pats (atom pats))
(reset! live-pats {scratch [1 1 1 1]})

(defn live-perc-sequencer
  ([metro live-patterns] 
   (let [cur-beat (metro)]
     (live-perc-sequencer metro live-patterns cur-beat cur-beat)))
  ([metro live-patterns beat start-beat]
   (doseq [[sound pattern] @live-patterns
           :when (= 1 (nth pattern (mod (- beat start-beat) (count pattern))))]
     (at (metro beat) (sound)))
   (let [next-beat (inc beat)]
     (apply-by (metro next-beat) #'live-perc-sequencer [metro live-patterns next-beat start-beat]))))

(live-perc-sequencer (metronome 280) live-pats)
(stop)

(defn create-mel [pitches timings]
  (map vector pitches (cycle timings)))

(def melody
  (create-mel
    [[:A#2 :9sus4] [:A#2 :9sus4] [:B2 :M7] [:C#3 :7] [:D#3 :m7] [:D#3 :m9] [:G#2 :7] [:G#2 :7]]
    [12 4]))

(defn reset-mel [pitches timings]
  (reset! live-melody (create-mel pitches timings)))

(def live-melody (atom melody))

(defn pause-mel []
  (reset! live-melody (create-mel [:_] [1])))

(defn simul-start [arr]
  (doall (pmap (fn [[func args]] (apply func args)) arr)))

(defn live-mel-sequencer
  ([metro live-patterns the-synth] 
   (let [cur-beat (metro)]
     (live-mel-sequencer metro live-patterns the-synth cur-beat 0)))
  ([metro live-melody the-synth beat offset]
   (let [[notes dur] (nth @live-melody (mod offset (count @live-melody)))
         next-beat (+ beat dur)
         play-fn (fn [n]
                   (let [s (the-synth n)]
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

(live-mel-sequencer (metronome 240) live-melody electric-organ)
(stop)

(defsynth scratch [amp 1 cutoff-lo 0 cutoff-hi 2000 gate 1 a 0.01 d 0.3 s 0 r 0]
  (let [snd (white-noise)
        snd (lpf snd (lin-lin:kr (mouse-y) 0 1 cutoff-lo cutoff-hi))
        snd (+ snd (delay-n snd))
        delay-amp (delay-n amp)
        env (env-gen (adsr a d s r) :gate gate :action FREE)]
    (out 0 (* amp env snd))))

(defsynth long-wobble [note 60 amp 0.5 lfo-freq 10 lfo-depth 10 cutoff 600 detune 0.5 a 0.1 d 0 s 1 r 0.2 gate 1 fm-sin-freq 10 fm-sin-depth 5]
  (let [freq (midicps note)
        saws (repeat 8 (+ freq (* fm-sin-depth (sin-osc fm-sin-freq))))
        saws (map (fn [freq] (+ freq (* detune (rand)))) saws)
        saws (saw saws)
        snd  (+ saws (delay-n saws 0.1) (delay-n saws 0.2))
        snd  (lpf snd (+ cutoff (* lfo-depth (sin-osc lfo-freq))))
        env  (env-gen (adsr a d s r) :gate gate :action FREE)]
    (out 0 (* amp env snd))))

(def w (long-wobble 30))
(kill w)
(ctl w :note 31)
(ctl w :amp 0.2)
(ctl w :lfo-freq 100)
(ctl w :lfo-depth 400)
(ctl w :)

(defsynth electric-organ [note 60 amp 0.5 a 0.0 d 0.0 s 1 r 0.0 detune 1 gate 1 cutoff 800]
  (let [freq (midicps note)
        saws (concat (repeat 8 freq) (repeat 8 (* freq 2)))
        saws (map (fn [freq] (+ freq (* detune (rand)))) saws)
        snd  (saw saws)
        snd  (lpf snd cutoff)
        env  (env-gen (adsr a d s r) :gate gate :action FREE)]
    (out [0 1] (* amp env snd))))

(defsynth deci-wobble []
  (let [temp-freq (/ 140 60 3)
        trig (impulse temp-freq)
        note (demand trig 0 (dseq [40 43 47 47 40 37 43 28] INF))
        note (slew:kr note 300 20)
        num-smp (/ (sample-rate) temp-freq)
        rate (/ (* 2 Math/PI) num-smp)
        rate (* rate 0.5 (demand:kr trig 0 (dseq [0.5 6 6 12 2 8 6 12] INF)))
        wobble (lag (cos (phasor:ar trig rate Math/PI (* 2 Math/PI))) 0.01)
        sub (* (lin-lin wobble -1 1 0 1)
               (sin-osc (/ (midicps note) 2 )))
        sub [sub sub]
        snd (+ (var-saw (midicps note) :width (lin-lin wobble -1 1 0.45 0.55))
               sub)
        snd (decimator snd 20000 (lin-lin wobble -1 1 1.2 8))
        snd (moog-ladder snd (lin-lin wobble -1 1 (midicps note) 25000) (lin-lin wobble -1 1 0.03 0.1))
        snd (* 0.75 [snd snd])
        snd [(delay-c snd 1 (lin-lin wobble -1 1 0 0.0012)) (delay-c snd 1 (lin-lin wobble -1 1 0.0012 0))]
        snd (* snd (linen:kr trig 0.01 2 (/ 1.3 temp-freq) :action NO-ACTION))
        ]
    (out 0 snd)))

(def d (deci-wobble))
(kill d)
(def d (distkick :action NO-ACTION))
(def d (load-sample "~/Music/Samples/DISTKICK.wav"))
(sample-player d)
(ctl d :gate 0)

(comment
  (reset! live-pats {scratch [1]})
  (swap! live-pats assoc distkick [1 _ _ _])
  (swap! live-pats dissoc subkick)
  (swap! live-pats assoc scratch [1])
  (swap! live-pats dissoc scratch)
  (swap! live-pats assoc openhat [_ _ 1 _])
  (swap! live-pats assoc clickhat [_ 1 _ _ _ _])
  (swap! live-pats assoc clapsnare [_ _ _ _ 1 _ _ 1 _ _ _ 1 _ 1 _ _])
  (swap! live-pats assoc glitchy [_ 1 _ _ _ _ _ _ _ _ _ _ _ _ _ _])
  (swap! live-pats dissoc glitchy)

  (let [metro (metronome 240)]
    (live-mel-sequencer metro live-melody electric-organ)
    (live-perc-sequencer metro live-pats))
  (stop))
