(ns overlord.blizzard-symphony
  (:use [overtone.live]
        [overtone.synth.sampled-piano]
        [overtone.synth.stringed]
        [overtone.synth.sts]
        [overtone.synth.ixi]
        [overtone.studio.scope]
        [overlord.samples]))

(def _ 0)
(def pats {subkick   [1 _ _ _]
           clapsnare [_ _ _ _ 1 _ _ 1 _ _ _ 1 _ 1 _ _]
           glitchy   [_ 1 _ _ _ _ _ _ _ _ _ _ _ _ _ _]
           clickhat  [_ 1 _ _ _ _]
           openhat   [_ _ 1 _]})

(def live-pats (atom pats))

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

(def metro (metronome 480))
(live-perc-sequencer metro live-pats)
(stop)

(defsynth fm-pulse-sin [freq 440 freq2 440]
  (out [0 1] (pulse (* freq2 (sin-osc freq)))))

(defsynth fm-sin-sin [freq 440 freq2 440]
  (out [0 1] (sin-osc (* freq2 (sin-osc freq)))))

(def my-gate (atom 1))

(defsynth supersaw [basefreq 440 detune 0.35 lfo-freq 10 depth 110 cutoff 880 a 0.5 d 0 s 1 r 0.35 gate 1]
  (let [freqs (repeat 8 basefreq)
        freqs (map #(+ (* detune (rand)) %) freqs)
        snd (saw freqs)
        snd (lpf snd (+ cutoff (* depth (sin-osc:kr lfo-freq))))]
    (out [0 1] 
         (*
          (env-gen (env-adsr a d s r) :gate gate :action FREE)
          snd))))

(definst midi-saw [note 60 velocity 100 detune 0.35 lfo-freq 10 depth 110 cutoff 880 a 0.5 d 0 s 1 r 0.35 gate 1]
  (let [basefreq (midicps note)
        freqs (repeat 8 basefreq)
        freqs (map #(+ (* detune (rand)) %) freqs)
        amp (/ velocity 127.0)
        snd (saw freqs)
        snd (lpf snd (+ cutoff (* depth (sin-osc:kr lfo-freq))))
        env (env-gen (env-adsr a d s r) :gate gate)]
    (* amp env snd)
  ))

(def sawer (midi-poly-player midi-saw))

(defn play-dur [metro beats args]
  (let [ss (apply supersaw args)]
    (at (metro (+ (metro) beats)) (ctl ss :gate 0))))

(defn play-beat [metro beat args]
  (let [ss (apply supersaw args)]
    (at (metro (inc beat)) (ctl ss :gate 0))))

(def metro (metronome 120))
(play-beat metro (metro) [:basefreq 220])

(def chord-prog [[:b3 :m6] [:b3 :7] [:d4 :M7] [:b3 :7]])
(def live-prog (atom chord-prog))

(defn play-chord [metro beat cur-chord]
  (map (comp (partial play-dur metro 1) vector midi->hz note) (apply chord cur-chord)))

(play-chord (metronome 120) 4 [:b3 :m6])

(defn play-chord-prog
  ([metro chord-prog]
   (let [cur-beat (metro)]
     (play-chord-prog metro chord-prog cur-beat cur-beat)))
  ([metro chord-prog beat start-beat]
   (doall 
     (let [cur-chord (nth @chord-prog (mod (- beat start-beat) (count @chord-prog)))]
       (println (str [cur-chord beat start-beat]))
       (play-chord metro beat cur-chord)))
   (let [next-beat (+ beat 1)]
     (apply-by (metro next-beat) #'play-chord-prog [metro chord-prog next-beat start-beat])
     )
  ))

(defn play-chord-wrapper [metro beat cur-chord]
  (let [my-chord cur-chord]
    (play-chord metro beat my-chord)))

(play-chord-wrapper (metronome 120) 1 [:b3 :m6])

(play-chord-prog (metronome 120) live-prog)
(stop)

(def ss (supersaw 330 0.2 :cutoff 440))
(ctl ss :gate 0)
(kill ss)
(map (comp (partial play-dur (metronome 120) 4) vector midi->hz note) (chord :b3 :m6))
(ctl ss :cutoff 880)
(scope 0)
(stop)

;(defn flatten1
  ;"Takes a map and returns a seq of all the key val pairs:
  ;(flatten1 {:a 1 :b 2 :c 3}) ;=> (:b 2 :c 3 :a 1)"
  ;[m]
  ;(reduce (fn [r [arg val]] (cons arg (cons val r))) [] m))

;(defn live-sequencer
  ;([curr-t sep-t live-patterns] (live-sequencer curr-t sep-t live-patterns 0))
  ;([curr-t sep-t live-patterns beat]
   ;(doseq [[sound pattern] @live-patterns
           ;:let [v (nth pattern (mod beat (count pattern)))
                 ;v (cond
                     ;(= 1 v)
                     ;[]
                     ;(map? v)
                     ;(flatten1 v)
                     ;:else
                     ;nil)]
           ;:when v]
     ;(at curr-t (apply sound v)))
   ;(let [new-t (+ curr-t sep-t)]
     ;(apply-by new-t #'live-sequencer [new-t sep-t live-patterns (inc beat)]))))

;(def a {:rate 0.5})
;(def b {:rate 3})
;(def c {:rate 10})
;(live-sequencer (+ 200 (now)) 200 live-pats)
;(stop)
