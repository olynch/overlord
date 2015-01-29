(ns overlord.live-perf
  (:use [overtone.live]
        [overlord.samples]))

(def _ 0)

(def live-pat (atom {(my-hat) [1 0 1 0]}))

(defsynth my-hat [a 0 d 0.05 s 0 r 0 cutoff 1000 gate 1]
  (let [snd (white-noise)
        snd (hpf snd cutoff)
        env (env-gen (adsr a d s r) :gate gate)]
    (out [0 1] (* snd env))))

(show-graphviz-synth my-kick)

(defsynth my-kick [freq 110 a 0 d 0.25 s 0 r 0 gate 1]
  (let [snd (sin-osc freq)
        env (env-gen (adsr a d s r) :gate gate)]
    (out [0 1] (* snd env))))

(def b (audio-bus))
(def b-s (my-kick 220 :out-bus b))
(compressor-demo [:after b-s ] b)
(stop)
(my-kick)
(kill k)
(def h (my-hat))
(def k (my-kick))
(def s (test-sin))
(ctl k :gate 0)
(stop)
(reset! live-pat {h [1 0 1 0]})
(defsynth test-sin [note 60]
  (out [0 1] (sin-osc (midicps note))))
(swap! live-pat assoc s [:C5 :C4])
(swap! live-pat assoc k [0 1 0 1])
(do
  (ctl h :gate 1)
  (ctl h :gate 0))

(def d (load-sample "~/Music/Samples/DISTKICK.wav"))

(defn live-perc-sequencer
  ([metro live-patterns] 
   (let [cur-beat (metro)]
     (live-perc-sequencer metro live-patterns cur-beat cur-beat)))
  ([metro live-patterns beat start-beat]
   (doseq [[sound pattern] @live-patterns
           :let [cur-note (nth pattern (mod (- beat start-beat) (count pattern)))]
           :when (not= cur-note 0)]
     (if (sample? sound)
       (sample-player sound)
       (do
         (at (metro beat) (ctl sound :gate 0 :note (note cur-note)))
         (at (+ (metro beat) 50) (ctl sound :gate 1 :note (note cur-note)))))
   (let [next-beat (inc beat)]
     (apply-by (metro next-beat) #'live-perc-sequencer [metro live-patterns next-beat start-beat]))))

(live-perc-sequencer (metronome 120) live-pat)
(ctl h :cutoff 1500)
(stop)
