(ns overlord.guitar-crap
  (:use [overtone.live]
        [overtone.synth.stringed]
        [overlord.endlessly-rising]))

(def our-guitar-chord-frets (assoc guitar-chord-frets :Bm6 [-1 2 0 1 0 2] :DM7 [-1 -1 0 2 2 2]))
(def oguitar-strum (partial strum-strings our-guitar-chord-frets guitar-string-notes))
(def pg (guitar))
(def mpp (midi-poly-player (partial ektara :gate 1)))
(mpp 60)
(def melody
  (create-melody
    [:C#5 :C#5 :C#5 :A4 :C#5 
     :D#5 :D#5 :D#5 :B4 :D#5
     :F#5 :F#5 :F#5 :E5 :F#5
     :D#5 :D#5 :D#5 :B4 :D#5]
    [1/2 1/4 1/12 1/12 1/12
     1/2 1/4 1/12 1/12 1/12
     1/2 1/4 1/12 1/12 1/12
     1/2 1/4 1/12 1/12 1/12]))
(def live-melody (atom melody))

(def chord-prog [[:Bm6 "d---------------------------dudu"] [:B7 "d--------------------dud"] [:DM7 "d------d"] [:B7 "d-------"]])
(def )
(def live-prog (atom chord-prog))
(reset! live-prog [[:Bm6 "ddududddudud"] [:B7 "ddududddudud"] [:DM7 "ddududddudud"] [:B7 "ddududddudud"]])

(defn- now+
  "add an epsilon of time to (now) to avoid lots of 'late' error messages"
  []
  (+ (now) 21)) ;; 21ms seems to get rid of most for me.

(defn- mkarg
  "useful for making arguments for the instruments strings"
  [s i]
  (keyword (format "%s-%d" s i)))

(defn pick-note
  "pick the instrument's string depending on the fret selected. A
  fret value less than -1 will cause no event; -1 or greater causes
  the previous note to be silenced; 0 or greater will also cause a
  new note event."
  ([the-inst string-index the-note t]
   (let []
     ;; turn off the previous note
     (if (>= the-note -1)
       (at t (ctl the-inst (mkarg "gate" string-index) 0)))
     ;; NOTE: there needs to be some time between these
     ;; FIXME: +50 seems conservative. Find minimum.
     (if (>= the-note 0)
       (at (+ t 50) (ctl the-inst
                         (mkarg "note" string-index) the-note
                         (mkarg "gate" string-index) 1)))))
  ([the-inst string-index the-note]
   (pick-note the-inst string-index the-note (now+))))


(defn live-melody-sequencer
  ([metro live-melody] 
   (let [cur-beat (metro)]
     (live-melody-sequencer metro live-melody cur-beat 0)))
  ([metro live-melody beat offset]
   (let [[cur-note dur] (nth @live-melody (mod offset (count @live-melody)))
         next-beat (+ beat dur)]
     (doall (my-pluck cur-note))
     (apply-by (metro next-beat) #'live-melody-sequencer [metro live-melody next-beat (inc offset)])
     )))

(def sg (guitar))
(def my-pluck (partial pick-note (guitar) 0))
(ctl sg :amp 0.2)
(ctl sg :distort 0.2)

(defn simul-start [arr]
  (doall (pmap (fn [[func args]] (apply func args)) arr)))

(let [metro (metronome 40)]
  (live-melody-sequencer metro live-melody))
(let [metro (metronome 40)
      cur-beat (metro)]
  (simul-start [[live-melody-sequencer [metro live-melody]]
                [live-chord-sequencer [metro live-prog]]]))
(stop)

(let [metro (metronome 20)]
  (live-chord-sequencer metro live-prog))

(defn play-chord [metro beat chord chord-pat]
  (let [num-strums (count chord-pat)]
    (println (str [chord chord-pat beat (metro)]))
    (for [[i dir] (map vector (range num-strums) chord-pat)
          :when (not= dir \-)
          :let [dir-sym (if (= dir \d) :down :up)]]
      (apply-by (metro (+ beat (* i (/ 1 num-strums)))) oguitar-strum [sg chord dir-sym]))))

(defn live-chord-sequencer
  ([metro live-prog]
   (let [cur-beat (metro)]
     (live-chord-sequencer metro live-prog cur-beat cur-beat)))
  ([metro live-prog beat start-beat]
     (let [[cur-chord strum-pat] (nth @live-prog (mod (- beat start-beat) (count @live-prog)))
           next-beat (inc beat)]
       (doall (play-chord metro beat cur-chord strum-pat))
       (apply-by (metro next-beat) #'live-chord-sequencer [metro live-prog next-beat start-beat])
       )))

(def sg (guitar))
(let [metro (metronome 20)]
  (live-chord-sequencer metro live-prog))
