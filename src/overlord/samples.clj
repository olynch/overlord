(ns overlord.samples
  (:use [overtone.live]))

(def sample-path "~/Music/Samples/")
(defn get-sample [path]
  (sample (str sample-path path ".wav")))

(def clickhat (get-sample "CLICKHAT"))
(def blooper (get-sample "BLOOPER"))
(def clapsnare (get-sample "CLAPSNARE"))
(def distkick (get-sample "DISTKICK"))
(def glitchy (get-sample "GLITCHY"))
(def homeclap (get-sample "HOMECLAP"))
(def openhat (get-sample "OPENHAT"))
(def rimshot (get-sample "RIMSHOT"))
(def subkick (get-sample "SUBKICK"))
(def tommy (get-sample "TOMMY"))
