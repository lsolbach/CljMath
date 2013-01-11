(ns org.soulspace.clj.math.geometry
  (:use 
    [org.soulspace.clj.math.math]
    [org.soulspace.clj.math.java-math :only [pi]]
    ))

(defn circle-circumference [r]
  (* 2 (pi) r))

(defn circle-area [r]
  (* (pi) (sqr r)))

