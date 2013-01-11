(ns org.soulspace.clj.math.clj-math
  (:use
    [org.soulspace.clj.math.math :only [sqr fixed-point average-damp]]
    ))

; clojure implementation of mathematical functions

(defn exp [b n]
  (cond 
    (= n 0) 1
    (even? n) (sqr (exp b (/ n 2)))
    :default (* b (exp b (- n 1)))))

(defn sqrt [x]
  "square root of x"
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

(defn cbrt [x]
  (fixed-point (average-damp (fn [y] (/ x (sqr y)))) 1.0))
