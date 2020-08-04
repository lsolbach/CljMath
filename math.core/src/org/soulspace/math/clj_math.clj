;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.clj-math
  (:use [org.soulspace.math.core :only [pi sqr fixed-point average-damp]]))

;;
;; Clojure implementation of some mathematical functions
;;

(defn rad-to-deg
  "Converts radians to degrees"
  [rad]
  (* (/ 180 pi) rad))

(defn deg-to-rad
  "Converts degrees to radians"
  [deg]
  (* deg (/ pi 180)))

(defn exp
  "Calculates the exponential function."
  [b n]
  (cond 
    (= n 0) 1
    (even? n) (sqr (exp b (/ n 2)))
    :default (* b (exp b (- n 1)))))

(comment
  (defn sqrt
    "Calculates the square root of x."
    [x]
    (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

  (defn cbrt
    "Calculates the cubic root of x."
    [x]
    (fixed-point (average-damp (fn [y] (/ x (sqr y)))) 1.0))
)

