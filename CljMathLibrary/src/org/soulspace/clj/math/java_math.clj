;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.java-math)

; clojure wrapper for java.lang.Math
(set! *warn-on-reflection* true)

(def pi
  (Math/PI))

(def e
  (Math/E))

(def ^:private ln-2 (Math/log 2))

(defn abs
  "Calculates the absolute of x (with java.lang.Math)."
  ^double [^double x]
  (Math/abs x))

(defn sign
  "Calculates the sign of x (with java.lang.Math)."
  ^double [^double x]
  (Math/signum x))

(defn floor
  "Calculates the floor of x (with java.lang.Math)."
  [x]
  (Math/floor x))

(defn ceil
  "Calculates the ceiling of x (with java.lang.Math)."
  [x]
  (Math/ceil x))

(defn sqrt
  "Calculates the square root of x (with java.lang.Math)."
  [x]
  (Math/sqrt x))

(defn cbrt
  "Calculates the cubic root of x (with java.lang.Math)."
  [x]
  (Math/cbrt x))

(defn pow
  "Calculates x raised to the power of y (with java.lang.Math)."
  [x y]
  (Math/pow x y))

(defn exp
  "Calculates the exponential function of x (with java.lang.Math)."
  [x]
  (Math/exp x))

(defn expm1
  "Calulates e to the power of x minus 1."
  [x]
  (Math/expm1 x))

(defn log
  "Calculates the natural logarithm (with base e) of x (with java.lang.Math)."
  [x]
  (Math/log x))

(defn log-with-base
  "Calculates the logarithm with base b of x."
  [b x]
  (/ (Math/log x) (Math/log b)))

(defn log10
  "Calculates the logarithm with base 10 of x (with java.lang.Math)."
  [x]
  (Math/log10 x))

(defn alog10
  "Calculates the inverse of the logarithm with base 2 of x (with java.lang.Math)."
  [x]
  (Math/pow 10 x))

(defn log2
  "Calculates the logarithm with base 2 of x (with java.lang.Math)."
  [x]
  (/ (Math/log x) ln-2))

(defn alog2
  "Calculates the inverse of the logarithm with base 2 of x (with java.lang.Math)."
  [x]
  (Math/pow 2 x))

(defn log1p
  "Calculates the natural logarithm of the sum of x and 1."
  [x]
  (Math/log1p x))

; Trigonometrical functions

(defn cos
  [x]
  "Calculates the cosine of x (with java.lang.Math)."
  (Math/cos x))

(defn sin
  "Calculates the sine of x (with java.lang.Math)."
  [x]
  (Math/sin x))

(defn tan
  "Calculates the tangens of x (with java.lang.Math)."
  [x]
  (Math/tan x))

(defn acos
  "Calculates the arc cosine of x (with java.lang.Math)."
  [x]
  (Math/acos x))

(defn asin
  "Calculates the arc sine of x (with java.lang.Math)."
  [x]
  (Math/asin x))

(defn atan
  "Calculates the arc tangens of x (with java.lang.Math)."
  [x]
  (Math/atan x))

(defn cosh
  "Calculates the hyperbolic cosine of x (with java.lang.Math)."
  [x]
  (Math/cosh x))

(defn sinh
  "Calculates the hyperbolic sine of x (with java.lang.Math)."
  [x]
  (Math/sinh x))

(defn tanh
  "Calculates the hyperbolic tangens of x (with java.lang.Math)."
  [x]
  (Math/tanh x))

(defn hypot
  "Calculates the hypothenuse of x and y (Pythagoras) (with java.lang.Math)."
  [x y]
  (Math/hypot x y))

(defn deg-to-rad
  "Converts degrees to radians (with java.lang.Math)."
  [deg]
  (Math/toRadians deg))

(defn rad-to-deg
  "Converts radians to degrees (with java.lang.Math)."
  [rad]
  (Math/toDegrees rad))
