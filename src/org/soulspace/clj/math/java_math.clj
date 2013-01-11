(ns org.soulspace.clj.math.java-math)

(def pi
  (Math/PI))

(def e
  (Math/E))

(defn abs [x]
  "Absolute of x (java.lang.Math)"
  (Math/abs x))

(defn sign [x]
  "Sign of x (java.lang.Math)"
  (Math/signum x))

(defn floor [x]
  "Floor of x (java.lang.Math)"
  (Math/floor x))

(defn ceil [x]
  "Ceiling of x (java.lang.Math)"
  (Math/ceil x))

(defn sqrt [x]
  "Square root of x (java.lang.Math)"
  (Math/sqrt x))

(defn cbrt [x]
  "Cubic root of x (java.lang.Math)"
  (Math/cbrt x))

(defn pow [x y]
  "x raised to the power of y (java.lang.Math)"
  (Math/pow x y))

(defn exp [x]
  "Exponential function of x (java.lang.Math)"
  (Math/exp x))

(defn expm1 [x]
  (Math/expm1 x))

(defn log [x]
  "Natural logarithm (with base e) of x (java.lang.Math)"
  (Math/log x))

(defn log10 [x]
  "Logarithm base 10 of x (java.lang.Math)"
  (Math/log10 x))

(defn log1p [x]
  (Math/log1p x))

; Trigonometrical functions

(defn cos [x]
  "Cosine of x (java.lang.Math)"
  (Math/cos x))

(defn sin [x]
  "Sine of x (java.lang.Math)"
  (Math/sin x))

(defn tan [x]
  "Tangens of x (java.lang.Math)"
  (Math/tan x))

(defn acos [x]
  "Arc cosine of x (java.lang.Math)"
  (Math/acos x))

(defn asin [x]
  "Arc sine of x (java.lang.Math)"
  (Math/asin x))

(defn atan [x]
  "Arc tangens of x (java.lang.Math)"
  (Math/atan x))

(defn cosh [x]
  "Hyperbolic cosine of x (java.lang.Math)"
  (Math/cosh x))

(defn sinh [x]
  "Hyperbolic sine of x (java.lang.Math)"
  (Math/sinh x))

(defn tanh [x]
  "Hyperbolic tangens of x (java.lang.Math)"
  (Math/tanh x))

(defn hypot [x y]
  "Hypothenuse of x and y (Pythagoras) (java.lang.Math)"
  (Math/hypot x y))

;(defn rad-by-deg [deg]
;  "Convert degrees to radians (java.lang.Math)"
;  (Math/toRadians deg))

;(defn deg-by-rad [rad]
;  "Convert radians to degrees (java.lang.Math)"
;  (Math/toDegrees rad))
