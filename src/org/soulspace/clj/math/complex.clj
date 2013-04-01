(ns org.soulspace.clj.math.complex
  (:use [org.soulspace.clj.math math java-math]))

(set! *warn-on-reflection* true)
(declare complex)

; Complex numbers
(defprotocol Complex
  "Protocol for complex numbers in algebraic form."
  (real [c] "Real part of the complex number.")
  (img [c] "Imaginary part of the complex number.")
  (add [c c2] "Addition of complex numbers.")
  (sub [c c2] "Substraction of complex numbers.")
  (mult [c c2] "Multiplication of complex numbers.")
  (div [c c2] "Division of complex numbers.")
  (sqr-complex [c] "Square of the complex number.")
  (sqrt-complex [c] "Square root of the complex number.")
  (abs-complex [c] "Absolute or norm of the complex number.")
  (conjugate [c] "Conjugate of the complex number.")
  (to-polar [c] "Polar form of the complex number."))

(defrecord DoubleComplexImpl [^double real ^double img]
  Complex
  (real [c] (:real c))
  (img [c] (:img c))
  (add [c c2] (complex (+ (:real c) (:real c2)) (+ (:img c) (:img c2))))
  (sub [c c2] (complex (- (:real c) (:real c2)) (- (:img c) (:img c2))))
  (mult [c c2]
        (complex (- (* (:real c) (:real c2)) (* (:img c) (:img c2)))
                 (+ (* (:real c) (:img c2)) (* (:img c) (:real c2)))))
  (div [c c2]
       (complex (/ (+ (* (:real c) (:real c2)) (* (:img c) (:img c2)))
                   (+ (sqr (:real c2)) (sqr (:img c2))))
                (/ (- (* (:img c) (:real c2)) (* (:real c) (:img c2)))
                   (+ (sqr (:real c2)) (sqr (:img c2))))))
  (sqr-complex [c] (mult c c))
  (sqrt-complex [c])
  (abs-complex [c] (sqrt (+ (sqr (:real c)) (sqr (:img c)))))
  (conjugate [c] (complex (:real c) (* -1 (:img c))))
  (to-polar [c]
            (cond
              (and (== (:real c) 0) (== (:img c) 0)) [0 0]
              (== (:real c) 0) [(:img c) (if (< (:img c) 0) (/ pi 2) (/ (* 3 pi) 2))]
              (< (:real c) 0) [(sqrt (+ (sqr (:real c)) (sqr (:img c)))) (+ pi (atan (/ (:img c) (:real c))))]
              :default [(sqrt (+ (sqr (:real c)) (sqr (:img c)))) (atan (/ (:img c) (:real c)))])))

(defprotocol PolarComplex
  "Protocol for complex numbers in polar format"
  (r [p] "")
  (a [p] "")
  (sqrt-polar [p])
  (to-complex [p]))

(defrecord DoublePolarComplexImpl [^double r ^double a]
  PolarComplex
  (r [p] (:r p))
  (a [p] (:a p))
  (sqrt-polar [p])
  (to-complex [p]
              (mult (complex (:r p)) (complex (cos (:a p)) (sin (:a p))))))

(defn complex
  ([r]
    (DoubleComplexImpl. r 0))
  ([r i]
    (DoubleComplexImpl. r i)))

(defn polar-complex [r a]
  (DoublePolarComplexImpl. r a))

(defn zero []
  (complex 0 0))

(defn one []
  (complex 1 0))

(defn i []
  (complex 0 1))
