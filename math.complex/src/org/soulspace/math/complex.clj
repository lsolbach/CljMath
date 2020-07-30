;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.complex
  (:use [org.soulspace.math math java-math]))

(set! *warn-on-reflection* true)
(declare complex)

;;
;; Complex numbers
;;

; TODO: extract functions from records

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

(defprotocol PolarComplex
  "Protocol for complex numbers in polar format."
  (r [p] "")
  (a [p] "")
  (sqrt-polar [p])
  (to-complex [p]))

(defrecord DoubleComplexImpl
  [^double real ^double img]
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
  (sqrt-complex [c]) ; TODO implement sqrt
  (abs-complex [c] (sqrt (+ (sqr (:real c)) (sqr (:img c)))))
  (conjugate [c] (complex (:real c) (* -1 (:img c))))
  (to-polar [c]
            (cond
              (and (== (:real c) 0) (== (:img c) 0)) [0 0]
              (== (:real c) 0) [(:img c) (if (< (:img c) 0) (/ pi 2) (/ (* 3 pi) 2))]
              (< (:real c) 0) [(sqrt (+ (sqr (:real c)) (sqr (:img c)))) (+ pi (atan (/ (:img c) (:real c))))]
              :default [(sqrt (+ (sqr (:real c)) (sqr (:img c)))) (atan (/ (:img c) (:real c)))])))

(defrecord DoublePolarComplexImpl
  [^double r ^double a]
  PolarComplex
  (r [p] (:r p))
  (a [p] (:a p))
  (sqrt-polar [p]) ; TODO
  (to-complex [p]
              (mult (complex (:r p)) (complex (cos (:a p)) (sin (:a p))))))

(defn complex
  "Creates a complex number from real and imaginary parts."
  ([r]
   (DoubleComplexImpl. r 0))
  ([r i]
   (DoubleComplexImpl. r i)))

(defn polar-complex
  "Creates a complex number from polar coordinates."
  [r a]
  (DoublePolarComplexImpl. r a))

; TODO define and use constants

(defn zero
  "Returns the complex number zero."
  []
  (complex 0 0))

(defn one
  "Returns the complex number one."
  []
  (complex 1 0))

(defn i
  "Returns the complex number i"
  []
  (complex 0 1))