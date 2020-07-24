;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.methods
  (:require [org.soulspace.math.java-math]))

;;
;; Mathemathical algorithms and methods
;;

;;
;; number theory
;;

(defn gcd
  "Calculates the greatest common divisor of x and y"
  [x y]
  (if (= y 0)
    x
    (gcd y (rem x y))))

; TODO this fn is subject to a StackOverflowException, find a better implementation.
(defn primes
  "Returns a lazy sequence of primes."
  ([]
   (primes (iterate inc 2)))
  ([s]
   (cons (first s)
         (lazy-seq (primes (filter #(not= 0 (mod % (first s))) (rest s)))))))

;;
;; 
;;

(defn quadratic-roots
  "Returns the roots of the quadratic equation (a * x^2 + b * x + c = 0)."
  [a b c]
  (let [d (- (* b b) (* 4 a c))]
    [(/ (+ (- b) (sqrt d)) (* 2 a))
     (/ (- (- b) (sqrt d)) (* 2 a))]))
