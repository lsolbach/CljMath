;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.math
  (:use 
    [org.soulspace.clj.math.java-math 
     ;:only [pi e floor abs exp sin]
     ]))

; mathematical functions 

; some functions are in the spirit of 
; 'Structure and Interpretation of Computer Programs'
; by Abelson, Sussman and Sussman

(def epsilon 0.001)
(def dx 0.0000001)

(defn sqr
  "Calculates the square of x."
  [x]
  (* x x))

(defn cube
  "Calculates the cube of x."
  [x]
  (* x x x))

(defn avg 
  "Calculates the avarage of x and y or of the values of coll."
  ([x y]
    (/ (+ x y) 2))
  ([coll]
    (/ (reduce + 0 coll) (count coll))))

(defn gcd
  "Calculates the greatest common divisor of x and y"
  [x y]
  (if (= y 0)
    x
    (gcd y (rem x y))))

(defn round-up
  [x n]
  (/ (floor (+ (* x (exp 10 n)) 0.5)) (exp 10 n )))

(defn close-enough?
  [x y]
  "Checks for a difference smaller than epsilon"
  (< (abs (- x y)) epsilon))

(defn average-damp [f]
  (fn [x] (avg x (f x))))


; refactor to loop/recur
(defn sum
  "Calculates the sum of term between a and b with the step function nxt."
  [term a nxt b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (nxt a) nxt b))))

; refactor to loop/recur
(defn prod
  "Calculates the product of term between a and b with the step function nxt."
  [term a nxt b]
  (if (> a b)
    0
    (* (term a)
       (prod term (nxt a) nxt b))))

(defn integral
  "Calculates the integral of function f between a and b with dx."
  [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(defn search
  "Searches for zero."
  [f neg-point pos-point] 
  (let [midpoint (avg neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
        midpoint
        (let [test-value (f midpoint)]
          (cond
            (pos? test-value) (recur f neg-point midpoint)
            (neg? test-value) (recur f midpoint pos-point)
            :default midpoint)))))

(defn half-intervall
  "Half intervall method for the function f and values a and b."
  [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond
      (and (neg? a-value) (pos? b-value)) (search f a b)
      (and (neg? b-value) (pos? a-value)) (search f b a)
      :default (throw (RuntimeException. (str "Values are not of opposite sign: " a " " b))))))

(defn fixed-point
  "Calculates a fixed point of the function f."
  [f first-guess]
  (defn try-guess [guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
          next-guess
          (recur next-guess))))
  (try-guess first-guess))

(defn deriv
  "Derives the function g."
  [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

(defn newton-transform
  [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newton-method
  "Newton method for searching a root of the function g."
  [g guess]
  (fixed-point (newton-transform g) guess))

(defn quadratic-roots
  "Returns the roots of the quadratic equation (a * x^2 + b * x + c = 0)."
  [a b c]
  (let [d (- (* b b) (* 4 a c))]
    [(/ (+ (- b) (sqrt d)) (* 2 a))
     (/ (- (- b) (sqrt d)) (* 2 a))]))

(defn factorial
  "Calculates the factorial of x."
  [x]
  (loop [curr x fact 1]
    (cond
      (<= curr 0) 0
      (= curr 1) fact
      :default (recur (dec curr) (*' curr fact)))))

(defn fibonacci
  "Calculates the fibonacci number of x."
  [x]
  (loop [a 1 b 0 cnt x]
    (cond
      (= cnt 0) b
      :default (recur (+' a b) a (dec cnt)))))