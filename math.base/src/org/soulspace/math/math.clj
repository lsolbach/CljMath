;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.math
  (:require [org.soulspace.math.java-math :refer :all]))
         ;:only [pi e floor abs sqrt exp sin atan asin]

;;
;; Mathematical functions and algorithms
;;
;; implements algorithms e.g. from
;;   Abelson, Sussman and Sussman; 'Structure and Interpretation of Computer Programs';
;;

;(set! *warn-on-reflection* true)

(def default-epsilon "Default tolerance (epsilon)." 0.00001)
(def default-dx "Default step size (delta x)." 0.0000001)

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

(defn factorial
  "Calculates the factorial of x."
  [^long x]
  (loop [curr x fact 1]
    (cond
      (<= curr 0) 0
      (= curr 1) fact
      :default (recur (dec curr) (*' curr fact)))))

(defn fibonacci
  "Calculates the fibonacci number of x."
  [^long x]
  (loop [a 1 b 0 cnt x]
    (cond
      (= cnt 0) b
      :default (recur (+' a b) a (dec cnt)))))

;
; special trigonometric functions
;
(defn atan2 [a b]
  "Calculates the arc tangens of a/b. Returns the result in the correct quadrant."
  (let [r (atan (/ a b))] ; TODO handle b = 0 case
    (cond
      (< b 0) (if (< a 0)
                 (- r pi) ; adjust quadrant
                 (+ r pi)) ; adjust quadrant
      :default r)))

(defn hav
  "Calculates the haversine function of the angle a."
  [x]
  (sqr (sin (/ x 2))))

(defn ahav
  "Calculates the arc haversine function of the value v."
  [x]
  (* 2 (asin (sqrt x))))

;
; special functions
;
(defn- tau-erf
  [x]
  (let [t (/ 1
             (+ 1 (* 1/2 (abs x))))]
    (- 1 (* t (exp (+ (* -1 x x)
                      -1.26551223
                      (*  1.00002368 t)
                      (*  0.37409196 t t)
                      (*  0.09678418 t t t)
                      (* -0.18628806 t t t t)
                      (*  0.27886807 t t t t t)
                      (* -1.13520398 t t t t t t)
                      (*  1.48851587 t t t t t t t)
                      (* -0.82215223 t t t t t t t t)
                      (*  0.17087277 t t t t t t t t t)))))))

(defn erf
  "Calculates the gaussian error function."
  [x]
  (let [z (tau-erf x)]
    (if (>= x 0)
      z
      (* -1 z))))

(defn erfc
  "Calculates the complementary gaussian error function."
  [x])
  ; TODO implement


(defn round-up
  "Rounds a value."
  [x n]
  (/ (floor (+ (* x (exp 10 n)) 0.5)) (exp 10 n)))

(defn close-enough?
  "Checks for a difference smaller than epsilon"
  ([x y]
   (close-enough? x y default-epsilon))
  ([x y epsilon]
   (< (abs (- x y)) epsilon)))

(defn average-damp [f]
  "Returns a function with average dampening for the given function."
  (fn [x] (avg x (f x))))

(defn sum
  "Calculates the sum of term between a and b with the step function nxt."
  [term a nxt b]
  ; TODO refactor to loop/recur
  (if (> a b)
    0
    (+ (term a)
       (sum term (nxt a) nxt b))))

(defn prod
  "Calculates the product of term between a and b with the step function nxt."
  [term a nxt b]
  ; TODO refactor to loop/recur
  (if (> a b)
    0
    (* (term a)
       (prod term (nxt a) nxt b))))

;;
;; 
;;

(defn quadratic-roots
  "Returns the roots of the quadratic equation (a * x^2 + b * x + c = 0)."
  [a b c]
  ; minus is subject to catastrophic cancelling of significant digits in
  ; floating point calculations
  ; TODO: refactor for better numeric stability
  (let [d (- (* b b) (* 4 a c))]
    [(/ (+ (- b) (sqrt d)) (* 2 a))
     (/ (- (- b) (sqrt d)) (* 2 a))]))

;;
;; calculus/analysis
;;
(defn search-value
  "Searches for value by interval search."
  ([f v low high]
   (search-value f v low high default-epsilon))
  ([f v low high epsilon]
   (let [mid (avg low high)]
     (if (close-enough? low high epsilon)
         mid
         (let [v-test (f mid)]
           (cond
             (> v-test v) (recur f v low mid epsilon)
             (< v-test v) (recur f v mid high epsilon)
             :default mid))))))

(defn search-zero
  "Searches for zero by interval search."
  ([f neg-point pos-point]
   (search-zero f neg-point pos-point default-epsilon))
  ([f neg-point pos-point epsilon]
   (let [midpoint (avg neg-point pos-point)]
     (if (close-enough? neg-point pos-point epsilon)
         midpoint
         (let [test-value (f midpoint)]
           (cond
             (pos? test-value) (recur f neg-point midpoint epsilon)
             (neg? test-value) (recur f midpoint pos-point epsilon)
             :default midpoint))))))

(defn half-intervall
  "Half intervall method for the function f and values a and b."
  ([f a b]
   (half-intervall f a b default-epsilon))
  ([f a b epsilon]
   (let [a-value (f a)
         b-value (f b)]
     (cond
       (and (neg? a-value) (pos? b-value)) (search-zero f a b epsilon)
       (and (neg? b-value) (pos? a-value)) (search-zero f b a epsilon)
       :default (throw (IllegalArgumentException. (str "Values are not of opposite sign: " a " " b)))))))

(defn fixed-point
  "Calculates a fixed point of the function f."
  [f first-guess]
  (loop [guess first-guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (recur next-guess)))))

(defn integral
  "Calculates the integral of function f between a and b with dx."
  ([f a b]
   (integral f a b default-dx))
  ([f a b dx]
   (* (sum f (+ a (/ dx 2)) (partial + dx) b) dx)))

(defn difference-quotient
  "Calculates the difference quotient of the function f at x with the delta dx."
  ([f x]
   (difference-quotient f x default-dx))
  ([f x dx]
   (/ (- (f (+ x dx)) (f x))
      dx)))

(defn derivation
  "Returns a funtion that is the derivation of the function f."
  ([f]
   (derivation f default-dx))
  ([f dx]
   (fn [x]
     ; (difference-quotient f x dx)
     (/ (- (f (+ x dx)) (f x))
        dx))))

(defn newton-transform
  "Returns a function which is the newton transfomation of the given function f."
  ([f]
   (newton-transform f default-dx))
  ([f dx]
   (fn [x]
     (- x (/ (f x) ((derivation f dx) x))))))

(defn newton-method
  "Newton method for searching a root of the function f starting with guess."
  ([f guess]
   (newton-method f default-dx guess))
  ([f dx guess]
   (fixed-point (newton-transform f dx) guess)))
