;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.statistics
  (:use [org.soulspace.clj.math math]
        [org.soulspace.clj.math.java-math :only [abs pow sqrt cbrt ceil]])
  (:require [org.soulspace.clj.math.matrix :as m]
            [org.soulspace.clj.math.vector :as v]))

; same as avg
;(defn geometric-average
;  "Returns the geometric average"
;  [coll]
;  (pow (* coll) (- (count coll))))

(def mean
  "Returns the mean of the values of the coll."
  avg)

(def geometric-average
  "Returns the geometric average (mean) of the values of the coll."
  avg)

(defn harmonic-average
  "Returns the harmonic average of the values of the coll."
  [coll]
  (reduce + 0 (map #(/ 1 %) coll)))

(defn square-average
  "Returns the square average of the values of the coll."
  [coll]
  (sqrt (/ (reduce + 0 (map sqr coll))
           (count coll))))

(defn cubic-average
  "Returns the cubic average of the values of the coll."
  [coll]
  (cbrt (/ (reduce + 0 (map cube coll))
           (count coll))))

(defn de-mean
  "Returns a collection with the mean substacted from each value of input coll."
  [coll]
  (let [x-mean (mean coll)]
    (map #(- % x-mean) coll)))

(defn quantile
  "Returns the q quantile"
  [q coll]
  (let [x  (* (count coll) q) ]
    (if (integer? x)
      (/ (+ (nth coll (- x 1)) (nth coll x))
         2)
      (nth coll (- (ceil x) 1)))))

(defn median
  "Returns the median / second quartile"
  [coll]
  (quantile (/ 1 2) coll))

(defn quartile1
  "Returns the first quartile"
  [coll]
  (quantile (/ 1 4) coll))

(defn quartile2
  "Returns the second quartile / median"
  [coll]
  (median coll))

(defn quartile3
  "Returns the third quartile"
  [coll]
  (quantile (/ 3 4) coll))

(defn variance
  "Returns the biased sample variance (n)"
  [coll]
  (let [mu (avg coll)
        n (count coll)]
    (/ (reduce + 0 (map #(sqr (- % mu)) coll))
       n)))

(defn unbiased-variance
  "Returns the unbiased sample variance (n-1)"
  [coll]
  (let [mu (avg coll)
        n (count coll)]
    (/ (reduce + 0 (map #(sqr (- % mu)) coll))
       (- n 1))))

(defn covariance
  "Returns the biased sample covariance (n)"
  [coll1 coll2]
  (let [mu1 (avg coll1)
        mu2 (avg coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       n)))

(defn unbiased-covariance
  "Returns the unbiased sample covariance (n-1)"
  [coll1 coll2]
  (let [mu1 (avg coll1)
        mu2 (avg coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       (- n 1))))

(defn deviation
  "Returns the biased sample deviation (n)"
  [coll]
  (sqrt (variance coll)))

(defn unbiased-deviation
  "Returns the unbiased sample deviation (n-1)"
  [coll]
  (sqrt (unbiased-variance coll)))

(defn correlation-coefficient
  "Returns the correlation coefficient"
  [coll1 coll2]
  (/ (covariance coll1 coll2)
     (* (deviation coll1) (deviation coll2))))

(defn linear-regression
  "Returns a vector [a b] of the linear regession coefficients for the equation y = ax + b."
  [coll1 coll2]
  (let [mu1 (avg coll1)
        mu2 (avg coll2)
        a (/ (covariance coll1 coll2) (variance coll1))]
    [a (- mu2 (* a mu1))]))

(defn q-value
  ""
  [q coll]
  (* (count coll) q))

;
; hypothesis tests
;
(defn- estimated-parameters
  "Calculates the probability and standard deviation for a given test outcome of n out of N."
  ([[N n]]
    (estimated-parameters N n))
  ([N n]
    (let [p (/ n N)]
      [p (sqrt (/ (* p (- 1 p)) N))])))

(defn a-b-test-statistic
  "Calculates the statistic for the hypothesis, that p-a and p-b are the same, given the outcomes for test a and b."
  [N-a n-a N-b n-b]
  (let [[p-a sigma-a] (estimated-parameters N-a n-a)
        [p-b sigma-b] (estimated-parameters N-b n-b)]
    (/ (- p-b p-a)
       (sqrt (+ (* sigma-a sigma-a) (* sigma-b sigma-b))))))

;
; rescaling of data matices
;
(defn scale
  "Returns the mean vector and the unbiased standard deviation vector for the colums of the matrix."
  [m]
  (let [cols (m/column-vectors m)]
    [(mapv avg cols) (mapv unbiased-deviation cols)]))

(defn rescale
  "Rescales the matrix m to have a mean of 0 and an unbiased standard deviation of 1."
  [m]
  (let [[rows cols] (m/shape m)
        [means stdevs] (scale m)]

    (defn rescaled [i j]
      (if (> (stdevs j) 0)
        (/ (- (m/element m i j) (means j))
           (stdevs j))
        (m/element m i j)))
    
    (m/build-matrix rows cols rescaled)))

(defn de-mean-matrix
  "Returns a matrix with the column mean substacted from each value of input matrix m."
  [m]
  (let [[rows cols] (m/shape m)
        [means _] (scale m)]
    (m/build-matrix rows cols (fn [i j] (- (m/element i j) (means j))))))

;
; gradient descent (TODO move to a different namespace (e.g. graadient or optimization))
;
(def step-sizes [100 10 1 0.1 0.01 0.001 0.0001 0.00001])

(defn safe-fn
  "Returns a function that is the same as f except that it returns infinity whenever f returns an error."
  [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Exception e
        Double/POSITIVE_INFINITY))))

(defn safe-apply
  "Returns a function that is the same as f except that it returns infinity whenever f returns an error."
  [f & args]
  (try
    (apply f args)
    (catch Exception e
      Double/POSITIVE_INFINITY)))

(defn partial-difference-quotient
  "Calculates the i-th partial difference quotient of f at v."
  [f v i h]
  (let [w (for [[j v-j] (map-indexed v)]
            (if (= i j)
              (+ v-j h)
              v-j))]
    (/ (- (f w) (f v))
       h)))

(defn estimate-gradient
  "Estimates the gradient of f at v."
  ([f v]
    (estimate-gradient f v 0.0000001))
  ([f v h]
    (mapv #(partial-difference-quotient f v % h) (range (count v)))))

(defn step
  "Returns the vector v moved step-size in direction."
  [v direction step-size]
  (for [[v-i direction-i] (map vector v direction)]
    (+ v-i (* step-size direction-i))))

(defn negated-fn
  "Returns a function that calculates the negated value (multiplied with -1) of f."
  [f]
  (fn [& args]
    (* -1 (apply f args))))

(defn negated-all-fn
  "Returns a function that calculates the negated values (multiplied with -1) of f for functions returning a sequence of values."
  [f]
  (fn [& args]
    (map #(* -1 %) (apply f args))))

(defn minimize-batch
  "Calculates ."
  ([target-fn gradient-fn theta-0]
    (minimize-batch target-fn gradient-fn theta-0 default-epsilon))
  ([target-fn gradient-fn theta-0 tolerance]
    (let [target-fn (safe-fn target-fn)]
      (loop [theta theta-0
             value (target-fn theta)]
        (let [gradient (gradient-fn theta)
              next-thetas (map #(step theta gradient (* -1 %)) step-sizes)
              next-theta (apply min-key target-fn next-thetas)
              next-value (target-fn next-theta)]
          (if (< (abs (- value next-value)) tolerance)
            theta
            (recur next-theta next-value)))))))
  
(defn maximize-batch
  "Calculates the theta that minimizes the target function by gradient descent."
  ([target-fn gradient-fn theta-0]
    (maximize-batch target-fn gradient-fn theta-0 default-epsilon))
  ([target-fn gradient-fn theta-0 tolerance]
    (minimize-batch (negated-fn target-fn) (negated-all-fn gradient-fn) theta-0 tolerance)))

(defn minimize-stochastic
  "Calculates ."
  ([target-fn gradient-fn x y theta-0]
    (minimize-stochastic target-fn gradient-fn x y theta-0 0.01))
  ([target-fn gradient-fn x y theta-0 alpha-0]
    ; TODO implement
    ))

(defn maximize-stochastic
  "Calculates ."
  ([target-fn gradient-fn x y theta-0]
    (maximize-stochastic target-fn gradient-fn x y theta-0 0.01))
  ([target-fn gradient-fn x y theta-0 alpha-0]
    (minimize-stochastic (negated-fn target-fn) (negated-all-fn gradient-fn) x y theta-0 alpha-0)))

(def direction
  "Returns the direction of the vector w (which is w normalized to length 1)"
  v/normalize)

(defn directional-variance-i
  "Calculates the variance of the row x-i in the direction of w."
  [x-i w]
  (sqr (v/dot-product x-i (direction w))))

(defn directional-variance
  "Calculates the variance of the matrix m in the direction of w."
  [m w]
  (reduce + (map #(directional-variance-i % w) m)))

(defn directional-variance-gradient-i
  "Calculates the contribution of row x-i to the gradient of the variance in direction w."
  [x-i w]
  (let [projection-length (v/dot-product x-i (direction w))]
    (mapv #(* 2 projection-length %) x-i)))

(defn directional-variance-gradient
  "Calculates the contribution of the matrix m to the gradient of the variance in direction w."
  [m w]
  (mapv #(v/vector-sum (directional-variance-gradient-i % w)) m))


(defn sum-of-squares-gradient
  "Calculates the gradient of the sum of squares of v."
  [v]
  (mapv #(* 2 %) v))

