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
  (:use 
    [org.soulspace.clj.math.math]
    [org.soulspace.clj.math.java-math :only [pow sqrt cbrt ceil]]
    ))

; same as avg
;(defn geometric-average [coll]
;  "Returns the geometric average"
;  (pow (* coll) (- (count coll))))

(def geometric-average avg)

(defn harmonic-average [coll]
  "Returns the harmonic average"
  (reduce + 0 (map #(/ 1 %) coll)))

(defn square-average [coll]
  "Returns the square average"
  (sqrt (/ (reduce + 0 (map sqr coll))
           (count coll))))

(defn cubic-average [coll]
  "Returns the cubic average"
  (cbrt (/ (reduce + 0 (map cube coll))
           (count coll))))

(defn quantile [q coll]
  "Returns the q quantile"
  (let [x  (* (count coll) q) ]
    (if (integer? x)
      (/ (+ (nth coll (- x 1)) (nth coll x))
         2)
      (nth coll (- (ceil x) 1)))))

(defn median [coll]
  "Returns the median / second quartile"
  (quantile (/ 1 2) coll))

(defn quartile1 [coll]
  "Returns the first quartile"
  (quantile (/ 1 4) coll))

(defn quartile2 [coll]
  "Returns the second quartile / median"
  (median coll))

(defn quartile3 [coll]
  "Returns the third quartile"
  (quantile (/ 3 4) coll))

(defn variance [coll]
  "Returns the biased sample variance (n)"
  (let [mu (avg coll)
        n (count coll)]
    (/ (reduce + 0 (map #(sqr (- % mu)) coll))
       n)))

(defn unbiased-variance [coll]
  "Returns the unbiased sample variance (n-1)"
  (let [mu (avg coll)
        n (count coll)]
    (/ (reduce + 0 (map #(sqr (- % mu)) coll))
       (- n 1))))

(defn covariance [coll1 coll2]
  "Returns the biased sample covariance (n)"
  (let [mu1 (avg coll1)
        mu2 (avg coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       n)))

(defn unbiased-covariance [coll1 coll2]
  "Returns the unbiased sample covariance (n-1)"
  (let [mu1 (avg coll1)
        mu2 (avg coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       (- n 1))))

(defn deviation [coll]
  "Returns the biased sample deviation (n)"
  (sqrt (variance coll)))

(defn unbiased-deviation [coll]
  "Returns the unbiased sample deviation (n-1)"
  (sqrt (unbiased-variance coll)))

(defn correlation-coefficient [coll1 coll2]
  "Returns the correlation coefficient"
  (/ (covariance coll1 coll2)
     (* (deviation coll1) (deviation coll2))))

(defn linear-regression [coll1 coll2]
  "returns [a b] y = ax + b"
  (let [mu1 (avg coll1)
        mu2 (avg coll2)
        a (/ (covariance coll1 coll2) (variance coll1))]
    [a (- mu2 (* a mu1))]))

(defn q-value [q coll]
  (* (count coll) q))
