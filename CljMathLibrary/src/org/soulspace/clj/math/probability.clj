(ns org.soulspace.clj.math.probability
  (:use [org.soulspace.clj.math math java-math]))


(defn uniform-pdf
  "Returns the probability density function for the uniform distribution."
  [x]
    (if (and (>= x 0) (< x 1))
      1
      0)

(defn uniform-cdf
  "Returns the cumulative distribution function for the uniform distribution."
  [x]
  (cond
    (< x 0) 0
    (< x 1) 1
    :else 1)))

(defn normal-pdf
  "Returns the probability density function for the normal distribution."
  ([x]
    (normal-pdf x 0 1))
  ([x mu sigma]
    (/ (exp (* -1 (/ (sqr (- x mu))
                     (* 2 sigma sigma))))
       (sqrt (* 2 pi)))))

(defn normal-cdf
  "Returns the cumulative distribution function for the normal distribution."
  ([x]
    (normal-cdf x 0 1))
  ([x mu sigma]
    (/ (+ 1 (erf (/ (- x mu)
                    (* (sqrt 2) sigma))))
       2)))

(defn inverse-normal-cdf
  "Returns the inverse of the cumulative distribution function for the normal distribution. Approximates the value with binary search."
  ([p]
    (inverse-normal-cdf p 0.00001))
  ([p e]
    (search-value normal-cdf p -10.0 10.0 e))
  ([p mu sigma e]
    (+ mu (* sigma (inverse-normal-cdf p 0.00001)))))

(defn bernoulli
  "Calculates the bernulli(p) random variable. Returns 1 with a probability of p and 0 with a probability of (1 - p)."
  [p]
  (if (< (rand) p)
    1
    0))

(defn binomial
  "Calculates the binomial(n, p) random variable."
  [n p]
  (reduce + 0 (take n (repeatedly #(bernoulli p)))))
