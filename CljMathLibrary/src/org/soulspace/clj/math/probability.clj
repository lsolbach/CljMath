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
  "Returns the probability density function for the uniform distribution."
  ([x]
    (normal-pdf x 0 1))
  ([x mu sigma]
    (/ (exp (* -1 (/ (sqr (- x mu))
                     (* 2 sigma sigma))))
       (sqrt (* 2 pi)))))

(defn normal-cdf
  "Returns the cumulative distribution function for the uniform distribution."
  ([x]
    (normal-cdf x 0 1))
  ([x mu sigma]
    (/ (+ 1 (erf (/ (- x mu)
                    (* (sqrt 2) sigma))))
       2)))