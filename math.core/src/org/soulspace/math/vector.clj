;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.vector
  (:require [org.soulspace.math.core :as m]))

;;
;; Vector functions
;;

(def dimension "Returns the dimension of the vector v."
  count)

(defn scalar-add
  "Adds a scalar s to the vector v."
  [s v]
  (mapv #(+ % s) v))

(defn scalar-product
  "Multiplies a scalar s to the vector v."
  [s v]
  (mapv #(* % s) v))

(defn add
  "Adds the vectors v and w."
  ([v] v)
  ([v w]
    (if (= (dimension v) (dimension w))
      (mapv + v w)
      (throw (IllegalArgumentException. "The vectors are not of the same dimension."))))
  ([v w & vs]
    (reduce add (conj (conj vs w) v ))))

(defn substract
  "Substracts the vectors v and w."
  [v w]
  (if (= (dimension v) (dimension w))
    (mapv - v w)
    (throw (IllegalArgumentException. "The Vectors are not of the same dimension."))))

(defn sum
  "Adds the given vectors."
  [& vs]
  (reduce add vs))

(defn dot-product
  "Calculates the dot product of the vectors v and w."
  [v w]
  (if (= (dimension v) (dimension w))
    (reduce + (map * v w))
    (throw (IllegalArgumentException. "The Vectors are not of the same dimension."))))

;(def multiply cross-product)

(defn sum-of-squares
  "Returns the sum of the squares of the elements of v."
  [v]
  (dot-product v v))

(defn magnitude
  "Returns the magnitude of the vector v."
  [v]
  (m/sqrt (sum-of-squares v)))

(defn distance
  "Returns the distance of the vectors v and w."
  [v w]
  (magnitude (substract v w)))

(defn normalize
  "Returns a vector with a length of 1 in the direction of v."
  [v]
  (scalar-product (/ 1 (magnitude v)) v))

(comment
  (defn cross-product
    [a b]
    (if (= (dimension a) (dimension b))
      nil ; TODO calculate cross product
      (throw (IllegalArgumentException. "The Vectors are not of the same dimension."))))
)
