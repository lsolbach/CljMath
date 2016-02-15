;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.vector
  (:use [org.soulspace.clj.math java-math]))

(def vector-dimension
  "Returns the dimension of the vector v."
  count)

(defn scalar-add
  "Adds a scalar s to the vector v."
  [s v]
  (mapv #(+ % s) v))

(defn scalar-multiply
  "Multiplies a scalar s to the vector v."
  [s v]
  (mapv #(* % s) v))

(defn vector-add
  "Adds the vectors."
  [v w]
  (if (= (count v) (count w))
    (mapv + v w)
    (throw (IllegalArgumentException. "The vectors are not of the same dimension."))))

(defn vector-substract
  "Substracts the vectors v and w."
  [v w]
  (if (= (count v) (count w))
    (mapv - v w)
    (throw (IllegalArgumentException. "The Vectors are not of the same dimension."))))

(defn vector-sum
  "Adds the vectors."
  [& vs]
  (reduce vector-add vs))

(defn dot-product
  "Returns the dot product of the vectors v and w."
  [v w]
  (if (= (count v) (count w))
    (reduce + (map * v w))
    (throw (IllegalArgumentException. "The Vectors are not of the same dimension."))))

(defn sum-of-squares
  "Returns the sum of the squares of the elements of v."
  [v]
  (dot-product v v))

(defn magnitude
  "Returns the magnitude of the vector v."
  [v]
  (sqrt (sum-of-squares v)))

(defn distance
  "Returns the distance of the vectors v and w."
  [v w]
  (magnitude (vector-substract v w)))

(defn normalize
  "Returns a vector with a length of 1 in the direction of v."
  [v]
  (scalar-multiply (/ 1 (magnitude v)) v))

(defn cross-product
  [a b]
  (if (= (count a) (count b))
    nil ; TODO calculate cross product
    (throw (IllegalArgumentException. "The Vectors are not of the same dimension."))))

(comment
(defprotocol Vector
  "Protocol for vectors."
  (scalar-add [v s])
  (scalar-product [v s])
  (vector-add [v v2])
  (vector-product [v v2])
  )

(defrecord VecVectorImpl
  [elements]
  Vector
  (scalar-add [v s])
  (scalar-product [v s])
  (vector-add [v v2])
  (vector-product [v v2])
  )
)
