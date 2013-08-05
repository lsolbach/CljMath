;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.vector)

(defn addition
  [a b]
  (if (= (count a) (count b))
    (vec (map + a b))
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))

(defn substraction
  [a b]
  (if (= (count a) (count b))
    (vec (map - a b))
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))

(defn scalar-product
  [r a]
  (vec (map #(* r %) a)))

(defn dot-product
  [a b]
  (if (= (count a) (count b))
    (reduce + (map * a b))
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))

(defn cross-product
  [a b]
  (if (= (count a) (count b))
    nil ; TODO calculate cross product
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))
