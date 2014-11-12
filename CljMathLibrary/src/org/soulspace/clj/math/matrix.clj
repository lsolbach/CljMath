;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.matrix)

(defprotocol Matrix
  "Protocol for matrices."
  (get-element [m i j])
  (row-vector [m i])
  (column-vector [m j])
  (scalar-sum [m r])
  (scalar-product [m r])
  (matrix-sum [m m2])
  (matrix-product [m m2])
  (upper-triangular [m])
  (lower-triangular [m])
  (transpose [m])
  (solve [m v]))

(defn get-element
  [m i j]
  (nth (nth m i) j))

(defn matrix-sum
  [m m2]
  )

(defn matrix-product
  [m m2]
  )

(defn transpose
  [m]
  )

(defn solve
  [m v]
  )

(defn upper-triangular
  [m]
  )

(defn lower-triangular
  [m]
  )
