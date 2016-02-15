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

(defn element
  "Returns the element of the matrix m at row i and column j."
  [m i j]
  (nth (nth m i) j))

(defn row-vector
  "Returns the row vector of the matrix m at row i."
  [m i]
  (nth m i))

(defn column-vector
  "Returns the column vector of the matrix m at column j."
  [m j]
  (for [i ]))

(defn scalar-add
  "Adds the scalar s to the matrix m."
  [m r]
  (for []))

(defn scalar-multiply
  "Multiplies the scalar s to the matrix m."
  [m r]
  )

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

(comment
(defprotocol Matrix
  "Protocol for matrices."
  (element [m i j] "Returns the element of the matrix m at row i and column j.")
  (row-vector [m i] "Returns the row vector of the matrix m at row i.")
  (column-vector [m j] "Returns the column vector of the matrix m at column j.")
  (upper-triangular [m] "Returns the upper triangular of the matrix m.")
  (lower-triangular [m] "Returns the upper triangular of the matrix m.")
  (scalar-sum [m s] "Calculates the scalar sum of the matrix m with the scalar s.")
  (scalar-product [m s] "Calculates the scalar product of the matrix m with the scalar s.")
  (matrix-sum [m m2] "Calculates the sum of the matrix m with the matrix m2.")
  (matrix-product [m m2] "")
  (transpose [m] "Returns the transposed matrix of the matrix m.")
  (solve [m v] ""))

(defrecord VecMatrixImpl
  [elements]
  Matrix
  (element [m i j]
    (nth (nth m i) j))
  (row-vector [m i]
    (nth m i))
  (column-vector [m j]
    (for [i ]))
  (scalar-sum [m r]
    )
  (scalar-product [m r])
  (matrix-sum [m m2])
  (matrix-product [m m2])
  (upper-triangular [m])
  (lower-triangular [m])
  (transpose [m])
  (solve [m v]))
)

