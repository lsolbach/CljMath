;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.matrix)

;;
;; Matrix functions
;;

(defn rows
  "Returns the number of rows of the matrix m."
  [m]
  (count m))

(defn columns
  "Returns the number of columns of the matrix m."
  [m]
  (count (m 0)))

(defn shape
  "Returns the shape of the matrix as a vector of the number of rows and columns."
  [m]
  [(count m) (count (m 0))])

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
  (mapv #(nth % j) m))

(defn transpose
  "Returns the transposed matrix of the matrix m."
  [m]
  (mapv #(column-vector m %) (range (columns m))))

(def column-vectors
  "Returns the column vectors of the matrix m."
  transpose)

(defn diagonal
  "Returns 1 if i = j, otherwise 0."
  [i j]
  (if (= i j)
    1
    0))

(defn build-matrix
  "Builds a new matrix of shape rows x cols with the value m[i, j] is f(i, j)."
  [rows cols f]
  (vec (for [i (range rows)]
         (vec (for [j (range cols)]
                (f i j))))))

(defn identity-matrix
  "Builds an identity matrix of shape n x n."
  [n]
  (build-matrix n n diagonal))

(defn scalar-add
  "Adds the scalar s to the matrix m."
  [s m]
  (mapv (partial mapv (partial + s)) m))

(defn scalar-multiply
  "Multiplies the scalar s to the matrix m."
  [s m]
  (mapv (partial mapv (partial * s)) m))

(defn matrix-add
  "Adds the matrices m and n."
  [m n]
  (if (= (shape m) (shape n))
    (mapv (partial mapv +) m n)
    (throw (IllegalArgumentException. "The matrices are not of the same shape."))))

(defn matrix-substract
  "Substracts the matrices m and n."
  [m n]
  (if (= (shape m) (shape n))
    (mapv (partial mapv -) m n)
    (throw (IllegalArgumentException. "The matrices are not of the same shape."))))

(defn matrix-sum
  "Adds the matrices."
  [& ms]
  (reduce matrix-add ms))

(defn matrix-product
  [m n])


; TODO check if this should be compliant to an L-R Zerlegung
(defn upper-triangular
  "Returns the upper triangular matrix of the matrix m."
  [m]
  (vec (for [i (range (rows m))]
         (vec (for [j (range (columns m))]
                (if (<= i j)
                  (element m i j)
                  0))))))

; TODO check if this should be compliant to an L-R Zerlegung
(defn lower-triangular
  "Returns the lower triangular matrix of the matrix m."
  [m]
  (vec (for [i (range (rows m))]
         (vec (for [j (range (columns m))]
                (if (>= i j)
                  (element m i j)
                  0))))))

; TODO
(defn solve
  [m v])

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
     (for [i]))
   (scalar-sum [m r])

   (scalar-product [m r])
   (matrix-sum [m m2])
   (matrix-product [m m2])
   (upper-triangular [m])
   (lower-triangular [m])
   (transpose [m])
   (solve [m v])))