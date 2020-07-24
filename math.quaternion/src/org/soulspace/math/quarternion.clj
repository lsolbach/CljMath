;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.quarternion
  (:use [org.soulspace.math math java-math]))

;;
;; Quarternions, hyper complex numbers of 4th dimension
;;

(set! *warn-on-reflection* true)
(declare quarternion)


(defprotocol Quaternion
  "Protocol for quarternions, hyper complex numbers of 4th dimension."
  (add [q1 q2] "Returns the addition of the quaternions q1 and q2.")
  (mult [q1 q2] "Returns the multiplication of the quarternions q1 and q2.")
  ;(hamilton-product [q1 q2] "Returns the hamilton product of the quarternions q1 and q2.")
  (conjugate [q] "Returns the conjugate q* of q.")
  (norm [q] "Returns the norm of q."))


(defrecord QuaternionImpl
  [a b c d]
  Quaternion
  (add [q1 q2]
    (quarternion (+ (:a q1) (:a q2)) (+ (:b q1) (:b q2)) (+ (:c q1) (:c q2)) (+ (:d q1) (:d q2))))
  (mult [q1 q2]
    (quarternion (- (* (:a q1) (:a q2)) (* (:b q1) (:b q2)) (* (:c q1) (:c q2)) (* (:d q1) (:d q2)))
                 (+ (* (:a q1) (:b q2)) (* (:b q1) (:a q2)) (* (:c q1) (:d q2)) (* -1 (:d q1) (:c q2)))
                 (+ (* (:a q1) (:c q2)) (* -1 (:b q1) (:d q2)) (* (:c q1) (:a q2)) (* (:d q1) (:b q2)))
                 (+ (* (:a q1) (:d q2)) (* (:b q1) (:c q2)) (* -1 (:c q1) (:b q2)) (* (:d q1) (:a q2)))))
  ;(hamilton-product [q1 q2] )
  (conjugate [q]
    (quarternion (:a q) (* -1 (:b q)) (* -1 (:c q)) (* -1 (:d q))))
  (norm [q]
    (sqrt (+ (sqr (:a q)) (sqr (:b q)) (sqr (:c q)) (sqr (:d q))))))


(defn quarternion
  "Creates a new quarternion from the real numbers a, b, c and d."
  [a b c d]
  (QuaternionImpl. a b c d))
