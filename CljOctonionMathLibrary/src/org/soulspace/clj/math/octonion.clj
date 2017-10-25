;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.octonion)

; Octonions, hyper complex numbers of the 8th dimension

(set! *warn-on-reflection* true)
(declare octonion)

(defprotocol Octonion
  "Protocol for octonions, hyper complex numbers of the 8th dimension."
  (add [q1 q2] "Returns the addition of the octonions o1 and o2.")
  (conjugate [q] "Returns the conjugate o* of o.")
  )

(defrecord OctonionImpl
  [a b c d e f g h]
  Octonion
  (add [q1 q2]
    (octonion (+ (:a o1) (:a o2)) (+ (:b o1) (:b o2)) (+ (:c o1) (:c o2)) (+ (:d o1) (:d o2))
              (+ (:e o1) (:e o2)) (+ (:f o1) (:f o2)) (+ (:g o1) (:g o2)) (+ (:h o1) (:h o2))))
  (conjugate [o]
    (octonion (:a o) (* -1 (:b o)) (* -1 (:c o)) (* -1 (:d o)) (* -1 (:e o)) (* -1 (:f o)) (* -1 (:g o)) (* -1 (:h o))))
  )

(defn octonion
  "Creates a new octonion from the real numbers a, b, c, d, e, f, g and h."
  [a b c d e f g h]
  (OctonionImpl. a b c d e f g h)
  )
