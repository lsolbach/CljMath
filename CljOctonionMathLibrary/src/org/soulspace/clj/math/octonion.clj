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
  )

(defrecord OctonionImpl
  [a b c d e f g h]
  Octonion
  )

(defn octonion
  "Creates a new quarternion from the real numbers a, b, c, d, e, f, g and h."
  [a b c d e f g h]
  (OctonionImpl. a b c d e f g h)
  )
