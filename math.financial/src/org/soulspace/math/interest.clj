;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.math.interest
  (:require [org.soulspace.math.core :as m]))

;;; german-english translations
;; Rente, Annuität = annuity
;; Kredit = credit, loan
;; Tilgung = redemption
;; Barwert = cash value

;;; var names
;; k0 := credit sum
;; kn := cash value
;; km := remaining credit sum
;; a := annuity / redemption
;; i := interest
;; q := interest factor (1 + decimal interest)
;; n := term
;; r := rate

(defn round-financial
  "Returns the value rounded financially according to the given precision"
  [precision x]
  (if (and (float? x) (> precision 0))
    (* (m/sign x) (/ (m/floor (+ (* (m/abs x) (m/pow 10 precision)) 0.5)) (m/pow 10 precision)))
    x))

(defn percent
  "Converts a percent value to a decimal value."
  [x]
  (/ x 100))

(defn percentage
  "Converts a decimal value to a percent value."
  [x]
  (* x 100))

(defn accumulate-by-interest
  ""
  [k0 i]
  (* k0 (1 + i)))

(defn discount-by-interest
  "Calculates the discounted value of 'kn'."
  [kn i]
  (* kn (1 - i)))

(defn cash-value
  "Calculates the cash value of a value 'kn' with interest 'q' before 'n' periods."
  [kn q n]
  (* kn (/ 1 (m/pow q n))))

(defn accumulated-value
  "Calculates the accumulated value of a base value 'k0' with interest 'q' after 'n' periods."
  [k0 q n]
  (* k0 (m/pow q n)))

;(defn annuity-accumulated-value)
;(defn annuity-cash-value)

(defn accumulated-rates
  ""
  [r q m]
  (* r
    (/ (- (m/pow q m) 1) (- q 1))))

(defn discounted-rates
  ""
  [r q m]
  (* r
    (/ (- (m/pow q m) 1) (- q 1))
    (/ 1 (m/pow q m))))

(defn annuity-factor
  "Calculates the annuity factor for interest 'q' and 'n' periods."
  [q n]
  (/
    (* (m/pow q n) (- q 1))
    (- (m/pow q n) 1)))

(defn annuity-rent-Rn
  ""
  [r q n]
  (accumulated-rates r q n))

(defn annuity-rate
  "Calculates the annuity rates for a base value 'k0' with interest 'q' and 'n' periods."
  [k0 q n]
  (* k0 (annuity-factor q n)))

(defn annuity-credit-Km
  ""
  [k0 a q m]
  (-
    (accumulated-value k0 q m)
    (accumulated-rates a q m)))

(defn annuity-term
  ""
  [k0 a q]
  (/
    (m/log (/ a (- a (* k0 (- q 1)))))
    (m/log q)))

(defn annuity-rate-by-interest
  ""
  [k0 i it]
  (* k0 (+ i it)))

(defn annuity-term-by-interest
  ""
  [i it]
  (/
    (m/log (+ 1 (/ i it)))
    (m/log (+ 1 i))))

(defn annuity-redemption-plan
  ""
  [k0 a q]
  (let [n (annuity-term k0 a q)]
    (loop [i 1 plan []]
      (if (> i n)
        plan
        (recur (+ i 1) (conj plan (annuity-credit-Km k0 a q i)))))))
