;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.test.complex
  (:use [clojure.test]
        [org.soulspace.clj.math.complex]))
  ;(:import [org.soulspace.clj.math.complex DoublePolarComplexImpl DoubleComplexImpl])


(deftest complex-test)


(deftest zero-test
  (is (= (zero) (complex 0 0))))

(deftest one-test
  (is (= (one) (complex 1 0))))

(deftest i-test
  (is (= (i) (complex 0 1))))

(deftest add-test
  (is (= (add (zero) (zero)) (zero)))
  (is (= (add (one) (zero)) (one)))
  (is (= (add (zero) (one)) (one)))
  (is (= (add (i) (zero)) (i)))
  (is (= (add (zero) (i)) (i)))
  (is (= (add (one) (one)) (complex 2 0)))
  (is (= (add (i) (i)) (complex 0 2)))
  (is (= (add (one) (i)) (complex 1 1)))
  (is (= (add (i) (one)) (complex 1 1)))
  (is (= (reduce add (zero) [(zero) (zero)]) (zero)))
  (is (= (reduce add (one) [(one) (one)]) (complex 3 0))))


(deftest mult-test
  (is (= (mult (one) (one)) (one)))
  (is (= (mult (zero) (one)) (zero)))
  (is (= (mult (one) (zero)) (zero)))
  (is (= (mult (one) (i)) (i)))
  (is (= (mult (i) (one)) (i))))


(deftest sub-test
  (is (= (sub (zero) (zero)) (zero))))


(deftest div-test
  (is (= (div (one) (one)) (one))))


(deftest conjugate-test
  (is (= (conjugate (complex 1 3)) (complex 1 -3)))
  (is (= (conjugate (complex 1 -3)) (complex 1 3))))


(deftest to-polar-test)


(deftest from-polar-test)


(run-tests)
