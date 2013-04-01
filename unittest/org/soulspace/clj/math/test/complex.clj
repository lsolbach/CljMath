(ns org.soulspace.clj.math.test.complex
  (:use [clojure.test]
        [org.soulspace.clj.math.complex]))

(deftest complex-test
  )

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
  (is (= (reduce add (one) [(one) (one)]) (complex 3 0)))
  )

(deftest mult-test
  (is (= (mult (one) (one)) (one)))
  (is (= (mult (zero) (one)) (zero)))
  (is (= (mult (one) (zero)) (zero)))
  (is (= (mult (one) (i)) (i)))
  (is (= (mult (i) (one)) (i)))
  )

(deftest sub-test
  (is (= (sub (zero) (zero)) (zero)))
  )

(deftest div-test
  (is (= (div (one) (one)) (one)))
  )

(deftest conjugate-test
  (is (= (conjugate (complex 1 3)) (complex 1 -3)))
  (is (= (conjugate (complex 1 -3)) (complex 1 3)))
  )

(deftest to-polar-test
  )

(deftest from-polar-test
  )

(run-tests)