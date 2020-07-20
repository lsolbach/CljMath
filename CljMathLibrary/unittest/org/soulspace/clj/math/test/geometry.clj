(ns org.soulspace.clj.math.test.geometry
  (:use [clojure.test]
        [org.soulspace.clj.math math java-math geometry]))

(deftest circle-circumference-test
  (is (= (circle-circumference 0) 0.0))
  (is (= (circle-circumference 1) 6.283185307179586))
  (is (= (circle-circumference 2) 12.566370614359172)))

(deftest circle-area-test
  (is (= (circle-area 0) 0.0))
  (is (= (circle-area 1) 3.141592653589793))
  (is (= (circle-area 2) 12.566370614359172)))

