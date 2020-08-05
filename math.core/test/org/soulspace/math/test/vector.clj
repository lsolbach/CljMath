(ns org.soulspace.math.test.vector
  (:require [clojure.test :refer :all]
            [org.soulspace.math.core :as m]
            [org.soulspace.math.vector :refer :all]))

(deftest add-test
  (are [x y] (= x y)
       [0] (add [0])
       [0] (add [0] [0])
       [0] (add [-1] [1])
       [2] (add [1] [1])
       [3] (add [1] [1] [1])
       [0 0] (add [0 0])
       [0 0] (add [0 0] [0 0])
       [0 0] (add [-1 0] [1 0])
       [2 0] (add [1 0] [1 0])
       [0 2] (add [0 1] [0 1])
       [3 0] (add [1 0] [1 0] [1 0])
       [0 3] (add [0 1] [0 1] [0 1])
       [3 0 0] (add [1 0 0] [1 0 0] [1 0 0])
       [0 3 0] (add [0 1 0] [0 1 0] [0 1 0])
       [3 3 3 3 3] (add [1 1 1 1 1] [1 1 1 1 1] [1 1 1 1 1])))

(deftest add-exception-test
  (is (thrown? Exception (add [1] [0 1])))
  (is (thrown? Exception (add [1 0 0 0] [0 1 0]))))

(deftest cross-product-test
  (are [x y] (= x y)
       [0 0 0] (cross-product [0 0 0] [0 0 0])
       [0 0 0] (cross-product [1 0 0] [1 0 0])
       [0 0 0] (cross-product [2 4 0] [2 4 0])
       [0 0 1] (cross-product [1 0 0] [0 1 0])
       [-1 0 0] (cross-product [0 0 1] [0 1 0])))

(deftest cross-product-exception-test
  (is (thrown? Exception (cross-product [1 0] [0 1])))
  (is (thrown? Exception (cross-product [1 0 0 0] [0 1 0 0]))))

(deftest angle-test
  (are [x y] (= x y)
       0.0 (angle [1 0] [1 0])
       m/half-pi (angle [1 0] [0 1])
       m/half-pi (angle [1 0 0 ] [0 1 0 ])
       m/half-pi (angle [1 0 0 0] [0 1 0 0])
       m/pi (angle [1 0] [-1 0])))
