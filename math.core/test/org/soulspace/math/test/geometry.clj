(ns org.soulspace.math.test.geometry
  (:require [clojure.test :refer :all]
            [org.soulspace.math.core :as m]
            [org.soulspace.math.geometry :refer :all]))

(deftest circle-circumference-test
  (is (= (circle-circumference 0) 0.0))
  (is (= (circle-circumference 1) m/double-pi))
  (is (= (circle-circumference 2) (* 4 m/pi))))

(deftest circle-area-test
  (is (= (circle-area 0) 0.0))
  (is (= (circle-area 1) m/pi))
  (is (= (circle-area 2) (* 4 m/pi))))

