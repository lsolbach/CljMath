;
;   Copyright (c) Ludger Solbach. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file license.txt at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
(ns org.soulspace.clj.math.test.statistics
  (:use [clojure.test]
        [org.soulspace.clj.math math java-math statistics]))

(deftest avg-test
  (is (== (avg [3 3 3 3 3]) 3))
  (is (== (avg [1 2 3 4 5]) 3))
  (is (== (avg [1 2 3 4]) 2.5))
  (is (== (avg [2 4 8 16]) 7.5))
  )

(deftest median-test
  (is (== (median [3 3 3 3 3]) 3))
  (is (== (median [1 2 3 4 5]) 3))
  (is (== (median [1 2 3 4]) 2.5))
  (is (== (median [2 4 8 16]) 6))
  )

(deftest variance-test
  (is (== (variance [3 3 3 3 3]) 0))
  (is (== (variance [1 2 3 4 5]) 2))
  (is (== (variance [1 2 3 4]) 1.25))
  (is (== (variance [2 4 8 16]) 115/4))
  )

(deftest deviation-test
  (is (== (deviation [3 3 3 3 3]) 0))
  (is (== (deviation [1 2 3 4 5]) (sqrt 2)))
  (is (== (deviation [1 2 3 4]) (sqrt 1.25)))
  (is (== (deviation [2 4 8 16]) (sqrt 115/4)))
  )

(deftest covariance-test
  (is (= (covariance [1 2 3 4] [1 2 3 4]) 5/4))
  )

(deftest linear-regression-test
  (is (= (linear-regression [1 2 3 4] [2 3 4 5]) [1N 1N]))
  (is (= (linear-regression [1 2 3 4] [1 2 3 4]) [1N 0N]))
  (is (= (linear-regression [1 2 3 4] [-1 -2 -3 -4]) [-1N 0N]))
  )

(deftest quantile-test
  (is (= (quantile 1/2 [1 2 3 4 5 6 7 8])))
  )

(run-tests)
