(ns confuse.binary-class-metrics-test
  (:require
   [confuse.binary-class-metrics :refer :all]
   [clojure.test :refer [is testing deftest]]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.impl.pprint :refer [pm]]
   [clojure.core.matrix.dataset :as cd]
   [confuse.util :refer [approx]]))

;;taken from https://en.wikipedia.org/wiki/Sensitivity_and_specificity#Worked_example
(def fixt
  (vec (concat
        (repeat 20 [1 1])
        (repeat 1820 [0 0])
        (repeat 180 [1 0])
        (repeat 10 [0 1]))))
(def actuals (mapv second fixt))
(def pred (mapv first fixt))


;;define the positive class
(def pclass 1)

(deftest actual-numbers
  (is (= 20 (true-positives actuals pred pclass)))
  (is (= 180 (false-positives actuals pred pclass)))
  (is (= 10 (false-negatives actuals pred pclass)))
  (is (= 1820 (true-negatives actuals pred pclass))))

(deftest true-positive-rate-test
  (is (approx 0.66 (sensitivity actuals pred pclass)))
  (is (approx 0.66 (true-positive-rate actuals pred pclass)))
  (is (approx 0.66 (recall actuals pred pclass))))

(deftest true-negative-rate-test
  (is (approx 0.91 (specificity actuals pred pclass)))
  (is (approx 0.91 (true-negative-rate actuals pred pclass))))

(deftest false-positive-rate-test
  (is (approx 0.09 (false-positive-rate actuals pred pclass))))

(deftest false-negative-rate-test
  (is (approx 0.33 (false-negative-rate actuals pred pclass))))

(deftest precision-test
  (is (approx 0.10 (precision actuals pred pclass))))

(deftest f1-score-test
  (is (approx 0.17 (f1-score actuals pred pclass))))

(deftest accuracy-test
  (is (approx 0.90 (accuracy actuals pred)))
  (is (approx 0.10 (misclassification-rate actuals pred))))

(deftest confusion-matrix-test
  (let [cm (confusion-matrix actuals pred)]
    (is (= 1820 (cm [0 0])))
    (is (= 10 (cm [0 1])))
    (is (= 180 (cm [1 0])))
    (is (= 20 (cm [1 1])))))

(deftest mcc
  (is (approx -0.33333 (mcc [+1 +1 +1 -1] [+1 -1 +1 +1] pclass)))
  (is (= 0.0 (mcc [1 2 3 4] [1 1 1 1] pclass))))
