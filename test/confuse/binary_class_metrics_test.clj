(ns confuse.binary-class-metrics-test
  (:require
   [confuse.binary-class-metrics :refer :all]
   [clojure.test :refer [is testing]]
   [clojure.spec :as s]
   [clojure.spec.test :as stest]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.impl.pprint :refer [pm]]
   [clojure.core.matrix.dataset :as cd]
   [clojure.spec.gen :as gen]))

;;taken from https://en.wikipedia.org/wiki/Sensitivity_and_specificity#Worked_example
(def fixt
  (vec (concat
        (repeat 20 [1 1])
        (repeat 1820 [0 0])
        (repeat 180 [1 0])
        (repeat 10 [0 1]))))

;;define the positive class
(def pclass 1)

(testing "actual numbers"
  (is (= 20 (true-positives fixt pclass)))
  (is (= 180 (false-positives fixt pclass)))
  (is (= 10 (false-negatives fixt pclass)))
  (is (= 1820 (true-negatives fixt pclass))))

(defn approx
  "returns true is expected and actual are within 0.05 of each other"
  [exp actual]
  (> 0.05  (Math/abs (- exp actual))))

(testing "true positive rate"
  (is (approx 0.66 (sensitivity fixt pclass)))
  (is (approx 0.66 (true-positive-rate fixt pclass)))
  (is (approx 0.66 (recall fixt pclass))))

(testing "true negative rate"
  (is (approx 0.91 (specificity fixt pclass)))
  (is (approx 0.91 (true-negative-rate fixt pclass))))

(testing "false positive rate"
  (is (approx 0.09 (false-positive-rate fixt pclass))))

(testing "false negative rate"
  (is (approx 0.33 (false-negative-rate fixt pclass))))

(testing "precision"
  (is (approx 0.10 (precision fixt pclass))))

(testing "f1 score"
  (is (approx 0.17 (f1-score fixt pclass))))

(testing "accuracy"
  (is (approx 0.90 (accuracy fixt)))
  (is (approx 0.10 (misclassification-rate fixt))))

(testing "confusion matrix"
  (let [cm (confusion-matrix fixt)]
    (is (= 1820 (cm [0 0])))
    (is (= 10 (cm [0 1])))
    (is (= 180 (cm [1 0])))
    (is (= 20 (cm [1 1])))))
