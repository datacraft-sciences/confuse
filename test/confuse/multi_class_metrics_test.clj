(ns confuse.multi-class-metrics-test
  (:require
   [confuse.multi-class-metrics :refer :all]
   [confuse.binary-class-metrics :as bcm]
   [confuse.util :refer [approx]]
   [clojure.test :refer [is testing deftest]]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.stats :as ms]
   [clojure.core.matrix.dataset :as cd]
   ))

(def fixt
  (vec (concat
        (repeat 5 [:cat :cat])
        (repeat 3 [:dog :cat])
        (repeat 2 [:cat :dog])
        (repeat 3 [:dog :dog])
        (repeat 1 [:rabbit :dog])
        (repeat 2 [:dog :rabbit])
        (repeat 11 [:rabbit :rabbit]))))

(def fixt2
  (vec (concat
        (repeat 5 [:cat :cat])
        (repeat 3 [:dog :cat])
        (repeat 2 [:cat :dog])
        (repeat 3 [:dog :dog])
        (repeat 1 [:rabbit :dog])
        (repeat 1 [:rabbit :cat])
        (repeat 2 [:dog :rabbit])
        (repeat 11 [:rabbit :rabbit]))))

(defn pred
  [k]
  (mapv #(% 0) k))

(defn ac
  [k]
  (mapv #(% 1) k))

(deftest TP
  (is (= 5 (bcm/true-positives (ac fixt) (pred fixt) :cat)))
  (is (= 17 (bcm/true-negatives (ac fixt) (pred fixt) :cat)))
  (is (= 3 (bcm/false-negatives (ac fixt) (pred fixt) :cat)))
  (is (= 2 (bcm/false-positives (ac fixt) (pred fixt) :cat))))

(deftest f-measures 
  (is (approx 0.70 (micro-avg-fmeasure (ac fixt) (pred fixt) #{:cat :dog :rabbit})))
  (is (approx 0.65 (macro-avg-fmeasure (ac fixt) (pred fixt) #{:cat :dog :rabbit}))))

(deftest precision
  (is (approx 0.70 (micro-avg-precision (ac fixt) (pred fixt) #{:cat :dog :rabbit})))
  (is (approx 0.66 (macro-avg-precision (ac fixt) (pred fixt) #{:cat :dog :rabbit}))))

(deftest recall
  (is (approx 0.70 (micro-avg-recall (ac fixt) (pred fixt) #{:cat :dog :rabbit})))
  (is (approx 0.65 (macro-avg-recall (ac fixt) (pred fixt) #{:cat :dog :rabbit}))))


(deftest multi-mcc
  ;;  verified against sklearn results

  (is (= 0.2711630722733202
         (multiclass-mcc
          [0 1 2 3 4 5 3 4 2 3]
          [0 1 2 3 2 2 2 3 4 2])))

  (is (= 0.21335229882506598
         (multiclass-mcc
          [0 1 2 3 4 2 3 4 2 3]
          [0 1 2 3 2 4 2 3 4 2])))

  (is (= -0.3651483716701107
         (multiclass-mcc [0 1 2 3]
                         [2 2 3 1])))

  (is (= -0.42600643361512924
         (multiclass-mcc [0 1 2 3 3 2 1]
                         [2 2 3 1 2 3 3])))

  (is (approx 0.478
              (multiclass-mcc
               [1 1 1 1 1 1 1 1 0 0 0 0]
               [0 0 1 1 1 1 1 1 0 0 0 1]))))
