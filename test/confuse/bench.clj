(ns confuse.bench
  (:require
   [confuse.binary-class-metrics :as bcm :refer :all]
   [clojure.test :refer [is testing deftest]]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.dataset :as cd]
            ;crit
   [criterium.core :refer [bench]]
   [clojure.spec.gen :as gen]))

(def bfix (vec (concat
        (repeat 20 [1 1])
        (repeat 1820 [0 0])
        (repeat 180 [1 0])
        (repeat 10 [0 1]))))
(def pred (mapv first bfix))
(def actual (mapv second bfix))
(bcm/redcounts [[1 1] [0 0]] (fn [[a b]] (= a b 0)) (fn [[a b]] (= a b 1)))
(bcm/cfcounts [[1 1] [0 0]])
(def cnt 250000)
(def b1mfix 
  (vec (concat
        (repeat cnt [1 1])
        (repeat cnt [0 0])
        (repeat cnt [1 0])
        (repeat cnt [0 1]))))

(bench (true-positive-rate bfix 1) :samples 10)
;mean-361 micro seconds

;;;;;;;;;;;;;;;;;;;;;;;;;
(bench (true-positive-rate bfix 1))
;mean time-417 micro seconds

(bench (true-positive-rate b1mfix 1))
;mean-195 milli seconds

(bench (bcm/true-positive-rate-red bfix 1))
;mean-253 micro seconds

(bench (bcm/true-positive-rate-red b1mfix 1))
;mean- 113 milli seconds

(bench (bcm/true-positive-rate-cf bfix 1))
;mean 1.9ms
(bench (bcm/true-positive-rate-cf b1mfix 1))
;mean 1.9 seconds

(bench (bcm/true-positive-rate-cm bfix 1))
;mean-8.1 ms
(bench (bcm/true-positive-rate-cm b1mfix 1))
;mean-4.27 secs

;with mset! instead of m/set-indexes!
(bench (bcm/true-positive-rate-cm bfix 1))
;mean-465 microms
(bench (bcm/true-positive-rate-cm b1mfix 1))
;mean 243 milli secs
