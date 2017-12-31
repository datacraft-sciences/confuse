(ns confuse.bench
  (:require
  ; [confuse.binary-class-metrics :as bcm :refer :all]
   [clojure.test :refer [is testing deftest]]
   [clojure.core.matrix :as m]
   [clojure.core.matrix.dataset :as cd]
            ;crit
   ;[criterium.core :refer [bench]]
   [citius.core :as c]
   ;[clojure.spec.gen :as gen]
   ))


;;This ns has utility code for benchmarking different methods for metric computation

;;fixtures

;;~2000 instances
(def bfix (vec (concat
                (repeat 20 [1 1])
                (repeat 1820 [0 0])
                (repeat 180 [1 0])
                (repeat 10 [0 1]))))
(def pred (mapv first bfix))
(def actual (mapv second bfix))

(def cnt 250000)
;~1M instances
(def b1mfix
  (vec (concat
        (repeat cnt [1 1])
        (repeat cnt [0 0])
        (repeat cnt [1 0])
        (repeat cnt [0 1]))))

(defn counts
  "filter & count"
  [pred-ac-seq filtfn]
  (-> (filter filtfn pred-ac-seq) count))

(defn redcounts
  "multiple if, 2 filters"
  [pred-ac-seq filt1 filt2]
  (reduce (fn [[acc1 acc2] x]
            (let [ac1 (if (filt1 x) (inc acc1) acc1)
                  ac2 (if (filt2 x) (inc acc2) acc2)]
              [ac1 ac2]))
          [0 0] pred-ac-seq))

(defn redcounts2
  "map over list of filters"
  [pred-ac-seq filters]
  (reduce (fn [acc x]
            (mapv #(if (%1 x) (inc %2) %2) filters acc))
          (vec (repeat (count filters) 0))
          pred-ac-seq))

(defn cfcounts
  "confusion matrix using update-in map"
  [pred-ac-seq]
  (reduce (fn [imap x]
            (update-in imap [x] (fnil inc 0)))
          {} pred-ac-seq))

(defn cmcounts
  "confusion matrix using mutable core.matrix matrix"
  [pred-ac-seq]
  (reduce (fn [m1 [x y]]
            (let [k (inc (m/mget m1 x y))]
              (m/mset! m1 x y k)
              m1))
          (m/mutable (m/new-matrix 2 2)) pred-ac-seq))

(defn true-positives
  "returns the count of true positives, defined as predicted positive class and actual positive class"
  ([pred-ac-seq] (true-positives pred-ac-seq 1))
  ([pred-ac-seq positive-class]
   (counts pred-ac-seq (fn [[a b]] (= a b positive-class)))))

(defn true-positive-rate
  "using counts fn"
  [pred-ac-seq positive-class]
  (double (/ (true-positives pred-ac-seq positive-class)
             (counts pred-ac-seq (fn [[pred ac]] (= ac positive-class))))))

(defn true-positive-rate-red
  "using redcounts fn"
  [pred-ac-seq positive-class]
  (let [[numer denom] (redcounts pred-ac-seq
                                 (fn [[pred ac]] (= ac positive-class))
                                 identity)]
    (double (/ numer denom))))

(defn true-positive-rate-red2
  ""
  [pred-ac-seq positive-class]
  (let [[numer denom] (redcounts2 pred-ac-seq
                                  [(fn [[pred ac]] (= ac positive-class))
                                   identity])]
    (double (/ numer denom))))
(defn true-positive-rate-cf
  "returns the true positive rate, defined as the count of correctly predicted positives divided by count of actual positives,
  Also known as sensitivity and recall"
  [pred-ac-seq positive-class]
  (let [imap (cfcounts pred-ac-seq)]
    (double (/ (imap [positive-class positive-class]) (apply + (vals imap))))))

(defn true-positive-rate-cm
  "returns the true positive rate, defined as the count of correctly predicted positives divided by count of actual positives,
  Also known as sensitivity and recall"
  [pred-ac-seq positive-class]
  (let [imat (cmcounts pred-ac-seq)]
    (double (/ (m/mget imat positive-class positive-class) (reduce + (m/to-vector imat))))))

(def descriptions
  ["filter & count"
   "multiple if, 2 filters"
   "confusion matrix using update-in map"
   "confusion matrix using mutable core.matrix matrix"
   "map over list of filters"])
(comment
  (c/with-bench-context descriptions
    {:chart-title "Comparison of counting methods"
     :chart-filename (format "confmat-counting-2kinst-%s.png" c/clojure-version-str)}
    (c/compare-perf
     "2k instances" (true-positive-rate bfix 1)
     (true-positive-rate-red bfix 1)
     (true-positive-rate-cf bfix 1)
     (true-positive-rate-cm bfix 1)
     (true-positive-rate-red2 bfix 1))))

(clojure.test/use-fixtures 
  :once (c/make-bench-wrapper descriptions
         {:chart-title "Comparison of counting methods"
          :chart-filename (format "confmat-counting-%s.png" c/clojure-version-str)}))

(deftest ^:benchmarking test-counts
  (c/compare-perf
   "2k instances" (true-positive-rate bfix 1)
   (true-positive-rate-red bfix 1)
   (true-positive-rate-cf bfix 1)
   (true-positive-rate-cm bfix 1)
   (true-positive-rate-red2 bfix 1))
  (c/compare-perf
   "1m instances" (true-positive-rate b1mfix 1)
   (true-positive-rate-red b1mfix 1)
   (true-positive-rate-cf b1mfix 1)
   (true-positive-rate-cm b1mfix 1)
   (true-positive-rate-red2 b1mfix 1)))

;(bench (true-positive-rate bfix 1) :samples 10)
;mean-361 micro seconds

;;;;;;;;;;;;;;;;;;;;;;;;;
(comment

  (bench (true-positive-rate bfix 1))
;mean time-417 micro seconds

  (bench (true-positive-rate b1mfix 1))
;mean-195 milli seconds

  (bench (true-positive-rate-red bfix 1))
;mean-253 micro seconds

  (bench (true-positive-rate-red b1mfix 1))
;mean- 113 milli seconds

  (bench (true-positive-rate-cf bfix 1))
;mean 1.9ms
  (bench (true-positive-rate-cf b1mfix 1))
;mean 1.9 seconds

  (bench (true-positive-rate-cm bfix 1))
;mean-8.1 ms
  (bench (true-positive-rate-cm b1mfix 1))
;mean-4.27 secs

  (bench (true-positive-rate-red2 bfix 1))
;2.5 msec
  (bench (true-positive-rate-red2 b1mfix 1))
;1.18 sec

;with mset! instead of m/set-indexes!
  (bench (true-positive-rate-cm bfix 1))
;mean-465 microms
  (bench (true-positive-rate-cm b1mfix 1))
;mean 243 milli secs
)
(comment
  (clojure.test/run-tests))
