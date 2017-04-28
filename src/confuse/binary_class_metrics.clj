(ns confuse.binary-class-metrics
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.impl.pprint :refer [pm]]
            [clojure.core.matrix.dataset :as cd]
            [com.rpl.specter :as sp]
            [clojure.spec.gen :as gen]))

(defn- accuracy-helper
  [pred-ac-seq filtfn]
  (let [denom (count pred-ac-seq)
        pred-ac-same (-> (filter filtfn pred-ac-seq) count double)]
    (/ pred-ac-same denom)))

(comment
  (defn counts
    [pred-ac-seq filtfn]
    (-> (filter filtfn pred-ac-seq) count)))

(defn counts
  ([actual predicted filt1]
   (first (reduce (fn [[acc1] x]
             (let [ac1 (if (filt1 x) (inc acc1) acc1)]
               [ac1]))
           [0] (mapv vector predicted actual))))
  ([actual predicted filt1 filt2]
   (let [[numer denom] (reduce (fn [[acc1 acc2] x]
                                 (let [ac1 (if (filt1 x) (inc acc1) acc1)
                                       ac2 (if (filt2 x) (inc acc2) acc2)]
                                   [ac1 ac2]))
                               [0 0] (mapv vector predicted actual))]
     (double (/ numer denom)))))

(defn accuracy
  "Accepts a vector where each element is a vector with 2 elements, the predicted
  and actual class. "
  [actual predicted]
  (counts actual predicted (fn [[a b]] (= a b))
          identity))

(comment
  (s/fdef accuracy
          :args (s/every #(= 2 (count %)))
          :ret double?)

  (stest/instrument `accuracy))

(defn true-positives
  "returns the count of true positives, defined as predicted positive class and actual positive class"
  ([actual predicted] (true-positives actual predicted 1))
  ([actual predicted positive-class]
   (counts actual predicted
           (fn [[a b]] (= a b positive-class)))))

(defn true-positive-rate
  "returns the true positive rate, defined as the count of correctly predicted positives divided by count of actual positives,
  Also known as sensitivity and recall"
  [actual predicted positive-class]
  (counts actual predicted (fn [[pred ac]] (= pred ac positive-class))
          (fn [[pred ac]] (= ac positive-class))))

(defn sensitivity
  "returns sensitivity, defined as the count of correctly predicted positives divided by count of actual positives. Also known as true positive rate or recall"
  [actual predicted positive-class]
  (true-positive-rate actual predicted positive-class))

(defn recall
  "returns sensitivity, defined as the count of correctly predicted positives divided by count of actual positives. Also known as true positive rate or sensitivity "
  [actual predicted positive-class]
  (true-positive-rate actual predicted positive-class))

(defn true-negatives
  "returns the count of true positives, defined as count of predicted negative class and actual negative class"
  [actual predicted positive-class]
  (counts actual predicted (fn [[pred ac]] (and (not= pred positive-class)
                                                (not= ac positive-class)))))

(defn true-negative-rate
  "returns the true negative rate, defined as the count of correctly predicted negatives divided by count of actual negatives "
  [actual predicted positive-class]
  (counts actual predicted
          (fn [[pred ac]] (and (not= pred positive-class)
                               (not= ac positive-class)))
          (fn [[pred ac]] (not= ac positive-class))))

(defn specificity
  "returns the specificity, also known as true negative rate, defined as the count of correctly predicted negatives divided by count of actual negatives "
  [actual predicted positive-class]
  (true-negative-rate actual predicted positive-class))

(defn false-positives
  "returns the count of false positives, defined as the count of predicted positive class and actual negative class"
  [actual predicted positive-class]
  (counts actual predicted
          (fn [[pred ac]] (and (= pred positive-class)
                               (not= ac positive-class)))))

(defn false-positive-rate
  "returns the false positive rate, defined as count of actual positives predicted as a negative, divided by count of actual negatives.
  Also known as fall-out."
  [actual predicted positive-class]
  (counts actual predicted
          (fn [[pred ac]] (and (= pred positive-class) (not= ac positive-class)))
          (fn [[pred ac]] (and (not= pred positive-class) (not= ac positive-class)))))

(defn false-negatives
  "returns the count of false negatives, defined as the count of predicted negative class and actual positive class"
  [actual predicted positive-class]
  (counts actual predicted
          (fn [[pred ac]] (and (not= pred positive-class)
                               (= ac positive-class)))))

(defn false-negative-rate
  "returns the false negative rate, defined as count of actual positive and predicted negative, divided by count of actual positives"
  [actual predicted positive-class]
  (counts actual predicted
                              (fn [[pred ac]] (and (not= pred positive-class)
                                                   (= ac positive-class)))
                              (fn [[pred ac]] (= ac positive-class))))

(defn precision
  "returns Precision, defined as the count of true positives over the count of true positives plus the count of false positives."
  [actual predicted positive-class]
  (counts actual predicted
          (fn [[pred ac]] (= ac pred positive-class))
          (fn [[pred ac]] (= pred positive-class))))

(defn recall
  "Returns the recall"
  [actual predicted positive-class]
  (true-positive-rate actual predicted positive-class))

(defn f1-score
  "returns the F1 score, defined as the harmonic mean of precision and recall."
  [actual predicted positive-class]
  (let [prec (precision actual predicted positive-class)
        recall (recall actual predicted positive-class)]
    (* 2 (/  (* prec recall) (+ prec recall)))))

(defn misclassification-rate
  "returns the misclassification rate, defined as (1 - accuracy) "
  [actual predicted]
  (- 1 (accuracy actual predicted)))

(defn- conf-mat
  [freq classes]
  (let [mapkeys (vec (for [i classes j classes] [i j]))]
    (merge-with + freq (zipmap mapkeys (repeat 0)))))

(defn confusion-matrix
  "returns a map representing the confusion matrix. The keys are a vector with [predicted, actual] and the values are the counts."
  (
  [actual predicted]
   (let [freq (frequencies (mapv vector predicted actual ) )
         classes (sort (set (mapv second (keys freq))))]
     (conf-mat freq classes)))
  ([actual predicted classes]
   (let [freq (frequencies (mapv vector predicted actual ))]
     (conf-mat freq classes))))

(defn confusion-matrix-str
  "returns a string representation given a confusion matrix as a map argument"
  [conf-mat]
  (let [classes (sort (set (mapv second (keys conf-mat))))
        nc (count classes)
        order (partition nc nc  (for [i classes j classes] [j i]))]
    (cd/dataset (into ["-"] classes)
                (mapv #(into [%2] (mapv (fn [i] (get conf-mat i 0)) %1)) order classes))))


