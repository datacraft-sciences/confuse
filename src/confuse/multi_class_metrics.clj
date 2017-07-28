(ns confuse.multi-class-metrics
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :refer [mean]]
            [clojure.core.matrix.dataset :as cd]
            [confuse.binary-class-metrics :as b :refer [counts]]
            [clojure.test :refer [is deftest]]
            [clojure.spec.gen :as gen]))

;http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.8244&rep=rep1&type=pdf
(defn micro-avg-fmeasure
  "returns the micro averaged fmeasure, defined as harmonic mean of precision and recall, where precision and recall are calculated by summing over all classes.
  See Section 4.2 in this paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.8244&rep=rep1&type=pdf
"
  ([actual predicted] (micro-avg-fmeasure actual predicted (set (into actual predicted))))
  ([actual predicted classes]
   (let [tpisum (apply + (mapv (partial b/true-positives actual predicted) classes))
         fpisum (apply + (mapv (partial b/false-positives actual predicted) classes))
         fnisum (apply + (mapv (partial b/false-negatives actual predicted) classes))
         pi (/ tpisum (+ tpisum fpisum))
         rho (/ tpisum (+ tpisum fnisum))]
     (double (/ (* 2 pi rho) (+ pi rho))))))

(defn macro-avg-fmeasure
  "returns the macro averaged fmeasure, defined as mean of F-measure for each class.
  See Section 4.2 in this paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.8244&rep=rep1&type=pdf
"
  ([actual predicted] (micro-avg-fmeasure actual predicted (set (into actual predicted))))
  ([actual predicted classes]
   (let [fsum (mapv (partial b/f1-score
                             actual predicted) classes)]
     (mean fsum))))

(defn micro-avg-precision
  "returns the micro averaged precision, defined as the sum of true positives for all classes,
  divided by the sum of true positives and false positives for all classes."
  ([actual predicted] (micro-avg-precision actual predicted (set (into actual predicted))))
  ([actual predicted classes]
  (let [tpisum (apply + (mapv (partial b/true-positives actual predicted) classes))
        fpisum (apply + (mapv (partial b/false-positives actual predicted) classes))]
    (double (/ tpisum (+ tpisum fpisum))))))

(defn macro-avg-precision
  "returns the macro averaged precision, defined as the mean of precision for each class"
  ([actual predicted ] (macro-avg-precision actual predicted (set (into actual predicted))))
  ([actual predicted classes]
  (mean (mapv (partial b/precision actual predicted) classes))))

(defn macro-avg-recall
  "returns the macro averaged recall, defined as the mean of recall for each class"
  ([actual predicted ] (macro-avg-recall actual predicted (set (into actual predicted))))
  ([actual predicted classes]
  (mean (mapv (partial b/recall actual predicted) classes))))

(defn micro-avg-recall
  "returns the micro averaged recall, defined as the sum of true positives for all classes,
  divided by the sum of true positives and false negatives for all classes"
  ([actual predicted ] (micro-avg-recall actual predicted (set (into actual predicted))))
  ([actual predicted classes]
  (let [tpisum (apply + (mapv (partial b/true-positives actual predicted) classes))
        fnisum (apply + (mapv (partial b/false-negatives actual predicted) classes))]
    (double (/ tpisum (+ tpisum fnisum))))))
