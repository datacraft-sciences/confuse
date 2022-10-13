(ns confuse.multi-class-metrics
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :refer [mean sum]]
            [clojure.core.matrix.dataset :as cd]
            [confuse.binary-class-metrics :as b :refer [counts]]
            [clojure.test :refer [is deftest]]))

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

(defn- div0-safe-map
  "catches divided by zero error for some classes and filters them"
  [fnx classes]
  (remove nil? (mapv #(try (fnx %)
                           (catch java.lang.ArithmeticException e
                             nil)) classes)))

(defn macro-avg-fmeasure
  "returns the macro averaged fmeasure, defined as mean of F-measure for each class.
  See Section 4.2 in this paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.8244&rep=rep1&type=pdf
"
  ([actual predicted] (micro-avg-fmeasure actual predicted (set (into actual predicted))))
  ([actual predicted classes]
   (let [fsum (div0-safe-map (partial b/f1-score
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
   (let [fsum (div0-safe-map (partial b/precision actual predicted) classes)]
     (mean fsum))))

(defn macro-avg-recall
  "returns the macro averaged recall, defined as the mean of recall for each class"
  ([actual predicted ] (macro-avg-recall actual predicted (set (into actual predicted))))
  ([actual predicted classes]
   (let [fsum (div0-safe-map (partial b/recall actual predicted) classes)]
     (mean fsum))))

(defn micro-avg-recall
  "returns the micro averaged recall, defined as the sum of true positives for all classes,
  divided by the sum of true positives and false negatives for all classes"
  ([actual predicted ] (micro-avg-recall actual predicted (set (into actual predicted))))
  ([actual predicted classes]
  (let [tpisum (apply + (mapv (partial b/true-positives actual predicted) classes))
        fnisum (apply + (mapv (partial b/false-negatives actual predicted) classes))]
    (double (/ tpisum (+ tpisum fnisum))))))



(defn multiclass-mcc
  "returns multiclass Matthews correlation coefficient.
  https://en.wikipedia.org/wiki/Phi_coefficient"
  ([actual predicted classes]
   (let [
         cm
         (->
          (b/confusion-matrix actual predicted classes)
          (b/confusion-matrix-str)
          (m/select :all :rest))


         true-k (fn [k] (sum (m/get-row cm k)))

         predicted-k (fn [k] (sum (m/get-column cm k)))

         sum-mult-fns (fn [fn-1 fn-2] (sum (map
                                           #(* (fn-1 %) (fn-2 %))
                                           (range (first
                                                   (m/shape cm))))))

         correct-samples (apply + (m/diagonal cm))
         total-samples (m/esum cm)

         cov-x-y
         (-
          (* correct-samples total-samples)
          (sum-mult-fns predicted-k true-k))

         cov-x-x
         (Math/sqrt (* (- (* total-samples total-samples)
                          (sum-mult-fns predicted-k predicted-k))
                       (- (* total-samples total-samples)
                          (sum-mult-fns true-k true-k))))]

     (if (= 0.0 cov-x-x)
       0.0
       (/ cov-x-y cov-x-x))))
  ([actual predicted]
   (multiclass-mcc actual predicted (distinct (concat actual predicted)))))
