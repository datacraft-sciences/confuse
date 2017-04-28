(ns confuse.multi-class-metrics
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :refer [mean]]
            [clojure.core.matrix.dataset :as cd]
            [confuse.binary-class-metrics :as b :refer [counts]]
            [clojure.test :refer [is deftest]]
            [clojure.spec.gen :as gen]))

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

(deftest TP
  (is (= 5 (b/true-positives fixt :cat)))
  (is (= 17 (b/true-negatives fixt :cat)))
  (is (= 3 (b/false-negatives fixt :cat)))
  (is (= 2 (b/false-positives fixt :cat))))

;http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.8244&rep=rep1&type=pdf
(defn micro-avg-fmeasure
  "returns the micro averaged fmeasure, defined as harmonic mean of precision and recall, where precision and recall are calculated by summing over all classes.
  See Section 4.2 in this paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.8244&rep=rep1&type=pdf
"
  ([pred-ac-seq classes]
   (let [tpisum (apply + (mapv (partial b/true-positives pred-ac-seq) classes))
         fpisum (apply + (mapv (partial b/false-positives pred-ac-seq) classes))
         fnisum (apply + (mapv (partial b/false-negatives pred-ac-seq) classes))
         pi (/ tpisum (+ tpisum fpisum))
         rho (/ tpisum (+ tpisum fnisum))]
     (double (/ (* 2 pi rho) (+ pi rho))))))

(micro-avg-fmeasure fixt #{:cat :dog :rabbit})
;0.70

(defn macro-avg-fmeasure
  "returns the macro averaged fmeasure, defined as mean of F-measure for each class.
  See Section 4.2 in this paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.8244&rep=rep1&type=pdf
"
  ([pred-ac-seq classes]
   (let [fsum (mapv (partial b/f1-score
                             pred-ac-seq) classes)]
     (mean fsum))))

(macro-avg-fmeasure fixt #{:cat :dog :rabbit})
;0.65

(defn micro-avg-precision
  [pred-ac-seq classes]
  (let [tpisum (apply + (mapv (partial b/true-positives pred-ac-seq) classes))
        fpisum (apply + (mapv (partial b/false-positives pred-ac-seq) classes))]
    (double (/ tpisum (+ tpisum fpisum)))))

(defn macro-avg-precision
  [pred-ac-seq classes]
  (mean (->> (mapv (partial b/precision pred-ac-seq) classes)
             ()
             )))

(defn macro-avg-recall
  [pred-ac-seq classes]
  (mean (mapv (partial b/recall pred-ac-seq) classes)))
