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

(defn counts
  [pred-ac-seq filtfn]
  (-> (filter filtfn pred-ac-seq) count))

(defn redcounts
  [pred-ac-seq filt1 filt2]
  (reduce (fn [[acc1 acc2] x]
            #_(println "acc1 " acc1 " acc2 " acc2 " x " x)
            (let [ac1 (if (filt1 x) (inc acc1) acc1)
                  ac2 (if (filt2 x) (inc acc2) acc2)]
              [ac1 ac2]))
          [0 0] pred-ac-seq))

(defn cfcounts
  [pred-ac-seq]
  (reduce (fn [imap x]
            (update-in imap [x] (fnil inc 0)))
          {} pred-ac-seq))

;(update-in [[0 0] [0 0]] [0 0] inc)
;(sp/setval [0 0] 1 [[0 0] [0 0]])
(defn cmcounts
  [pred-ac-seq]
  (reduce (fn [m1 [x y]]
            (let [k (inc (m/mget m1 x y))]
              (m/mset! m1 x y k)
              m1))
          (m/mutable (m/new-matrix 2 2)) pred-ac-seq))
(let [m1 (m/mutable (m/new-matrix 2 2))
      kfn (fn [a b] (do (println "a " a " b " b) (inc b)))
      k1fn (fn [a] (do (println "a " a) (inc a)))]
  ;(m/emap! k1fn m1)
 ; (m/set-indices! m1 [[0 0]] [-1])
  (inc (m/mget m1 0 0)))
()
(cmcounts [[0 0] [1 1]])
(update-in {[0 1] 0} [[0 1]] (fnil inc 0))
(update-in {[0 1] 0} [[1 1]] (fnil inc 0))
(defn accuracy
  "Accepts a vector where each element is a vector with 2 elements, the predicted
  and actual class. "
  [pred-ac-seq]
  (accuracy-helper pred-ac-seq (fn [[a b]] (= a b))))

(comment
  (s/fdef accuracy
          :args (s/every #(= 2 (count %)))
          :ret double?)

  (stest/instrument `accuracy))

(defn true-positives
  "returns the count of true positives, defined as predicted positive class and actual positive class"
  ([pred-ac-seq] (true-positives pred-ac-seq 1))
  ([pred-ac-seq positive-class]
   (counts pred-ac-seq (fn [[a b]] (= a b positive-class)))))

(defn true-positive-rate
  "returns the true positive rate, defined as the count of correctly predicted positives divided by count of actual positives,
  Also known as sensitivity and recall"
  [pred-ac-seq positive-class]
  (double (/ (true-positives pred-ac-seq positive-class)
             (counts pred-ac-seq (fn [[pred ac]] (= ac positive-class))))))

(defn true-positive-rate-red
  "returns the true positive rate, defined as the count of correctly predicted positives divided by count of actual positives,
  Also known as sensitivity and recall"
  [pred-ac-seq positive-class]
  (let [[numer denom] (redcounts pred-ac-seq
                                 (fn [[pred ac]] (= ac positive-class))
                                 identity)]
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
(defn sensitivity
  "returns sensitivity, defined as the count of correctly predicted positives divided by count of actual positives. Also known as true positive rate or recall"
  [pred-ac-seq positive-class]
  (true-positive-rate pred-ac-seq positive-class))

(defn recall
  "returns sensitivity, defined as the count of correctly predicted positives divided by count of actual positives. Also known as true positive rate or sensitivity "
  [pred-ac-seq positive-class]
  (true-positive-rate pred-ac-seq positive-class))

(defn true-negatives
  "returns the count of true positives, defined as count of predicted negative class and actual negative class"
  [pred-ac-seq positive-class]
  (counts pred-ac-seq (fn [[pred ac]] (and (not= pred positive-class)
                                           (not= ac positive-class)))))

(defn true-negative-rate
  "returns the true negative rate, defined as the count of correctly predicted negatives divided by count of actual negatives "
  [pred-ac-seq positive-class]
  (double (/ (true-negatives pred-ac-seq positive-class)
             (counts pred-ac-seq (fn [[pred ac]] (not= ac positive-class))))))

(defn specificity
  "returns the specificity, also known as true negative rate, defined as the count of correctly predicted negatives divided by count of actual negatives "
  [pred-ac-seq positive-class]
  (true-negative-rate pred-ac-seq positive-class))

(defn false-positives
  "returns the count of false positives, defined as the count of predicted positive class and actual negative class"
  [pred-ac-seq positive-class]
  (counts pred-ac-seq (fn [[pred ac]] (and (= pred positive-class)
                                           (not= ac positive-class)))))

(defn false-positive-rate
  "returns the false positive rate, defined as count of actual positives predicted as a negative, divided by count of actual negatives.
  Also known as fall-out."
  [pred-ac-seq positive-class]
  (double (/ (false-positives pred-ac-seq positive-class)
             (counts pred-ac-seq (fn [[pred ac]] (not= ac positive-class))))))

(defn false-negatives
  "returns the count of false negatives, defined as the count of predicted negative class and actual positive class"
  [pred-ac-seq positive-class]
  (counts pred-ac-seq (fn [[pred ac]] (and (not= pred positive-class)
                                           (= ac positive-class)))))
(defn false-negative-rate
  "returns the false negative rate, defined as count of actual positive and predicted negative, divided by count of actual positives"
  [pred-ac-seq positive-class]
  (double (/ (false-negatives pred-ac-seq positive-class)
             (counts pred-ac-seq (fn [[pred ac]] (= ac positive-class))))))

(defn precision
  "returns Precision, defined as the count of true positives over the count of true positives plus the count of false positives."
  [pred-ac-seq positive-class]
  (let [tp (true-positives pred-ac-seq positive-class)]
    (double (/ tp
               (counts pred-ac-seq (fn [[pred ac]] (= pred positive-class)))))))

(defn recall
  "Returns the recall"
  [pred-ac-seq positive-class]
  (true-positive-rate pred-ac-seq positive-class))

(defn f1-score
  "returns the F1 score, defined as the harmonic mean of precision and recall."
  [pred-ac-seq positive-class]
  (let [prec (precision pred-ac-seq positive-class)
        recall (recall pred-ac-seq positive-class)]
    (* 2 (/  (* prec recall) (+ prec recall)))))

(defn misclassification-rate
  "returns the misclassification rate, defined as (1 - accuracy) "
  [pred-ac-seq]
  (- 1 (accuracy pred-ac-seq)))

(defn- conf-mat
  [freq classes]
  (let [mapkeys (vec (for [i classes j classes] [i j]))]
    (merge-with + freq (zipmap mapkeys (repeat 0)))))

(defn confusion-matrix
  "returns a map representing the confusion matrix. The keys are a vector with [predicted, actual] and the values are the counts."
  ([pred-ac-seq]
   (let [freq (frequencies pred-ac-seq)
         classes (sort (set (mapv second (keys freq))))]
     (conf-mat freq classes)))
  ([pred-ac-seq classes]
   (let [freq (frequencies pred-ac-seq)]
     (conf-mat freq classes))))

(defn confusion-matrix-str
  "returns a string representation given a confusion matrix as a map argument"
  [conf-mat]
  (let [classes (sort (set (mapv second (keys conf-mat))))
        nc (count classes)
        order (partition nc nc  (for [i classes j classes] [j i]))]
    (cd/dataset (into ["-"] classes)
                (mapv #(into [%2] (mapv (fn [i] (get conf-mat i 0)) %1)) order classes))))
(def fixt
  (vec (concat
        (repeat 20 [1 1])
        (repeat 1820 [0 0])
        (repeat 180 [1 0])
        (repeat 10 [0 1]))))

(println (pm (confusion-matrix-str (confusion-matrix fixt))))
