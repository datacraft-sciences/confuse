# confuse

Binary and Multi-class evaluation metrics for classification tasks in machine learning.

## Installation

Add the following to your :dependencies:

[![Clojars Project](https://img.shields.io/clojars/v/datacraft-sciences/confuse.svg)](https://clojars.org/datacraft-sciences/confuse)

## Supported metrics

### Counts 
* True positives
* False positives
* False negatives
* True negatives 

### Binary class Metrics

* sensitivity / specificity / true positive rate
* specificity / true negative rate
* false positive rate
* false negative rate
* precision
* f1-score
* accuracy
* misclassification-rate
* confusion matrix

### Multi-class metrics
* Micro averaged fmeasure/precision/recall
* Macro averaged fmeasure/precision/recall

## Usage

* Define the predicted and actuals in 2 different vectors
* Call any of the apis with the actuals, predicted vectors as arguments
* Some APIs require the positive class to be provided as a third argument 

```  clojure
(:require [confuse.binary-class-metrics :refer :all])
(def pred [1 0 1 1 0])
(def ac [1 1 1 1 0])

;;assume the positive class is 1
(true-positives ac pred 1)
;;3

;;calculate accuracy
(accuracy ac pred)
;;0.8

;;predicted and actuals could be keywords too
(def spam-pred [:spam :spam :notspam :notspam :spam])
(def spam-actual [:spam :notspam :spam :notspam :spam])

;;pass the positive class as the third argument
(true-positives spam-actual spam-pred :spam)
;;2

;;Confusion matrix
;;returns a map where the key is frequency [predicted actual] and value is the count 
;;against that key 
(confusion-matrix spam-actual spam-pred)
;;{[:spam :spam] 2, [:spam :notspam] 1, [:notspam :spam] 1, [:notspam :notspam] 1}
```
## License

Copyright © 2017 Datacraft Sciences

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
