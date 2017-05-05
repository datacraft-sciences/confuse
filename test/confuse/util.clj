(ns confuse.util)

(defn approx
  "returns true is expected and actual are within tolerance of each other"
  ([exp actual] (approx exp actual 0.01))
  ([exp actual tolerance]
  (> tolerance (Math/abs (- exp actual)))))
