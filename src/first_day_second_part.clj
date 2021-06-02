(ns first-day-second-part)

(require '[clojure.java.io :as io])

(def file-name "firstday_input.txt")
(defn read-lines-as-digits [file] (map #(Integer/parseInt %) (clojure.string/split-lines (slurp (io/resource file)))))
(defn repeat-input [data]
  (if (= (count data) 1)
    (read-lines-as-digits file-name)
    (drop 1 data)))

(loop [acc-result 0
       seen #{}
       data (read-lines-as-digits file-name)]
  (if (contains? seen acc-result)
    acc-result
    (recur (+ acc-result (first data)) (conj seen acc-result) (repeat-input data))))
