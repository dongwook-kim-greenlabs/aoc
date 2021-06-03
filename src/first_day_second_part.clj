(ns first-day-second-part)

(def file-name "firstday_input.txt")
(defn repeat-input [data]
  (if (= (count data) 1)
    (utils/read-lines file-name #(Integer/parseInt %))
    (drop 1 data)))

(loop [acc-result 0
       seen #{}
       data (utils/read-lines file-name #(Integer/parseInt %))]
  (if (contains? seen acc-result)
    acc-result
    (recur (+ acc-result (first data)) (conj seen acc-result) (repeat-input data))))
