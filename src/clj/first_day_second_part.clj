(ns first-day-second-part
  (:require [utils]))

(def file-name "firstday_input.txt")
;(defn repeat-input [data]
;  (if (= (count data) 1)
;    (utils/read-lines file-name #(Integer/parseInt %))
;    (drop 1 data)))

(loop [acc-result 0
       seen? #{}
       data (cycle (utils/read-lines file-name #(Integer/parseInt %)))] ; repeat-input 으로 호출하도록
  (if (seen? acc-result)
    acc-result
    (recur (+ acc-result (first data)) (conj seen? acc-result) (rest data))))

;(cycle (utils/read-lines file-name #(Integer/parseInt %)))

; (drop 1 coll) = (rest coll) = (next coll)
; (take 1 coll) = (first coll)

;(#{1,2,3} 1)
