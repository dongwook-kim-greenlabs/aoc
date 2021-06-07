(ns first-day-second-part
  (:require [utils]))

(def file-name "firstday_input.txt")
;(defn repeat-input [data]
;  (if (= (count data) 1)
;    (utils/read-lines file-name #(Integer/parseInt %))
;    (drop 1 data)))

(let [input (utils/read-lines file-name #(Integer/parseInt %))]
  (loop [acc-result 0
         seen? #{}
         data (cycle input)]
    (if (seen? acc-result)
      acc-result
      (recur (+ acc-result (first data))
             (conj seen? acc-result)
             (rest data)))))

(defn acc-seqs [input] (reductions + input))

(defn get-first-duplicated-value [coll]
  (loop [[first-ele & rest-eles] coll
         acc-seqs]
    (if rest-eles
      (recur rest-eles acc-seqs))))

;(->> (utils/read-lines ...)
;     acc-seqs
;     get-first-duplicated-value)
;SOLID

;(cycle (utils/read-lines file-name #(Integer/parseInt %)))

; (drop 1 coll) = (rest coll) = (next coll)
; (take 1 coll) = (first coll)

;(#{1,2,3} 1)
