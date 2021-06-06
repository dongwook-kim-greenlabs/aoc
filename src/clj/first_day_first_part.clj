(ns first-day-first-part
  (:require [clojure.java.io :as io]
            [utils]))

(def file-name "firstday_input.txt")

(loop [acc-result 0
       data (utils/read-lines file-name #(Integer/parseInt %))]
  (if (empty? data)
    acc-result
    (recur (+ acc-result (nth data 0)) (drop 1 data))))

(map (fn [v] (Integer/parseInt v)) (clojure.string/split-lines (slurp (io/resource file-name))))

(reduce + (map (fn [v] (Integer/parseInt v)) (clojure.string/split-lines (slurp (io/resource file-name)))))

(->> (slurp (io/resource file-name))
     (clojure.string/split-lines)
     (map #(Integer/parseInt %))
     (reduce +))

; (reduce + (list 1 2 3)) = (+ (+ 1 2) 3))
; (apply + (list 1 2 3)) = (+ 1 2 3)

;(->> input
;     (parse)
;     (process)
;     (print))
; PPAP : parse(원하는 형태로 raw input을 가공) - process - aggregate - print

(comment
  (def input (->> (slurp (io/resource file-name))
                  (clojure.string/split-lines))))


;refactor
(->> (utils/read-lines file-name #(Integer/parseInt %))
     (reduce +))