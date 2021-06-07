(ns first-day-first-part
  (:require [clojure.java.io :as io]
            [utils]))

(def file-name "firstday_input.txt")

(loop [acc-result 0
       data (utils/read-lines file-name #(Integer/parseInt %))]
  (if (empty? data)
    acc-result
    (recur (+ acc-result (nth data 0)) (drop 1 data))))

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

  (map (fn [v] (Integer/parseInt v)) (str/split-lines (slurp (io/resource file-name))))

  (reduce + (map (fn [v] (Integer/parseInt v)) (clojure.string/split-lines (slurp (io/resource file-name)))))


  (def input (->> (slurp (io/resource file-name))
                  (clojure.string/split-lines))))


;refactor
(defn part1 []
  (->> (utils/read-lines file-name #(Integer/parseInt %))
       (reduce +)))

; comment 는 함수 사이에 배치하지 않고 하단에 배치한다.
; 주로 comment 로 하단에서 작성완료 후 method extract 이후에 상단으로 mv

; io 하는 부분을 part1 함수에서 분리