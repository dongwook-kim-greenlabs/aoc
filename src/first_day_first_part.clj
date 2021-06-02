(ns first-day-first-part)

(require '[clojure.java.io :as io])

(def file-name "firstday_input.txt")
(defn read-lines-as-digits
  "([파일명])
    파일로부터 line들을 읽어들여 Integer 로 parsing 된 LazySeq를 반환합니다.

    Example:
    (read-lines-as-digits 'firstday_input.txt')
    => (+19 -1 -3 +2)"
  [file] (map #(Integer/parseInt %) (clojure.string/split-lines (slurp (io/resource file)))))

(loop [acc-result 0
       data (read-lines-as-digits file-name)]
  (if (empty? data)
    (println acc-result)
    (recur (+ acc-result (nth data 0)) (drop 1 data))))
