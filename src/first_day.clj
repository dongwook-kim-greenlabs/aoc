(ns first-day)

(require '[clojure.java.io :as io])

(def file-name "firstday_input.txt")
(defn read-lines-as-digits [file] (map read-string (clojure.string/split-lines (slurp (io/resource file)))))

(loop [acc-result 0
       data (read-lines-as-digits file-name)]
  (if (empty? data)
    (println acc-result)
    (recur (+ acc-result (nth data 0)) (drop 1 data))))
