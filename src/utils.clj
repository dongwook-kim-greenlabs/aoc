(ns utils)

(require '[clojure.java.io :as io])

(defn read-lines
  "([파일명 function])
    파일로부터 line들을 읽어들여 function 로 parsing 된 LazySeq를 반환합니다.

    Example:
    (read-lines 'firstday_input.txt' #(Integer/parseInt %))
    => (+19 -1 -3 +2)"
  [file function] (map function (clojure.string/split-lines (slurp (io/resource file)))))