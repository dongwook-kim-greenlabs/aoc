(ns third_day
  (:require [clojure.string :as c-str]
            [clojure.set :as c-set]
            [utils]))

(def file-name "thirdday_input.txt")


(def x1 "#1 @ 1,3: 4x4")
(def x2 "#2 @ 3,1: 4x4")
(def x3 "#3 @ 5,5: 2x2")


(defn- split [raw-string]
  (map c-str/trim
       (c-str/split raw-string #"@|:|,|x")))

(defn- cast-int [pos-func coll]
  (map #(Integer/parseInt %) (pos-func coll)))

(defn- map-set [coll]
  (map set coll))

(defn parse [string]
  (zipmap [:box-id :margin-left :margin-top :fabric-width :fabric-height]
          (cons (first (split string)) (cast-int rest (split string)))))

(defn get-fabrics [{:keys [box-id margin-left margin-top fabric-width fabric-height]}]
  {(for [x (range margin-left (+ margin-left fabric-width))
         y (range margin-top (+ margin-top fabric-height))]
     [x y]) box-id})
; key-value reverse 한 경우 zipmap

(defn check-intersection [datum data]
  (map (fn [x] (apply c-set/intersection (map-set [datum x]))) data))

(defn find-overlaps [cnt [f & data]] ; 구조분해
  (let [overlaps (c-set/union cnt (set
                                    (apply c-set/union
                                           (check-intersection f data))))]
    (if (seq data) ; not empty 대신 seq or some?
      (recur overlaps data)
      cnt)))

(->> (utils/read-lines file-name str)
     (map parse)
     (map get-fabrics)
     (apply merge) ;; +keys extract method 후 verb 로 naming
     (keys)
     (find-overlaps #{})
     (count))

; part2 solution

(defn find-not-overlapped [overlapped data]
  (if (empty? (c-set/intersection (first data) overlapped))
    (first data)
    (if (seq data)
      (recur overlapped (rest data)))))

(def parsed-data (->> (utils/read-lines file-name str)
                      (map parse)
                      (map get-fabrics)))

(def overlapped (->> parsed-data
                     (apply merge)
                     (keys)
                     (find-overlaps #{})))

(def not-overlapped (->> parsed-data
                         (apply merge)
                         (keys)
                         (map set)
                         (find-not-overlapped overlapped)
                         (seq) ; seq + sort 대신 sorted-set 으로
                         (sort)))

(get (apply merge parsed-data) not-overlapped)
