(ns second_day)

(def file-name "2day_part1_input.txt")

; part 1 solution

(defn exists? [m x]
  "[map integer]
    map의 value 중에서 x로 넘겨 받은 값이 존재한다면 true 를 반환합니다.
  "
  (not (empty? (filter (fn [[_ v]] (= v x)) m))))
(defn identity-counter [data]
  "[data]
    data에서 truthy 값만 세어 반환합니다."
  (count (filter identity data)))

(*
  (->> (utils/read-lines file-name frequencies)
       (map #(exists? % 2))
       (identity-counter))
  (->> (utils/read-lines file-name frequencies)
       (map #(exists? % 3))
       (identity-counter)))

;part 2 solution

(defn compare-string [orig-string dest-string]
  (map (fn [orig-string dest-string] (= orig-string dest-string)) orig-string dest-string))

(defn get-correct-box [orig-string dest-string]
  (= (count (filter identity (compare-string orig-string dest-string))) (dec (count orig-string))))

(defn convert-int [bool-list]
  (map {false 0 true 1} bool-list))

(defn remove-different-character [s1 s2]
  (let [index (.indexOf (convert-int (compare-string s1 s2)) 0)]
    (apply str (subs s1 0 index) (subs s1 (inc index)))))


(loop [data (utils/read-lines file-name seq)]
  (let [correct-box (filter (fn [x] (get-correct-box (first data) x)) (rest data))]
    (if (not (empty? correct-box))
      (println
        (remove-different-character (apply str (first data)) (apply str (first correct-box)))))
    (if (not (empty? data))
      (recur (rest data)))))
