(ns second_day_first_part)

(def file-name "2day_part1_input.txt")

(utils/read-lines file-name frequencies)
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
