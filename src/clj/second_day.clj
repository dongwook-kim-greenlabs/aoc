(ns second_day
  (:require [clojure.data]
            [utils]))

(def file-name "2day_part1_input.txt")

; part 1 solution

(defn exists? [m x]
  "[map integer]
    map의 value 중에서 x로 넘겨 받은 값이 존재한다면 true 를 반환합니다.
  "
  (filter (fn [[_ v]] (= v x)) m))
; condition 에서 truthly / falsey 로 주로 체크

(defn identity-counter [data]
  "[data]
    data에서 truthy 값만 세어 반환합니다."
  (count (keep data)))

(let [freq (utils/read-lines file-name frequencies)]
  (->> [2 3]
       (map #(exists? freq %))
       (map identity-counter)))




;(defn solve [input]
;  (let [twos (->)]
;    (* twos threes)))
;
;(->> (parse input)
;     (solve))

;part 2 solution

(defn compare-string

  [orig-string dest-string]
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

;refactor
;part1
(defn parse-first-part [data]
  (->> (frequencies data)
       vals
       set))

(defn process-first-part [data]
  (letfn [(sum-only-value-exists [data value] (reduce + (map (fn [x] (if (x value) 1 0)) data)))]
    (*
      (sum-only-value-exists data 2)
      (sum-only-value-exists data 3))))

(->> (utils/read-lines file-name seq)
     (map parse-first-part)
     process-first-part)

;part2

;; 1. 입력의 모든 쌍에서
;; 2. 쌍을 비교했을 때 글자 하나만 다른 쌍을 찾아서
;; 3. 그 쌍에서 달랐던 글자를 제거한 문자열을 반환

(defn only-one-character-diff? [orig-string dest-string]
  "
  Example:
  (only-one-character-diff? \"ddd\" \"add\")
  => true
  (only-one-character-diff? \"dsd\" \"add\")
  => false
  "
  (let [diff (->> (map seq [orig-string dest-string])
                  (apply clojure.data/diff)
                  frequencies)
        diff-words (->> (last diff)
                        first
                        frequencies)]
    (and (= (get diff-words nil) 1)
         (= (count (first (last diff))) 26))))

(defn get-only-same-character [orig-string dest-string]
  "['ddd' 'add']
  => 'dd'"
  ;(letfn [both-diff-values (last (apply clojure.data/diff))])
  (->> (map seq [orig-string dest-string])
       (apply clojure.data/diff)
       last ; context 가 일치하는 경우는 lambda 로 묶어 주는게 좀더 가독성이 낫다 (like (last (apply clojure.data/diff)) -> into one
       (apply str)))

(defn process-part-two [data]
  (loop [[first-ele & rest-eles] data]
    (let [result (->> (map
                        (fn [x] (only-one-character-diff? first-ele x))
                        rest-eles)
                      (filter identity)
                      first)]
      (if (and (seq rest-eles) (empty? result))
        (recur rest-eles)
        (apply get-only-same-character result)))))


; (1 2 3 4) -> ( ((1 2) (1 3) (1 4)) ((2 3) (2 4)) ((3 4)) ) -> flatten -> answer!
; 위의 combinations pair로 만드는걸 lazySeq / loop-recur 로 만들어볼것)

(comment (->> (utils/read-lines file-name str)
              process-part-two))