(ns day4_2020
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [utils :refer [read-lines]]))

(def file-name "2020_day4_input")

(defn filter-blank-line
  "
  seq 의 element 중 [\"\"] 인 것을 배제합니다.

  Example:
  (filter-blank-line [[\"asd\"] [\"\"] [\"sdf\" \"qwe\"] [\"\"] [\"\"] [\"dfg\"]])
  => ([\"asd\"] [\"sdf\" \"qwe\"] [\"dfg\"])\n
  "
  [input]
  (filter #(not= % [""]) input))

(defn hash-map-by-delimeter
  "
  string을 ' '로 나누고 입력받은 delimeter로 잘라 map을 만듭니다.

  Example:
  (hash-map-by-delimeter #\":\" \"byr:1988 hcl:#623a2f eyr:2029 ecl:brn iyr:2018 hgt:167cm pid:470443459\")
  => {\"byr\" \"1988\", \"hcl\" \"#623a2f\", \"eyr\" \"2029\", \"ecl\" \"brn\", \"iyr\" \"2018\", \"hgt\" \"167cm\", \"pid\" \"470443459\"}\n
  "
  [delimeter input]
  (->> (str/split input #" ")
       (map (fn [x] (str/split x delimeter)))
       (into {})))

(defn parse [input]
  (->> (partition-by #(= % "") input)
       filter-blank-line
       (map #(str/join " " %))
       (map #(hash-map-by-delimeter #":" %))))

(defrecord Passport [birth-year
                     issue-year
                     expiration-year
                     height
                     hair-color
                     eye-color
                     id
                     country-id])

(s/def :part1/birth-year string?)
(s/def :part1/issue-year string?)
(s/def :part1/expiration-year string?)
(s/def :part1/height string?)
(s/def :part1/hair-color string?)
(s/def :part1/eye-color string?)
(s/def :part1/id string?)
(s/def :part1/country-id string?)

(s/def :part1/passport (s/keys :req [:part1/birth-year
                                        :part1/issue-year
                                        :part1/expiration-year
                                        :part1/height
                                        :part1/hair-color
                                        :part1/eye-color
                                        :part1/id]
                                  :opt [:part1/country-id]))

(def passport-keys {"byr" :part1/birth-year
                    "iyr" :part1/issue-year
                    "eyr" :part1/expiration-year
                    "hgt" :part1/height
                    "hcl" :part1/hair-color
                    "ecl" :part1/eye-color
                    "pid" :part1/id
                    "cid" :part1/country-id})

(def passport-keys2 {"byr" :part2/birth-year
                     "iyr" :part2/issue-year
                     "eyr" :part2/expiration-year
                     "hgt" :part2/height
                     "hcl" :part2/hair-color
                     "ecl" :part2/eye-color
                     "pid" :part2/id
                     "cid" :part1/country-id})

(defn replace-keys
  "
  hash-map 의 키를 입력받은 coll에 맞는 value로 변환한다.
  "
  [input coll]
  (->> (map (fn [[k v]] [(coll k) v]) input)
       (into {})))

(defn process [predicate input]
  (->> (map #(map->Passport %) input)
       (filter #(s/valid? predicate %))
       count))

;part two
(defn parse-only-int [input]
  "
  Example:
  (parse-only-int \"123cm\")
  => 123
  "
  (->> (re-find #"\d+" input)
       Integer/parseInt))

(s/def :part2/birth-year #(< 1919 (parse-only-int %) 2003))
(s/def :part2/issue-year #(< 2009 (parse-only-int %) 2021))
(s/def :part2/expiration-year #(< 2019 (parse-only-int %) 2031))
(s/def :part2/height (fn [x] (cond
                               (str/includes? x "cm") (< 149 (parse-only-int x) 194)
                               (str/includes? x "in") (< 58 (parse-only-int x) 77))))
(s/def :part2/hair-color (fn [x] (re-matches #"^#[0-9a-f]{6}$" x)))
(s/def :part2/eye-color #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :part2/id #(re-matches #"[0-9]{9}" %))

(s/def :part2/passport (s/keys :req [:part2/birth-year
                                     :part2/issue-year
                                     :part2/expiration-year
                                     :part2/height
                                     :part2/hair-color
                                     :part2/eye-color
                                     :part2/id]
                               :opt [:part1/country-id]))


(comment
  ; part one
  (->> (read-lines file-name str)
       parse
       (map (fn [x] (replace-keys x passport-keys)))
       (process :part1/passport))

  ;part two
  (->> (read-lines file-name str)
       parse
       (map (fn [x] (replace-keys x passport-keys2)))
       (process :part2/passport))

  (def raw-map {:a 1 :b 2 :c 3})
  (def key-map {:a :aa :b :bb :c :cc})
  (into {} (map (fn [[k v]] [(key-map k) v]) raw-map)))

