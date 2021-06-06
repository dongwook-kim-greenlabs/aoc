(ns third_day)
(require '[clojure.string :as c-str])
(require '[clojure.set :as c-set])

(def file-name "thirdday_input.txt")

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

(defn check-intersection [datum data]
  (map (fn [x] (apply c-set/intersection (map-set [datum x]))) data))

(defn find-overlaps [cnt data]
  (let [overlaps (c-set/union cnt (set
                                    (apply c-set/union
                                           (check-intersection (first data) (rest data)))))]
    (if (not (empty? data))
      (recur overlaps (rest data))
      cnt)))

(comment (->> (utils/read-lines file-name str)
              (map parse)
              (map get-fabrics)
              (apply merge)
              (keys)
              (find-overlaps #{})
              (count)))

; part2 solution

(defn find-not-overlapped [overlapped data]
  (if (empty? (c-set/intersection (first data) overlapped))
    (first data)
    (if (not (empty? data))
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
                         (seq)
                         (sort)))

(comment (get (apply merge parsed-data) not-overlapped))
