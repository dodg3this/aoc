(ns year-2023
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]))

;;Day 01
;;--------------------------------------------------------------------------------

(def sample-01
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def sample-01-b
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defn day-01-a [input]
  (->> (s/split input #"\n")
       (map #(let [nums (re-seq #"\d" %)]
               (Integer/parseInt (str  (first nums) (last nums)))))
       (reduce +)))

(def day-01-b-nums
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn to-num [s]
  (get day-01-b-nums s s))

(defn day-01-b [input]
  (->> (s/split input #"\n")
       (map #(let [f (re-find #"\d|one|two|three|four|five|six|seven|eight|nine" %)
                   l (re-find #"\d|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin" (s/reverse %))]
               (Integer/parseInt (str (to-num f) (to-num (s/reverse l))))))
       (reduce +)))

;; (day-01-b sample-01-b)
;; (clojure.pprint/pprint (day-01-a (slurp "resources/day-01.txt")))
;; (clojure.pprint/pprint (day-01-b (slurp "resources/day-01.txt")))

;;Day 02
;;--------------------------------------------------------------------------------

(defn bigger [a b]
  (if (> a b) a b))

(def sample-02
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn day-02 [input]
  (for [l (s/split input #"\n")
        :let [i (re-find #"Game (\d+):" l)
              draws (s/split (second (s/split l #": ")) #"; ")
              drws (reduce #(->> (into {} (for [d  (s/split %2 #", ")
                                                :let [[n c] (s/split d #" ")]]
                                            [c (parse-long n)]))
                                 (merge-with bigger %1)) {} draws)]] [(parse-long (second i)) drws]))

(defn day-02-a [input]
  (reduce +  (keys (into {} (filter (fn [[i drws]]
                                      (let [blues (get drws "blue" 0)
                                            reds (get drws "red" 0)
                                            greens (get drws "green" 0)]
                                        (and (<= blues 14)
                                             (<= reds 12)
                                             (<= greens 13))))
                                    (day-02 input))))))

(defn day-02-b [input]
  (reduce + (map (fn [[i drws]]
                   (apply * (vals drws)))
                 (day-02 input))))

;; (day-02-b sample-02)
;; (day-02-b (slurp "resources/day-02.txt"))

;;Day-03
;;--------------------------------------------------------------------------------

(def sample-03
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn adjacents [i l]
  (let [row (quot i 10)
        column (mod i 10)]
    (cond
      (zero? column) #{})))

(defn day-03-a [input]
  (let [lines (s/split input #"\n")
        normalized (apply str lines)
        numbers (map #(vec [% (re-seq #"\d+" %)]) lines)]
    (map (fn [[l nums]]
           (for [n nums
                 :let [i (+ (s/index-of normalized l) (s/index-of l n))
                       c (count n)
                       indexes (map #(get normalized %) (filter nat-int? #{(+ i c) (- i 1) (- i 10) (- i 11) (- i 9) (+ i 10) (+ i 11) (+ i 9)}))]]
             [i c indexes n])) numbers)))

;;Day 05
;;----------------------------------------------------------------------------------

(def sample-05
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn next-map [ranges n]
  ;; (pprint/pprint (str "^^^^^^^^^^^^^^" n))

  (loop [mappings ranges result -1]
    (cond
      (nat-int? result) result
      (empty? mappings) n
      :else (let [[sink-start source-start range] (first mappings)]
              (if (<= source-start n (+ source-start range))
                (+ sink-start (- n source-start))
                (recur (rest mappings) result))))))

(defn day-05-a [input]
  (let [[initial-seeds & sink-source-map] (s/split input #"\n\n")

        initial-seed (apply concat (map  (fn [[start r]] (range start (+ r start))) (partition 2 (map parse-long (re-seq #"\d+" initial-seeds)))))]
    (first (sort (let [mappings (for [m sink-source-map
                                      :let [lines (s/split m #"\n")
                                            [name & data] lines
                                            mappings  (map #(->> (s/split % #" ")
                                                                 (map parse-long)) data)]
                                      ;; :when (= name "seed-to-soil map:")
                                      ]
                                  mappings)]
                   (reduce (fn [r mp]
                             (let [b (map #(next-map mp %) r)]
                               (pprint/pprint (apply str b))
                               b)) initial-seed mappings))))))

;; (clojure.pprint/pprint (day-05-a sample-05))
;; (pprint/pprint (day-05-a (slurp "resources/day-05.txt")))

;; Day 06
;; --------------------------------------------------------------------------------

(def sample-06
  "Time:      7  15   30
Distance:  9  40  200")

(defn input-06 [input]
  (apply zipmap (map #(->> (re-seq #"\d+" %)
                           (map parse-long)) (s/split input #"\n"))))
(defn input-06-b [input]
  (map #(->> (apply str (re-seq #"\d+" %))
             (parse-long)) (s/split input #"\n")))
(defn day-06-a [input]
  (apply * (->> (input-06 input)
                (map (fn [[t d]]
                       (count (for [i (range 1 t)
                                    :let [di (* i (- t i))]
                                    :when (> di d)]
                                i)))))))

(defn day-06 [[t d]]
  (count (for [i (range 1 t)
               :let [di (* i (- t i))]
               :when (> di d)]
           i)))

(defn day-06-b [input]
  (apply * (->> (input-06 input)
                (map day-06))))
;; (day-06-a sample-06)
;; (day-06-a (slurp "resources/day-06.txt"))
;; (day-06 (input-06-b (slurp "resources/day-06.txt")))
