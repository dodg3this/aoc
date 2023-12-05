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
