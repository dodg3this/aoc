(ns year-2023
  (:require [clojure.string :as s]
            [clojure.core.reducers :as r]
            [clojure.set :as set]
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

;;Day 07
;;-------------------------------------------------------------------

(def sample-07
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(def cards (zipmap '(\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A) (range 2 15)))
(def cards-b (zipmap '(\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A) (range 2 15)))

(def type-hierarchy
  {:high-card 1
   :pair 2
   :two-pairs 3
   :three-of-a-kind 4
   :full-house 5
   :four-of-a-kind 6
   :five-of-a-kind 7})

(defn sort-by-vals [hand]
  (let [f (frequencies hand)]
    (into (sorted-map-by (fn [key1 key2]
                           (compare [(get f key2) key2]
                                    [(get f key1) key1])))
          f)))

(defn joke [hand]
  (let [jokers (count (filter #(= \J %) hand))
        f (sort-by-vals (map cards-b (filter #(not= \J %) hand)))]
    ;; (pprint/pprint (str "**********"jokers f))
    (if (seq f)
      (update f (first (keys f)) + jokers)
      {\J jokers})))

(defn hand-type [freq]
  (let [f freq
        v (vals freq)]
    (cond
      (= 1 (count f)) :five-of-a-kind
      (= 2 (count f)) (cond
                        (= 1 (count (filter #(= 4 %) v))) :four-of-a-kind
                        :else :full-house)
      (= 3 (count f)) (cond
                        (= 1 (count (filter #(= 3 %) v))) :three-of-a-kind
                        :else :two-pairs)
      (= 4 (count f)) :pair
      :else :high-card)))

(defn tie-breaker [hand1 hand2]

  (loop [h1 hand1 h2 hand2]
    (let [c1 (first h1)
          c2 (first h2)]
      (if (= c1 c2)
        (recur (rest h1) (rest h2))
        (> c1 c2)))))

(defn strongest [hand1 hand2]
  (let [t1 (hand-type hand1)
        t2 (hand-type hand2)]
    (if (= (type-hierarchy t1) (type-hierarchy t2))
      (tie-breaker (map cards-b hand1) (map cards-b hand2))
      ;; (stronger (sort-by-vals (map cards hand1)) (sort-by-vals (map cards hand2))) ;;real poker rules
      (> (type-hierarchy t1) (type-hierarchy t2)))))

(defn strongest-b [hand1 hand2]
  (let [t1 (hand-type (joke hand1))
        t2 (hand-type (joke hand2))]
    (if (= (type-hierarchy t1) (type-hierarchy t2))
      (tie-breaker (map cards-b hand1) (map cards-b hand2))
      (> (type-hierarchy t1) (type-hierarchy t2)))))

(defn day-07 [input]
  (let [data (s/split input #"\n")]
    (->> data
         (map #(s/split % #" "))
         (into (sorted-map-by strongest-b))
         (vals)
         (interleave (range (count data) 0 -1))
         (partition 2)
         (reduce (fn [r [k bet]]
                   (+ r (* k (parse-long bet)))) 0))))

;; Day 08
;; -------------------------------------------------------------------

(def sample-08
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def sample-08-2
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(def sample-08-b
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b & more]
  (if (empty? more)
    (/ (* a b) (gcd a b))
    (recur (lcm a b) (first more) (rest more))))

(defn path-length [{:keys [start destinations directions lookup r] :or {r 0} :as data}]
  (if (some #(= start %) destinations)
    r
    (let [n (condp = (first directions)
              "L" (first (get lookup start))
              "R" (second (get lookup start)))]
      (recur (assoc data :start n :directions (rest directions) :r (inc r))))))

(defn parse-08 [input]
  (let [[ds _ & rs] (clojure.string/split input #"\n")
        node-map (into {} (map #(let [[b & rst] (->> (re-find #"(\S+) = \((\S+), (\S+)\)" %)
                                                     rest)] [b rst]) rs))
        directions (s/split ds #"")]
    [directions node-map]))

(defn day-08 [[directions node-map] starting-points destinations]
  (for [starting-point starting-points]
    (path-length {:start starting-point :destinations destinations :directions  (cycle directions) :lookup  node-map})))

(defn day-08-b [input]
  (let [[directions node-map] (parse-08 input)
        starting-points (filter #(s/ends-with? % "A") (keys node-map))
        destinations (filter #(s/ends-with? % "Z") (keys node-map))]
    (apply lcm
           (day-08 [directions node-map] starting-points destinations))))

(defn day-08-a [input]
  (first (day-08 (parse-08 input) ["AAA"] ["ZZZ"])))

;; (day-08-a (slurp "resources/day-08.txt"))
;; (pprint/pprint (day-08-b (slurp "resources/day-08.txt")))

;;Day 09
;;-------------------------------------------------------------------

(def sample-09
  "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn parse-09 [input] (->> (s/split input #"\n")
                            (map #(->> (s/split % #" ")
                                       (map parse-long)))))

(defn convergence [a]
  (loop [sq a r (cons (first a) '())]
    (let [diff (->> (take-while #(= 2 (count %)) (partition-all 2 1 sq))
                    (map #(- (second %) (first %))))]
      (if (every? zero? diff)
        r
        (recur diff (cons (first diff) r))))))

(defn backtrack [a]
  (reduce (fn [r n]
            (- n r)) 0 a))

(defn day-09 [input]
  (apply +
         (for [sq (parse-09 (slurp input))]
           (backtrack (convergence sq)))))

;; (day-09 "resources/day-09.txt")
;; (day-09 sample-09)

;;Day 10
;;-------------------------------------------------------------------

(def sample-10-a
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(def sample-10-b
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(def down #{"|" "L" "J"})

(def up #{"|" "F" "7"})

(def left #{"-" "L" "F"})

(def right #{"-" "J" "7"})

(def possible-paths
  {"|" [nil up nil down]
   "-" [right nil left nil]
   "F" [right nil nil down]
   "7" [nil nil left down]
   "J" [nil up left nil]
   "L" [right up nil nil]
   "S" [right up left down]
   "." [nil nil nil nil]})

(defn next-poss [[x y] row-size column-size]
  (map (fn [[x y]]
         (if (and (>= x 0)
                  (< x row-size)
                  (< y column-size)
                  (>= y 0))
           [x y]
           nil))
       [[x (+ y 1)]
        [(- x 1) y]
        [x (- y 1)]
        [(+ x 1) y]]))

(defn valid-paths [a paths]
  (map (fn [[a paths]]
         (if (contains? paths a)
           a
           nil))
       (partition 2 (interleave paths (possible-paths a)))))

(defn get-pos [data pos]
  (if (nil? pos)
    nil
    (nth (nth data (first pos)) (second pos))))

(defn next-index [path data row-size column-size prev-pos]
  (let [current (get-pos data path)
        next-positions (next-poss path row-size column-size)
        valid-paths (valid-paths current (map #(get-pos data %) next-positions))]
    (first (filter #(and (not= prev-pos (first %))
                         (some? (second %))) (zipmap next-positions  valid-paths)))))

(defn area
  "https://en.wikipedia.org/wiki/Shoelace_formula"
  [loop]
  (abs (/ (reduce + (map (fn [[[x1 y1] [x2 y2]]]
                           (- (* x1 y2) (* x2 y1))) (partition 2 1  loop)))
          2)))

(defn interior-points
  "https://en.wikipedia.org/wiki/Pick%27s_theorem"
  [loop]
  (- (+ 1 (area loop)) (/ (- (count loop) 1) 2)))

(defn day-10 [input]
  (let [data (->> (s/split input #"\n")
                  (map #(s/split % #"")))
        row-size (count (first data))
        column-size (count data)
        starting-pos (->> data
                          (map-indexed (fn [i row]
                                         (map-indexed (fn [j c]
                                                        (when (= c "S") [i j])) row)))
                          (apply concat)
                          (filter identity)
                          (first))

        next-positions (next-poss starting-pos row-size column-size)
        some-path  (first (filter #(some? (second %)) (zipmap next-positions (valid-paths (get-pos data starting-pos)
                                                                                          (map #(get-pos data %)
                                                                                               next-positions)))))

        paths (loop [path some-path r [starting-pos (first some-path)] prev-pos starting-pos]
                (if (nil? (second path))
                  r
                  (let [next-idx (next-index (first path) data row-size column-size prev-pos)
                        r (if (some? (first next-idx))
                            (conj r (first next-idx))
                            (conj r starting-pos))]
                    (recur next-idx r (first path)))))]

    (pprint/pprint (interior-points paths))))

;; (day-10 sample-10-a)
;; (day-10 sample-10-b)
;; (day-10 (slurp "resources/day-10.txt"))

;; Day 11
;; -------------------------------------------------------------------

(def sample-11
  "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defn invert-matrix [m]
  (apply mapv vector m))

(def expansion-factor 1000000)

(defn shortest-distances [points]
  (for [[x1 y1] points
        [x2 y2] points

        :let [distance (+ (Math/abs (- x1 x2))
                          (Math/abs (- y1 y2)))]
        :when (not= 0 distance)]
    distance))

(defn expansions [matrix]
  (map (fn [[row column]]
         (letfn [(factor [x]
                   (if (every? #(= "." %) x)
                     expansion-factor
                     1))]
           [(factor row) (factor column)])) matrix))

(defn coordinates [m]
  (let [expns (expansions (partition 2 (interleave m (invert-matrix m))))]
    (->> (map-indexed (fn [i row]
                        (map-indexed (fn [j c]
                                       (if (= "#" c)
                                         [(reduce + (map first (take i expns))) (reduce + (map second (take j  expns)))]
                                         nil)) row)) m)
         (apply concat)
         (filter identity))))

(defn day-11 [input]
  (/
   (->> (s/split input #"\n")
        (map #(s/split % #""))
        (coordinates)
        (shortest-distances)
        (apply +))
   2))

;; (pprint/pprint (day-11 sample-11))
;; (time (day-11 (slurp "resources/day-11.txt")))

;; Day 12
;; -------------------------------------------------------------------

(def sample-12
  "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(def sample-12-a

  ".??..??...?##. 1,1,3")

;; (defn valid-combinations [req-match r]
;;   (let [possible-chars (map #(if (= \? %)
;;                                [\. \#]
;;                                [%]) r)
;;         possible-combs
;;         (map #(apply str %) (loop [pc possible-chars r #{[]}]
;;                               (if (empty? pc)
;;                                 r
;;                                 (let [cs (first pc)
;;                                       r (for [c cs
;;                                               l r]
;;                                           (conj l c))]
;;                                   (recur (rest pc) r)))))]
;;     (pprint/pprint (str "**********" (count valid-combs)))

;;     (count (filter (fn [comb] (= (map count (re-seq #"\#+" comb)) req-match)) possible-combs))))

(def valid-combinations
  (memoize (fn
             [pattern size splits]
             (pprint/pprint pattern)
             (pprint/pprint size)
             (pprint/pprint splits)
             (pprint/pprint (reduce + (rest splits)))
             (pprint/pprint (count (rest splits)))
             (pprint/pprint "**********")
             (let [a (first splits)
                   r (rest splits)
                   after (+ (reduce + r) (count r))
                   cnt
                   (apply + (for [before (range (- (+ size 1) (+ a after)))
                                  :while (or (zero? before) (and  (not= \. (nth pattern (- before 1))) (not= \? (nth pattern (- before 1)))))]
                              (do (pprint/pprint (str a " " before " " after))
                                  (cond
                                    (every? #(or (= \? %) (= \# %)) (subs pattern before (+ before a))) (do (pprint/pprint "here")
                                                                                                            (cond
                                                                                                              (empty? r)
                                                                                                              (if (every? #(or (= \? %) (= \. %)) (subs pattern (+ before a)))
                                                                                                                1
                                                                                                                0)
                                                                                                              (or (= \? (nth pattern (+ before a))) (= \. (nth pattern (+ before a))))
                                                                                                              (valid-combinations (subs pattern (+ before a 1)) (- size (+ before a 1)) r)
                                                                                                              :else 0))
                                    :else 0))))]
               (pprint/pprint pattern)
               (pprint/pprint (str "@@@@@@@ " cnt))
               cnt))))

(defn calculate-valid-combs [lines]
  (for [l lines]
    (let [[m cs] (s/split l #" ")
          ;; m1 (cond
          ;;      (= \. (last m)) (str "?" m)
          ;;      (and (= \# (last m))
          ;;           (= \. (first m))) (str m "?")
          ;;      (and (= \# (last m))
          ;;           (= \? (first m))) (str m "?")
          ;;      (= \# (last m)) (str m ".")
          ;;      :else (str "?" m))
          ;; m (s/join "?" (repeat 2 m) )
          ;; cs (s/join "," (repeat 2 cs))
          req-match (map parse-long (s/split cs #","))]
      (valid-combinations m (count m) req-match)
      ;; (pprint/pprint (str "**********" m1))
      ;; [(valid-combinations req-match m) (valid-combinations req-match m1)]
      )))
;; (pprint/pprint (->> (s/split sample-12-a #"\n")
;;                     ;; (count)
;;                     ;; (take 5)
;;                     (calculate-valid-combs)
;;                     ;; (map #(apply * (conj (repeat 4 (second %)) (first %))))
;;                     ;; (extrapolate)
;;                     ;; (apply map *)

;;                     ;; (apply +)
;;                     ))

;; Day 13
;; -------------------------------------------------------

(def sample-13
  "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn off-by [r1 r2]
  (count (filter identity (apply map #(not= %1 %2)  [r1 r2]))))

(defn reflection-indices [sqs off-factor]
  (->> (map-indexed #(when %2 %1) (map #(= off-factor (off-by (first %) (second %))) (partition 2 1 sqs)))
       (filter identity)))

(defn reflected-rows [rows idx]
  (->> [(reverse  (take (inc idx) rows)) (drop (inc idx) rows)]
       (apply interleave)
       (partition 2)))

(defn reflection-point [rows]
  (let [reflections (reflection-indices rows 0)
        off-by-1s (reflection-indices rows 1)]
    (first (concat (filter (fn [idx]  (let [off-bys (->> (reflected-rows rows idx)
                                                         (map #(off-by (first %) (second %))))]
                                        (= (frequencies off-bys)
                                           {0 (dec (count off-bys)) 1 1}))) reflections)
                   (filter (fn [idx] (->> (reflected-rows rows idx)
                                          rest
                                          (every? #(zero? (off-by (first %) (second %)))))) off-by-1s)))))

(defn parse-input-13 [input]
  (->> (s/split input #"\n\n")
       (map #(s/split % #"\n"))))

(defn patterns [matrices]
  (->> matrices
       (map reflection-point)
       (filter some?)
       (map inc)
       (apply +)))

(defn day-13 [input]
  (let [matrices (parse-input-13 input)]
    (+ (* 100 (patterns matrices))
       (patterns (map invert-matrix matrices)))))

;; (time (day-13 (slurp "resources/day-13.txt")))

;; Day 14
;; -------------------------------------------------------

(def sample-14
  "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(def col-size 100)

(defn transpose [matrix]
  (apply map vector matrix))

(defn load [col]
  (let [_ (count col)]
    (reduce (fn [[r before] [_chunk-idx [chunk-size rr-count]]]
              (let [start-val (- col-size before)]
                [(+ r  (apply + (range start-val (- start-val rr-count) -1)))  (inc (+ before chunk-size))]))
            [0 0] col)))

(defn day-14 [input]
  (->> (s/split input #"\n")
       transpose
       (map #(map-indexed (fn [i chunk]
                            [i [(count chunk) (count (filter (fn [char]
                                                               (= \O char)) chunk))]]) (s/split (apply str %) #"#")))
       (map load)
       (map first)
       (apply +)))

;;Day 15
;;--------------------------------------------------------

(def sample-15
  "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn hash-256 [strng]
  (reduce (fn [r char]
            (mod (* 17 (+ r (int char))) 256)) 0 strng))

(def lens-regex
  #"(?<box>\w+)(?<operation>=|-)(?<focallength>\d*)")

(defn find-and-replace [box [label focal-length]]
  (if (some #(= label (first %)) box)
    (vec (map (fn [b]
                (if (= label (first b))
                  [label focal-length]
                  b)) box))
    (conj box [label focal-length])))

(defn find-and-remove [box label]
  (vec (remove #(= label (first %)) box)))

(defn follow [steps]
  (let [state (reduce (fn [r [label box-idx operation focallength]]
                        (let [new-r  (condp = operation
                                       "=" (update-in r [box-idx]  (fnil find-and-replace []) [label focallength])
                                       "-" (update-in r [box-idx] (fnil find-and-remove []) label))]
                          new-r)) {} steps)]
    state))

(defn day-15 [input]
  (->> (s/split (s/trim-newline input) #",")
       (map #(let [matcher (re-matcher lens-regex %)
                   _              (.matches matcher)
                   box (.group matcher "box")]
               [box (hash-256 box) (.group matcher "operation") (parse-long (.group matcher "focallength"))]))
       follow
       (reduce-kv (fn [r k v]
                    (+ r (* (inc k) (reduce + (map-indexed #(* (inc %1) (second %2)) v))))) 0)))

;; (day-15 sample-15)
;; (day-15 (slurp "resources/day-15.txt"))

;; Day 16
;; -------------------------------------------------------
