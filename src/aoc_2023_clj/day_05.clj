(ns aoc-2023-clj.day-05
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def demo-input "seeds: 79 14 55 13

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

(def puzzle-input (slurp (io/reader (io/resource "input_day_05.txt"))))

; The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.

; The rest of the almanac contains a list of maps which describe how to convert numbers from a source category into numbers in a destination category.
; That is, the section that starts with seed-to-soil map: describes how to convert a seed number (the source) to a soil number (the destination).
; This lets the gardener and his team know which soil to use with which seeds, which water to use with which fertilizer, and so on.

; Rather than list every source number and its corresponding destination number one by one, the maps describe entire ranges of numbers that can be converted.
; Each line within a map contains three numbers: the destination range start, the source range start, and the range length.

; Consider again the example seed-to-soil map:

; 50 98 2
; 52 50 48

; The first line has a destination range start of 50, a source range start of 98, and a range length of 2.
; This line means that the source range starts at 98 and contains two values: 98 and 99.
; The destination range is the same length, but it starts at 50, so its two values are 50 and 51.
; With this information, you know that seed number 98 corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.

; The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97.
; This corresponds to a destination range starting at 52 and also containing 48 values: 52, 53, ..., 98, 99.
; So, seed number 53 corresponds to soil number 55.

; Any source numbers that aren't mapped correspond to the same destination number.
; So, seed number 10 corresponds to soil number 10.

; So, the entire list of seed numbers and their corresponding soil numbers looks like this:

; seed  soil
; 0     0
; 1     1
; ...   ...
; 48    48
; 49    49
; 50    52
; 51    53
; ...   ...
; 96    98
; 97    99
; 98    50
; 99    51

; With this map, you can look up the soil number required for each initial seed number:

;     Seed number 79 corresponds to soil number 81.
;     Seed number 14 corresponds to soil number 14.
;     Seed number 55 corresponds to soil number 57.
;     Seed number 13 corresponds to soil number 13.

; The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed.
; Using these maps, find the lowest location number that corresponds to any of the initial seeds.
; To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number.
; In this example, the corresponding types are:

;     Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
;     Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
;     Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
;     Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.

; So, the lowest location number in this example is 35.

; What is the lowest location number that corresponds to any of the initial seed numbers?

; Part 1
; Let us first parse the input

(defn parse-mapping
  "Parse string representation of a mapping onto {:src :dest :len}"
  [mappings]
  (->> mappings
       (map #(str/split % #" "))
       (map #(into {} {:dest (edn/read-string (first %))
                       :src (edn/read-string (second %))
                       :len (edn/read-string (last %))}))))

(parse-mapping ["0 15 37"
                "37 52 2"]) ; ({:dest 0, :src 15, :len 37} {:dest 37, :src 52, :len 2})

(defn parse-input
  "Creates data structure out of string inputs"
  [input]
  (let [line-by-line (str/split input #"\n")
        paragraphs (drop 1 (str/split input #"\n\n"))]
    (into {:seeds (map (partial edn/read-string) (re-seq #"\d+" (first line-by-line)))}
          (for [p paragraphs]
            {(keyword (first (str/split p #" ")))
             (parse-mapping (drop 1 (str/split p #"\n")))}))))

; (parse-input demo-input)

; Given that we can parse the input we need to map from source to destination

(defn next-requirement
  [requirement mappings]
  (let [applicable-mappings (filter #(<= (:src %) requirement (+ (:src %) (:len %))) mappings)]
    (if (seq applicable-mappings)
      (let [{:keys [src dest]} (first applicable-mappings)]
        (+ dest (- requirement src)))
      requirement)))

(let [input (parse-input demo-input)]
  (next-requirement (first (:seeds input)) (:seed-to-soil input))) ; 81

(defn seed->categories [input seed]
  (reduce
   (fn [seed-requirements mapping-key]
     ; TODO: call stuff here
     (conj seed-requirements (next-requirement (last seed-requirements) (mapping-key input))))
   [seed]
   (drop 1 (keys input))))

(let [input (parse-input demo-input)]
  (keys input)) ; [79 81 81 81 74 78 78 82]
  ; (seed->categories input (first (:seeds input)))) ; [79 81 81 81 74 78 78 82]

(defn part-1 [input]
  (let [input-data (parse-input input)]
    (->> input-data
         :seeds
         (map #(seed->categories input-data %))
         (map last)
         (apply min)))) ; (82 86 86 35)
; ([79 81 81 81 74 78 78 82]
;  [14 14 53 53 46 82 82 86]
;  [55 57 57 53 46 82 82 86]
;  [13 13 52 41 34 34 35 35])

(part-1 demo-input) ; 35
(part-1 puzzle-input) ; 313045984

; Part 2

; Everyone will starve if you only plant such a small number of seeds.
; Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.

; The values on the initial seeds: line come in pairs.
; Within each pair, the first value is the start of the range and the second value is the length of the range.
; So, in the first line of the example above:

; seeds: 79 14 55 13

; This line describes two ranges of seed numbers to be planted in the garden.
; The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92.
; The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.

; Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.

; In the above example, the lowest location number can be obtained from seed number 82, which corresponds to
; soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46.
; So, the lowest location number is 46.

; Consider all of the initial seed numbers listed in the ranges on the first line of the almanac.
; What is the lowest location number that corresponds to any of the initial seed numbers?

(parse-input demo-input)

(defn part-2 [input]
  (let [input-data (parse-input input)
        seed-ranges (partition 2 2 (:seeds input-data))
        seeds (apply concat (map (fn [[s l]] (range s (+ s l))) seed-ranges))]
    (->> seeds
         (map #(seed->categories input-data %))
         (map last)
         (apply min))))

(part-2 demo-input) ; 46
(part-2 puzzle-input)

(let [input-data (parse-input puzzle-input)
      seed-ranges (partition 2 2 (:seeds input-data))
      seeds (apply concat (map (fn [[s l]] (range s (+ s l))) seed-ranges))]
  (->> seeds
       (map #(last (seed->categories input-data %)))
       (apply min)))
