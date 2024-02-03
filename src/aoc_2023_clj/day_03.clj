(ns aoc-2023-clj.day-03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def puzzle-input (slurp (io/reader (io/resource "input_day_03.txt"))))

; The task is to add up all the numbers in the grid with an adjacent symbol (not `.`).
; AFAIK we can either
;   1. Go over all numbers and check if they have an adjacent symbol, or
;   2. Go over symbols and see if they have an adjacent number.
;
; It sounds annoying to do either,
; but recovering the number after finding them in (2) sounds terrible.
; So (1) it is.

; Let us do some ground work:
; Put stuff in a data structure:
;   - width & length
;   - vector of vectors for data input
;   - vector of strings (just in case)
; And we will want to first get a map of position (x,y) to number

(defn input->data
  [input] (let [split-input (str/split input #"\n")]
            {:input (vec split-input)
             :size (count (first split-input))
             :data (for [line split-input]
                     (vec line))}))

(def demo-data (input->data demo-input))
(def puzzle-data (input->data puzzle-input))

(defn data->position-number-map
  [data] (apply concat
                (for [[y line] (map-indexed vector (:input data))]
                  (map #(vector [(str/index-of line %) y] %) (re-seq #"\d+" line)))))

(take 3 (data->position-number-map demo-data)) ; ([[7 6] "755"] [[2 2] "35"] [[0 0] "467"])
(data->position-number-map (input->data puzzle-input))

; Now some helper functions:
;   - Check if position is in the grid (`in-grid?`)
;   - Get positions around a number (`positions-around-number`)
;   - See if there is a symbol at a particular positions (`is-symbol?`)

(defn in-grid?
  "Returns whether position ``[x y]`` is on a grid of ``size`` "
  [size [x y]] (and
                (<= 0 x (dec size))
                (<= 0 y (dec size))))

(in-grid? 3 [0 0])   ; true
(in-grid? 3 [1 2])   ; true
(in-grid? 3 [0 -1])  ; false
(in-grid? 3 [3 0])   ; false
(in-grid? 3 [1 4])   ; false

(defn positions-around-number
  [{size :size} pos number]
  (let [x (first pos)
        y (second pos)
        l (count number)]
    (filter (partial in-grid? size)
            ; Generate all the positions: start with left and right of number
            (into [[(dec x) y] [(+ x l) y]]
                  ; Then add up and bottom row
                  (apply concat
                         (for [x (range (dec x) (inc (+ x l)))]
                           [[x (dec y)] [x (inc y)]]))))))

(positions-around-number demo-data [0 0] "467") ; ([3 0] [0 1] [1 1] [2 1] [3 1])

(defn is-symbol?
  [{data :data} [x y]]
  (not (contains? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.} (nth (nth data y) x))))

(is-symbol? demo-data [3 8]) ; true
(is-symbol? demo-data [0 0]) ; false
(is-symbol? demo-data [5 5]) ; true
(is-symbol? demo-data [3 1]) ; true
(is-symbol? demo-data [1 3]) ; false

(defn has-neighbouring-symbol?
  [data [pos num]]
  (some true? (map
               #(is-symbol? data %)
               (positions-around-number data pos num))))

; So now, given a way to get all positions around and test for symbol, we can do:
;   1. Find all numbers, for each number
;     1. Find their neighboring positions
;     2. See if any of them is a symbol
;   3. Filter number based on that

(defn part-1
  [input]
  (let [data (input->data input)
        position->number (data->position-number-map data)]
    (->> position->number
         (filter (partial has-neighbouring-symbol? data))
         (map #(Integer/parseInt (second %)))
         (apply +)
         )))

(part-1 demo-input) ; 4361
(def nums (apply + (part-1 puzzle-input))) ; 513782

(let [n 499]
  (count (filter (partial = n) nums)))
