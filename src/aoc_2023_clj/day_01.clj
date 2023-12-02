(ns aoc-2023-clj.day-01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def puzzle-input (slurp (io/reader (io/resource "input_day_01.txt"))))

; We are to
;   - grab the first and last number in each line (e.g. "1 " and "2" in "1abc2")
;   - concatenate those (e.g. "12")
;   - Add the resulting numbers

(defn puzzle1 [input]
  (apply +
         (for [line (str/split input #"\n")]
           (let [numbers (re-seq #"\d" line)]
             (Integer/parseInt (str (first numbers) (last numbers)))))))

(puzzle1 demo-input) ; 142
(puzzle1 puzzle-input) ; 52974

; Part 2
; Apparently some numbers are written out.
; So we are going to replace these written words with their number, then apply `puzzle1`.


(def demo-input-2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
")

(def replacement-map
  {#"one" "1" #"two" "2" #"three" "3" #"four" "4" #"five" "5" #"six" "6" #"seven" "7" #"eight" "8" #"nine" "9"})

(defn replace-words-with-their-numbers
  "Replaces all word 'numbers' in the string with their actual number"
  [string replacements] (reduce #(str/replace %1 (first %2) (second %2)) string replacements))

; Awww, after trying it turns out this is not okay:
(replace-words-with-their-numbers "eightwothree" replacement-map) ; "8wo3"

; So, instead, we first need to find them all with regexes:
(def match-all-numbers #"\d|one|two|three|four|five|six|seven|eight|nine")

; That does not work either....
(re-seq match-all-numbers "eighthree") ; ("eight")

; So let us be dumb and just replace them such that it neighboring numbers still exist

(def replacement-with-remaining-letters-map
  {#"one" "o1e" #"two" "t2o" #"three" "t3e" #"four" "4" #"five" "5e" #"six" "6" #"seven" "7n" #"eight" "e8t" #"nine" "n9e"})

(replace-words-with-their-numbers "eightwothree" replacement-with-remaining-letters-map) ; "e8t2ot3e"


(let [input-with-replaced-numbers (replace-words-with-their-numbers demo-input-2 replacement-with-remaining-letters-map)]
      (puzzle1 input-with-replaced-numbers)) ; 281

(let [input-with-replaced-numbers (replace-words-with-their-numbers puzzle-input replacement-with-remaining-letters-map)]
      (puzzle1 input-with-replaced-numbers))
; 53340


