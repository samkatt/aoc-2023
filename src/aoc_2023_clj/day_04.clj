(ns aoc-2023-clj.day-04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
")

(def puzzle-input (slurp (io/reader (io/resource "input_day_04.txt"))))

; For each number in the second "row" that is also in the first "row", we get a point.
; So we make a set of the first "row", and simply check of membership when we go over the second.

; Let us first create a function that will store everything.
; For each line a dictionary: {:id -> num, :winning -> list of nums, :yours -> list of numbers}

(defn line->data
  "Takes in an input line and returns 
  {:id num, :winning list-of-num, :yours list-of-num}"
  [line]
  (let [[col1 col2] (str/split (second (str/split line #":")) #"\|")]
    {:id (Integer/parseInt (re-find #"\d+" line))
     :winning (map #(Integer/parseInt %) (re-seq #"\d+" col1))
     :yours (map #(Integer/parseInt %) (re-seq #"\d+" col2))}))

(line->data "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
; {:id 1, :winning (41 48 83 86 17), :yours (83 86 6 31 17 9 48 53)}

(defn input->data
  "Takes in a input and returns list of
  {:id num, :winning list-of-num, :yours list-of-num}"
  [input] (map line->data (str/split input #"\n")))

(take 3 (input->data demo-input))
; ({:id 1, :winning (41 48 83 86 17), :yours (83 86 6 31 17 9 48 53)}
;  {:id 2, :winning (13 32 20 16 61), :yours (61 30 68 82 17 32 24 19)}
;  {:id 3, :winning (1 21 53 59 44), :yours (69 82 63 72 16 21 14 1)})

; Then we define a function that will return a list of winning numbers that you have.
; And a function that computes a score.

(defn card->your-winning-numbers
  "Returns your winning numbers"
  [{:keys [winning yours]}]
  (let [winning (set winning)]
    (filter #(contains? (set winning) %) yours)))

(card->your-winning-numbers (first (input->data demo-input))) ; (83 86 17 48)

(defn winning-numbers->score
  "Computes the score given a set of numbers"
  [nums] (let [n (count nums)]
           (case n
             0 0
             1 1
             (apply * (repeat (dec n) 2)))))
(winning-numbers->score  []) ; 0
(winning-numbers->score  [230]) ; 1
(winning-numbers->score  [230 23]) ; 2
(winning-numbers->score  [230 23 234 123]) ; 8

; And now we simply count the score

(->> puzzle-input
     input->data
     (map card->your-winning-numbers)
     (map winning-numbers->score)
     (apply +)) ; 25010

; Part two.

; The number of winning numbers tell us, instead of points, which "copies" you win.
; As in, for card n, if you have 3 winning numbers, then you get additional copy of
; n+1, n+2, and n+3.

; So what it seems we need to do is maintain a map card-num -> copies (vector).
; Then, when we process a new card we:
;   - figure out how many "copies" we gain (i.e. "which")
;   - add "number of times we have copies of card" to each of those copies
; Then, at the end, we go over this map and count the number of copies in total.

; We will do this with ``reduce`` to maintain a list of copies:
;   - start with vector of zeros
;   - we process each card k with c copies:
;     - compute number of copies l
;     - loop over k+1 ... k+l+1 and add k to each

(defn process-card
  "Returns new amount of `copies` given the card"
  [copies {:keys [id] :as card}]
  ; Here we want to figure out how many winning we have and add to copies
  (let [c (nth copies (dec id))
        k (count (card->your-winning-numbers card))]
    (reduce
     #(update %1 %2 (partial + (inc c)))
     copies
     (range id (+ id k)))))

(process-card [0 0 0] {:id 1 :winning [1 2] :yours [1]}) ; [1 1 0]
(process-card [1 0 0] {:id 1 :winning [1 2] :yours [1]}) ; [1 2 0]
(process-card [1 0 0 0] {:id 1 :winning [1 2] :yours [1 1]}) ; [1 2 2 0]

(defn part-2 [input]
  (let [data (input->data input)
        copies (vec (repeat (count data) 0))]
    (map inc (reduce process-card copies data))))

(apply +
       (part-2 demo-input)) ; [0 1 3 7 13 0]
; 30

(apply + (part-2 puzzle-input)) ; 9924412
