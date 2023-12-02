(ns aoc-2023-clj.day-02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def constraints {:r 12 :g 13 :b 14})

(def demo-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def puzzle-input (slurp (io/reader (io/resource "input_day_02.txt"))))

; I do not want to overthink this and will just first process the text.
; Each line will become a list of color -> numbers.

(def colors-to-char {"blue" :b "green" :g "red" :r})

(defn line-to-data
  "Given string line returns list of color -> numbers of the games"
  [line] (let [games (str/split (second (str/split line #":")) #";")]
           (for [game games]
             (let [numbers (map #(Integer/parseInt %) (re-seq #"\d+" game))
                   colors (map colors-to-char (re-seq #"[a-z]+" game))]
               (into {} (map vector colors numbers))))))

(map line-to-data (take 2 (str/split demo-input #"\n")))
; (({:b 3, :r 4} {:r 1, :g 2, :b 6} {:g 2})
;  ({:b 1, :g 2} {:g 3, :b 4, :r 1} {:g 1, :b 1}))

; We then make sure we track the game number.
(defn text-to-data
  "Main function to extract all data in text into ({id: ({:r 10 ...} ...)}) "
  [text] (into {}
               (map #(vector (inc %1) %2)
                    (range)
                    (map line-to-data (str/split text #"\n")))))

(take 2 (text-to-data demo-input))
; ([1 ({:b 3, :r 4} {:r 1, :g 2, :b 6} {:g 2})]
;  [2 ({:b 1, :g 2} {:g 3, :b 4, :r 1} {:g 1, :b 1})])

; We then have to filter out games that are not possible according to our `constraints`

(defn game-possible?
  "Returns whether a game is possible"
  [[_ draws]]
  (every? identity (for [draw draws]
                     (every? identity
                             (map
                               ; Test whether each color is less than those in constraints.
                              #(<= (second %) (constraints (first %)))
                              draw)))))

(->> demo-input
     text-to-data
     (filter game-possible?)
     (map first)
     (apply +)) ; 8

(->> puzzle-input
     text-to-data
     (filter game-possible?)
     (map first)
     (apply +)) ; 2545

; Part 2: instead of filtering out, we should now figure out least amount necessary.
; Note, we could have flatten the data structure in the previous: no need to check for each draw separately.
; Here we can do that again: maintain "maximum" of each color really.

; So we will compute 
;   - for each game:
;     - the least amount for each draw.
;     - take the max for each.
;     - get power by multiplying this minimum set
;   - add up these numbers

(defn max-of-color-maps
  "Given two sets of colors, returns the max of both"
  [current-max next-draw] (reduce #(update %1 (first %2) max (second %2)) current-max next-draw))

(max-of-color-maps {:g 1 :r 0 :b 4} {:b 3 :r 4}) ; {:g 1, :r 4, :b 4}

(defn minimum-colors
  "Returns map color -> n that is required for a game"
  [[_ draws]] (reduce max-of-color-maps {:g 0 :r 0 :b 0} draws))

(->> demo-input
     text-to-data
     ; Get minimum colors for each game
     (map minimum-colors)
     ; (take 3) ; ({:g 2, :r 4, :b 6} {:g 3, :r 1, :b 4} {:g 13, :r 20, :b 6})
     ; Actually just get the values
     (map #(map second %))
     ; (take 3); ((2 4 6) (3 1 4) (13 20 6))
     ; Multiply these
     (map #(apply * %))
     ; (take 3)  ; (48 12 1560)
     (apply +)
     ) ; 2286

; Now we apply the same to the `puzzle-input`

(->> puzzle-input
     text-to-data
     (map minimum-colors)
     (map #(apply * (map second %)))
     (apply +)
     ) ; 78111
