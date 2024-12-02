(def grammar
  '{:main (sequence (<- :d+) :s+ (<- :d+))})

(defn get-columns-in-file
  "Returns a tuple of the two columns of numbers as arrays."
  [f]
  (def [left right] [@[] @[]])
  (each line (file/lines f)
      (def [l r] (peg/match grammar line))
      (array/push left (scan-number l))
      (array/push right (scan-number r)))
  [left right])

(defn get-column-sums
  "Given two columns of numbers, returns the sum of the distances of each pair."
  [[left-column right-column]]
  (var total-sum 0)
  (for i 0 (length left-column)
    (def [left right] [(i left-column) (i right-column)])
    (+= total-sum (math/abs (- left right))))
  total-sum)

(defn get-column-similarity-score
  "Given two columns of numbers, calculates their similarity score."
  [[left-column right-column]]
  (var total-score 0)
  (for i 0 (length left-column)
    (def current (i left-column))
    (def score (count (fn (x) (= x current)) right-column))
    (+= total-score (* current score)))
  total-score)

(with [f (file/open "1.input.txt")] 
  (def [left right] (get-columns-in-file f))
  (def [left-sorted right-sorted] [(sorted left) (sorted right)])
  (def total-distance (get-column-sums [left-sorted right-sorted]))
  (def total-similarity-score (get-column-similarity-score [left-sorted right-sorted]))
  (print "The Total Distance is: " total-distance)
  (print "The Similarity Score is: " total-similarity-score))
