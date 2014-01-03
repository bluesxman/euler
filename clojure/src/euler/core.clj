(ns euler.core)

;; See http://projecteuler.net/problems


;; Problem 1 - sum of multiples of 5 and 3 up to but not including 1000
(apply + (filter #(or (zero? (mod % 3)) (zero? (mod % 5))) (range 1000)))


;; Problem 2 - sum even fibs before and including 4 million
(loop [x 1
       y 2
       sum 2]
  (if (>= y 4e6)
    sum
    (let [next-fib (+ x y)]
      (recur y next-fib (if (even? next-fib) (+ sum next-fib) sum)))
    ))

(apply + (filter even? '(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)))


;; Problem 3 - find largest prime factor of 600851475143
;; Prime number seq from clojure docs:
;; http://clojuredocs.org/clojure_core/clojure.core/lazy-seq#example_1000
(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

(loop [number 600851475143
       factors '() ;; no need for a list but I wanted to see all the factors
       primes (sieve (iterate inc 2))]
  (if (= 1 number)
    factors
    (let [f (first primes)
          result (/ number f)
          remain (mod number f)]
      (if (zero? remain)
        (recur result (conj factors f) primes)
        (recur number factors (rest primes))))))



;; Problem 4 - largest palindrome made from the product of two 3-digit numbers
;; This was the solution on "Test-Driving Clojure in Light Table", if I remember right
(->>
 (for [x (range 100 1000)
       y (range 100 1000)
       :let [s (str (* x y))]
       :when (= s (clojure.string/reverse s))]
   (* x y))
 (apply max))


;; Problem 5 - Smallest number that can be evenly divided by all numbers from 1 - 20
(loop [num-seq (iterate #(+ % 10) 20)] ;; has to end in a zero
  (let [remain (map #(mod (first num-seq) %) (range 1 21))
        all-divis? (reduce #(and %1 (zero? %2)) true remain)]
    (if all-divis?
      (first num-seq)
      (recur (rest num-seq)))))

(map #(mod 232792560 %) (range 1 21))


;; Problem 6 - difference between the square of the sums and sum of the squares of the
;; first 100 natural numbers
(defn sum-of-square
  [x y]
  (reduce #(+ (* %2 %2) %1) (range x (inc y))))

(sum-of-square 1 10)

(defn square-of-sum
  [x y]
  (let [s (reduce + (range x (inc y)))]
    (* s s)))

(square-of-sum 1 10)


(- (square-of-sum 1 100) (sum-of-square 1 100))


;; Problem 7 - 10001st prime
(defn- prime?
  "Primes contains all primes that could possibly divide into n"
  [n primes]
  (loop [i 0]
    (cond
     (>= i (count primes)) true
     (= (primes i) n) true
     (zero? (mod n (primes i))) false
     :else (recur (inc i)))))

(prime? 5 [2])

(defn gen-prime
  [nth-prime]
  (loop [primes []
         i 2]
    (if (= (count primes) nth-prime)
      (primes (dec nth-prime))
      (if (prime? i primes)
        (recur (conj primes i) (inc i))
        (recur primes (inc i))))))

(gen-prime 10001)


;; Problem 8 - Greatest product of 5 consecutive digits
(def k-digits
  "3167176531330624919225119674426574742355349194934
  96983520312774506326239578318016984801869478851843
  85861560789112949495459501737958331952853208805511
  12540698747158523863050715693290963295227443043557
  66896648950445244523161731856403098711121722383113
  62229893423380308135336276614282806444486645238749
  30358907296290491560440772390713810515859307960866
  70172427121883998797908792274921901699720888093776
  65727333001053367881220235421809751254540594752243
  52584907711670556013604839586446706324415722155397
  53697817977846174064955149290862569321978468622482
  83972241375657056057490261407972968652414535100474
  82166370484403199890008895243450658541227588666881
  16427171479924442928230863465674813919123162824586
  17866458359124566529476545682848912883142607690042
  24219022671055626321111109370544217506941658960408
  07198403850962455444362981230987879927244284909188
  84580156166097919133875499200524063689912560717606
  05886116467109405077541002256983155200055935729725
  71636269561882670428252483600823257530420752963450")

(use 'clojure.string)

(loop [digits (map read-string (filter #(not (blank? %)) (map str (seq k-digits))))
       greatest 0]
  (if (< (count digits) 5)
    greatest
    (let [product (apply * (take 5 digits))]
      (recur (drop 1 digits) (max greatest product)))))


;; Problem 9 - Product of pythagorean triplet whose sum equals 1000
;; Inefficient but wanted to play with list comprehensions
(for [a (range 1 1000)
      b (range 1 1000)
      c (range 1 1000)
      :when (and
             (< a b c)
             (= (+ (* a a) (* b b)) (* c c))
             (= 1000 (+ a b c)))]
  (* a b c))


;; Problem 10 - Sum all primes below 2 million

;; Based on Michelle O'Reilly's "The Genuine Sieve of Eratosthenes"
;; http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;; Using a tree (insead of a heap) as on page 6 since clojure trees are log32(n) (i.e. fast) and they're default clojure
;; A priority queue for clojure is here: https://github.com/clojure/data.priority-map
;; No wheel optimization, just skipping even candidates

;; 1) Get the list of primes at n.
;; 2) From that list create a new list of keys by adding each prime to n.
;; 3) For each new key, take the list in the table at that location (if it exists) and append the prime to the list.
;; 4) Remove n from the table...
;; 5) and add the new key-value pairs to the table.
(defn- update-composites
  [table n]
  (let [primes (table n)
        iter #(+ n (* 2 %))
        new-keys (map iter primes)
        new-vals (map #(conj (table (iter %)) %) primes)]
    (apply assoc (dissoc table n) (interleave new-keys new-vals))))

;; 1) Take the next candidate from the sequence.
;; 2) If it's in the table then its a composite.
;; 3) If it's a composite then skip it, update the table, and recurse.
;; 4) Else it's a prime so add it to the sequence, add the prime to the table, and recurse.
;; 5) The first possible composite of the prime is its square so use this as the key.
;; 6) The value in the table is a list containing the prime.  Since eventually "iterators"
;; for primes can resolve to the same key in the table, >1 prime can be at the same
;; key in the table.  Therefore values in the table are lists of primes.
(defn- inc-sieve
  [num-seq table]
  (let [n (first num-seq)]
    (if (contains? table n)
      (inc-sieve (rest num-seq) (update-composites table n))
      (cons n (lazy-seq (inc-sieve (rest num-seq) (assoc table (* n n) (list n))))))))

(defn prime-seq
  "Creates infinite sequence of primes using incremental functional sieve."
  []
  (cons 2 (inc-sieve (iterate #(+ 2 %) 3) {})))

(time (apply + (take-while #(< % 2e6) (prime-seq))))


;; Problem 11 - Largest product of 4 in same direction
(def string-grid-20
 "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
  81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
  52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
  22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
  24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
  32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
  67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
  24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
  21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
  78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
  16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
  86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
  19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
  04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
  88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
  04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
  20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
  20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
  01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(def grid-20
  "create a vector of vectors, row-oriented"
  (loop [grid []
         num-seq (map #(. Integer parseInt %) (re-seq #"\w+" string-grid-20))]
    (if (empty? num-seq)
      grid
      (recur (conj grid (vec (take 20 num-seq))) (drop 20 num-seq)))))

(defn left-indexer [[r c]] (vector r (inc c)))
(defn down-indexer [[r c]] (vector (inc r) c))
(defn rdiag-indexer [[r c]] (vector (inc r) (inc c)))
(defn ldiag-indexer [[r c]] (vector (inc r) (dec c)))

(defn grid-vals
  "Creates a sequence of values from a part of the grid. row and col determine
  the starting point.  n is size of the sequence of values.  indexer is a traversal
  function that returns the next point in the grid from a given row and column."
  [grid row col n indexer]
  (let [idx-seq (iterate indexer [row col])
        get-val (fn [[r c]] ((grid r) c))
        num-seq (map get-val idx-seq)]
    (take n num-seq)))

(defn max-directional
  [grid n]
  (let [max-row (count grid)
        max-col (count (grid 0))
        directions [left-indexer down-indexer rdiag-indexer ldiag-indexer]]
    (for [row (range max-row)
          col (range max-col)
          dir (range (count directions))
          :when (case dir
                  0 (<= col (- max-col n))  ;; left
                  1 (<= row (- max-row n))  ;; down
                  2 (and (<= col (- max-col n)) (<= row (- max-row n)))  ;; right diagonal
                  3 (and (>= col (dec n)) (<= row (- max-row n))))]  ;; left diagonal
      (apply * (grid-vals grid row col n (directions dir))))))

(apply max (max-directional grid-20 4))


;; Problem 12 - First triangle number to have over 500 divisors
;; needs problem 10 for primes
(defn triangle-numbers []
  (->>
   (iterate (fn [[n sum]] [(inc n) (+ n sum)]) [1 0])
   (drop 1)
   (map #(% 1))))

(defn factors [n]
  (loop [facts []
         primes (prime-seq)
         remain n]
   (if (= remain 1)
     facts
     (let [p (first primes)
           r (mod remain p)]
       (if (zero? r)
         (recur (conj facts p) primes (/ remain p))
         (recur facts (rest primes) remain))))))

;; using algorithm from:  http://www.wikihow.com/Determine-the-Number-of-Divisors-of-an-Integer
(defn count-divisors [n]
  (->>
   (partition-by identity (factors n))
   (map count)
   (map inc)
   (apply *)))

(time (first (drop-while #(< (count-divisors %) 500) (triangle-numbers))))
