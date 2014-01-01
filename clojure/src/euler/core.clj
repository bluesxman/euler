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
      (recur (drop 1 digits) (max greatest product)))
      ))


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

