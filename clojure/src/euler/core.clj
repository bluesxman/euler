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

(for [denom (range 1 11)
      num (iterate inc 10)
      :let [remain (map #(mod num %) denom)
            all-divis? (reduce #(and %1 (not= 0 %2)) true remain)]
      :when all-divis?
      :while (not all-divis?)]
  num)

(range 1 11)

(def remain  (map #(mod 2520 %) (range 1 11)))
(reduce #(and %1 (not= 0 %2)) true remain)

