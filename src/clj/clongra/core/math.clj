;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

(defn ^double ln
  "Returns the natural logarithm (base e) of x."
  {:inline (fn [x] `(. Math (log ~x)))
   :inline-arities #{1}}

  [x] (. Math (log x)))


(defn ^double lg
  "Returns the logarithm of x to an arbitrary passed base"
  {:inline (fn [base x] `(/ (ln ~x) (ln ~base)))
   :inline-arities #{2}}

  [base x] (/ (ln x) (ln base)))


(defn ^double lg2
  "Returns the logarithm of x to base 2."
  {:inline (fn [x] `(lg 2 ~x))
   :inline-arities #{1}}

  [x] (lg 2.0 x))


(defn ^double lg10
  "Returns the logarithm of x to base 10."
  {:inline (fn [x] `(. Math (log10 ~x)))
   :inline-arities #{1}}

  [x] (. Math (log10 x)))


(defn ^double tanh
  "Returns the hyperbolic tangent of x."
  {:inline (fn [x] `(. Math (tanh ~x)))
   :inline-arities #{1}}

  [x] (. Math (tanh x)))


(defn positive?
  ([x] (when (> x 0) x))

  ([x & ys] (and (positive? x) (every? positive? ys))))


(defn negative?
  ([x] (when (< x 0) x))

  ([x & ys] (and (negative? x) (every? negative? ys))))


(defn non-negative?
  ([x] (when (>= x 0) x))

  ([x & ys] (and (non-negative? x) (every? non-negative? ys))))


;; (defn factors
;;   "Returns a sequence (1 2 3 ... n) or an empty sequence if n < 1."
;;   [n]
;;   (assert (integer? n))
;;   (range 1 (inc n)))

;; (defn factorial
;;   ([multop n]
;;      (assert (non-negative? n))
;;      (reduce multop (factors n)))

;;   ([n] (factorial *' n)))


(defn divisible-by?
  [x n] (zero? (mod x n)))


;; (defn mean
;;   [& args] (/ (apply +' args) (count args)))

(defn square
  ([*op x] (*op x x))

  ([x] (square * x)))

;; (defn heron-sqrt
;;   [x steps]
;;   (letfn [(better [val] (mean val (/ x val)))]

;;     (loop [val (Integer. 1)
;; 	   n steps]
;;       (if (zero? n)
;; 	val
;; 	(recur (better val) (dec n))))))


(defn **
  "The exponentiation operator. Raises base to pow. It's a wrapper
  around @see clojure.contrib.ccmath/expt."
  [base pow] (cmath/expt base pow))


(defn fast-expt
  [a n]
  (assert (>= n 0))
  (loop [a a
	 n (long n)
	 b (Integer. 1)]
    (cond (zero? n) b
	  (even? n) (recur (square a) (long (/ n 2)) b)
	  :else (recur a (dec n) (*' a b)))))


(declare bits)

(defn frankow-expt
  [a n]
  (loop [bits-coll (bits n)
	 a a
	 val (Integer. 1)]
    (cond (empty? bits-coll)
	  val

	  (zero? (first bits-coll))
	  (recur (rest bits-coll) (*' a a) val)

	  :else
	  (recur (rest bits-coll) (*' a a) (*' a val)))))


(defn abs
  "Returns an absolute value of x"
  [x]
  (if (< x 0) (- x) x))


(dyndef *ε* 0.000001)

(defn ^Double ε [] (dynval *ε*))

(defmacro with-ε
  [ε & body]
  `(binding [*ε* (double ~ε)] ~@body))


(defn fixed-point
  "Fixed point operator for f with x0 := start and the close-enough?
  binary predicate for xn and xn-1."
  [f start close-enough?]
  (loop [xn-1 start
         xn   (f start)]
    (if (close-enough? xn xn-1)
      xn

      (recur xn (f xn)))))


(defn Newtons-method
  "Solves the equation f(x) = 0 using Newton's method with x0 := start
  and using the the close-enough? binary predicate for xn and xn-1."
  [f f' start close-enough?]
  (fixed-point (fn [x] (- x (/ (f x) (f' x)))) start close-enough?))


;; (defn root-3
;;   [x]
;;   (Newtons-method (fn [^double y] (- (* y y y) x)) ;; f
;;                   (fn [^double y] (* 2 y y))       ;; f'
;;                   1                      ;; start
;;                   (ε 0.00000001)           ;;close-enough?
;;                   ))


(defmulti ∀
  "A generalized universal quantification operator"
  {:arglists '([& args])}

  multiarg-class-dispatch)


(defmulti ∃
  "A generalized existential quantification operator"
  {:arglists '([& args])}

  multiarg-class-dispatch)


(defmethod ∃ :noarg
  []
  ;; Returns a predicate that checks for for the "existence" of
  ;; the argument, i.e. it's not nil-ness or non-emptiness
  (fn [arg]
    (if (iseqable? arg)
      (seq arg)

      arg)))


(defn congruent?
  "Answers true if a = b (mod n)."
  [^Number a ^Number b ^Number n]
  (let [a (.longValue a)
        b (.longValue b)
        n (.longValue n)]

    (assert (positive? n))
    (zero? (mod (- a b) n))))


(def ^:private KNOWN-PRIMES
  (->> (range 0 1001)
       (map   #(symbol-value (symbol (str "PRIMES-" %)) *ns*))
       (map   var-root)
       (apply concat)))


(def ^:private KNOWN-PRIMES-SET
  (set KNOWN-PRIMES))


(def ^:private LARGEST-KNOWN-PRIME
  (last KNOWN-PRIMES))


(defn known-prime?
  [x]
  (assert (<= x LARGEST-KNOWN-PRIME)
          (str "The value " x " exceeds the maximum known prime. "
               "Unable to tell whether it's a prime or not.") )

  (KNOWN-PRIMES-SET x))


(defn ^:dynamic prime-factors
  "Performs a naive prime factorization of a number."
  [x]
  (if (or (= x 0) (= x 1))
    []

    (let [first-primediv (first (filter #(= 0 (mod x %)) KNOWN-PRIMES))]
      (when-not first-primediv
        (terror "The factorized number" x "lays beyond the range"
                   "of known primes."))

      (conj (prime-factors (/ x first-primediv)) first-primediv))))

(redef-as-dynamic-memo prime-factors)


(defn number-of-divisors
  [x]
  (->> x
       prime-factors
       freqdist
       vals
       (map inc)
       (reduce *)))


;; (defn triangular-numbers
;;   []
;;   (map #(/ (* % (inc %)) 2) (iterate inc 1)))


;; (defn euler-12
;;   []
;;   (->> (triangular-numbers)
;;        (filter #(> (number-of-divisors %) 500))
;;        first
;;        (memoizing [prime-factors])))

(defn N
  "Returns an N (natural numbers) set := 0, 1, 2, ..., Long/MAX_VALUE.
  Optionally allows to specify the start (first) number."
  ([] (N 0))
  ([start]
     (->> start
          (iterate inc))))


(def ^:private N-LEN (inc' Long/MAX_VALUE))

(defn N-eseq
  "Returns an enhanced N version."
  ([] (N-eseq 0))

  ([start]
     (->> (N start)
          (with-len-value N-LEN)
          (with-enth #(do
                        (when (< % 0) (terror IndexOutOfBoundsException %))
                        (+ start %))))))


(defn N'
  "Returns an infinite N (natural numbers) set := 0, 1, 2, ...
  Optionally allows to specify the start (first) number. Unlimited
  integral range."
  ([]      (N' 0))
  ([start] (iterate inc' start)))


(defn N'-eseq
  ([] (N'-eseq 0))

  ([start]
     (->> (N' start)
          infinite
          (with-enth #(do
                        (when (< % 0) (terror IndexOutOfBoundsException %))
                        (+' start %))))))


;; FACTORIALS

(defn factorial
  "Iteratively calculates n!. Does not verify the correctness of n.
  Operates within Long range."
  [n]
  (loop [i n
         result 1]
    (if (zero? i)
      result

      (recur (dec i) (long (* result i))))))

;; cache for n in 0 .. 20, others => overflow
(redef-as-memo factorial)


(defn factorial'
  "Iteratively calculates n!. Does not verify the correctness of n.
  Operates within an arbitrary precision value."
  [n]
  (loop [i n
         result (Long/valueOf 1)] ;; to avoid reflection warnings
    (if (zero? i)
      result

      (recur (dec' i) (*' result i)))))


(defn factorials'
  "Returns an infinite stream 0!, 1!, 2!, 3!, ..."
  []
  (iterate-with *' 1 (N' 1)))


(defn factorials'-eseq
  "An eseq version of factorials'"
  []
  (->> (factorials')
       (with-enth #(factorial' %))
       infinite))


;; FACTORADIC (FACTORIAL NUMBER SYSTEM)

;; (defn decimal-digits
;;   "Returns the decimal digits of n from left to right. The argument is
;;   assumed to be a non-negative integer."
;;   [n]
;;   (map #(Character/getNumericValue ^Character %) (str n)))


(defn factoradic
  "Converts the decimal value n into a factoradic form and returns
  it's digits (from left to right) including the rightmost 0."
  [n]
  (let [place-values (->> (factorials')
                          (take-while #(<= % n))
                          reverse)

        gen (fn [[d r] pv] (pair (quot r pv) (rem r pv)))]

    (->> place-values
         (iterate-with gen (pair nil n))
         rest
         (map pair-first))))


(defn factoradic-to-decimal
  "Takes the digits of the factoradic number (as returned by @see
  factoradic) and returns it's decimal value."
  [digits]
  (reduce +' (map *' (reverse digits) (factorials'))))


;; (defn test-factoradic
;;   [n]
;;   (assert (= n (factoradic-to-decimal (factoradic n)))
;;           (str "Problem for " n)))
