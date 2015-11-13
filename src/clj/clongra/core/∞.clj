;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-04-02

(in-ns 'clongra.core)

;; CONSTANTS

(def +∞ jclongra.math.Infinity/POSITIVE)
(def -∞ jclongra.math.Infinity/NEGATIVE)

;; OPERATORS

(defn +∞?
  [x]
  (cond (identical? x +∞)
        true

        (instance? Double x)
        (= Double/POSITIVE_INFINITY (.doubleValue ^Double x))

        (instance? Float x)
        (= Float/POSITIVE_INFINITY (.floatValue ^Float x))

        :else false))


(defn -∞?
  [x]
  (cond (identical? x -∞)
        true

        (instance? Double x)
        (= Double/NEGATIVE_INFINITY (.doubleValue ^Double x))

        (instance? Float x)
        (= Float/NEGATIVE_INFINITY (.floatValue ^Float x))

        :else false))


(defn ∞?
  [x]
  (or (+∞? x) (-∞? x)))


;; ARITHMETIC OPS

(defn ∞+
  "An ∞ aware + operator."
  ([]  (+))
  ([x] (+ x))
  ([x y]
     (cond (+∞? x)
           (if (-∞? y)
             (terror ArithmeticException "Can't (+ +∞ -∞)")
             +∞)

           (-∞? x)
           (if (+∞? y)
             (terror ArithmeticException "Can't (+ -∞ +∞)")
             -∞)

           (+∞? y) +∞
           (-∞? y) -∞
           :else (+ x y)))

  ([x y & more]
     (reduce ∞+ (∞+ x y) more)))


(defn ∞+'
  "An ∞ aware +' operator."
  ([]  (+'))
  ([x] (+' x))
  ([x y]
     (cond (+∞? x)
           (if (-∞? y)
             (terror ArithmeticException "Can't (+' +∞ -∞)")
             +∞)

           (-∞? x)
           (if (+∞? y)
             (terror ArithmeticException "Can't (+' -∞ +∞)")
             -∞)

           (+∞? y) +∞
           (-∞? y) -∞
           :else (+' x y)))

  ([x y & more]
     (reduce ∞+' (∞+' x y) more)))


(defn ∞-
  "An ∞ aware - operator."
  ([x]
     (cond (+∞? x) -∞
           (-∞? x) +∞
           :else  (- x)))

  ([x y]
     (cond (+∞? x)
           (if (+∞? y)
             (terror ArithmeticException "Can't (- +∞ +∞)")
             +∞)

           (-∞? x)
           (if (-∞? y)
             (terror ArithmeticException "Can't (- -∞ -∞)")
             -∞)

           (+∞? y) -∞
           (-∞? y) +∞
           :else (- x y)))

  ([x y & more]
     (reduce ∞- (∞- x y) more)))


(defn ∞-'
  "An ∞ aware -' operator."
  ([x]
     (cond (+∞? x) -∞
           (-∞? x) +∞
           :else  (-' x)))

  ([x y]
     (cond (+∞? x)
           (if (+∞? y)
             (terror ArithmeticException "Can't (-' +∞ +∞)")
             +∞)

           (-∞? x)
           (if (-∞? y)
             (terror ArithmeticException "Can't (-' -∞ -∞)")
             -∞)

           (+∞? y) -∞
           (-∞? y) +∞
           :else (-' x y)))

  ([x y & more]
     (reduce ∞-' (∞-' x y) more)))


(defn ∞*
  "An ∞ aware * operator."
  ([]  (*))
  ([x] (* x))
  ([x y]
     (cond (+∞? x)
           (cond (+∞?   y) +∞
                 (-∞?   y) -∞
                 (pos?  y) +∞
                 (neg?  y) -∞
                 (zero? y) 0
                 :else     (terror IllegalArgumentException y))

           (-∞? x)
           (cond (+∞?   y) -∞
                 (-∞?   y) +∞
                 (pos?  y) -∞
                 (neg?  y) +∞
                 (zero? y) 0
                 :else     (terror IllegalArgumentException y))

           (pos? x)
           (cond (+∞? y) +∞
                 (-∞? y) -∞
                 :else   (* x y))

           (neg? x)
           (cond (+∞? y) -∞
                 (-∞? y) +∞
                 :else   (* x y))

           (zero? x) 0

           :else (terror IllegalArgumentException x)))

  ([x y & more]
     (reduce ∞* (∞* x y) more)))


(defn ∞*'
  "An ∞ aware *' operator."
  ([]  (*'))
  ([x] (*' x))
  ([x y]
     (cond (+∞? x)
           (cond (+∞?   y) +∞
                 (-∞?   y) -∞
                 (pos?  y) +∞
                 (neg?  y) -∞
                 (zero? y) 0
                 :else     (terror IllegalArgumentException y))

           (-∞? x)
           (cond (+∞?   y) -∞
                 (-∞?   y) +∞
                 (pos?  y) -∞
                 (neg?  y) +∞
                 (zero? y) 0
                 :else     (terror IllegalArgumentException y))

           (pos? x)
           (cond (+∞? y) +∞
                 (-∞? y) -∞
                 :else   (*' x y))

           (neg? x)
           (cond (+∞? y) -∞
                 (-∞? y) +∞
                 :else   (*' x y))

           (zero? x) 0

           :else (terror IllegalArgumentException x)))

  ([x y & more]
     (reduce ∞*' (∞*' x y) more)))


(defn ∞div
  "An ∞ aware / operator."
  ([x] (if (∞? x) 0 (/ x)))

  ([x y]
     (cond (+∞? x)
           (cond (+∞?   y) (terror ArithmeticException "Can't (/ +∞ +∞)")
                 (-∞?   y) (terror ArithmeticException "Can't (/ +∞ -∞)")
                 (pos?  y) +∞
                 (neg?  y) -∞
                 (zero? y) (terror ArithmeticException "Can't (/ +∞ 0)")
                 :else     (terror IllegalArgumentException y))

           (-∞? x)
           (cond (+∞?   y) (terror ArithmeticException "Can't (/ -∞ +∞)")
                 (-∞?   y) (terror ArithmeticException "Can't (/ -∞ -∞)")
                 (pos?  y) -∞
                 (neg?  y) +∞
                 (zero? y) (terror ArithmeticException "Can't (/ -∞ 0)")
                 :else     (terror IllegalArgumentException y))

           (∞? y) 0
           :else (/ x y)))

  ([x y & more]
     (reduce ∞div (∞div x y) more)))


(defn ∞zero?
  "∞ aware zero? operator."
  [x]
  (and (not (∞? x)) (zero? x)))


(defn ∞mod
  "∞ aware mod operator. Works according to the following rules:
  0 mod (L+ | L- | +∞ | -∞) = 0

  L+ mod +∞ = L+
  L+ mod -∞ = -∞

  L- mod +∞ = +∞
  L- mod -∞ = L-

  (+∞ | -∞) mod (L+ | L- | +∞ | -∞) → ERROR"
  [num div]
  (cond
    (+∞? num) (terror ArithmeticException "Can't (∞mod +∞ ...)")
    (-∞? num) (terror ArithmeticException "Can't (∞mod -∞ ...)")

    (zero? num) 0

    (pos? num)
    (cond (+∞? div) num
          (-∞? div) -∞
          :else (mod num div))

    (neg? num)
    (cond (+∞? div) +∞
          (-∞? div) num
          :else (mod num div))

    :else (terror IllegalArgumentException num)))


;; RELATIONAL OPS

(defn ∞=
  ([x] true)
  ([x y]
     (cond (+∞? x) (+∞? y)
           (-∞? x) (-∞? y)
           :else (= x y)))

  ([x y & more]
     (if (∞= x y)
       (if (next more)
         (recur y (first more) (next more))
         (∞= y (first more)))
       false)))


(defn ∞not=
  ([x] false)
  ([x y] (not (∞= x y)))
  ([x y & more] (not (apply ∞= x y more))))


(defn ∞<
  ([x] true)
  ([x y]
     (cond (-∞? x) (not (-∞? y))
           (+∞? x) false
           (-∞? y) false
           (+∞? y) true
           :else (< x y)))

  ([x y & more]
     (if (∞< x y)
       (if (next more)
         (recur y (first more) (next more))
         (∞< y (first more)))
       false)))


(defn ∞<=
  ([x] true)
  ([x y]
     (cond (-∞? x) true
           (+∞? x) (+∞? y)
           (-∞? y) false
           (+∞? y) true
           :else (<= x y)))

  ([x y & more]
     (if (∞<= x y)
       (if (next more)
         (recur y (first more) (next more))
         (∞<= y (first more)))
       false)))


(defn ∞>
  ([x]   true)
  ([x y] (∞< y x))
  ([x y & more]
     (if (∞> x y)
       (if (next more)
         (recur y (first more) (next more))
         (∞> y (first more)))
       false)))


(defn ∞>=
  ([x]   true)
  ([x y] (∞<= y x))
  ([x y & more]
     (if (∞>= x y)
       (if (next more)
         (recur y (first more) (next more))
         (∞>= y (first more)))
       false)))


(defn ∞min
  ([x] x)
  ([x y]
     (cond (-∞? x) -∞
           (+∞? x) y
           (-∞? y) -∞
           (+∞? y) x
           :else (min x y)))

  ([x y & more]
     (reduce ∞min (∞min x y) more)))


(defn ∞max
  ([x] x)
  ([x y]
     (cond (-∞? x) y
           (+∞? x) +∞
           (-∞? y) x
           (+∞? y) +∞
           :else (max x y)))

  ([x y & more]
     (reduce ∞max (∞max x y) more)))
