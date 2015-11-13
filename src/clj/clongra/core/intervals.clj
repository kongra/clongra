;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-04-14

(in-ns 'clongra.core)

;; INTERVALS REPRESENTATION

(defrecord ^:private Interval [start end]
  java.lang.Object
  (toString [this] (str "[" start ", " end "]")))

;; (def-print-dup clongra.core.Interval clojure.core/str)

;; CREATION

(defn interv
  "Creates a new interval [start, end]. start must be ∞<= end (with
  respect to infinity). When called with one argument, converts into
  an interval."
  ([start end]
     (let [intv (Interval. start end)]
       (when-not (∞<= start end)
         (terror IllegalArgumentException "Malformed interval" intv))

       intv))

  ([x]
     (cond (instance? Interval x) x
           (sequential? x) (let [[start end] x] (interv start end))
           :else (Interval. x x))))


(defn intervalue
  "Creates an interval around x with the +/-tolerance."
  [x tolerance]
  (Interval. (- x tolerance) (+ x tolerance)))


(defn intervalue'
  "Works like intervalue but uses +' an -' instead + and -"
  [x tolerance]
  (Interval. (-' x tolerance) (+' x tolerance)))


(defn consecutive-intervs
  "For x1, x2, x3, ... returns a sequence of [x1, x2], [x2, x3], ..."
  [& xs]
  (->> xs
       (partition 2 1)
       (map interv)))


(declare iterate-with)

(defn cummulative-intervs
  "For x1, x2, x3, ... returns [x1, x1+x2], [x1+x2, x1+x2+x3], ..."
  [& xs]
  (->> xs
       (iterate-with ∞+ 0)
       next
       (apply consecutive-intervs)))


(defn cummulative-intervs'
  "Works like cummulative-intervs bu uses ∞+' instead of ∞+"
  [& xs]
  (->> xs
       (iterate-with ∞+' 0)
       next
       (apply consecutive-intervs)))


;; POSSIBLE ENDPOINT MODES (ENDPS)

(defl "[, ]" (pair ∞<= ∞<=))
(defl "[, )" (pair ∞<= ∞<))
(defl "(, ]" (pair ∞< ∞<=))
(defl "(, )" (pair ∞< ∞<))


;; INTERVAL MEMBERSHIP

(defn ^Long interv-compare
  "Returns:
  * -1 when x lays to the left  of the intv
  *  1 when x lays to the right of the intv
  *  0 when x is within the intv.

  Endps may be one of:
  * (lv \"[, ]\") - default
  * (lv \"[, )\")
  * (lv \"(, ]\")
  * (lv \"(, )\")"
  ([endps intv x]
     (let [intv (interv intv)
           op1 (pair-first endps)
           op2 (pair-second endps)]

       (cond (not (op1 (:start intv) x)) -1
             (not (op2 x (:end intv)))    1
             :else                        0)))

  ([intv x]
     (interv-compare (lv "[, ]") intv x)))


(defn within-interv?
  "Tests whether x belongs to the interval."
  ([endps intv x]
     (zero? (.longValue (interv-compare endps intv x))))

  ([intv x]
     (zero? (.longValue (interv-compare intv x)))))


;; ADDITION

(defn interv+
  ([]  (interv 0))
  ([x] (interv x))
  ([x y]
     (let [x (interv x)
           y (interv y)

           x1 (:start x)
           x2 (:end   x)
           y1 (:start y)
           y2 (:end   y)]

       (Interval. (∞+ x1 y1) (∞+ x2 y2))))

  ([x y & more]
     (reduce interv+ (interv+ x y) more)))


(defn interv+'
  ([]  (interv 0))
  ([x] (interv x))
  ([x y]
     (let [x (interv x)
           y (interv y)

           x1 (:start x)
           x2 (:end   x)
           y1 (:start y)
           y2 (:end   y)]

       (Interval. (∞+' x1 y1) (∞+' x2 y2))))

  ([x y & more]
     (reduce interv+' (interv+' x y) more)))


;; SUBTRACTION

(defn interv-
  ([x]
     (let [x  (interv x)
           x1 (:start x)
           x2 (:end   x)]
       (Interval. (∞- x2) (∞- x1))))

  ([x y]
     (let [x (interv x)
           y (interv y)

           x1 (:start x)
           x2 (:end   x)
           y1 (:start y)
           y2 (:end   y)]

       (Interval. (∞- x1 y2) (∞- x2 y1))))

  ([x y & more]
     (reduce interv- (interv- x y) more)))


(defn interv-'
  ([x]
     (let [x  (interv x)
           x1 (:start x)
           x2 (:end   x)]
       (Interval. (∞-' x2) (∞-' x1))))

  ([x y]
     (let [x (interv x)
           y (interv y)

           x1 (:start x)
           x2 (:end   x)
           y1 (:start y)
           y2 (:end   y)]

       (Interval. (∞-' x1 y2) (∞-' x2 y1))))

  ([x y & more]
     (reduce interv-' (interv-' x y) more)))


;; MULTIPLICATION

(defn interv*
  ([]  (interv 1))
  ([x] (interv x))
  ([x y]
     (let [x (interv x)
           y (interv y)

           x1 (:start x)
           x2 (:end   x)
           y1 (:start y)
           y2 (:end   y)

           x1y1 (∞* x1 y1)
           x1y2 (∞* x1 y2)
           x2y1 (∞* x2 y1)
           x2y2 (∞* x2 y2)]

       (Interval. (∞min x1y1 x1y2 x2y1 x2y2)
                  (∞max x1y1 x1y2 x2y1 x2y2))))

  ([x y & more]
     (reduce interv* (interv* x y) more)))


(defn interv*'
  ([]  (interv 1))
  ([x] (interv x))
  ([x y]
     (let [x (interv x)
           y (interv y)

           x1 (:start x)
           x2 (:end   x)
           y1 (:start y)
           y2 (:end   y)

           x1y1 (∞*' x1 y1)
           x1y2 (∞*' x1 y2)
           x2y1 (∞*' x2 y1)
           x2y2 (∞*' x2 y2)]

       (Interval. (∞min x1y1 x1y2 x2y1 x2y2)
                  (∞max x1y1 x1y2 x2y1 x2y2))))

  ([x y & more]
     (reduce interv*' (interv*' x y) more)))


;; DIVISION (CURRENT IMPL. EXCLUDES INTERVALS WITH 0 INSIDE)

(defn interv-div
  ([x] (interv-div 1 x))
  ([x y]
     (let [y  (interv y)
           y1 (:start y)
           y2 (:end   y)]

       (when (within-interv? y 0)
         (terror IllegalArgumentException "Can't divide by" y
                 "- it has 0 inside."))

       (interv* x (Interval. (∞div 1 y2) (∞div 1 y1)))))

  ([x y & more]
     (reduce interv-div (interv-div x y) more)))


(defn interv-div'
  ([x] (interv-div' 1 x))
  ([x y]
     (let [y  (interv y)
           y1 (:start y)
           y2 (:end   y)]

       (when (within-interv? y 0)
         (terror IllegalArgumentException "Can't divide by" y
                 "- it has 0 inside."))

       (interv*' x (Interval. (∞div 1 y2) (∞div 1 y1)))))

  ([x y & more]
     (reduce interv-div' (interv-div' x y) more)))


;; APPLYING MONOTONIC FUNCTIONS

(defn interv-apply
  "Under an assumption that f is monotonic on the whole intv range,
  calculates f(intv)."
  [f intv]
  (let [intv (interv intv)
        x1 (f (:start intv))
        x2 (f (:end   intv))]

    (Interval. (∞min x1 x2) (∞max x1 x2))))
