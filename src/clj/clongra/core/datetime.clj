;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

;; DATE-TIME UTILITY FUNCTIONS BASED ON SEAN DEVLIN'S ABSTRACTION
;; GRAFTING http://vimeo.com/8801325 and
;; http://blip.tv/clojure/
;;    sean-devlin-protocol-xiii-clojure-protocols-explained-4540688

(defprotocol Timeable
  (^Long to-ms [t] "Converts the argument to milliseconds."))


(extend-protocol Timeable
  java.lang.Number
  (to-ms [x] x)

  java.util.Date
  (to-ms [d] (.getTime d))

  java.util.Calendar
  (to-ms [c] (.getTimeInMillis c))

  java.sql.Timestamp
  (to-ms [t] (.getTime t))

  nil
  (to-ms [_] nil)

  org.joda.time.DateTime
  (to-ms [dt] (.getMillis dt))

  clojure.lang.IPersistentMap
  (to-ms [obj]
    (let [default-map {:year 1970
                       :month 1
                       :day 1
                       :hour 1
                       :minute 0
                       :second 0
                       :ms 0}

          resulting-map (merge default-map obj)
          [y mo d h mi s ms] ((juxt :year :month :day :hour
                                    :minute :second :ms)
                              resulting-map)]

      (to-ms (org.joda.time.DateTime. (int y) (int mo) (int d)
                                      (int h) (int mi) (int s) (int ms))))))


(defn ^java.util.Date date
  ([]
     (java.util.Date.))
  ([time]
     (java.util.Date. (to-ms time))))


(defn ^java.util.GregorianCalendar greg-cal
  ([]
     (java.util.GregorianCalendar.))
  ([time]
     (doto (java.util.GregorianCalendar.)
       (.setTime (date time)))))


(defn ^java.sql.Timestamp sql-timestamp
  ([]
     (java.sql.Timestamp. (to-ms (date))))
  ([time]
     (java.sql.Timestamp. (to-ms time))))


(defn compare-time
  [a b]
  (.compareTo (date a) (date b)))


(defn time-before?
  "Tests to determine if time a is before time b"
  [a b]
  (= (compare-time a b) -1))


(defn time-after?
  "Tests to determine if time a is after time b"
  [a b]
  (= (compare-time a b) 1))


(defn ^org.joda.time.DateTime joda
  ([]
     (org.joda.time.DateTime/now))

  ([time]
     (org.joda.time.DateTime. (to-ms time))))


;; BENCHMARKING

(defmacro time*
  "Evaluates expr n times and returns a collection of times every
  evaluation took (in ms)."
  [n expr]
  `(doall (map (fn [param#]
		 (let [start# (. System (nanoTime))
		       ret# ~expr]
		   (/ (double (- (. System (nanoTime)) start#))
		      1000000.0)))

               (range ~n))))


(defmacro time-repeat*
  "Executes (time* n (dotimes [_ repeats] expr))."
  [n repeats expr]
  `(time* ~n (dotimes [i# ~repeats] ~expr)))


(defmacro microbench*
  "Evaluates the expression n number of times, returning the
  average time spent in computation, removing highest and lowest
  values.

  If the body of expr returns nil, only the timing is returned
  otherwise the result is printed - does not affect timing.

  Before timings begin, a warmup is performed lasting either 1
  minute or 1 full computational cycle, depending on which comes
  first.

  Thanks to Lau B. Jensen:
  http://www.bestinclass.dk/index.clj/2010/02/
                            benchmarking-jvm-languages.html"
  ([n expr] `(microbench ~n 30 ~expr))

  ([n wormup expr] {:pre [(> n 2)]}
     `(let [warm-up#  (let [start# (System/currentTimeMillis)]
			(println "Warming up for" ~wormup "[s] ...")
			(while (< (System/currentTimeMillis)
				  (+ start# (* ~wormup 1000)))
                          (with-out-str ~expr)
                          (System/gc))
			(println "Benchmarking..."))
	    timings#  (doall
		       (for [pass# (range ~n)]
			 (let [start#    (System/nanoTime)
			       retr#     ~expr
			       timing#   (/ (double
					     (- (System/nanoTime)
						start#))

					    1000000.0)]
			   (when retr# (println retr#))
			   (System/gc)
			   timing#)))
	    runtime#  (reduce + timings#)
	    highest#  (apply max timings#)
	    lowest#   (apply min timings#)]

	(println "Total runtime: " runtime# " msecs")
	(println "Highest time : " highest# " msecs")
	(println "Lowest time  : " lowest# " msecs")
	(println "Average      : " (/ (- runtime#
					 (+ highest# lowest#))
				      (- (count timings#) 2))
		 " msecs")
	timings#)))


(defmacro microbench-repeat*
  ([n repeats wormup expr]
     `(microbench* ~n ~wormup (dotimes [i# ~repeats] ~expr)))

  ([n repeats expr]
     `(microbench* ~n (dotimes [i# ~repeats] ~expr))))


(defn ^jclongra.core.Stopwatch stopwatch
  "Creates, starts and returns a new Stopwatch instance."
  []
  (jclongra.core.Stopwatch/start))


(extend-protocol Timeable
  jclongra.core.Stopwatch
  (to-ms [st] (.elapsedTime st)))


(def ^:dynamic *timer* nil)

(defn etime
  "Returns a time in msecs since the start of the timing."
  []
  (when-let [timer *timer*]
    (to-ms timer)))


(defn msecs
  "Returns a time string in msecs since the start of the timing."
  []
  (let [et (or (etime) "-")]
    (str et " msecs")))


(defmacro timing
  [& body]
  `(binding [*timer* (stopwatch)] ~@body))
