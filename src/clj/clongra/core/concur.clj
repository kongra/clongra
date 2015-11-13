;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-05

(in-ns 'clongra.core)

;; CHANNEL NIL WRAPPER

(defonce ^:private CHANNEL-NIL ::CHANNEL-NIL)

(defn safe-chan-value
  [obj]
  (if (nil? obj) CHANNEL-NIL obj))


(defn unsafe-chan-value
  [obj]
  (if (identical? CHANNEL-NIL obj) nil obj))


;; TIMING OUT THE FUNCTION CALLS

(defonce ^:private ^:const TIMEOUT ::TIMEOUT)

(defn timeout?
  [obj]
  (identical? obj TIMEOUT))


(defn timeout-call
  "Calls the passed nullary function f and returns the value of the
  call. If the call fails to end within the specified timeout-msecs,
  the timeout value is being returned."
  [f timeout-msecs]
  (when-not (integer? timeout-msecs)
    (terror "Illegal timeout-msecs"
            timeout-msecs ". An integer required!!!"))

  (deref (future (f)) (long timeout-msecs) TIMEOUT))


(defn timeout-call!
  "Works like an ordinary timeout-call, but it does not allow the
  async thread to terminate when encountering the timeout."
  ([f timeout-msecs]
     (when-not (integer? timeout-msecs)
       (terror "Illegal timeout-msecs"
               timeout-msecs ". An integer required!!!"))

     (let [p  (promise)
           th (Thread. #(deliver p (f)))
           _  (.start th)
           result (deref p (long timeout-msecs) TIMEOUT)]

       (when (timeout? result)
         (.stop th))

       result)))


;; FUTURES UTILS

(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))


(defmacro wait-futures
  [n & exprs]
  `(doseq [f# (futures ~n ~@exprs)]
     @f#))
