;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2015-12-20

(ns clongra.oloops
  (:refer-clojure :exclude [dotimes])
  (:use [clongra.core])
  (:gen-class))

;; ENHANCED LOOPS WITH odo

(defmacro forever
  [& body]
  (let [v (gensym "v")]
    `(loop []
       (let [~v (odo ~@body)]
         (cond (obreak?  ~v) nil
               (oreturn? ~v) ~v
               :else (recur))))))


(defmacro dotimes
  "Works like clojure.core/dotimes, but uses odo over its body."
  [bindings & body]
  (assert (vector? bindings) "bindings must be a vector")
  (assert (= 2 (count bindings)) "bindings must have exactly 2 forms")
  (let [[i n] bindings
        v (gensym "v")
        N (gensym "n")]

    `(let [~N (long ~n)]
       (loop [~i 0]
         (when (< ~i ~N)
           (let [~v (odo ~@body)]
             (cond (obreak?  ~v) nil
                   (oreturn? ~v) ~v
                   :else (recur (unchecked-inc ~i)))))))))


(defmacro doarray
  [[i v array] & body]
  `(dotimes [~i (alength ~array)]
     (let [~v (aget ~array ~i)]
       ~@body)))
