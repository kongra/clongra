;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2015-12-20

(ns clongra.oloops
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


;; (i/fori )
;; (i/doarri)
