;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2015-12-19

(ns clongra.locrefs
  (:refer-clojure :exclude [boolean byte short char int long
                            float double reset!])
  (:require [clojure.walk :as cwalk])
  (:gen-class))

;; FAST MUTABLE, THREAD-UNSAFE (LOCAL) REFS FOR JAVA PRIMITIVE TYPES.
;; NO IDENTITY SEMANTICS.
;; (:requite [clongra.locrefs :as lr])

;; CONSTRUCTORS

(defn boolean
  {:inline (fn [b] `(jclongra.locrefs.LRboolean. ~b))}
  [b]
  (jclongra.locrefs.LRboolean. (clojure.core/boolean b)))


(defn byte
  {:inline (fn [b] `(jclongra.locrefs.LRbyte. ~b))}
  [b]
  (jclongra.locrefs.LRbyte. (clojure.core/byte b)))


(defn short
  {:inline (fn [s] `(jclongra.locrefs.LRshort. ~s))}
  [s]
  (jclongra.locrefs.LRshort. (clojure.core/short s)))


(defn char
  {:inline (fn [c] `(jclongra.locrefs.LRchar. ~c))}
  [c]
  (jclongra.locrefs.LRchar. (clojure.core/char c)))


(defn int
  {:inline (fn [n] `(jclongra.locrefs.LRint. ~n))}
  [n]
  (jclongra.locrefs.LRint. (clojure.core/int n)))


(defn long
  {:inline (fn [n] `(jclongra.locrefs.LRlong. ~n))}
  [n]
  (jclongra.locrefs.LRlong. (clojure.core/long n)))


(defn float
  {:inline (fn [x] `(jclongra.locrefs.LRfloat. ~x))}
  [x]
  (jclongra.locrefs.LRfloat. (clojure.core/float x)))


(defn double
  {:inline (fn [x] `(jclongra.locrefs.LRdouble. ~x))}
  [x]
  (jclongra.locrefs.LRdouble. (clojure.core/double x)))


;; ACCESSORS

(defmacro value  [lr]       `(.value ~lr))
(defmacro reset! [lr value] `(.set ~lr ~value))

;; TRANSFORMATION

(defmacro over!
  [lr form]
  (if (seq? form)
    (let [[f & args] form]
      `(let [lr# ~lr]
         (.set lr# (~f (.value lr#) ~@args)) lr#))

    `(let [lr# ~lr]
       (.set lr# (~form (.value lr#))) lr#)))
