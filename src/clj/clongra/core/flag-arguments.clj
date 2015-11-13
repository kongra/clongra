;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-20

(in-ns 'clongra.core)

(defmacro flag-value
  [flag]
  (assert (symbol? flag) "The flag must be a symbol.")
  (let [sf (str flag)
        sf (if (.endsWith sf "?")
             (.substring sf 0 (- (.length sf) 1))
             sf)
        _ (assert (not-blank? sf)
                  "The flag symbol must not be empty nor be ?.")

        pred #{(keyword sf) `'~(symbol sf) sf true}]

    `(if (~pred ~flag) true false)))


(defmacro with-flag-value
  [flag & body]
  `(let [~flag (flag-value ~flag)]
     ~@body))


(defmacro if-flag
  ([flag then else]
     `(if (flag-value ~flag) ~then ~else))

  ([flag then]
     `(if (flag-value ~flag) ~then)))


(defmacro if-not-flag
  ([flag then else]
     `(if-not (flag-value ~flag) ~then ~else))

  ([flag then]
     `(if-not (flag-value ~flag) ~then)))


(defmacro when-flag
  [flag & body]
  `(when (flag-value ~flag) ~@body))


(defmacro when-not-flag
  [flag & body]
  `(when-not (flag-value ~flag) ~@body))
