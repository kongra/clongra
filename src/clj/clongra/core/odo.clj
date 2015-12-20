;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2015-12-20

(in-ns 'clongra.core)

;; ENHANCED do (OPTIONAL do) WITH break AND TOP-LEVEL break (return)

(deftype ^:private OBreak  [])
(deftype ^:private OReturn [])

(def OBREAK  (OBreak. ))
(def ORETURN (OReturn.))

(defmacro obreak  [] `OBREAK )
(defmacro oreturn [] `ORETURN)

(defmacro obreak?  [x] `(ref= ~x ^Object OBREAK))
(defmacro oreturn? [x] `(ref= ~x ^Object ORETURN))

;; (defmacro obreak?  [x] `(identical? ~x OBREAK))
;; (defmacro oreturn? [x] `(identical? ~x ORETURN))

(defmacro odo
  ([]     nil)
  ([form] form)

  ([form & forms]
   (let [v (gensym "v")]
     `(let [~v ~form]
        (if (or (obreak? ~v) (oreturn? ~v))
          ~v
          (odo ~@forms))))))
