;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

;; AS/ORIGINAL PROTOCOL

(defprotocol Asable
  (as [type] [type obj]
    "Returns the obj wrapped with type instance. When called with type
    only returns an unary procedure that wraps it's argument with a
    type instance."))


(defprotocol Originable
  (original [obj] "An operator complementary to as."))


(extend-protocol Originable
  nil
  (original [_] nil)

  java.lang.Object
  (original [obj] obj))
