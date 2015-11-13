;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2015-11-12

(ns clongra.core
    (:require [clojure.string             :as cstr ]
              [clojure.walk               :as cwalk]
              [clojure.math.combinatorics :as combo]
              [clojure.math.numeric-tower :as cmath])

    (:gen-class))

(load "core/constants")
(load "core/keywords")
(load "core/str")
(load "core/datetime")
(load "core/exceptions")
(load "core/log")
(load "core/assert")
(load "core/flag-arguments")
(load "core/ns")
(load "core/∞")
(load "core/labels")
(load "core/utils")
(load "core/intervals")
(load "core/eseqs")
(load "core/random")
(load "core/algo")
(load "core/dynvars")
(load "core/as")
(load "core/sys")
(load "core/doclean")
(load "core/1e4-primes")
(load "core/math")
(load "core/bitsbytes")
(load "core/search")
(load "core/resource")
(load "core/inflector")
(load "core/concur")
