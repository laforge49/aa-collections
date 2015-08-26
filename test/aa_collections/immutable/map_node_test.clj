(ns aa-collections.immutable.map-node-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable.map-node :refer :all])
  (:import (clojure.lang MapEntry)))

(def m0 (emty-node))
(def m1 (.insert m0 (new MapEntry 1 1001)))
(pnodev m1 "m1")
