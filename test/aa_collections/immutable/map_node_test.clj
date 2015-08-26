(ns aa-collections.immutable.map-node-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable.map-node :refer :all])
  (:import (clojure.lang MapEntry)))

(def m0 (emty-node))

(def m1 (.insert m0 (new MapEntry 1 1001)))
(pnodev m1 "m1")
(pnodev (.delete m1 1) "m1 - 1")

(def m13 (.insert m1 (new MapEntry 3 1003)))
(pnodev m13 "m13")
(pnodev (.delete m13 1) "m13 - 1")
