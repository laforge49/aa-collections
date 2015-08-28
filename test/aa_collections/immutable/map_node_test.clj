(ns aa-collections.immutable.map-node-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable.map-node :refer :all]
            [aa-collections.immutable.map-iterator :refer :all])
  (:import (clojure.lang MapEntry)))

(def m0 (emty-node))

(def m1 (.insert m0 (new MapEntry "1" 1001)))
(pnodev m1 "m1")
(pnodev (.delete m1 "1") "m1 - 1")

(def m13 (.insert m1 (new MapEntry "3" 1003)))
(pnodev m13 "m13")
(println "m13 level" (.level m13))
(pnodev (.delete m13 "1") "m13 - 1")
(pnodev (.delete (.delete m13 "1") "3") "m13 - -")
(def m123 (.insert m13 (new MapEntry "2" 1002)))
(pnodev m123 "m123")
(pnodev (.delete m123 "1") "m123 - 1")
(pnodev (.delete m123 "2") "m123 - 2")
(pnodev (.delete m123 "3") "m123 - 3")
(pnodev (.insert m123 (new MapEntry "1" 1001)) "m123 + 1")
(pnodev (.insert m123 (new MapEntry "1" 1010)) "m123 + 1")

(println (new-map-seq m0))
(println (new-map-seq m1))
(println (new-map-seq m13))
(println (new-map-seq m123))

(println "")
(def mi (new-map-iterator m123))
(println (.hasNext mi))
(println (.next mi))
(println (.hasNext mi))
(println (.next mi))
(println (.hasNext mi))
(println (.next mi))
(println (.hasNext mi))

