(ns aa-collections.immutable-set-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable-set :refer :all])
  (:import (clojure.lang Reversible IPersistentSet IPersistentCollection)))

(def a0 (aa-empty-set))
(def c0 #{})
(println"a0:" a0 c0)
(def b0 (seq a0))
(def d0 (seq c0))
(println "b0:" b0 d0)
(println "first b0:" (first b0) (first d0))
(println "rest b0:" (rest b0) (rest d0))
(println "next b0:" (next b0) (next d0))
(def a5 (conj a0 1 2 3 4 5))
(println "a5:" a5)
(println "seq a5:" (seq a5))
(println "?" (.count (.seq a5)))