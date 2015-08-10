(ns aa-collections.immutable-set-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable-set :refer :all])
  (:import (clojure.lang Reversible IPersistentSet IPersistentCollection)))

(def x (->AAImmutableSet))

(println (instance? Reversible x))
(println (instance? IPersistentSet x))
(println (instance? IPersistentCollection x))
(println (count x))
(println (seq x))
