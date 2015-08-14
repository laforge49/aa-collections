(ns aa-collections.immutable-set-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable-set :refer :all])
  (:import (clojure.lang Reversible IPersistentSet IPersistentCollection)))

(def x (aa-empty-set-node))

(println (instance? Reversible x))
(println (instance? IPersistentSet x))
(println (instance? IPersistentCollection x))
(println (nada? x))
(println (nada? nil))
(println (count x))
(println (seq x))
(println (.-level x))
(println x (empty x))
(println (skew x))
(println (split x))
(println (revise x :level 1))
(println (new-node x nil 0 nil nil))
