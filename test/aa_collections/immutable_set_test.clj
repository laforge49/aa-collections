(ns aa-collections.immutable-set-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable-set :refer :all])
  (:import (clojure.lang Reversible IPersistentSet IPersistentCollection)))

(def x (aa-empty-set-node))

(println (nada? x))
(println (nada? nil))
(println (count x))
(println (to-str x))
(def x1 (insert x 1))
(println (to-str x1))
(def x13 (insert x1 3))
(println (to-str x13))
(def x123 (insert x13 2))
(println (to-str x123))
(def x1235 (insert x123 5))
(println (to-str x1235))
(def x12345 (insert x1235 4))
(println (to-str x12345))
(println (sfirst x12345))
(println (snext x12345 1))
(println (snext x12345 2))
(println (snext x12345 3))
(println (snext x12345 4))
(println (snext x12345 5))


